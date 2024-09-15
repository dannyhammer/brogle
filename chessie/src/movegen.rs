use std::fmt::{self, Write};

use arrayvec::ArrayVec;
use types::Rank;

use super::{
    attacks_for, bishop_attacks, compute_attacks_to, compute_pinmasks_for, pawn_attacks,
    pawn_pushes, ray_between_exclusive, ray_between_inclusive, ray_containing, rook_attacks,
    Bitboard, Color, Move, MoveKind, MoveList, Piece, PieceKind, Position, Square,
};

/// An iterator over all of the moves for a given chess position.
///
/// This structure initially computes board information such as checkers and pinned pieces,
/// and only generates a new move when the `.next()` method is called while iterating over
/// this struct.
#[derive(Clone, PartialEq, Eq)]
pub struct MoveGen<'a> {
    /// The position for which to generate moves.
    position: &'a Position,

    /// The move generator will only generate moves that place pieces on squares defined in this mask.
    ///
    /// By default, this is set to empty and enemy-occupied squares.
    to_mask: Bitboard,

    /// The move generator will only generate moves that moves pieces from squares defined in this mask.
    ///
    /// By default, this is set to friendly-occupied squares.
    /// However, if the King is in double-check, this is set to a bitboard of only the King's square,
    /// and cannot be overridden.
    from_mask: Bitboard,

    /// The current square whose piece we are generating moves for.
    current_square: Square,

    /// The remaining mobility of the piece at `self.current_square`.
    ///
    /// The least-significant bit is removed every time we enumerate a move from within this bitboard.
    current_mobility: Bitboard,

    /// An index into a list of [`PieceKind`] values that a Pawn can promote to.
    ///
    /// This is used to properly enumerate all possible promotions for a Pawn.
    current_promotion_index: u8,

    /// All squares whose pieces are attacking the side-to-move's King.
    checkers: Bitboard,

    /// If `self.checkers` is empty, this is [`Bitboard::FULL_BOARD`].
    /// Otherwise, it is the path from every checker to the side-to-move's King.
    checkmask: Bitboard,

    /// All rays from enemy sliders to the side-to-move's King where there is only a single piece preventing check.
    pinmask: Bitboard,

    /// If the side-to-move is in Check, this contains the rays between the enemy sliding checkers and the side-to-move's King.
    ///
    /// This is used to prevent the King from "retreating" along the ray that the enemy slider attacks.
    discoverable_checks: Bitboard,

    /// All squares (pseudo-legally) attacked by a specific color.
    attacks_by_color: [Bitboard; Color::COUNT],

    /// Pseudo-legal attacks from every given square on the board.
    attacks_by_square: [Bitboard; Square::COUNT],

    /// The square where the side-to-move's King resides.
    king_square: Square,

    /// A list to store all moves as they are generated.
    ///
    /// This is useful for when you want to generate a certain number of moves and then re-use them without re-generating them.
    moves: MoveList,
}

impl<'a> MoveGen<'a> {
    /// Create a new [`MoveGen`] that will generate all possible legal moves.
    ///
    /// This will compute data masks for checkers, pinmasks, etc., but will not
    /// compute any legal moves. That is accomplished by calling the `.next()`
    /// on this struct as an iterator.
    pub fn new(position: &'a Position) -> Self {
        let color = position.current_player();
        let king_square = position.king(color).to_square_unchecked();

        // Checkmask and pinmasks for legal move generation
        let mut discoverable_checks = Bitboard::EMPTY_BOARD;
        let checkers = compute_attacks_to(position, king_square, color.opponent());
        let (pinmask_ortho, pinmask_diag) = compute_pinmasks_for(position, king_square, color);

        // These are the rays containing the King and his Checkers
        // They are used to prevent the King from retreating along a line he is checked on!
        // Note: A pawn can't generate a discoverable check, as it can only capture 1 square away.
        for checker in checkers & position.sliders(color.opponent()) {
            discoverable_checks |= ray_containing(king_square, checker) ^ checker;
        }

        // If there are no checkers, the checkmask is the whole board
        let checkmask = if checkers.is_empty() {
            Bitboard::FULL_BOARD
        }
        // Otherwise, the checkmask is the path from the checker(s) to the King
        else {
            let mut checkmask = Bitboard::EMPTY_BOARD;

            // There is *usually* only one checker, so this rarely loops.
            for checker_square in checkers {
                checkmask |= ray_between_exclusive(king_square, checker_square);
            }

            // OR with the checkers to include them in the checkmask (knights)
            checkmask | checkers
        };

        // Can only go to squares that are empty or enemy-occupied
        let to_mask = position.enemy_or_empty(color);

        // If we're in double check, we can only generate moves for the King
        let from_mask = if checkers.population() > 1 {
            king_square.bitboard()
        } else {
            position.color(color)
        };

        // Compute attack/defend maps by square and color
        let blockers = position.occupied();
        let mut attacks_by_color = [Bitboard::default(); Color::COUNT];
        let mut attacks_by_square = [Bitboard::default(); Square::COUNT];
        for square in blockers {
            let piece = position.piece_at(square).unwrap();
            let color = piece.color();

            let default_attacks = attacks_for(&piece, square, blockers);
            attacks_by_square[square] = default_attacks;
            attacks_by_color[color] |= default_attacks;
        }

        Self {
            position,
            current_square: from_mask.lsb().unwrap_or_default(),
            current_mobility: Bitboard::EMPTY_BOARD,
            current_promotion_index: 0,
            to_mask,
            from_mask,
            checkers,
            checkmask,
            pinmask: pinmask_diag | pinmask_ortho,
            discoverable_checks,
            attacks_by_color,
            attacks_by_square,
            king_square,
            moves: ArrayVec::default(),
        }
    }

    /*
    /// Create a new [`MoveGen`] that will generate all possible legal moves.
    pub fn new_legal() -> Self {
        Self::new().only_legal()
    }

    /// Consumes `self`, returning a [`MoveGen`] that only generates legal moves.
    pub fn only_legal(mut self) -> Self {
        // TODO: Refactor `MoveGen::new` to not compute checkmask, pinmasks, and other legality data, and move that logic here.
        // That way, we can create a `MoveGen` that generates pseudo-legal moves, and one that generates legal moves.
        self
    }
     */

    /// Consumes `self`, returning a [`MoveGen`] that only generates moves _from_ the squares in `mask`.
    ///
    /// Note that if the position is currently in double check, the provided mask will have no effect,
    /// as only the King is allowed to move during a double check.
    pub fn only_moves_from(mut self, mask: impl Into<Bitboard>) -> Self {
        // If in double check, we cannot generate moves for anything other than the King
        self.from_mask = if self.checkers.population() <= 1 {
            mask.into()
        } else {
            self.king_square.bitboard()
        };

        self
    }

    /// Consumes `self`, returning a [`MoveGen`] that only generates moves _to_ the squares in `mask`.
    pub fn only_moves_to(mut self, mask: impl Into<Bitboard>) -> Self {
        self.to_mask = mask.into();
        self
    }

    /// Consumes `self`, returning a [`MoveGen`] that only generates moves that capture enemy pieces.
    pub fn only_captures(self) -> Self {
        let opponent = self.position.current_player().opponent();
        let mask = self.position.color(opponent);
        self.only_moves_to(mask)
    }

    /// Consumes `self`, returning a list of all legal moves.
    pub fn legal_moves(self) -> MoveList {
        // let color = self.position.current_player();
        // let enemy_or_empty = self.position.enemy_or_empty(color);

        // In double-check, so only the King can move. Move him somewhere not attacked.
        /*
        if self.checkers.population() > 1 {
            let enemy_attacks = self.attacks_by_color[color.opponent()];
            let attacks = self.attacks_by_square[self.king_square];
            let safe_squares = !(enemy_attacks | self.discoverable_checks);

            // Castling is illegal when in check, so just capture or evade
            for to in attacks & enemy_or_empty & safe_squares {
                let kind = if self.position.has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                self.moves.push(Move::new(self.king_square, to, kind));
            }

            return self.moves;
        }
         */

        /*
        for from in self.from_mask {
            self.current_square = from;
            let piece = self.position.piece_at(from).unwrap();
            self.current_mobility = self.generate_legal_mobility_for(piece, from) & self.to_mask;
            for to in self.current_mobility {
                let mv = self.serialize_move(from, to);
                self.moves.push(mv);

                // If a Pawn promotion occurred, we may need to enumerate more promotions
                if mv.is_promotion() {
                    // Increase the promotion index
                    self.current_promotion_index += 1;

                    // If we've exceeded the maximum number of possible promotions, we're done enumerating promotions
                    if self.current_promotion_index >= PROMOTIONS.len() {
                        self.current_promotion_index = 0;
                        self.current_mobility.clear_lsb();
                    }
                }
                // Otherwise, we can just clear the LSB and enumerate the move to the next square
                else {
                    self.current_mobility.clear_lsb();
                }
            }
        }
          */

        self.collect()
        // self.moves
    }

    /// Consumes `self`, returning a list of all legal captures.
    pub fn legal_captures(self) -> MoveList {
        self.only_captures().collect()
    }

    /// Fetches a list of all moves generated so far.
    pub fn generated_moves(&self) -> &MoveList {
        &self.moves
    }

    /// Mutably fetches a list of all moves generated so far.
    pub fn generated_moves_mut(&mut self) -> &mut MoveList {
        &mut self.moves
    }

    pub const fn is_in_check(&self) -> bool {
        self.checkers.population() > 0
    }

    /// Generates the next legal move for the current position.
    fn generate_next_move(&mut self) -> Option<Move> {
        // If we've enumerated all available moves for the current piece, move on to the next piece in the "from" mask.
        // We loop here because the piece at the next square in the "from" mask may not have any legal moves, so we must skip it.
        while self.current_mobility.is_empty() {
            self.current_square = self.from_mask.pop_lsb()?;
            let piece = self.position.piece_at(self.current_square)?; // We could safely `.unwrap()` this because there's guaranteed to be a piece here, but this looks cleaner

            // Mask off anything not in the "to" mask
            self.current_mobility =
                self.generate_legal_mobility_for(piece, self.current_square) & self.to_mask;
        }

        // Create a `Move` struct from the move data.
        let to = self.current_mobility.lsb()?; // We could safely `.unwrap()` here because of the loop above that ensures `self.current_mobility` is nonempty.
        let mv = self.serialize_move(self.current_square, to);

        // If a Pawn promotion occurred, we may need to enumerate more promotions
        if mv.is_promotion() {
            // Increase the promotion index
            self.current_promotion_index += 1;

            // If we've exceeded the maximum number of possible promotions, we're done enumerating promotions
            if self.current_promotion_index >= 4 {
                self.current_promotion_index = 0;
                self.current_mobility.clear_lsb();
            }
        }
        // Otherwise, we can just clear the LSB and enumerate the move to the next square
        else {
            self.current_mobility.clear_lsb();
        }

        // Store the move in the ever-growing list of generated moves.
        self.moves.push(mv);
        Some(mv)
    }

    /// Constructs a [`Move`], given the `from` and `to` squares, as well as the struct's internal context.
    fn serialize_move(&self, from: Square, to: Square) -> Move {
        let piece = self.position.piece_at(from).unwrap(); // Safe unwrap because there must be a piece here for us to move.
        let color = self.position.current_player();

        // By default, the move is either quiet or a capture
        let kind = if self.position.has(to) {
            MoveKind::Capture
        } else {
            MoveKind::Quiet
        };

        // Pawns have a lot of special cases
        if piece.is_pawn() {
            // If `to` is two squares ahead, it's a Pawn double-push
            let kind = if Some(to) == from.forward_by(color, 2) {
                MoveKind::PawnPushTwo
            }
            // If the Pawn reached the back rank, it's a promotion
            else if to.rank() == Rank::eighth(color) {
                // Fetch the kind of piece to which this Pawn will be promoted
                let promotion = match self.current_promotion_index {
                    0 => PieceKind::Queen,
                    1 => PieceKind::Knight,
                    2 => PieceKind::Rook,
                    3 => PieceKind::Bishop,
                    _ => unreachable!(),
                };

                // If there was a piece at the destination, it's a promotion and a capture
                if kind == MoveKind::Capture {
                    MoveKind::PromoCapt(promotion)
                } else {
                    MoveKind::Promote(promotion)
                }
            }
            // If the Pawn changed files and there isn't a piece at the destination, it's en passant
            else if to.file() != from.file() && kind == MoveKind::Quiet {
                MoveKind::EnPassantCapture
            }
            // If all else failed, it's a quiet single pawn push
            else {
                kind
            };

            Move::new(from, to, kind)
        }
        // The King only has one special case- castling
        else if piece.is_king() {
            // If the King is moving from his start square, it could be castling
            let kind = if from == Square::E1.rank_relative_to(color) {
                let castling_rights = self.position.castling_rights();

                // If we can short castle, and the destination square is either our short-side Rook (for Chess960)
                //  or the standard G1 (color-relative), then this is a short castle.
                if castling_rights.kingside[color]
                    .is_some_and(|sq| to == sq || to == Square::G1.rank_relative_to(color))
                {
                    MoveKind::KingsideCastle
                }
                // If we can long castle, and the destination square is either our long-side Rook (for Chess960)
                //  or the standard C1 (color-relative), then this is a long castle.
                else if castling_rights.queenside[color]
                    .is_some_and(|sq| to == sq || to == Square::C1.rank_relative_to(color))
                {
                    MoveKind::QueensideCastle
                }
                // Otherwise, this isn't a castling move.
                else {
                    kind
                }
            }
            // If the King was not moving from his starting square, this cannot be castling.
            else {
                kind
            };

            Move::new(from, to, kind)
        }
        // All other pieces have simple moves- quiets or captures
        else {
            Move::new(from, to, kind)
        }
    }

    /// Generates a [`Bitboard`] of all legal moves for `piece` at `square`.
    fn generate_legal_mobility_for(&self, piece: Piece, square: Square) -> Bitboard {
        let color = piece.color();

        // Only Pawns and Kings have special movement
        match piece.kind() {
            PieceKind::Pawn => self.generate_legal_pawn_mobility(color, square),
            PieceKind::King => self.generate_legal_king_mobility(color, square),
            PieceKind::Knight | PieceKind::Bishop | PieceKind::Rook | PieceKind::Queen => {
                self.generate_legal_normal_piece_mobility(color, square)
            }
        }
    }

    /// Generates a [`Bitboard`] of all legal moves for a Pawn at `square`.
    fn generate_legal_pawn_mobility(&self, color: Color, square: Square) -> Bitboard {
        let blockers = self.position.occupied();

        // Pinned pawns are complicated:
        // - A pawn pinned horizontally cannot move. At all.
        // - A pawn pinned vertically can only push forward, not capture.
        // - A pawn pinned diagonally can only capture it's pinner.
        let is_pinned = self.pinmask.get(square);
        let pinmask = Bitboard::from_bool(!is_pinned) | ray_containing(square, self.king_square);

        // If en passant can be performed, check its legality.
        // If not, default to an empty bitboard.
        let ep_bb = self
            .position
            .ep_square()
            .map(|ep_square| self.generate_ep_bitboard(color, square, ep_square))
            .unwrap_or_default();

        // Get a mask for all possible pawn double pushes.
        let all_but_this_pawn = blockers ^ square;
        let double_push_mask = all_but_this_pawn | all_but_this_pawn.advance_by(color, 1);
        let pushes = pawn_pushes(square, color) & !double_push_mask & !blockers;

        // Attacks are only possible on enemy occupied squares, or en passant.
        let enemies = self.position.color(color.opponent());
        let attacks = pawn_attacks(square, color) & (enemies | ep_bb);

        (pushes | attacks) & (self.checkmask | ep_bb) & pinmask
    }

    /// Generate a [`Bitboard`] for the legality of performing an en passant capture with the Pawn at `square`.
    ///
    /// If en passant is legal, the returned bitboard will have a single bit set, representing a legal capture for the Pawn at `square`.
    /// If en passant is not legal, the returned bitboard will be empty.
    fn generate_ep_bitboard(&self, color: Color, square: Square, ep_square: Square) -> Bitboard {
        // If this Pawn isn't on an adjacent file and the same rank as the enemy Pawn that caused en passant to be possible, it can't perform en passant
        if square.distance_ranks(ep_square) != 1 || square.distance_files(ep_square) != 1 {
            return Bitboard::default();
        }

        // Compute a blockers bitboard as if EP was performed.
        let ep_bb = ep_square.bitboard();
        let ep_target_bb = ep_bb.retreat_by(color, 1);
        let blockers_after_ep = (self.position.occupied() ^ ep_target_bb ^ square) | ep_bb;

        // If, after performing EP, any orthogonal sliders can attack our King, EP is not legal
        let enemy_ortho_sliders = self.position.orthogonal_sliders(color.opponent());
        let enemy_diag_sliders = self.position.diagonal_sliders(color.opponent());

        // If enemy sliders can attack our King, then EP is not legal to perform
        let possible_checkers = (rook_attacks(self.king_square, blockers_after_ep)
            & enemy_ortho_sliders)
            | (bishop_attacks(self.king_square, blockers_after_ep) & enemy_diag_sliders);

        // eprintln!("EP for {color} Pawn at {square}->{ep_square} is safe!");

        Bitboard::from_bool(possible_checkers.is_empty()) & ep_bb
    }

    /// Generates a [`Bitboard`] of all legal moves for the King at `square`.
    fn generate_legal_king_mobility(&self, color: Color, square: Square) -> Bitboard {
        let attacks = self.attacks_by_square[square];

        // If in check, we cannot castle- we can only attack with the default movement of the King.
        let castling = if self.checkers.is_empty() {
            // Otherwise, compute castling availability like normal
            let kingside = self.position.castling_rights().kingside[color].map(|rook_sq| {
                self.generate_castling_bitboard(color, rook_sq, Square::G1.rank_relative_to(color))
            });

            let queenside = self.position.castling_rights().queenside[color].map(|rook_sq| {
                self.generate_castling_bitboard(color, rook_sq, Square::C1.rank_relative_to(color))
            });

            kingside.unwrap_or_default() | queenside.unwrap_or_default()
        } else {
            Bitboard::default()
        };

        // Safe squares are ones not attacked by the enemy or part of a discoverable check
        let safe_squares = !(self.attacks_by_color[color.opponent()] | self.discoverable_checks); // Not attacked by the enemy (even after King retreats)

        // All legal attacks that are safe and not on friendly squares
        (attacks | castling) & safe_squares & self.position.enemy_or_empty(color)
    }

    /// Generate a bitboard for `color`'s ability to castle with the Rook on `rook_square`, which will place the King on `dst_square`.
    fn generate_castling_bitboard(
        &self,
        color: Color,
        rook_square: Square,
        dst_square: Square,
    ) -> Bitboard {
        // All squares between the King and Rook must be empty
        let blockers = self.position.occupied();
        let squares_that_must_be_empty = ray_between_exclusive(self.king_square, rook_square);
        let squares_are_empty = (squares_that_must_be_empty & blockers).is_empty();

        // All squares between the King and his destination must not be attacked
        let enemy_attacks = self.attacks_by_color[color.opponent()];
        let squares_that_must_be_safe = ray_between_inclusive(self.king_square, dst_square);
        let squares_are_safe = (squares_that_must_be_safe & enemy_attacks).is_empty();

        Bitboard::from_square(dst_square)
            & Bitboard::from_bool(squares_are_empty && squares_are_safe)
    }

    /// Generates a [`Bitboard`] of all legal moves for a non-Pawn and non-King piece at `square`.
    fn generate_legal_normal_piece_mobility(&self, color: Color, square: Square) -> Bitboard {
        // Check if this piece is pinned along any of the pinmasks
        let is_pinned = self.pinmask.get(square);
        let pinmask = Bitboard::from_bool(!is_pinned) | ray_containing(square, self.king_square);

        // Pseudo-legal attacks that are within the check/pin mask and attack non-friendly squares
        self.attacks_by_square[square]
            & self.checkmask
            & pinmask
            & self.position.enemy_or_empty(color)
    }
}

impl<'a> Iterator for MoveGen<'a> {
    type Item = Move;
    fn next(&mut self) -> Option<Self::Item> {
        self.generate_next_move()
    }
}

impl<'a> fmt::Debug for MoveGen<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let format = |to_fmt: &[(Bitboard, &str)]| {
            let strings = to_fmt
                .iter()
                .map(|(b, s)| (b.to_string(), s))
                .collect::<Vec<_>>();

            let splits = strings
                .iter()
                .map(|(b, _)| b.split('\n').collect::<Vec<_>>())
                .collect::<Vec<_>>();

            let labels = strings.iter().fold(String::new(), |mut acc, (_, s)| {
                _ = write!(acc, "{s:10}\t\t");
                acc
            });

            let boards = (0..8).fold(String::new(), |mut acc, i| {
                _ = writeln!(
                    acc,
                    "{}",
                    (0..splits.len()).fold(String::new(), |mut output, j| {
                        _ = write!(output, "{}\t", splits[j][i]);
                        output
                    })
                );
                acc
            });

            format!("{labels}\n{boards}")
        };

        let color = self.position.current_player();

        let check_data = format(&[
            (self.attacks_by_color[color.opponent()], "Enemy attacks"),
            (self.checkers, "Checkers"),
            (self.checkmask, "Checkmask"),
            (self.pinmask, "Pinmask"),
            (self.discoverable_checks, "Disc. Checks"),
        ]);

        let mobility_data = format(&[
            (self.position.color(color), "Friendlies"),
            (self.position.color(color.opponent()), "Enemies"),
            (self.from_mask, "\"From\" squares"),
            (self.to_mask, "\"To\" squares"),
            (
                self.current_mobility,
                &format!("Mobility at {}", self.current_square),
            ),
        ]);

        write!(
            f,
            "Position:\n{:?}\n\n{check_data}\n\n{mobility_data}",
            self.position
        )
    }
}

/*
pub struct Moves {
    piece: Piece,
    square: Square,
    mobility: Bitboard,
}

pub struct MovesIter {
    moves: Moves,
    promotion: u8,
}
 */
