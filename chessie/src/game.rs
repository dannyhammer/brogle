use std::ops::Deref;

use anyhow::Result;

use super::{
    attacks_for, bishop_attacks, compute_attacks_to, compute_pinmasks_for, pawn_attacks,
    pawn_pushes, ray_between_exclusive, ray_between_inclusive, ray_containing, rook_attacks,
    Bitboard, Color, Move, MoveKind, MoveList, PieceKind, Position, Rank, Square, ZobristKey,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Game {
    pub(crate) position: Position,
    attacks_by_color: [Bitboard; Color::COUNT],
    attacks_by_square: [Bitboard; Square::COUNT],
    pinmasks: (Bitboard, Bitboard),
    checkers: Bitboard,
    discoverable_checks: Bitboard,
    legal_mobility: [Bitboard; Square::COUNT],
    history: Vec<ZobristKey>,
}

impl Game {
    /// Creates a new, empty [`Game`].
    pub fn new(position: Position) -> Self {
        let mut movegen = Self {
            position,
            attacks_by_color: [Bitboard::default(); Color::COUNT],
            pinmasks: (Bitboard::default(), Bitboard::default()),
            checkers: Bitboard::default(),
            discoverable_checks: Bitboard::default(),
            attacks_by_square: [Bitboard::default(); Square::COUNT],
            legal_mobility: [Bitboard::default(); Square::COUNT],
            history: Vec::with_capacity(128),
        };

        movegen.generate_pseudo_legal();

        movegen
    }

    /// Creates a new [`Game`] from the provided FEN string.
    pub fn from_fen(fen: &str) -> Result<Self> {
        Ok(Self::new_legal(Position::from_fen(fen)?))
    }

    /// Consumes `self` and returns a [`Game`] after having applied the provided [`Move`].
    pub fn with_move_made(mut self, mv: Move) -> Self {
        self.make_move(mv);
        self
    }

    /// Returns `true` if the game is in a position that is identical to the position it was in before.
    ///
    /// This is for checking "two-fold" repetition.
    ///
    ///
    /// # Example
    /// ```
    /// # use chessie::{Game, Move};
    /// let mut game = Game::default();
    /// game.make_move(Move::from_uci(&game, "b1a3").unwrap());
    /// assert_eq!(game.is_repetition(), false);
    /// game.make_move(Move::from_uci(&game, "b8a6").unwrap());
    /// assert_eq!(game.is_repetition(), false);
    /// game.make_move(Move::from_uci(&game, "a3b1").unwrap());
    /// assert_eq!(game.is_repetition(), false);
    /// game.make_move(Move::from_uci(&game, "a6b8").unwrap());
    /// assert_eq!(game.is_repetition(), true);
    /// ```
    pub fn is_repetition(&self) -> bool {
        for prev in self.history.iter().rev().skip(1).step_by(2) {
            if *prev == self.position().key() {
                return true;
            }
        }

        false
    }

    /// Applies the move, if it is legal to make. If it is not legal, returns an `Err` explaining why.
    pub fn make_move_checked(&mut self, mv: Move) -> Result<()> {
        self.check_legality_of(mv)?;
        self.make_move(mv);
        Ok(())
    }

    /// Applies the provided [`Move`]. No enforcement of legality.
    pub fn make_move(&mut self, mv: Move) {
        self.history.push(self.position().key());
        self.position = self.position().clone().with_move_made(mv);
        self.generate_legal();
    }

    /// Applies the provided [`Move`]s. No enforcement of legality.
    pub fn make_moves(&mut self, moves: impl IntoIterator<Item = Move>) {
        for mv in moves {
            self.make_move(mv);
        }
    }

    pub fn generate_pseudo_legal(&mut self) {
        let blockers = self.occupied();

        for square in blockers {
            let piece = self.piece_at(square).unwrap();
            let color = piece.color();

            let default_attacks = attacks_for(&piece, square, blockers);
            self.attacks_by_square[square] = default_attacks;
            self.attacks_by_color[color] |= default_attacks;
        }
    }

    pub fn new_legal(position: Position) -> Self {
        let mut movegen = Self::new(position);
        movegen.generate_legal();
        movegen
    }

    pub const fn position(&self) -> &Position {
        &self.position
    }

    pub const fn attacks_by_color(&self, color: Color) -> Bitboard {
        self.attacks_by_color[color.index()]
    }

    pub const fn attacks_by_square(&self, square: Square) -> Bitboard {
        self.attacks_by_square[square.index()]
    }

    pub const fn pinmasks(&self) -> (Bitboard, Bitboard) {
        self.pinmasks
    }

    pub const fn pinmask(&self) -> Bitboard {
        self.pinmask_ortho().or(self.pinmask_diag())
    }

    pub const fn pinmask_ortho(&self) -> Bitboard {
        self.pinmasks().0
    }

    pub const fn pinmask_diag(&self) -> Bitboard {
        self.pinmasks().1
    }

    pub const fn checkers(&self) -> Bitboard {
        self.checkers
    }

    /// Returns `true` if `square` is attacked by `color`.
    pub fn is_attacked_by(&self, square: Square, color: Color) -> bool {
        (self.attacks_by_color(color.opponent()) & square.bitboard()).is_nonempty()
    }

    pub const fn is_in_check(&self) -> bool {
        self.checkers().is_nonempty()
    }

    pub const fn is_in_double_check(&self) -> bool {
        self.checkers().population() > 1
    }

    pub fn legal_captures(&self) -> MoveList {
        self.legal_moves()
            .into_iter()
            .filter(|mv| mv.is_capture())
            .collect()
    }

    pub fn legal_moves(&self) -> MoveList {
        let mut moves = MoveList::default();

        let color = self.current_player();
        let king_square = self.king(color).to_square_unchecked();
        let enemy_or_empty = self.enemy_or_empty(color);
        let checkmask = match self.checkers.population() {
            // Not in check; checkmask is irrelevant
            0 => Bitboard::FULL_BOARD,
            // In single-check, so something must capture or block the check, or the king must leave check
            1 => {
                // Need to OR so that the attacking piece appears in the checkmask (Knights)
                ray_between_inclusive(king_square, self.checkers.to_square_unchecked())
                    | self.checkers
            }

            // In double-check, so only the King can move. Move him somewhere not attacked.
            _ => {
                let enemy_attacks = self.attacks_by_color(color.opponent());
                let attacks = self.attacks_by_square(king_square);
                let safe_squares = !(enemy_attacks | self.discoverable_checks);

                // Castling is illegal when in check, so just capture or evade
                for to in attacks & enemy_or_empty & safe_squares {
                    let kind = if self.has(to) {
                        MoveKind::Capture
                    } else {
                        MoveKind::Quiet
                    };

                    moves.push(Move::new(king_square, to, kind));
                }

                return moves;
            }
        };

        // let pinmask = self.pinmask();
        // eprintln!("PINMASK:\n{pinmask:?}");
        // eprintln!("CHECKMASK:\n{checkmask:?}");
        // eprintln!("ENEMY_OR_EMPTY:\n{enemy_or_empty:?}");

        // Pawns are... weird
        self.compute_pawn_moves(color, checkmask, &mut moves);

        // For sliding pieces, we need a blocker mask to compute pseudo-legal moves
        self.compute_normal_piece_moves(color, king_square, checkmask, &mut moves);
        self.compute_king_moves(color, self.occupied(), &mut moves); // TODO: legal_mask for King?

        moves
    }

    /*
    // TODO: https://github.com/dannyhammer/brogle/issues/9
    fn compute_pawn_moves(
        &self,
        color: Color,
        checkmask: Bitboard,
        moves: &mut MoveList,
    ) {
        // Fetch all pinned and unpinned pawns
        let pinned_pawns = self.pawns(color) & self.pinmask();
        let unpinned_pawns = self.pawns(color) & !self.pinmask();
        // eprintln!("PINNED PAWNS:\n{pinned_pawns:?}");
        // eprintln!("UNPINNED PAWNS:\n{unpinned_pawns:?}");

        // Pinned pawns may push along their pin ray
        let pinned_pushes = pinned_pawns.advance_by(color, 1) & self.pinmask();
        // Unpinned pawns may push normally
        let unpinned_pushes = unpinned_pawns.advance_by(color, 1);
        let pushes = pinned_pushes | unpinned_pushes;
        // eprintln!("PUSHES:\n{pushes:?}");

        // Cannot push outside of checkmask or into an occupied spot
        let legal_push_mask = !self.occupied() & checkmask;
        let single_pushes = pushes & legal_push_mask;
        // If it can push once, check if it's on the third rank. If so, it can push again.
        let third_rank = Bitboard::third_rank(color);
        let double_pushes = (single_pushes & third_rank).advance_by(color, 1) & legal_push_mask;

        // eprintln!("DOUBLE PUSHES:\n{double_pushes:?}");

        // Cannot capture outside of checkmask or into an empty or friendly spot
        let legal_enemies = self.color(color.opponent()) & checkmask;
        let east_captures = self.pawns(color).advance_by(color, 1).east() & legal_enemies;
        let west_captures = self.pawns(color).advance_by(color, 1).west() & legal_enemies;

        // Now generate the moves for these
        for to in single_pushes {
            let from = to.backward_by(color, 1).unwrap();
            if to.rank() == Rank::eighth(color) {
                moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Knight)));
                moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Bishop)));
                moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Rook)));
                moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Queen)));
            } else {
                moves.push(Move::new(from, to, MoveKind::Quiet));
            }
        }

        for to in double_pushes {
            let from = to.backward_by(color, 2).unwrap();
            moves.push(Move::new(from, to, MoveKind::Quiet));
        }

        for to in west_captures {
            let from = to.backward_by(color, 1).unwrap().left_by(color, 1).unwrap();

            if to.rank() == Rank::eighth(color) {
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Knight)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Bishop)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Rook)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)));
            } else {
                moves.push(Move::new(from, to, MoveKind::Quiet));
            }
        }

        for to in east_captures {
            let from = to
                .backward_by(color, 1)
                .unwrap()
                .right_by(color, 1)
                .unwrap();
            if to.rank() == Rank::eighth(color) {
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Knight)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Bishop)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Rook)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)));
            } else {
                moves.push(Move::new(from, to, MoveKind::Quiet));
            }
        }
    }
     */

    fn compute_pawn_moves(&self, color: Color, checkmask: Bitboard, moves: &mut MoveList) {
        // A Bitboard and Square of our King
        let king_bb = self.king(color);
        let king_square = king_bb.to_square_unchecked();

        // Enemy sliders; used for checking if EP is legal
        let enemy_rooks = self.orthogonal_sliders(color.opponent());
        let enemy_bishops = self.diagonal_sliders(color.opponent());

        for from in self.piece_parts(color, PieceKind::Pawn) {
            // The File / Rank / Diagonal containing our King and this Piece
            let pinning_ray = ray_containing(from, king_square);

            // Check if this piece is pinned along any of the pinmasks
            let is_pinned = self.pinmask().get(from);
            let pinmask = Bitboard::from_bool(!is_pinned) | pinning_ray;
            // println!("PAWN PINMASK ({is_pinned}):\n{pinmask:?}");

            // A pinned pawn's movement depends on its pin:
            //  - If pinned on a file, it can only push
            //  - If pinned on a rank, it cannot do anything
            //  - If pinned on a diagonal, it can only capture, and only along that diagonal

            // A Bitboard of this piece
            let piece_bb = from.bitboard();
            // All pseudo-legal attacks for this Pawn
            let attacks = pawn_attacks(from, color);

            // En passant happens so rarely, so we have an expensive check for its legality
            let ep_bb = if let Some(ep_square) = self.position().ep_square() {
                // Compute a blockers bitboard as if EP was performed.
                let ep_bb = ep_square.bitboard();
                let ep_target_bb = ep_bb.advance_by(color.opponent(), 1);
                let board_after_ep = (self.occupied() ^ ep_target_bb ^ piece_bb) | ep_bb;

                // If enemy sliders can attack our King, then EP is not legal to perform
                let checkers = (rook_attacks(king_square, board_after_ep) & enemy_rooks)
                    | (bishop_attacks(king_square, board_after_ep) & enemy_bishops);

                Bitboard::from_bool(checkers.is_empty()) & ep_bb
            } else {
                // eprintln!("EP IS NOT SAFE");
                Bitboard::default()
            };

            // eprintln!("EP:\n{ep_bb}");

            let enemy = self.color(color.opponent());
            // Check if this piece is pinned along any of the pinmasks

            // By default, a pawn can push forward two on it's starting rank, and one elsewhere
            // If there is a piece in front of this pawn, we cannot push two
            let all_but_this_pawn = self.occupied() ^ piece_bb;
            let shift_mask = all_but_this_pawn | all_but_this_pawn.advance_by(color, 1);
            let pushes = pawn_pushes(from, color) & !shift_mask;

            let pseudo_legal = (pushes & self.empty()) | (attacks & (enemy | ep_bb)); // Original

            // Note that we must OR with the checkmask just in case the king is being checked by a pawn that can be captured by EP
            let legal = pseudo_legal & (checkmask | ep_bb) & pinmask;

            for to in legal {
                let mut kind = if self.position().board().has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                // Special pawn cases
                if Some(to) == from.forward_by(color, 2) {
                    kind = MoveKind::PawnPushTwo;
                } else if to.file() != from.file() && self.position().board().piece_at(to).is_none()
                {
                    // A piece was NOT at the captured spot, so this was en passant
                    kind = MoveKind::EnPassantCapture;
                }

                // Regardless of whether this was a capture or quiet, it may be a promotion
                if to.rank() == Rank::eighth(color) {
                    if let MoveKind::Capture = kind {
                        // The pawn can reach the enemy's home rank and become promoted
                        kind = MoveKind::PromoCapt(PieceKind::Knight);
                        moves.push(Move::new(from, to, kind));
                        kind = MoveKind::PromoCapt(PieceKind::Bishop);
                        moves.push(Move::new(from, to, kind));
                        kind = MoveKind::PromoCapt(PieceKind::Rook);
                        moves.push(Move::new(from, to, kind));
                        // This gets pushed to the move list after this if-else chain
                        kind = MoveKind::PromoCapt(PieceKind::Queen);
                    } else {
                        // The pawn can reach the enemy's home rank and become promoted
                        kind = MoveKind::Promote(PieceKind::Knight);
                        moves.push(Move::new(from, to, kind));
                        kind = MoveKind::Promote(PieceKind::Bishop);
                        moves.push(Move::new(from, to, kind));
                        kind = MoveKind::Promote(PieceKind::Rook);
                        moves.push(Move::new(from, to, kind));
                        // This gets pushed to the move list after this if-else chain
                        kind = MoveKind::Promote(PieceKind::Queen);
                    }
                }

                moves.push(Move::new(from, to, kind));
            }
        }
    }

    fn compute_normal_piece_moves(
        &self,
        color: Color,
        king_square: Square,
        checkmask: Bitboard,
        moves: &mut MoveList,
    ) {
        // Loop over every square containing this piece
        let normal_pieces = self.color(color) ^ self.king(color) ^ self.pawns(color);

        // for from in self.piece_parts(color, PieceKind::Knight) {
        for from in normal_pieces {
            let pseudo_legal = self.attacks_by_square(from);

            // Check if this piece is pinned along any of the pinmasks
            let is_pinned = self.pinmask().get(from);
            let pinmask = Bitboard::from_bool(!is_pinned) | ray_containing(from, king_square);

            for to in pseudo_legal & checkmask & pinmask & self.enemy_or_empty(color) {
                let kind = if self.position().board().has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                moves.push(Move::new(from, to, kind));
            }
        }
    }

    fn compute_king_moves(&self, color: Color, blockers: Bitboard, moves: &mut MoveList) {
        // Loop over every square containing this piece
        for from in self.piece_parts(color, PieceKind::King) {
            let pseudo_legal = self.attacks_by_square(from);

            // A king can move anywhere that isn't attacked by the enemy
            let enemy_attacks = self.attacks_by_color(color.opponent());

            let castling_availability =
                |side: [Option<Square>; Color::COUNT], dst_square: Square| {
                    // Check if we can castle at all on this side
                    if let Some(rook_square) = side[color] {
                        // No squares between the King and his destination may be under attack
                        let must_be_safe = ray_between_inclusive(from, dst_square);
                        let is_safe = (must_be_safe & enemy_attacks).is_empty();

                        // All squares between the King and Rook must be empty
                        let must_be_clear = ray_between_exclusive(from, rook_square);
                        let is_clear = (must_be_clear & blockers).is_empty();

                        if is_safe && is_clear {
                            Bitboard::from_square(dst_square)
                        } else {
                            Bitboard::EMPTY_BOARD
                        }
                    } else {
                        Bitboard::EMPTY_BOARD
                    }
                };

            let kingside = castling_availability(
                self.position().castling_rights().kingside,
                Square::G1.rank_relative_to(color),
            );
            // eprintln!("kingside:\n{kingside}\n");

            let queenside = castling_availability(
                self.position().castling_rights().queenside,
                Square::C1.rank_relative_to(color),
            );
            // eprintln!("queenside:\n{queenside}\n");

            let attack_or_castle = pseudo_legal | kingside | queenside; // Any attacks or castling
            let safe_squares = !(enemy_attacks | self.discoverable_checks); // Not attacked by the enemy (even after King retreats)

            let enemy_or_empty = self.enemy_or_empty(color);
            let legal = attack_or_castle & enemy_or_empty & safe_squares;

            for to in legal {
                let mut kind = if self.position().board().has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                if from == Square::E1.rank_relative_to(color) {
                    if to == Square::G1.rank_relative_to(color)
                        && self.position().castling_rights().kingside[color].is_some()
                    {
                        kind = MoveKind::KingsideCastle;
                    } else if to == Square::C1.rank_relative_to(color)
                        && self.position().castling_rights().queenside[color].is_some()
                    {
                        kind = MoveKind::QueensideCastle;
                    }
                }

                moves.push(Move::new(from, to, kind));
            }
        }
    }

    pub fn generate_legal(&mut self) {
        let color = self.position().current_player();
        let king_square = self.position().board().king(color).to_square_unchecked();

        self.checkers = compute_attacks_to(self.position().board(), king_square, color.opponent());
        self.pinmasks = compute_pinmasks_for(self.position().board(), king_square, color);

        // These are the rays containing the King and his Checkers
        // They are used to prevent the King from retreating along a line he is checked on!
        // Note: A pawn can't generate a discoverable check, as it can only capture 1 square away.
        for checker in self.checkers & !self.position().board().kind(PieceKind::Pawn) {
            self.discoverable_checks |= ray_containing(king_square, checker) ^ checker.bitboard();
        }
    }
}

impl Deref for Game {
    type Target = Position;
    fn deref(&self) -> &Self::Target {
        &self.position
    }
}

impl Default for Game {
    fn default() -> Self {
        Self::new(Position::default())
    }
}
