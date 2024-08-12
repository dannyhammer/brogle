use brogle_core::{BitBoard, ChessBoard, Color, Game, PieceKind};

// use super::piece_square_tables::{CONTROL_CENTER, KING_SAFETY, PAWN_PUSH};

/// A struct to encapsulate the logic of evaluating a chess position.
pub struct Evaluator<'a> {
    game: &'a Game,
}

impl<'a> Evaluator<'a> {
    /// Create a new [`Evaluator`] instance to evaluate the supplied [`Game`].
    pub fn new(game: &'a Game) -> Self {
        Self { game }
    }

    /// Run the evaluator, yielding a result that is positive if good for White and negative if good for Black.
    ///
    /// Presently, this just computes the material difference.
    pub fn eval(self) -> i32 {
        let color = self.game.current_player();
        material_difference(self.game.board(), color)
    }

    /*
    /// Run a complex evaluation, checking some basic Piece-Square Tables.
    pub fn complex_eval(self) -> i32 {
        let color = self.game.current_player();
        self.eval_for(color)
    }
     */

    /*
    /// Evaluates the board from `color`'s perspective. A positive number is good here.
    fn eval_for(self, color: Color) -> i32 {
        let board = self.game.board();
        let mat = material_difference(board, color);
        let game_percentage = material_remaining_percentage(board, color.opponent());

        let restrict_king = restrict_enemy_king_movement(board, color, game_percentage);

        let pawn_pushes = {
            let ours = PAWN_PUSH.apply_for(board, color, Some(PieceKind::Pawn));
            let theirs = PAWN_PUSH.apply_for(board, color.opponent(), Some(PieceKind::Pawn));
            ours - theirs
        };

        let king_safety = ({
            let ours = KING_SAFETY.apply_for(board, color, Some(PieceKind::King));
            let theirs = KING_SAFETY.apply_for(board, color.opponent(), Some(PieceKind::King));
            ours - theirs
        } as f32
            * 1.0
            - game_percentage) as i32;

        let center_control = {
            let ours = CONTROL_CENTER.apply_for(board, color, None);
            let theirs = CONTROL_CENTER.apply_for(board, color.opponent(), None);
            ours - theirs
        };

        mat + pawn_pushes + king_safety + center_control + restrict_king
    }
     */
}

/// Computes the difference in material on the board.
///
/// If positive, White has more material.
/// If negative, Black has more material.
/// If zero, both sides have equal material.
const fn material_difference(board: &ChessBoard, color: Color) -> i32 {
    let friendly = count_material(board, color);
    let enemy = count_material(board, color.opponent());

    friendly - enemy
}

/// Returns a value of the provided `PieceKind`.
///
/// Values are obtained from here: <https://www.chessprogramming.org/Simplified_Evaluation_Function>
pub const fn value_of(kind: PieceKind) -> i32 {
    match kind {
        PieceKind::Pawn => 100,
        PieceKind::Knight => 320,
        PieceKind::Bishop => 330,
        PieceKind::Rook => 500,
        PieceKind::Queen => 900,
        PieceKind::King => 20_000,
    }
}

/// Counts the material value of all pieces of the specified color.
const fn count_material(board: &ChessBoard, color: Color) -> i32 {
    let mut score = 0;
    let color = board.color(color);

    score += count_material_of(board, color, PieceKind::Pawn);
    score += count_material_of(board, color, PieceKind::Knight);
    score += count_material_of(board, color, PieceKind::Bishop);
    score += count_material_of(board, color, PieceKind::Rook);
    score += count_material_of(board, color, PieceKind::Queen);

    score
}

/// Counts the material value of the specified piece kind/color on the board.
const fn count_material_of(board: &ChessBoard, color: BitBoard, kind: PieceKind) -> i32 {
    let value = value_of(kind);
    let pieces = color.and(board.kind(kind));
    (pieces.population() as i32) * value
}

/*
/// Grants a bonus to positions where the king of `enemy_color` is restricted in his movement,
/// such as being forced into a corner.
fn restrict_enemy_king_movement(board: &ChessBoard, color: Color, endgame_weight: f32) -> i32 {
    let mut score = 0;

    let enemy_king = board.king(color.opponent()).to_tile_unchecked();
    let friendly_king = board.king(color).to_tile_unchecked();

    // Force enemy King away from center, into corners
    score += enemy_king.distance_to(Tile::E5);

    // Push our King towards enemy King
    score += (Tile::A1.distance_to(Tile::H8) - friendly_king.distance_to(enemy_king)) * 10;

    (score as f32 * endgame_weight * 10.0) as i32
}

/// Divides the original material value of the board by the current material value, yielding a percentage.
///
/// Lower numbers are closer to the beginning of the game. Higher numbers are closer to the end of the game.
///
/// The King is ignored when performing this calculation.
fn material_remaining_percentage(board: &ChessBoard, color: Color) -> f32 {
    let original = value_of(PieceKind::Pawn) * 8
        + value_of(PieceKind::Knight) * 2
        + value_of(PieceKind::Bishop) * 2
        + value_of(PieceKind::Rook) * 2
        + value_of(PieceKind::Queen);

    let current = count_material(board, color);

    if current == 0 {
        1.0
    } else {
        1.0 - current as f32 / original as f32
    }
}
 */
