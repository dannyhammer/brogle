use brogle_core::{ChessBoard, Color, Game, PieceKind};

use super::piece_square_tables::psq_eval;

/// Initial material value of all pieces of a given side (minus King)
const INITIAL_MATERIAL_VALUE: i32 = value_of(PieceKind::Pawn) * 8
    + value_of(PieceKind::Knight) * 2
    + value_of(PieceKind::Bishop) * 2
    + value_of(PieceKind::Rook) * 2
    + value_of(PieceKind::Queen);

/// A struct to encapsulate the logic of evaluating a chess position.
pub struct Evaluator<'a> {
    /// Game to evaluate.
    game: &'a Game,

    /// Percentage of game completion, in the range `[0, 100]`.
    ///
    /// Higher number means more of the opponent's pieces are missing
    endgame_weight: i32,
}

impl<'a> Evaluator<'a> {
    /// Create a new [`Evaluator`] instance to evaluate the supplied [`Game`].
    pub fn new(game: &'a Game) -> Self {
        let endgame_weight = endgame_weight(game.board(), game.current_player().opponent());

        Self {
            game,
            endgame_weight,
        }
    }

    /// Run the evaluator for the specified color.
    ///
    /// A positive number is good for `color`.
    /// A negative number is good for `color`'s opponent.
    pub fn eval(&self, color: Color) -> i32 {
        let friendly = self.compute_score_for(color);
        let enemy = self.compute_score_for(color.opponent());

        friendly - enemy
    }

    /// Run the evaluator, yielding a result that is positive if good for the current player and negative if good for the opponent.
    pub fn eval_current_player(&self) -> i32 {
        self.eval(self.game.current_player())
    }

    /// Compute the evaluation score for `color` only.
    pub fn compute_score_for(&self, color: Color) -> i32 {
        let material = count_material(self.game.board(), color);
        let mut positional = 0;

        for (tile, piece) in self.game.all_for(color) {
            positional += psq_eval(piece, tile, self.endgame_weight);
        }

        material + positional
    }
}

/// Computes the difference in material on the board.
///
/// If positive, `color` has more material.
/// If negative, `color.opponent()` has more material.
/// If zero, both sides have equal material.
pub const fn material_difference(board: &ChessBoard, color: Color) -> i32 {
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
///
/// Does NOT count the material of the King, as it cannot be removed from the board.
const fn count_material(board: &ChessBoard, color: Color) -> i32 {
    let mut score = 0;

    score += count_material_of(board, color, PieceKind::Pawn);
    score += count_material_of(board, color, PieceKind::Knight);
    score += count_material_of(board, color, PieceKind::Bishop);
    score += count_material_of(board, color, PieceKind::Rook);
    score += count_material_of(board, color, PieceKind::Queen);

    score
}

/// Counts the material value of the specified piece kind/color on the board.
const fn count_material_of(board: &ChessBoard, color: Color, kind: PieceKind) -> i32 {
    let pieces = board.piece_parts(color, kind);

    (pieces.population() as i32) * value_of(kind)
}

/// Computes the value of the remaining material for `color`.
const fn material_remaining(board: &ChessBoard, color: Color) -> i32 {
    INITIAL_MATERIAL_VALUE - count_material(board, color)
}

/// Divides the original material value of the board by the current material value, yielding an `f32` in the range `[0.0, 1.0]`
///
/// Lower numbers are closer to the beginning of the game. Higher numbers are closer to the end of the game.
///
/// The King is ignored when performing this calculation.
pub fn endgame_weight_percentage(board: &ChessBoard, color: Color) -> f32 {
    (material_remaining(board, color) as f32) / (INITIAL_MATERIAL_VALUE as f32)
}

/// Same as [`endgame_weight_percentage`], but returns an `i32` in the range `[0, 100]`
pub const fn endgame_weight(board: &ChessBoard, color: Color) -> i32 {
    (material_remaining(board, color) * 100 / INITIAL_MATERIAL_VALUE * 100) / 100
}
