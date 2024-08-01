use dutchess_core::{BitBoard, ChessBoard, Color, PieceKind, Position, Tile};

use super::piece_square_tables::{CONTROL_CENTER, KING_SAFETY, PAWN_PUSH};

pub struct Evaluator<'a> {
    position: &'a Position,
}

impl<'a> Evaluator<'a> {
    pub fn new(position: &'a Position) -> Self {
        Self { position }
    }

    pub fn eval(self) -> i32 {
        let color = self.position.current_player();
        material_difference(self.position.board(), color)
    }

    pub fn complex_eval(self) -> i32 {
        let color = self.position.current_player();
        self.eval_for(color)
    }

    fn eval_for(self, color: Color) -> i32 {
        let board = self.position.board();
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
}

const fn material_difference(board: &ChessBoard, color: Color) -> i32 {
    let friendly = count_material(board, color);
    let enemy = count_material(board, color.opponent());

    friendly - enemy
}

pub const fn value_of(kind: PieceKind) -> i32 {
    match kind {
        PieceKind::Pawn => 100,
        PieceKind::Knight => 300,
        PieceKind::Bishop => 310,
        PieceKind::Rook => 500,
        PieceKind::Queen => 900,
        PieceKind::King => 2000,
    }
}

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

const fn count_material_of(board: &ChessBoard, color: BitBoard, kind: PieceKind) -> i32 {
    let value = value_of(kind);
    let pieces = color.and(board.kind(kind));
    (pieces.population() as i32) * value
}

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
        + value_of(PieceKind::Queen) * 1;

    let current = count_material(board, color);

    if current == 0 {
        1.0
    } else {
        1.0 - current as f32 / original as f32
    }
}
