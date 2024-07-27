use dutchess_core::{BitBoard, ChessBoard, Color, PieceKind, Position};

use super::piece_square_tables::{CONTROL_CENTER, KING_SAFETY, PAWN_PUSH};

pub fn eval(position: &Position) -> i32 {
    eval_for(position, position.current_player())
}

fn eval_for(position: &Position, color: Color) -> i32 {
    let mat = material_difference(position, color);

    let board = position.bitboards();

    let pawn_pushes = {
        let ours = PAWN_PUSH.apply_for(board, color, Some(PieceKind::Pawn));
        let theirs = PAWN_PUSH.apply_for(board, color.opponent(), Some(PieceKind::Pawn));
        ours - theirs
    };

    let king_safety = {
        let ours = KING_SAFETY.apply_for(board, color, Some(PieceKind::King));
        let theirs = KING_SAFETY.apply_for(board, color.opponent(), Some(PieceKind::King));
        ours - theirs
    };

    let center_control = {
        let ours = CONTROL_CENTER.apply_for(board, color, None);
        let theirs = CONTROL_CENTER.apply_for(board, color.opponent(), None);
        ours - theirs
    };

    mat + pawn_pushes + king_safety + center_control
}

const fn eval_for_color(position: &Position, color: Color) -> i32 {
    let board = position.bitboards();
    let material = count_material(board, color);

    material
}

const fn material_difference(position: &Position, color: Color) -> i32 {
    let friendly = eval_for_color(position, color);
    let enemy = eval_for_color(position, color.opponent());

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
    score += count_material_of(board, color, PieceKind::King);

    score
}

const fn count_material_of(board: &ChessBoard, color: BitBoard, kind: PieceKind) -> i32 {
    let value = value_of(kind);
    let pieces = color.and(board.kind(kind));
    (pieces.population() as i32) * value
}

/*
const fn original_material() -> i32 {
    value_of(PieceKind::Pawn) * 16
        + value_of(PieceKind::Knight) * 4
        + value_of(PieceKind::Bishop) * 4
        + value_of(PieceKind::Rook) * 4
        + value_of(PieceKind::Queen) * 2
        + value_of(PieceKind::King) * 2
}

/// Divides the original material value of the board by the current material value, yielding a percentage.
///
/// Higher numbers are closer to the beginning of the game. Lower numbers are closer to the end of the game.
fn material_percentage(board: &ChessBoard) -> f32 {
    let white_material = count_material(board, Color::White);
    let black_material = count_material(board, Color::White);

    original_material() as f32 / (white_material + black_material) as f32
}

 */
