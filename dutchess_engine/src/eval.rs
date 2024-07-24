use dutchess_core::{BitBoard, ChessBoard, Color, PieceKind};

pub fn eval(board: &ChessBoard) -> i32 {
    let white_material = count_material(board, Color::White) as i32;
    let black_material = count_material(board, Color::White) as i32;

    let center_control = both(&CENTER_CONTROL, board, None);
    let pawn_advancement = both(&PAWN_PUSH, board, Some(PieceKind::Pawn));

    (white_material - black_material) + pawn_advancement + center_control
}

fn both(pst: &[usize], board: &ChessBoard, kind: Option<PieceKind>) -> i32 {
    let (w, b) = if let Some(kind) = kind {
        (
            apply_piece_square_table(pst, board, board.color(Color::White) & board.kind(kind)),
            apply_piece_square_table(pst, board, board.color(Color::Black) & board.kind(kind)),
        )
    } else {
        (
            apply_piece_square_table(pst, board, board.color(Color::White)),
            apply_piece_square_table(pst, board, board.color(Color::Black)),
        )
    };

    w as i32 - b as i32
}

fn count_material(board: &ChessBoard, color: Color) -> usize {
    let color = board.color(color);

    ((color & board.kind(PieceKind::Pawn)).population() * 1
        + (color & board.kind(PieceKind::Knight)).population() * 3
        + (color & board.kind(PieceKind::Bishop)).population() * 3
        + (color & board.kind(PieceKind::Rook)).population() * 5
        + (color & board.kind(PieceKind::Queen)).population() * 9
        + (color & board.kind(PieceKind::King)).population() * 20) as usize
}

fn apply_piece_square_table(pst: &[usize], board: &ChessBoard, pieces: BitBoard) -> usize {
    let mut score = 0;

    for square in pieces {
        if board.has(square) {
            score += pst[square.index()];
        }
    }

    score
}

const CENTER_CONTROL: [usize; 64] = [
    0, 0, 0, 0, 0, 0, 0, 0, //
    0, 0, 0, 0, 0, 0, 0, 0, //
    1, 2, 3, 4, 4, 3, 2, 1, //
    1, 2, 3, 5, 5, 3, 2, 1, //
    1, 2, 3, 5, 5, 3, 2, 1, //
    1, 2, 3, 4, 4, 3, 2, 1, //
    0, 0, 0, 0, 0, 0, 0, 0, //
    0, 0, 0, 0, 0, 0, 0, 0, //
];

const PAWN_PUSH: [usize; 64] = [
    6, 6, 6, 6, 6, 6, 6, 6, //
    5, 5, 5, 5, 5, 5, 5, 5, //
    4, 4, 4, 4, 4, 4, 4, 4, //
    3, 3, 3, 3, 3, 3, 3, 3, //
    2, 2, 2, 2, 2, 2, 2, 2, //
    1, 1, 1, 1, 1, 1, 1, 1, //
    0, 0, 0, 0, 0, 0, 0, 0, //
    0, 0, 0, 0, 0, 0, 0, 0, //
];
