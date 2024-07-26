use dutchess_core::{ChessBoard, Color, PieceKind, Position};

pub fn eval(position: &Position) -> i32 {
    eval_for(position, position.current_player())
}

pub fn eval_for(position: &Position, color: Color) -> i32 {
    let friendly = eval_for_color(position, color);
    let enemy = eval_for_color(position, color.opponent());

    friendly - enemy
}

fn eval_for_color(position: &Position, color: Color) -> i32 {
    let board = position.bitboards();
    let material = count_material(board, color);

    material
}

const fn value_of(kind: PieceKind) -> i32 {
    match kind {
        PieceKind::Pawn => 100,
        PieceKind::Knight => 300,
        PieceKind::Bishop => 300,
        PieceKind::Rook => 500,
        PieceKind::Queen => 900,
        PieceKind::King => 2000,
    }
}

fn count_material(board: &ChessBoard, color: Color) -> i32 {
    let mut score = 0;
    let color = board.color(color);

    for kind in PieceKind::iter() {
        let value = value_of(kind);
        let pieces = color & board.kind(kind);
        score += (pieces.population() as i32) * value;
    }

    score
}

/*
fn both(pst: &[i32], board: &ChessBoard, kind: Option<PieceKind>) -> i32 {
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


fn apply_piece_square_table(pst: &[i32], board: &ChessBoard, pieces: BitBoard) -> i32 {
    let mut score = 0;

    for square in pieces {
        if board.has(square) {
            score += pst[square.index()];
        }
    }

    score
}

const CENTER_CONTROL: [i32; 64] = [
    0, 0, 0, 0, 0, 0, 0, 0, //
    0, 0, 0, 0, 0, 0, 0, 0, //
    1, 2, 3, 4, 4, 3, 2, 1, //
    1, 2, 3, 5, 5, 3, 2, 1, //
    1, 2, 3, 5, 5, 3, 2, 1, //
    1, 2, 3, 4, 4, 3, 2, 1, //
    0, 0, 0, 0, 0, 0, 0, 0, //
    0, 0, 0, 0, 0, 0, 0, 0, //
];

const PAWN_PUSH: [i32; 64] = [
    6, 6, 6, 6, 6, 6, 6, 6, //
    5, 5, 5, 5, 5, 5, 5, 5, //
    4, 4, 4, 4, 4, 4, 4, 4, //
    3, 3, 3, 3, 3, 3, 3, 3, //
    2, 2, 2, 2, 2, 2, 2, 2, //
    1, 1, 1, 1, 1, 1, 1, 1, //
    0, 0, 0, 0, 0, 0, 0, 0, //
    0, 0, 0, 0, 0, 0, 0, 0, //
];

 */
