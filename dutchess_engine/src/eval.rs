use chess::{Board, Color, Piece, EMPTY};

pub fn eval(state: &Board) -> i32 {
    fn both(state: &Board, f: impl Fn(&Board, Color) -> usize) -> i32 {
        f(state, Color::White) as i32 - f(state, Color::Black) as i32
    }

    let material = both(state, count_material);

    let center_control = both(state, encourage_center_control);

    material + center_control
}

fn count_material(state: &Board, color: Color) -> usize {
    let color = state.color_combined(color);

    (color & state.pieces(Piece::Pawn)).count() * 1
        + (color & state.pieces(Piece::Knight)).count() * 3
        + (color & state.pieces(Piece::Bishop)).count() * 3
        + (color & state.pieces(Piece::Rook)).count() * 5
        + (color & state.pieces(Piece::Queen)).count() * 9
        + (color & state.pieces(Piece::King)).count() * 20
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

fn encourage_center_control(state: &Board, color: Color) -> usize {
    let mut score = 0;

    for square in !EMPTY {
        if let Some(_piece) = state.piece_on(square) {
            score += CENTER_CONTROL[square.to_index()];
        }
    }

    score
}
