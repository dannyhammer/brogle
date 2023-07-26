use chess::{BitBoard, Board, Color, Piece};

fn main() {
    let board = Board::default();

    println!("Eval: {}", eval(board));
}

pub fn eval(state: Board) -> i32 {
    let mut score = 0;
    let white = state.color_combined(Color::White);
    let black = state.color_combined(Color::Black);
    let pawn = state.pieces(Piece::Pawn);
    let knight = state.pieces(Piece::Knight);
    let bishop = state.pieces(Piece::Bishop);
    let rook = state.pieces(Piece::Rook);
    let queen = state.pieces(Piece::Queen);
    let king = state.pieces(Piece::King);

    let material = |color: &BitBoard| {
        (color & pawn).count() * 1
            + (color & knight).count() * 3
            + (color & bishop).count() * 3
            + (color & rook).count() * 5
            + (color & queen).count() * 9
            + (color & king).count() * 20
    };

    score += material(white) as i32;
    score -= material(black) as i32;

    score
}
