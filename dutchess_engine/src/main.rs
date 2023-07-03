use dutchess_engine::*;

fn main() {
    let board = ChessBoard::default();
    // let mut board = board.state.white[PieceKind::Pawn];

    // println!("{board}");
    // println!("{}", *board.state.to_fen());

    println!("SIZE: {}", std::mem::size_of::<Color>());

    // board
}
