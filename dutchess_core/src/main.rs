// use chess::{BitBoard, Board, Color, Piece};

use dutchess_core::core::*;

fn main() {
    let game = Game::default_setup();
    println!("{game}");

    println!("{}", game.state().attacks(Tile::E2));

    // let moves = game.get_legal_moves();
    // println!("{:?}", moves.into_iter().collect::<Vec<_>>());

    // for chessmove in game.state.get_legal_moves() {
    //     let from = chessmove.from();
    //     let piece = game.state().board().piece_at(from).unwrap();
    //     println!("{piece} at {from} can {chessmove}");
    // }

    // let board = ChessBoard::new().with_default_setup();
    // println!("{board}");
    // println!("{}", state.to_fen());
    // let tile = Tile::C4;
    // let piece = Piece::BLACK_ROOK;
    // let moves = moves_for(&piece, tile, state.board());
    // println!("{moves}");
    // let board = Board::default();

    // println!("Eval: {}", eval(board));
}
