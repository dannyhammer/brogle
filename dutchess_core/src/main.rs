// use chess::{BitBoard, Board, Color, Piece};

use dutchess_core::core::*;

fn main() {

    // let game = Game::default_setup();
    // let fen = "8/8/2p1p3/3K3r/8/2n5/b7/8 w - - 0 1";
    // let game = Game::from_fen(fen).unwrap();
    // println!("{game}");

    // let moves = game.state().compute_legal_for();

    // println!("{}", game.state().attacks(Tile::E2));

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

    /*
    for from in Tile::iter() {
        for to in Tile::iter() {
            let ray = ray_between(from, to);
            if !ray.is_empty() {
                println!("{from} -> {to}\n{ray}\n---------------");
            }
        }
    }
     */
}
