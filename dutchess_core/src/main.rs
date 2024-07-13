// use chess::{BitBoard, Board, Color, Piece};

use dutchess_core::core::{
    gen::write_moves, get_bishop_moves, get_rook_moves, print_magics, BitBoard, Game, Tile,
};

fn main() {
    // let game = Game::default_setup();
    // println!("{game}");

    // let moves = game.get_legal_moves(Tile::A1);
    // println!("{}", moves);

    // magics_gen();
    // print_magics();
    // write_moves();

    // . . . X . . . X
    // . . . . . . . .
    // . . . X . . . .
    // . . . . . . . .
    // . . . . . . . X
    // . . X . . . . .
    // . . . X . X . .
    // . . . . . . . .
    let blockers =
        BitBoard::new(0b1000100000000000000010000000000010000000000001000010100000000000);
    // let moves = get_rook_moves(Tile::D4, blockers);
    let tile = Tile::D4;
    println!("Blockers:\n{blockers}");
    println!("Tile: {tile}");
    println!("Rook moves:\n{}", get_rook_moves(tile, blockers));
    println!("Bishop moves:\n{}", get_bishop_moves(tile, blockers));

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

/*
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
 */
