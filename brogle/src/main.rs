use brogle::*;
use brogle_core::*;
use piece_square_tables::*;

fn main() -> anyhow::Result<()> {
    env_logger::init();
    brogle::Engine::default().run()

    // println!("{}", TEST_PSQ);

    // psq_eval(Piece::WHITE_QUEEN, Tile::C2, 0);
    // psq_eval(Piece::BLACK_QUEEN, Tile::C2, 0);
    // println!();
    // psq_eval(Piece::WHITE_QUEEN, Tile::C2.flipped(), 0);
    // psq_eval(Piece::BLACK_QUEEN, Tile::C2.flipped(), 0);

    // Ok(())
}
