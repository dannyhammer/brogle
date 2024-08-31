use brogle::Engine;

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let mut engine = Engine::default();

    // let args = std::env::args();

    // for arg in args {
    //     println!("{arg}");
    // }

    engine.run()
}
