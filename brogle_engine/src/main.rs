use anyhow::Result;
use brogle_engine::Engine;

fn main() -> Result<()> {
    env_logger::init();
    Engine::default().run()
}
