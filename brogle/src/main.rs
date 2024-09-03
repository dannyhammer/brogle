fn main() -> anyhow::Result<()> {
    env_logger::init();
    brogle::Engine::default().run()
}
