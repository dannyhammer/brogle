use std::{
    io::{self, Write},
    process::{Child, ChildStdin, Command, Stdio},
    thread,
    time::Duration,
};

/*
use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long)]
    name: String,

    /// Number of times to greet
    #[arg(short, long, default_value_t = 1)]
    count: u8,
}

fn main() {
    let args = Args::parse();

    for _ in 0..args.count {
        println!("Hello {}!", args.name);
    }
}
 */

fn message_engine(stdin: &mut ChildStdin, msg: &str) -> io::Result<()> {
    stdin.write_all(msg.as_bytes())?;
    stdin.write_all("\n".as_bytes())
}

fn launch_engine(cmd: &str) -> Result<Child, io::Error> {
    Command::new(cmd)
        .stdin(Stdio::piped())
        // .stdout(Stdio::piped())
        .spawn()
}

fn main() -> io::Result<()> {
    println!("Brogle Arena");

    let mut white = launch_engine("./target/release/brogle_engine")?;

    let mut white_stdin = white.stdin.take().expect("Failed to open stdin for White");

    message_engine(&mut white_stdin, "uci")?;
    message_engine(&mut white_stdin, "isready")?;
    message_engine(&mut white_stdin, "ucinewgame")?;
    message_engine(&mut white_stdin, "position startpos")?;
    message_engine(
        &mut white_stdin,
        "go wtime 300000 btime 300000 winc 0 binc 0",
    )?;
    thread::sleep(Duration::from_secs(1));
    message_engine(&mut white_stdin, "quit")?;

    let res = white.wait_with_output().unwrap();
    println!("{:?}", res.status);
    println!("STDOUT: {}", String::from_utf8_lossy(&res.stdout));
    println!("STDERR: {}", String::from_utf8_lossy(&res.stderr));

    Ok(())
}
