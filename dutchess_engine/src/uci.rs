use std::fmt;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum UciCommand {
    Uci,
    Debug(bool),
    IsReady,
    SetOption(String, Vec<String>),
    UciNewGame,
    // Pos(Game),
    // Go(SearchControls),
    Stop,
    // PonderHit,
    Quit,
}

impl UciCommand {
    pub fn new(input: &str) -> Result<Self, String> {
        let mut split = input.split_whitespace().map(|word| word.to_lowercase());
        let first = split.next().ok_or(format!("Cannot parse empty command"))?;

        let cmd = match first.as_str() {
            "uci" => Self::Uci,
            "debug" => {
                let debug = split.next().ok_or(format!("No debug option found"))?;
                match debug.as_str() {
                    "on" => Self::Debug(true),
                    "off" => Self::Debug(false),
                    _ => return Err(format!("Debug must be either `on` or `off`")),
                }
            }
            "isready" => Self::IsReady,
            "setoption" => Self::SetOption(String::from("TODO"), vec![]),
            "ucinewgame" => Self::UciNewGame,
            "stop" => Self::Stop,
            "quit" => Self::Quit,
            _ => return Err(format!("Unrecognized command `{first}`")),
        };

        Ok(cmd)
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum UciResponse {
    Id(String, String),
    UciOk,
    ReadyOk,
    // Opt(UciOption),
    // BestMove(Move),
    // Info(UciInfo),
}

impl UciResponse {
    //
}

impl fmt::Display for UciResponse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let response = match self {
            Self::Id(name, author) => {
                format!("id name {name}\nid author {author}")
            }
            Self::UciOk => format!("uciok"),
            Self::ReadyOk => format!("readyok"),
        };
        write!(f, "{response}\n")
    }
}

/*


pub enum GuiToEngine {
    UCI,
    Debug(bool),
    IsReady,
    SetOption(String, Vec<String>),
}

impl fmt::Display for GuiToEngine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let uci = match self {
            Self::UCI => format!("uci"),
            Self::SetOption(name, values) => {
                format!("{name} {values:?}")
            }
            Self::IsReady => format!("isready"),
            Self::Debug(flag) => format!("debug "),
        };
        write!(f, "{uci}")
    }
}

 */
