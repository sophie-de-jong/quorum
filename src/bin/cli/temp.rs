use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use crossterm::cursor::{MoveLeft, MoveToNextLine};
use crossterm::style::Print;
use crossterm::terminal::{Clear, ClearType};
use crossterm::{ExecutableCommand, QueueableCommand};
use egg::Symbol;
use quorum::config::Config;
use quorum::parse::{Expr, Rewrite};
use thiserror::Error;

type ConfigAttribute = String;
type ConfigValue = String;

pub enum ReplCommand {
    // General.
    Help,
    Quit,
    Clear,

    // Rule specific
    Enable(String),
    Disable(String),
    Add(String, String),

    // Config specific
    Config,
    Reload,
    Set(ConfigAttribute, ConfigValue),
    Default(ConfigAttribute),
    Import(PathBuf),
    Export(PathBuf),

    // Program specific
    History,
    Explain,
    Rules,
    Strategies,
}

#[derive(Debug, Error)]
pub enum CommandParseError {
    #[error("empty input string")]
    Empty,
    #[error("command does not start with a `:`")]
    NoColon,
    #[error("argument is missing")]
    MissingArgument,
    #[error("argument is invalid")]
    InvalidArgument,
    #[error("command is invalid")]
    InvalidCommand,
    #[error("too many arguments")]
    TooManyArguments,
}

impl ReplCommand {
    pub const COMMANDS_INFO: &[(&str, &str, &str)] = &[
        ("help", "", "show this help menu"),
        ("quit", "", "exit the REPL"),
        ("exit", "", "exit the REPL"),
        ("clear", "", "clear the screen"),
        ("enable", "<rule>", "enable the runner to use this rule"),
        ("disable", "<rule>", "disable the runner to use this rule"),
        ("add", "<name> <lhs> => <rhs>", "add a new rule"),
        ("config", "", "show the current loaded configuration"),
        ("reload", "", "reload the configuration"),
        ("set", "<key> <value>", "modify a config value"),
        (
            "default",
            "<key>",
            "reset a config value to its original state",
        ),
        ("import", "<path>", "import a configuration"),
        ("export", "<path>", "export the current configuration"),
        ("history", "", "show the REPL history"),
        ("explain", "", "explain how an expression was simplified"),
        ("rules", "", "show the current rules"),
        ("strategies", "", "show the current strategies"),
    ];
}

impl FromStr for ReplCommand {
    type Err = CommandParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split_whitespace();
        let first = match iter.next() {
            Some(s) if s.starts_with(':') => s,
            Some(_) => return Err(CommandParseError::NoColon),
            None => return Err(CommandParseError::Empty),
        };

        let cmd = match first {
            ":help" => ReplCommand::Help,
            ":quit" | ":exit" => ReplCommand::Quit,
            ":clear" => ReplCommand::Clear,
            ":enable" => match iter.next() {
                Some(arg) => ReplCommand::Enable(arg.to_string()),
                None => return Err(CommandParseError::MissingArgument),
            },
            ":disable" => match iter.next() {
                Some(arg) => ReplCommand::Disable(arg.to_string()),
                None => return Err(CommandParseError::MissingArgument),
            },
            ":add" => match iter.next() {
                Some(arg1) => match iter.next() {
                    Some(arg2) => ReplCommand::Add(arg1.to_string(), arg2.to_string()),
                    None => return Err(CommandParseError::MissingArgument),
                },
                None => return Err(CommandParseError::MissingArgument),
            },
            ":config" => ReplCommand::Config,
            ":reload" => ReplCommand::Reload,
            ":set" => match iter.next() {
                Some(arg1) => match iter.next() {
                    Some(arg2) => ReplCommand::Set(arg1.to_string(), arg2.to_string()),
                    None => return Err(CommandParseError::MissingArgument),
                },
                None => return Err(CommandParseError::MissingArgument),
            },
            ":default" => match iter.next() {
                Some(arg) => ReplCommand::Default(arg.to_string()),
                None => return Err(CommandParseError::MissingArgument),
            },
            ":import" => match iter.next() {
                Some(arg) => ReplCommand::Default(arg.to_string()),
                None => return Err(CommandParseError::MissingArgument),
            },
            ":export" => match iter.next() {
                Some(arg) => ReplCommand::Default(arg.to_string()),
                None => return Err(CommandParseError::MissingArgument),
            },
            ":history" => ReplCommand::History,
            ":explain" => ReplCommand::Explain,
            ":rules" => ReplCommand::Rules,
            ":strategies" => ReplCommand::Strategies,
            _ => return Err(CommandParseError::InvalidCommand),
        };

        if iter.next().is_some() {
            Err(CommandParseError::TooManyArguments)
        } else {
            Ok(cmd)
        }
    }
}

pub struct App<W> {
    pub is_running: bool,
    writer: W,
    buf: String,
    history: Vec<String>,
    rules: Vec<Rewrite>,
    config: Config,
}

impl<W> App<W> {
    pub fn new(writer: W) -> io::Result<App<W>> {
        let config = Config::load()?;
        Ok(App {
            is_running: true,
            writer,
            buf: String::new(),
            history: Vec::new(),
            rules: config.rules(),
            config,
        })
    }
}

impl<W: Write + QueueableCommand + ExecutableCommand> App<W> {
    pub fn add_char(&mut self, c: char) -> io::Result<()> {
        self.buf.push(c);
        self.writer.queue(Print(c))?.flush()
    }

    pub fn backspace(&mut self) -> io::Result<()> {
        if self.buf.pop().is_some() {
            self.writer
                .queue(MoveLeft(1))?
                .queue(Print(" "))?
                .queue(MoveLeft(1))?
                .flush()
        } else {
            Ok(())
        }
    }

    pub fn enter(&mut self) -> io::Result<()> {
        self.writer.execute(MoveToNextLine(1))?;

        match self.buf.parse() {
            Ok(command) => self.run_command(command)?,
            Err(CommandParseError::Empty) => (),
            Err(CommandParseError::NoColon) => match quorum::parse::parse_expr(&self.buf) {
                Ok(expr) => self.run_expression(expr)?,
                Err(err) => {
                    self.writer.queue(Print(err))?;
                }
            },
            Err(err) => {
                self.writer.queue(Print(err))?;
            }
        }

        let entry = std::mem::take(&mut self.buf);
        self.history.push(entry);
        self.writer
            .queue(MoveToNextLine(2))?
            .queue(Print("> "))?
            .flush()
    }

    pub fn run_command(&mut self, command: ReplCommand) -> io::Result<()> {
        match command {
            ReplCommand::Help => self.show_help(),
            ReplCommand::Quit => self.quit(),
            ReplCommand::Clear => self.clear(),
            ReplCommand::Enable(rule) => self.enable_rule(rule),
            ReplCommand::Disable(rule) => self.disable_rule(rule),
            ReplCommand::Add(lhs, rhs) => self.add_rule(lhs, rhs),
            ReplCommand::Config => self.show_config(),
            ReplCommand::Reload => self.reload_config(),
            ReplCommand::Set(key, val) => self.set_config_value(key, val),
            ReplCommand::Default(key) => self.reset_config_value(key),
            ReplCommand::Import(path) => self.import_config(&path),
            ReplCommand::Export(path) => self.export_config(&path),
            ReplCommand::History => self.show_history(),
            ReplCommand::Explain => self.explain_expr(),
            ReplCommand::Rules => self.show_rules(),
            ReplCommand::Strategies => self.show_strategies(),
        }
    }

    pub fn run_expression(&mut self, expr: Expr) -> io::Result<()> {
        todo!()
    }

    pub fn quit(&mut self) -> io::Result<()> {
        self.is_running = false;
        Ok(())
    }

    pub fn clear(&mut self) -> io::Result<()> {
        self.writer.queue(Clear(ClearType::All))?.flush()
    }

    pub fn show_help(&mut self) -> io::Result<()> {
        self.writer
            .queue(Print("Available commands:"))?
            .queue(MoveToNextLine(1))?;

        for (name, args, description) in ReplCommand::COMMANDS_INFO {
            writeln!(self.writer, "  :{name} {args} — {description}")?;
        }

        self.writer.flush()
    }

    fn enable_rule(&mut self, rule: impl Into<Symbol>) -> io::Result<()> {
        todo!("Enable a rewrite rule by name")
    }

    fn disable_rule(&mut self, rule: impl Into<Symbol>) -> io::Result<()> {
        todo!("Disable a rewrite rule by name")
    }

    fn add_rule(&mut self, _lhs: String, _rhs: String) -> io::Result<()> {
        todo!("Add a new rewrite rule dynamically")
    }

    fn show_config(&mut self) -> io::Result<()> {
        self.writer
            .queue(Print(format!("{:#?}\n", self.config)))?
            .flush()
    }

    fn reload_config(&mut self) -> io::Result<()> {
        self.config = Config::load()?;
        self.rules = self.config.rules();
        self.writer.queue(Print("Config reloaded.\n"))?.flush()
    }

    fn set_config_value(&mut self, _key: String, _value: String) -> io::Result<()> {
        todo!("Set a configuration value at runtime")
    }

    fn reset_config_value(&mut self, _key: String) -> io::Result<()> {
        todo!("Reset a configuration value to default")
    }

    fn import_config(&mut self, path: &Path) -> io::Result<()> {
        self.config = Config::load_from_path(path)?;
        self.rules = self.config.rules();
        self.writer.queue(Print("Config reloaded.\n"))?.flush()
    }

    fn export_config(&mut self, path: &Path) -> io::Result<()> {
        todo!("Export configuration to file")
    }

    fn show_history(&mut self) -> io::Result<()> {
        todo!("Show REPL input history")
    }

    fn explain_expr(&mut self) -> io::Result<()> {
        todo!("Explain expression simplification process")
    }

    fn show_rules(&mut self) -> io::Result<()> {
        self.writer
            .queue(Print("Loaded rewrite rules:\n"))?
            .queue(Print(format!("{:#?}\n", self.rules)))?
            .flush()
    }

    fn show_strategies(&mut self) -> io::Result<()> {
        todo!("Display available rewrite strategies")
    }
}
