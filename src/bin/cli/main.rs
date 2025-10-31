mod app;
mod temp;

use crossterm::QueueableCommand;
use crossterm::cursor::MoveToNextLine;
use crossterm::event;
use crossterm::event::Event;
use crossterm::event::KeyCode;
use crossterm::event::KeyModifiers;
use crossterm::style::Print;
use crossterm::terminal::*;
use std::io::{self, Write};

use crate::temp::App;

fn repl() -> io::Result<()> {
    let mut stdout = io::stdout();
    stdout
        .queue(Print("Quorum v0.1"))?
        .queue(MoveToNextLine(1))?
        .queue(Print(
            "Type expressions to simplify, or :help for commands.",
        ))?
        .queue(MoveToNextLine(1))?
        .queue(Print("> "))?
        .flush()?;

    let mut app = App::new(&mut stdout)?;

    enable_raw_mode()?;
    while app.is_running {
        if let Event::Key(event) = event::read()? {
            match (event.code, event.modifiers) {
                (KeyCode::Char('c'), KeyModifiers::CONTROL) => app.quit()?,
                (KeyCode::Char('d'), KeyModifiers::CONTROL) => app.quit()?,
                (KeyCode::Esc, _) => app.quit()?,

                (KeyCode::Char('l'), KeyModifiers::CONTROL) => app.clear()?,

                (KeyCode::Char(c), _) => app.add_char(c)?,
                (KeyCode::Backspace, _) => app.backspace()?,
                (KeyCode::Enter, _) => app.enter()?,
                (_, _) => (),
            }
        }
    }
    disable_raw_mode()?;
    Ok(())
}

fn main() {
    if let Err(err) = repl() {
        eprintln!("{err}")
    }
}
