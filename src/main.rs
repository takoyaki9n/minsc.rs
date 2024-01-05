use eval::Interpreter;
use parser::parse;
use rustyline::{
    error::ReadlineError, highlight::MatchingBracketHighlighter, history::FileHistory,
    validate::MatchingBracketValidator, Config, Editor,
};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter, Validator};

mod env;
mod eval;
mod expression;
mod parser;
mod value;

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
struct MinscHelper {
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
}

fn editor() -> rustyline::Result<Editor<MinscHelper, FileHistory>> {
    let config = Config::builder()
        .completion_type(rustyline::CompletionType::List)
        .build();
    let helper = MinscHelper {
        highlighter: MatchingBracketHighlighter::new(),
        validator: MatchingBracketValidator::new(),
    };
    let mut editor = Editor::with_config(config)?;
    editor.set_helper(Some(helper));

    Ok(editor)
}

fn main() -> rustyline::Result<()> {
    let interpreter = Interpreter::new();
    interpreter.init();

    let mut editor = editor()?;

    loop {
        let result = editor.readline("minsc.rs> ");
        match result {
            Ok(input) => {
                if input == "exit" {
                    println!("Bye");
                    break;
                }

                match parse(&input) {
                    Ok(None) => continue,
                    Ok(Some(expr)) => match interpreter.eval(expr) {
                        Ok(evaled) => println!("{}", evaled),
                        Err(error) => println!("{}", error),
                    },
                    Err(error) => {
                        println!("{:?}", error)
                    }
                }

                editor.add_history_entry(input.as_str())?;
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                println!("Bye");
                break;
            }
            Err(err) => {
                println!("Error: {err:?}");
                break;
            }
        }
    }

    Ok(())
}
