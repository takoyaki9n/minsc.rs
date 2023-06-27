use eval::Interpreter;
use inquire::{
    ui::{RenderConfig, Styled},
    Text,
};
use nom::Finish;
use parser::parse;

mod env;
mod eval;
mod expression;
mod parser;
mod value;

fn render_config() -> RenderConfig {
    let mut config = RenderConfig::empty();
    let prefix = Styled::new("minsc.rs>");
    config.prompt_prefix = prefix;
    config.answered_prompt_prefix = prefix;

    config
}

fn main() {
    let interpreter = Interpreter::new();
    interpreter.init();

    loop {
        let result = Text::new("").with_render_config(render_config()).prompt();

        match result {
            Ok(input) => {
                if input == "exit" {
                    println!("Bye");
                    break;
                }

                match parse(&input).finish() {
                    Ok((_, None)) => continue,
                    Ok((_, Some(expr))) => match interpreter.eval(expr) {
                        Ok(reducted) => println!("{}", reducted),
                        Err(error) => println!("{}", error),
                    },
                    Err(error) => println!("{:?}", nom::error::convert_error(input.as_str(), error)),
                }
            }
            Err(error) => {
                println!("{}", error);
                break;
            }
        }
    }
}
