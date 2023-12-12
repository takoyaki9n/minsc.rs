use eval::Interpreter;
use inquire::{
    ui::{RenderConfig, Styled},
    Text,
};
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
            }
            Err(error) => {
                println!("{}", error);
                break;
            }
        }
    }
}
