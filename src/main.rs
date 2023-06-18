use built_in_procs::numbers::define_procs;
use env::{Env, EnvMaker};
use eval::eval;
use inquire::{
    ui::{RenderConfig, Styled},
    Text,
};
use parser::parse;

mod built_in_procs;
mod env;
mod eval;
mod expression;
mod parser;
mod value;

fn render_config() -> RenderConfig {
    let mut config = RenderConfig::empty();
    let prefix = Styled::new("minsc>");
    config.prompt_prefix = prefix;
    config.answered_prompt_prefix = prefix;

    config
}

fn main() {
    let env = Env::empty();
    define_procs(&env);
    loop {
        let result = Text::new("").with_render_config(render_config()).prompt();

        match result {
            Ok(input) => {
                if input == "exit" {
                    println!("Bye");
                    break;
                }

                match parse(&input) {
                    Ok((_, expr)) => match eval(expr, &env) {
                        Ok(reducted) => println!("{}", reducted),
                        Err(error) => println!("{}", error),
                    },
                    Err(error) => println!("{}", error),
                }
            }
            Err(error) => {
                println!("{}", error);
                break;
            }
        }
    }
}
