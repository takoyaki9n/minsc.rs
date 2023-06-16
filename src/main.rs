#![allow(dead_code)]

use inquire::{
    ui::{RenderConfig, Styled},
    Text,
};

mod built_in_procs;
mod env;
mod eval;
mod expression;
mod parser;
mod value;

fn main() {
    let render_config = RenderConfig::default().with_prompt_prefix(Styled::new("minsc>"));
    let input = Text::new("")
        .with_initial_value("(+ 1 2)")
        .with_render_config(render_config)
        .prompt();

    match input {
        Ok(input) => println!("-> {}", input),
        Err(_) => println!("An error happened when asking for your name, try again later."),
    }
}
