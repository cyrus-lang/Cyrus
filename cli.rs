use evaluator::eval;
use object::{
    env::Env,
    object::{EvalError, Object},
};
use parser::Parser;
use std::{
    cell::RefCell,
    env, fs,
    io::{self, Write},
    ops::Index,
    rc::Rc,
};

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        let option = args.index(1).as_str();

        match option {
            "version"| "v" => {
                show_version();
            }
            "run" | "r" => {
                let file_path = args.index(2);

                match fs::read_to_string(file_path) {
                    Err(err) => {
                        write_line(&err.to_string());
                    }
                    Ok(file_content) => match run(&file_content) {
                        Err(e) => {
                            write_line(e.as_str());
                        }
                        Ok(_) => {}
                    },
                }
            }
            "help" | "h" | _ => {
                show_help_menu();
            }
        };
    } else {
        show_help_menu();
        std::process::exit(0);
    }
}

fn write_line(input: &str) {
    println!("{} {}", RESULT, input);
}

fn show_help_menu() {
    println!("usage: cyrus [command] [switches] [arguments]");
    println!("interactive, i: run an interactive shell");
    println!("version, v: print the version number");
    println!("help, h: show this message");
    println!("run, r: <file_path>");
}

fn show_version() {
    println!("Cyrus {}", VERSION);
}

fn run(input: &str) -> Result<Rc<Object>, EvalError> {
    let env: Env = Rc::new(RefCell::new(Default::default()));
    let node = Parser::parse(input.to_string()).unwrap();

    eval(node, &env)
}
