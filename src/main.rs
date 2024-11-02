use type_checker::{env::Env, parser::parse_expression, synth::synth};

fn main() {
    repl();
}

fn repl() {
    let env = Env::new();
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim();

        let ast = parse_expression(input);
        let _type = synth(&env, &ast);
        println!("{}", _type.to_string());
    }
}
