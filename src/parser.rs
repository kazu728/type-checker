use swc_common::{sync::Lrc, FileName, SourceMap};
use swc_ecma_ast::{EsVersion, Expr};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsSyntax};

pub fn parse_expression(input: &str) -> Box<Expr> {
    let cm = Lrc::new(SourceMap::default());
    let fm = cm.new_source_file(Lrc::new(FileName::Custom("input.ts".into())), input.into());

    let lexer = Lexer::new(
        Syntax::Typescript(TsSyntax {
            tsx: false,
            decorators: false,
            disallow_ambiguous_jsx_like: true,
            dts: false,
            no_early_errors: false,
        }),
        EsVersion::Es2020,
        StringInput::from(&*fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);

    match parser.parse_expr() {
        Ok(expr) => expr,
        Err(err) => {
            eprintln!("Parse error: {:?}", err);
            std::process::exit(1);
        }
    }
}
