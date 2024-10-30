use std::io::Write;

use wasm_bindgen::prelude::*;
use web_sys::HtmlElement;

use crate::{
    eval::{Env, Evaluator},
    parser::Parser,
};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_element(e: &JsValue);
}

struct ConsoleWriter {
    console: HtmlElement,
}

impl ConsoleWriter {
    pub fn new(console: HtmlElement) -> Self {
        return ConsoleWriter { console };
    }
}

impl Write for ConsoleWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Ok(new_value) = String::from_utf8(buf.to_vec()) {
            let trimmed_value = new_value.trim();
            if trimmed_value.is_empty() {
                return Ok(buf.len());
            }

            let prev_values = self.console.inner_html();
            let new_html = if prev_values.is_empty() {
                format!("<span>{trimmed_value}</span>")
            } else {
                format!("{prev_values}<span>{trimmed_value}</span>")
            };

            self.console.set_inner_html(&new_html);

            return Ok(buf.len());
        }

        return Ok(0);
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[wasm_bindgen]
pub fn eval_code(code: String, console: HtmlElement) -> String {
    let stderr = ConsoleWriter::new(console.clone());
    let stdout = ConsoleWriter::new(console);

    let ast = Parser::new(code).parse();
    let mut evaluator = Evaluator::new(Env::new(), stdout, stderr);
    let output = evaluator.eval(ast);

    return format!("{output}");
}
