import "./scripts/editor.js"
import { eval_code } from "./pkg/monkelang.js"
import view from "./scripts/editor.js";

let $run = document.querySelector("#run");
let $console = document.querySelector("#console_output");

$run.addEventListener('click', function() {
	$console.replaceChildren();
	let output = eval_code(view.state.doc.toString(), $console);
	if (output != "") {
		$console.innerHTML += `<span>${output}</span>`;
	}
});
