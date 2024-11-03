import init, { eval_code } from "./pkg/monkelang.js"
import view from "./scripts/editor.js";
import "./scripts/templates.js";

await init();
let $run = document.querySelector("#run");
let $console = document.querySelector("#console_output");

$run.addEventListener('click', function() {
	$run.disabled = true;

	$console.replaceChildren();
	let output = eval_code(view.state.doc.toString(), $console);
	if (output != "") {
		$console.innerHTML += `<span>${output}</span>`;
	}

	setTimeout(() => {
		$run.disabled = false;
	}, 1000);
});

