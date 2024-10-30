import { EditorView, basicSetup } from "codemirror";
import { EditorState } from "@codemirror/state";

let startState = EditorState.create({
	doc: "Start Typing...",
	extensions: [basicSetup]
});

const view = new EditorView({
	state: startState,
	parent: document.querySelector('#editor'),
});

export default view;
