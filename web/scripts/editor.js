import { EditorView, basicSetup } from "codemirror";
import { EditorState } from "@codemirror/state";

let startState = EditorState.create({
	doc: `print "Hello world";`,
	extensions: [basicSetup]
});

const view = new EditorView({
	state: startState,
	parent: document.querySelector('#editor'),
});

export function replaceText(text) {
	view.dispatch({
		changes: { from: 0, to: view.state.doc.length, insert: text }
	})
}

export default view;
