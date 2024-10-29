/** @type {import('tailwindcss').Config} */
export default {
	content: ["./index.html"],
	theme: {
		extend: {
			boxShadow: {
				"custom-btn": "inset 0.0625rem 0.0625rem 0.0625rem #ffb147, inset -0.0625rem -0.0625rem 0.0625rem #d72d12, 0.125rem 0.125rem 0.25rem #000",
				"custom-dot": "inset 2px 2px 8px 0 #bab7b1, inset -2px -2px 8px 0 #f0eeeb",
				"custom-active": "inset 0 0 0.25rem #000, inset 0.0625rem 0.0625rem 0.0625em transparent, inset -0.0625rem -0.0625rem 0.0625rem transparent, 0.125rem 0.125rem 0.25rem transparent;",
				"console": "inset 0 0 5px -1px rgba(207, 203, 215, 1), inset 0 0 11px 0 rgba(207, 203, 215, 1);",
			}
		},
	},
	plugins: [],
}

