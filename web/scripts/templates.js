import { replaceText } from "./editor";

const data = {
	arr: [
		["Hello world", `print "Hello world";`],
		["Computing int", `(10 + 2) * 30 + 5`],
		[
			"Variable", `let x = 10;
let y = 15;

x + y;` ],

		[
			"Conditionals", `if true {
  print "Hello";
} else {
  print "unreachable";
}`,
		],

		[
			"Array", `let arr = ["one", "two", "three"];
print arr[0];
print arr[1];
print arr[2];

print "---- >8 ----";

print len(arr);
print first(arr);
print last(arr);

print "---- >8 ----";

let arr = push(arr, "four");
print last(arr);`],

		[
			"function", `let factorial = fn(n) {
  if n == 0 {
    1
  } else {
    n * factorial(n - 1)
  }
};

factorial(5)`],

		[
			"Closures", `let newAdder = fn(x) {
  fn(y) { x + y }
};

let addTwo = newAdder(2);
addTwo(2);`],

		[
			"Array - Map", `let map = fn(arr, f) {
  let iter = fn(arr, accumulated) {
    if (len(arr) == 0) {
      accumulated
    } else {
	  push(accumulated, f(first(arr)));
      iter(rest(arr), accumulated)
    }
  };

  iter(arr, []);
};

let arr = [1, 2, 3, 4];

let double = fn(x) { x * 2 };

map(arr, double);`],

		[
			"Array - reduce", `let reduce = fn(arr, initial, f) {
  let iter = fn(arr, result) {
    if (len(arr) == 0) {
      result
    } else {
      iter(rest(arr), f(result, first(arr)))
    }
  };

  iter(arr, initial);
};

let arr = [1, 2, 3, 4];

let sum = fn(arr) {
  reduce(arr, 0, fn(initial, el) { initial + el })
};

sum(arr);`]
	],
};


document.addEventListener('alpine:init', () => {
	Alpine.data('display', () => ({
		title: data.arr[0][0],
		index: 0,
		increment() {
			this.index = (this.index + 1) % data.arr.length
			this.title = data.arr[this.index][0];
			replaceText(data.arr[this.index][1]);
		},
		decrement() {
			if (this.index == 0) {
				this.index = data.arr.length - 1;
				this.title = data.arr[this.index][0];
				replaceText(data.arr[this.index][1]);

				return;
			}

			this.index--;
			this.title = data.arr[this.index][0];
			replaceText(data.arr[this.index][1]);
		}
	}))
});
