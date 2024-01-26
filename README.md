# WABC

A compiler for a simple C like language that compiles down to llvm IR. Thrown together over a week while following a [dabeaz course](https://dabeaz.com/compiler.html) in order to learn about compilers and clojure.

```
func fact(n) {
  var i = 1;
  var acc = 1;
  while i < n {
    acc = acc * i;
    i = i + 1;
  }
  return acc;
}
var res = fact(5);
print res;
```

See compiler.clj for a high level overview of the compiler.