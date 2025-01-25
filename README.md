# Boron

A simple (in-progres) interpreted and dynamically typed scripting language

# Examples
```
let y := 0;
for x in 1..5 {
  y = +(x, y);
}
y <-- prints 10
```

```
let y := 10;
while >(y, 7) {
  y = -(y, 1);
  a = +(a, 1);
}
(y, a)
```

# TODO
- [ ] Parse `foo()[bar]` properly
- [ ] Add any semblance of error handling
