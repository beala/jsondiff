jsondiff
========

Usage:
```
> jsondiff '{"name":"A. Turing", "birthday":{"month":"June", "day":23, "year":1912}, "fields":["math", "cryptanalysis", "cs"]}' '{"name":"Alan Turing", "fields":["math","biology"], "birthday":{"month":"6", "day":23, "year":1912}}'

root.birthday.month
  "June"
  "6"
root.fields.array[1]
  "cryptanalysis"
  "biology"
root.fields.array[2]
  "cs"
  Missing
root.name
  "A. Turing"
  "Alan Turing"
```

Build:
```
cabal build
```
