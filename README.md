# tex2typst

> Work in progress. Does not yet support all builtin identifiers or layouts.
The goal is to generate high quality conversions with a complete focus on the math subset of each language.

Convert TeX Math to [Typst](https://github.com/typst/typst) Math. 

## Build from source

```console
nix build
./result/bin/tex2typst < example.tex
frac(1, 123)
```
