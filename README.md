# hedra

As in polyhedra. Generates dice rolls in the REPL or in a standalone executable.

Example REPL session:

```
$ cabal v2-repl
λ> 2 `d` 8
8 | 1 7
λ> 1 `d` 20
13
λ> 1 `d` 100
25 | 20 5
λ> 4 `d` f
3 | [ ] [+] [+] [+]
```

Example session in the executable:

```
$ cabal v2-run hedra
hedra> 2d8
2d8: 14 | 6 8
hedra> 1d20
1d20: 1
hedra> d20
d20: 5
hedra> d100
d100: 61 | 60 1
hedra> d%
d%: 5 | 00 5
hedra> 4df
4df: 2 | [ ] [+] [+] [ ]
```
