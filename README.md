# Command-line program to union yaml-files
The command-line program `yaml-union` unions yaml-files recursively.
When a key appears in multiple files, the values will be overridden with the
order given on the command-line. If both values are hashes, the overrides will
be done recursively.

The program could be useful in conjunction with `pandoc` when passing in
meta-data from external files.

e.g.

   yaml-union -d meta1.yaml meta2.yaml | pandoc text.md --template tmpl.html

## Installation

```bash
git clone https://github.com/michelk/yaml-union.hs
cd yaml-unions.hs
stack install
```

## Todo
- Optional define yaml-entries on the command-line eg `yaml-union -V blue=blau col.yaml`
