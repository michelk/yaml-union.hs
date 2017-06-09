---
title: yaml-union
section: 1
date: January 6, 2016
author:
- Michel Kuhlmann
...

# NAME

yaml-union -- Union yaml-files on the command-line

# SYNOPSIS

yaml-union [options] [files..]

# DESCRIPTION

`yaml-union` unions yaml-files recursively. When a key appears in multiple
files, the values will be overridden with the order given on the command-line.
If both values are hashes, the overrides will be done recursively. 

The program could be useful in conjunction with `pandoc` when passing in
meta-data from external files.

# OPTIONS

`-d,--dashes` 

  Whether to print dashes at the beginning and end of the document

# TODO

- Optional define yaml-entries on the command-line e.g. 
  `yaml-union -V blue=blau col.yaml`


# EXAMPLES

## Dictionary

de.yaml:
	
```yaml
name: Michel Kuhlmann
blue: blau
green: grün
```

en.yaml: 

```yaml
blue: blue
green: green
```

	yaml-union de.yaml en.yaml

will create

```yaml
name: Michel Kuhlmann
blue: blue
green: green
```

## Pandoc

	yaml-union -d meta1.yaml meta2.yaml | pandoc text.md --template tmpl.html

