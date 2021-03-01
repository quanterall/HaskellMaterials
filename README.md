# HaskellMaterials

## Installing `stack`

[How to install Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) is a
reasonable place to start.

Why `stack`? It's likely the easiest way for new people to start and (probably) the most common
choice in industry, though other tools are catching up.

## Making your first example project

`stack` has an analog for `mix new`:

```bash
$ stack new MyExampleProject simple
$ cd MyExampleProject
$ stack build
```

The `simple` part is which template we want to use. The `simple` one is enough for what we need,
but entire web development templates are available and as you would expect you can make your own
ones.

When we execute the above stack will automatically download an appropriate `ghc` version (GHC is the
Haskell compiler that is most commonly used).

## Example of Haskell syntax and use of common libraries

The syntax of Haskell can't really be summarized neatly in one go because there's been so much new
syntax added over the years via extensions that it'd be too much to take in. However,
[here](./misc/syntax.md) is an attempt at introducing most of the basic syntax that a simple project
could use.

## Next steps

[Interacting with the terminal](./steps/01-interacting-with-the-terminal.md) has introductory
material about executing effectful things and an intro to OS interaction in Haskell code.