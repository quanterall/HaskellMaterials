# Parsing with `megaparsec`

- [Parsing with `megaparsec`](#parsing-with-megaparsec)
  - [`ParsecT e s m a`](#parsect-e-s-m-a)
    - [Errors (`e`)](#errors-e)
    - [Stream (`s`)](#stream-s)
    - [Monad (`m`)](#monad-m)
    - [Result (`a`)](#result-a)
  - [Creating your own `Parser` type](#creating-your-own-parser-type)
  - [Parsing `/etc/hosts`](#parsing-etchosts)
    - [Some basic imports to get out of the way](#some-basic-imports-to-get-out-of-the-way)
  - [Some useful helpers for parsing](#some-useful-helpers-for-parsing)
    - [Our simple `Parser` type](#our-simple-parser-type)
    - [What will we be parsing in `/etc/hosts`?](#what-will-we-be-parsing-in-etchosts)
    - [What does the information look like?](#what-does-the-information-look-like)
    - [Starting from the top](#starting-from-the-top)
  - [Parsing a simple scripting language](#parsing-a-simple-scripting-language)
    - [The language](#the-language)
    - [An additional helper](#an-additional-helper)
    - [A plan of action](#a-plan-of-action)
      - [Some initial types](#some-initial-types)
      - [Some initial high-level code](#some-initial-high-level-code)
      - [The `AssignValue` statement](#the-assignvalue-statement)
    - [What next?](#what-next)
  - [Final words](#final-words)

It's very common to write compilers and interpreters for both small and large languages in Haskell.
For this task there are several tools, but a particularly interesting one is the
[megaparsec](https://www.stackage.org/lts-18.23/package/megaparsec-9.0.1) library.

## `ParsecT e s m a`

Parsing in `megaparsec` is intrinsically tied to the
[`ParsecT`](https://www.stackage.org/haddock/lts-18.23/megaparsec-9.0.1/Text-Megaparsec.html#t:ParsecT)
type. As the "T" suffix suggests, it's a monad transformer, which means it's a monad that wraps
another monad.

In very basic use, we can wrap what is known as the `Identity` monad, which is a monad that has no
effects. It's akin to having a "pure" monad, i.e. one that just deals with values. This is what the
[`Parsec`](https://www.stackage.org/haddock/lts-18.23/megaparsec-9.0.1/Text-Megaparsec.html#t:Parsec)
type is actually for; it's an alias for `ParsecT e s Identity`. This means that we can use `Parsec`
as our type when we're not interested in baking in any extra effects.

### Errors (`e`)

The `e` in `ParsecT e s m a` is what type of custom error data type we have. When we intend to only
signal textual error information, as can be the case with many compilers or the like, we can set
this to `Void`, which is a type that has **no inhabitants**, meaning it is impossible to construct.

### Stream (`s`)

The stream type `s` in `ParsecT e s m a` determines what your source type will be, i.e. whether or
not you're parsing a `String`, `Text` and so on.

### Monad (`m`)

If we want to do interesting things while parsing, we can bake in effects from whatever monad we
have that can support those things. We'll look at a fairly simple example of where this is useful
later in this document. For the sake of example, however, let's say we want to be able to read and
modify a map of strings to integers while parsing, our `ParsecT` usage might look as follows:

```haskell
parseSomeIntegerInOurFile :: ParsecT Void Text (RIO (IORef (Map String Int))) Int
parseSomeIntegerInOurFile = do
  ...
```

The monad we wrap in this case has one clear purpose; to read and modify this map of strings while
we are parsing our text.

### Result (`a`)

Every `ParsecT` expression has a type `a`, which always represents the result of executing that
particular action. In our previous example it was an `Int`, which means that if we executed this
expression `intValue` would be of type `Int`:

```haskell
data SomeOtherThing = SomeOtherThing
  { intValue :: !Int,
    textValue :: !Text
  }

parseSomeOtherThing :: ParsecT Void Text (RIO (IORef (Map String Int))) SomeOtherThing
parseSomeOtherThing = do
  intValue <- parseSomeIntegerInOurFile
  _ <- char ':'
  textValue <- many alphaNumChar
  pure $ SomeOtherThing {intValue, textValue}
```

## Creating your own `Parser` type

As you can see in the examples above, there is a lot of tedium in specifying your entire `ParsecT`
type all the time. Especially so because it simply cannot be composed if the types are different
anyway, so it only makes sense to have one type representing it.

A fairly common thing when we write a parser is to create a type alias or a `newtype` that wraps the
`ParsecT` type and any extra monads we'd like to have access to. One example of this could look as
follows:

```haskell
type Parser = ParsecT Void Text (RIO (IORef (Map String Int)))

data SomeOtherThing = SomeOtherThing
  { intValue :: !Int,
    textValue :: !Text
  }

parseSomeIntegerInOurFile :: Parser Int
parseSomeIntegerInOurFile = do
  ...

parseSomeOtherThing :: Parser SomeOtherThing
parseSomeOtherThing = do
  intValue <- parseSomeIntegerInOurFile
  _ <- char ':'
  textValue <- many alphaNumChar
  pure $ SomeOtherThing {intValue, textValue}
```

At this point we've baked in the assumption that each parser function has access to a reference to
a map of strings to integers, and each of our parsing functions can be described simply as
`Parser a` rather than `ParsecT Void Text (RIO (IORef (Map String Int))) a`.

If we want to extend what the parsers have access to we can do so exactly as we would otherwise,
either via a deeper monad stack or simply just a bigger environment with more things in it. The
latter is what I personally would do in most circumstances, because it's just inherently the most
flexible and straight-forward way to do things.

## Parsing `/etc/hosts`

As a straight forward example of parsing that does not need any interesting effects, let's parse
`/etc/hosts` into custom data types.

### Some basic imports to get out of the way

```haskell
module HostsParser where

import RIO
import qualified RIO.Text as Text
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
```

## Some useful helpers for parsing

There are 3 functions that end up being useful in almost every case of parsing, in order to keep
code focused on the important bits, i.e. not talking too explicitly about whitespace and parsing it
away at every step:

```haskell
-- | Defines how whitespace is consumed.
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space MChar.space1 (Lexer.skipLineComment "#") Megaparsec.empty

-- | Applies a parser and any amount of whitespace after.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

-- | Reads a specific string of text and any amount of whitespace after.
symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer
```

### Our simple `Parser` type

We'll start with defining our `Parser` type:

```haskell
-- We could also use `Parsec` here, since it's just an alias for using `Identity` as `m`.
type Parser = ParsecT Void Text Identity
```

### What will we be parsing in `/etc/hosts`?

It doesn't serve much of a purpose to cover literally everything from this format, if we're being
honest, so I'm going to concentrate mainly on how to cover IPv4 addresses and hostnames. This should
illustrate most of what you need in order to get started with parsing. Most of your knowledge in
this area should come from motivated application of the basic ideas here and trying to solve
problems (even if made up) with parsing.

Here is an example of a file:

```text
# This file was automatically generated by WSL. To stop automatic generation of this file, add the following entry to /etc/wsl.conf:
# [network]
# generateHosts = false
127.0.0.1	localhost
127.0.1.1	Spectre.localdomain	Spectre
﻿
192.168.0.12	chaosknight
192.168.0.23	omniknight
127.0.0.1	dashboard.local
192.168.100.5	host.docker.internal
192.168.100.5	gateway.docker.internal
127.0.0.1	kubernetes.docker.internal
```

Note that the `<feff>` character here is output by my editor and that it represents an invisible
space. It's not terribly uncommon that you'll find these types of characters in files, so it's
important to keep this in mind when writing parsers.

### What does the information look like?

```haskell
newtype Filename = Filename {unFilename :: Text}
  deriving (Eq, Show)

data HostEntry = HostEntry
  { hostEntryIp :: !IPAddress,
    hostEntryHostnames :: ![Text]
  }
  deriving (Eq, Show)

data IPAddress = IPAddress !Int !Int !Int !Int
  deriving (Eq, Show)
```

### Starting from the top

It's not unreasonable to start out with a high-level view of this problem. We want to parse several
`hosts` entries from this file and they are separated by newlines. Each entry will start with an IP
address and then a list of hostnames:

```haskell
-- | This is the function we use in order to actually read a file and parse it.
parseHostsFile :: FilePath -> IO (Either (Megaparsec.ParseErrorBundle Text Void) [HostEntry])
parseHostsFile path = do
  contents <- readFileUtf8 path
  pure $ parseHostsText (Filename path) contents

-- | This is the function that actually makes sure we run the parser on content. Everything that
-- follows from the invocation of `hostsFileP` is going to be executing in our `Parser` context.
parseHostsText :: Filename -> Text -> Either (Megaparsec.ParseErrorBundle Text Void) [HostEntry]
parseHostsText (Filename filename) =
  -- The reason we use `runParser` here instead of `runParserT` is because we have the `Identity`
  -- monad as our baked in monad. This way we will automatically unwrap the value since it doesn't
  -- depend on any interesting `m` type.
  --
  -- Note how we consume all initial whitespace in the file before we start parsing. This turns out
  -- to be a fairly common occurrence, as it's common for configuration files and other types of
  -- files to start with comments in the language/format that they're written in.
  Megaparsec.runParser (spaceConsumer *> lexeme hostsFileP) filename

-- | This is a parser for what we can consider the entire file, so it's in effect a parser that
-- should, in a higher-level manner, describe the entire file.
hostsFileP :: Parser [HostEntry]
hostsFileP =
  -- Each entry is parsed with `lexeme hostentryP`, which means that we'll consume all whitespace
  -- after the entry. This means we can just say "We want to read many host entries and whitespace
  -- after", as each entry will follow the whitespace that is also consumed.
  many $ lexeme hostEntryP
```

We've now described that a hosts file is parsed by reading at least one host entry, and if we have
several they will be separated by newlines. At this point we have to consider how we want to parse a
`HostEntry`:

```haskell
hostEntryP :: Parser HostEntry
hostEntryP = do
  -- The line can start with any amount of space characters and then we'll expect to read an IP
  -- address. Note how we are using `*>` here to say that the thing on the right is what we want the
  -- expression to return, but we still want to run the thing on the left. The result of the left
  -- expression is going to be discarded.
  ipAddress <- MChar.space *> ipAddressP

  -- We expect at least a single space between the IP address and the host names, but we will
  -- consume more if available. We use `hspace1` because we specifically expect spaces, not other
  -- whitespace like newlines.
  MChar.hspace1

  -- After we've consumed all the spaces we want to read a list of host names separated by some
  -- amount of spaces. We use `hspace1` because it does not consume newlines.
  hostnames <- Megaparsec.sepBy1 hostNameP MChar.hspace1

  -- If we've reached this point, none of the parsers have failed
  pure HostEntry {hostEntryIp = ipAddress, hostEntryHostnames = hostnames}
```

We now have to define how to parse an IP address as well as a host name:

```haskell
ipAddressP :: Parser IPAddress
ipAddressP = do
  -- We want to parse exactly four integers separated by periods. If we were interested in parsing
  -- any amount of digits separated by periods we could use `sepBy1 Lexer.decimal (MChar.char '.')`

  -- Note how we use the `<*` here to say that whatever is on the left side is what we want the
  -- expression to actually return, but we still want to execute the thing on the right.
  a <- Lexer.decimal <* MChar.char '.'
  b <- Lexer.decimal <* MChar.char '.'
  c <- Lexer.decimal <* MChar.char '.'
  IPAddress a b c <$> Lexer.decimal

hostNameP :: Parser Text
hostNameP = do
  -- We want our host names to start with letters, so let's say that explicitly.
  firstCharacter <- MChar.letterChar
  -- The remaining characters can really be any of several choices:
  -- letters, digits, underscores, dashes or periods.
  remainingCharacters <- some hostNameCharacterP

  pure $ Text.pack (firstCharacter : remainingCharacters)

hostNameCharacterP :: Parser Char
hostNameCharacterP =
  Megaparsec.choice
    [ MChar.letterChar,
      MChar.digitChar,
      MChar.char '_',
      MChar.char '-',
      MChar.char '.'
    ]
```

If we now run `parseHostsFile` as follows we should see that it succeeds:

```haskell
Q> parseHostsText (Filename "/etc/hosts") "127.0.0.1\tlocalhost"
Right
    [ HostEntry
        { hostEntryIp = IPAddress 127 0 0 1
        , hostEntryHostnames = [ "localhost" ]
        }
    ]
Q> parseHostsText (Filename "/etc/hosts") "127.0.0.1\tlocalhost\n   192.168.0.101  \tomniknight omniknight.local"
Right
    [ HostEntry
        { hostEntryIp = IPAddress 127 0 0 1
        , hostEntryHostnames = [ "localhost" ]
        }
    , HostEntry
        { hostEntryIp = IPAddress 192 168 0 101
        , hostEntryHostnames =
            [ "omniknight"
            , "omniknight.local"
            ]
        }
    ]
```

## Parsing a simple scripting language

What follows below is a description and parsing implementation of a very simple scripting language.
The code for this is available
[here](https://github.com/quanterall/parsing-with-megaparsec/blob/main/src/ScriptingLanguage.hs)
and it will be annotated and added here in time. What you see below is the current progress of
taking it and adding it here in what is intended to be an understandable way.

### The language

While this is meant to show more interesting things, the fundamental purpose is still just to be an
example of something that is more interesting and involves baking in effects in our `Parser` type.
The purpose is not at all to make some big, complete language. With that in mind, let's look at an
example script:

```c
// Comments start with two slashes

// We can assign a string to an identifier by using the equals sign and double quotes
user = "pesho"

// We can assign the result of running a shell command to a value
result = 'ls -l'

// We can assign the stdout of a shell command to an identifier by enclosing the
// command in single quotes and accessing the `.output` field of the shell invocation
output = 'ls -l'.out

// Or we can pull out stderr
error = 'ls -l'.err

// Likewise the exit code can be accessed as well
exitCode = 'ls -l'.code

// We can use an `if` to conditionally execute code
if result {
  'echo Success!'
} else {
  'echo Failure!'
}

// We can use string interpolation with backticks and curly braces
'echo `Output: {output} | Error: {error} | Exit code: {exitCode}`'
```

Looking at the above, there are some things that should stand out:

- We have bindings to values and these can be referenced later. We don't want to references values
  that don't actually exist yet.
- We have a shell execution construct that has different "fields" that we can access.
- We have a basic `if` for conditional execution. This also means we need value comparisons.
- We have a basic string interpolation construct.

### An additional helper

There is a helper that can be very useful in a bigger parsing endeavor; `reportError`:

```haskell
-- | Returns an error to be displayed.
reportError :: String -> Parser a
reportError = Megaparsec.ErrorFail >>> Set.singleton >>> Megaparsec.fancyFailure
```

This helper allows us to return an arbitrary error string in our parser, which can allow us to give
better error information than just what the parser itself provides in terms of expectations.

### A plan of action

When it comes to slightly bigger problems like this, it can be helpful to start from the top and
establish a high-level plan that still does not define every possible construct yet.

With that in mind we'll define the concept of bindings as well as literal values so that we can
parse the first line of the script above (`user = "pesho"`):

#### Some initial types

Let's look at some initial types that should reflect how the language works and what's in it:

```haskell
-- Our parser will have access to `ParsingState`
type Parser = Megaparsec.ParsecT Void Text (RIO ParsingState)

newtype BindingName = BindingName {unBindingName :: Text}
  deriving (Eq, Ord, Show)

data ParsingState = ParsingState
  { -- | This holds current bindings within the script. Note that this does not consider scopes.
    -- A binding is just a name and an associated expression that can be evaluated.
    bindingsRef :: !(IORef (Map BindingName Expression))
  }

-- | Scripts are composed out of statements and expressions.
data ScriptComponent
  = Statement !Statement
  | Expression !Expression
  deriving (Eq, Show)

-- | A statement is an action of some sort in our code, that won't have a return value.
data Statement
  = AssignValue !BindingName !Expression
  | IfStatement !Expression ![ScriptComponent] ![ScriptComponent]
  deriving (Eq, Show)

-- | An expression is a piece of code that can be evaluated and has a return value after evaluation.
data Expression
  = -- | "A string"
    StringLiteral !Text
  | -- | 42
    IntegerLiteral !Integer
  | -- | 1337.0
    FloatLiteral !Double
  | -- | true / false
    BooleanLiteral !Bool
  | -- | `A string with a {binding}`
    InterpolatedString ![StringInterpolationFragment]
  | -- | 'ls -l `{inputDirectory}`'
    -- 'ls -l `{inputDirectory}`'.out
    -- 'ls -l `{inputDirectory}`'.err
    -- 'ls -l `{inputDirectory}`'.code
    ShellCommand ![ShellCommandText] !(Maybe ShellCommandComponent)
  | -- | (When a binding with the name `binding` exists) -> binding
    BindingExpression !BindingName
  deriving (Eq, Show)
```

This should give a high-level view of what we are working with. Some of the types are not shown here
but we'll take a look at those when the time comes to use them.

We have both statements and expressions, meaning we have pieces of code that have no return value
or represent no value, in statements. Expressions, on the other hand, can be used where one expects
a value, i.e. when we assign a value to a variable, or in the condition part of an `if` statement.

This means, of course, that we cannot assign the "result" of an `if` to a variable. Ideally, in a
real programming language, you would want to be able to do this, because `if` should ideally be an
expression, but we are writing a simple scripting language.

#### Some initial high-level code

```haskell
parseScript ::
  Filename ->
  Text ->
  IO (Either (Megaparsec.ParseErrorBundle Text Void) [ScriptComponent])
parseScript (Filename filename) text = do
  bindingsRef' <- newIORef mempty
  let initialState = ParsingState {bindingsRef = bindingsRef'}
  -- `runParserT` here is going to return a `RIO ParsingState (Either ...)` so we take that and
  -- give it to `runRIO` which will unpack that into an `IO (Either ...)`
  runRIO initialState $ Megaparsec.runParserT scriptComponentsP filename text

scriptComponentsP :: Parser [ScriptComponent]
scriptComponentsP =
  -- A script is composed of a series of statements and expressions that can be preceded by
  -- whitespace and comments. The potential whitespace and script components will be separated by
  -- newlines.
  Megaparsec.sepEndBy
    (maybeWhiteSpaceAnd $ Megaparsec.choice [Statement <$> statementP, Expression <$> expressionP])
    (some MChar.newline)
  where
    maybeWhiteSpaceAnd :: Parser a -> Parser a
    maybeWhiteSpaceAnd p = optional spaceConsumer *> p

statementP :: Parser Statement
statementP = undefined

assignValueP :: Parser Statement
assignValueP = undefined

ifStatementP :: Parser Statement
ifStatementP = undefined

expressionP :: Parser Expression
expressionP = undefined
```

We start our parsing in `parseScript`, where we also set up our initial state

#### The `AssignValue` statement

If we want to parse assign statements, we'll have to define both our `statementP` function as well
as a `assignValueP` function:

```haskell
statementP :: Parser Statement
statementP = do
  Megaparsec.choice [ifStatementP, assignValueP]

bindingNameP :: Parser BindingName
bindingNameP = do
  -- A binding name starts with a letter
  initialCharacter <- MChar.letterChar
  -- ... the rest of the characters can be any alphanumeric character or underscore
  restOfName <- Megaparsec.many (MChar.alphaNumChar <|> MChar.char '_')
  pure $ BindingName $ Text.pack (initialCharacter : restOfName)

bindValue :: BindingName -> Expression -> Parser ()
bindValue bindingName expression = do
  -- We read our bindings reference (a map of binding names to expressions)
  ref <- asks bindingsRef
  -- We then modify it by inserting our expression in the corresponding slot
  modifyIORef ref $ Map.insert bindingName expression

assignValueP :: Parser Statement
assignValueP = do
  -- If we can read a binding name followed by an equals sign and an expressions we want to add it
  -- to the bindings map.
  bindingName <- lexeme bindingNameP
  _ <- symbol "="
  expression <- expressionP
  bindValue bindingName expression
  pure $ AssignValue bindingName expression

ifStatementP :: Parser Statement
ifStatementP = undefined -- We leave `if` statements out for now
```

We also need to parse some kind of expression that we can use as part of our assignment. Let's
implement reading of literal strings:

```haskell
expressionP :: Parser Expression
expressionP =
  -- for now we can leave only string literals
  Megaparsec.choice [stringLiteralP]

stringLiteralP :: Parser Expression
stringLiteralP = do
  -- A string starts with a double quote
  _ <- MChar.char '\"'
  -- The contents of it will be many printable characters, but we can also have escaped quotes,
  -- which we need to handle. The `try` function here will try to apply a parser, but if it fails
  -- will not end up consuming any input. This makes it so that we can say "Try to parse this but
  -- put the content back if you end up failing".
  -- When this first part fails, we'll move on tho `MChar.printChar` because we use `<|>`, the
  -- alternative operator.
  -- The string stops when we find a normal double quote.
  string <-
    Megaparsec.manyTill (Megaparsec.try readEscapedQuote <|> MChar.printChar) (MChar.char '\"')
  pure $ StringLiteral $ Text.pack string
  where
    readEscapedQuote :: Parser Char
    readEscapedQuote = do
      -- An escaped double quote is a backslash followed by a double quote
      MChar.char '\\' *> MChar.char '\"'
```

If we try this out in the REPL, we should see that it works:

```haskell
Q> parseScript (Filename "hello") "test = \"hello\""
Right
    [ Statement
        ( AssignValue
            ( BindingName
                { unBindingName = "test" }
            )
            ( StringLiteral "hello" )
        )
    ]
```

Let's implement `ifStatementP` so that we can consider our `statementP` function complete:

```haskell
ifStatementP :: Parser Statement
ifStatementP = do
  -- An `if` statement starts with the literal symbol "if", then an expression to evalue for
  -- truthiness, then a list of statements to execute if the expression is true, and then the symbol
  -- "else" and a list of statements to execute if the expression is false.
  _ <- symbol "if "
  condition <- lexeme expressionP
  _ <- symbol "{"
  thenBranch <- lexeme scriptComponentsP
  _ <- symbol "}"
  _ <- symbol "else"
  _ <- symbol "{"
  elseBranch <- lexeme scriptComponentsP
  _ <- MChar.char '}'
  pure $ IfStatement condition thenBranch elseBranch
```

We can try to parse an `if` statement in the REPL:

```haskell
Q> parseScript (Filename "hello") "if \"value\" { test = \"hello\" } else { test = \"world\" }"
Right
    [ Statement
        ( IfStatement
            ( StringLiteral "value" )
            [ Statement
                ( AssignValue
                    ( BindingName
                        { unBindingName = "test" }
                    )
                    ( StringLiteral "hello" )
                )
            ]
            [ Statement
                ( AssignValue
                    ( BindingName
                        { unBindingName = "test" }
                    )
                    ( StringLiteral "world" )
                )
            ]
        )
    ]
```

Currently the only thing we have as an expression is a string literal, so that's what we've had to
pass above. Let's implement some different literals:

```haskell
expressionP :: Parser Expression
expressionP =
  Megaparsec.choice
    [ stringLiteralP,
      booleanLiteralP,
      -- The reason we want to use `floatLiteralP` here before `integerLiteralP` is that they start
      -- with the same thing; a (potentially) signed number. Float then requires more (a period).
      -- If we successfully read the number part, we don't want to fail because we failed to read
      -- the period, so we put the input back in the stream with `try`, allowing `integerLiteralP`
      -- to succeed.
      Megaparsec.try floatLiteralP,
      integerLiteralP
    ]

integerLiteralP :: Parser Expression
integerLiteralP =
  -- We use `signed` here to say that we want the capability to read both negative and positive
  -- integer literals. The `pure ()` is how to read space between the sign and the number. Here we
  -- are saying that we don't want to consume any space.
  IntegerLiteral <$> Lexer.signed (pure ()) Lexer.decimal

floatLiteralP :: Parser Expression
floatLiteralP = FloatLiteral <$> Lexer.signed (pure ()) Lexer.float

booleanLiteralP :: Parser Expression
booleanLiteralP = do
  -- A boolean literal is either `True` or `False`. We read it by saying that there are two strings
  -- you are allowed to match, then we create the boolean by comparing the result to the literal
  -- string "True".
  text <- ["True", "False"] & fmap MChar.string & Megaparsec.choice
  pure $ BooleanLiteral $ text == "True"
```

Now we can try passing different literals as the expression for our conditional:

```haskell
Q> parseScript (Filename "hello") "if 1 { test = \"hello\" } else { test = \"world\" }"
Right
    [ Statement
        ( IfStatement
            ( IntegerLiteral 1 )
            [ Statement
                ( AssignValue
                    ( BindingName
                        { unBindingName = "test" }
                    )
                    ( StringLiteral "hello" )
                )
            ]
            [ Statement
                ( AssignValue
                    ( BindingName
                        { unBindingName = "test" }
                    )
                    ( StringLiteral "world" )
                )
            ]
        )
    ]
Q> parseScript (Filename "hello") "if 1.2 { test = \"hello\" } else { test = \"world\" }"
Right
    [ Statement
        ( IfStatement
            ( FloatLiteral 1.2 )
            [ Statement
                ( AssignValue
                    ( BindingName
                        { unBindingName = "test" }
                    )
                    ( StringLiteral "hello" )
                )
            ]
            [ Statement
                ( AssignValue
                    ( BindingName
                        { unBindingName = "test" }
                    )
                    ( StringLiteral "world" )
                )
            ]
        )
    ]
Q> parseScript (Filename "hello") "if True { test = \"hello\" } else { test = \"world\" }"
Right
    [ Statement
        ( IfStatement ( BooleanLiteral True )
            [ Statement
                ( AssignValue
                    ( BindingName
                        { unBindingName = "test" }
                    )
                    ( StringLiteral "hello" )
                )
            ]
            [ Statement
                ( AssignValue
                    ( BindingName
                        { unBindingName = "test" }
                    )
                    ( StringLiteral "world" )
                )
            ]
        )
    ]
```

We are still unable to read interpolated strings, so let's add that:

```haskell
interpolatedStringP :: Parser Expression
interpolatedStringP = InterpolatedString <$> stringInterpolationFragmentsP

stringInterpolationFragmentsP :: Parser [StringInterpolationFragment]
stringInterpolationFragmentsP = do
  -- An interpolated string starts with a backtick and we read interpolation fragments until another
  -- backtick is read.
  MChar.char '`' *> Megaparsec.manyTill interpolationFragmentP (MChar.char '`')

interpolationFragmentP :: Parser StringInterpolationFragment
interpolationFragmentP =
  -- An interpolation fragment is either a binding fragment or a literal fragment. If reading a
  -- binding fragment fails, we'll put whatever we read back in the input stream and read it as a
  -- literal.
  Megaparsec.choice [Megaparsec.try bindingFragmentP, literalFragmentP]

bindingFragmentP :: Parser StringInterpolationFragment
bindingFragmentP = do
  -- A binding fragment is a curly brace followed by an available binding name, then a closing curly
  -- brace.
  _ <- MChar.char '{'
  bindingName <- availableBindingP
  _ <- MChar.char '}'
  pure $ BindingFragment bindingName

literalFragmentP :: Parser StringInterpolationFragment
literalFragmentP =
  -- A literal fragment is just all characters that aren't backticks or opening curly braces.
  (Text.pack >>> LiteralFragment) <$> Megaparsec.some literalFragmentCharacterP
  where
    literalFragmentCharacterP :: Parser Char
    literalFragmentCharacterP = Megaparsec.satisfy (`notElem` ['`', '{'])

availableBindingP :: Parser BindingName
availableBindingP = do
  -- An available binding is one where we can read a binding name and look it up in our bindings to
  -- verify that it is available.
  bindingName <- bindingNameP
  ref <- asks bindingsRef
  bindingExists <- liftIO $ Map.member bindingName <$> readIORef ref
  if bindingExists
    then pure bindingName
    else reportError $ "Binding " <> Text.unpack (unBindingName bindingName) <> " is not defined"
```

Note how we now also have the `availableBindingP` function that will error out if we are reading a
binding and it does not exist in our available binding map.

With this we also add `interpolatedStringP` to our `expressionP`:

```haskell
expressionP :: Parser Expression
expressionP =
  Megaparsec.choice
    [ -- The three different string openers are very distinct and can be matched very easily with an
      -- opening character (double quote, single quote or backtick), so we can put them first
      -- without worrying that anything else contends for the same opening input. This allows the
      -- parser to try to read the initial part, fail and move on to other alternatives with no
      -- consumed input.
      -- "..."
      stringLiteralP,
      -- `...`
      interpolatedStringP,
      booleanLiteralP,
      Megaparsec.try (BindingExpression <$> availableBindingP),
      -- The reason we want to use `floatLiteralP` here before `integerLiteralP` is that they start
      -- with the same thing; a (potentially) signed number. Float then requires more (a period).
      -- If we successfully read the number part, we don't want to fail because we failed to read
      -- the period, so we put the input back in the stream with `try`, allowing `integerLiteralP`
      -- to succeed.
      Megaparsec.try floatLiteralP,
      integerLiteralP
    ]
```

The only thing we have left to add is shell command strings. These are interesting because they can
also contain interpolated strings, so it's good that we already have a parser for that:

```haskell
shellCommandP :: Parser Expression
shellCommandP = do
  -- A shell command starts with a single quote followed by a special string that can either be
  -- shell command text or interpolated string text. It can then be followed by an accessor to say
  -- which part of the result we want to access.
  _ <- MChar.char '\''
  shellCommandText <- shellCommandTextP
  maybeShellCommandComponent <- Megaparsec.optional shellCommandComponentP
  pure $ ShellCommand shellCommandText maybeShellCommandComponent
  where
    shellCommandComponentP :: Parser ShellCommandComponent
    shellCommandComponentP = do
      -- A shell command component is an accessor for the result of a shell command. We might want
      -- to access standard out, standard error or the exit code.
      _ <- MChar.char '.'
      Megaparsec.choice
        [ MChar.string "out" *> pure ShellStandardOut,
          MChar.string "err" *> pure ShellStandardError,
          MChar.string "code" *> pure ShellExitCode
        ]

    shellCommandTextP :: Parser [ShellCommandText]
    shellCommandTextP =
      -- Shell command text is comprised of either string interpolation fragments or shell command
      -- literal text. This continues until we read a single quote.
      Megaparsec.manyTill
        ( Megaparsec.choice
            [ ShellCommandInterpolation <$> stringInterpolationFragmentsP,
              shellCommandLiteralP
            ]
        )
        (MChar.char '\'')

    shellCommandLiteralP :: Parser ShellCommandText
    shellCommandLiteralP =
      -- A shell command literal is just any character that is not a backtick or single quote.
      (Text.pack >>> ShellCommandLiteral) <$> Megaparsec.some shellLiteralCharacterP

    shellLiteralCharacterP :: Parser Char
    shellLiteralCharacterP = Megaparsec.satisfy (`notElem` ['\'', '`'])
```

With this we should actually be ready to parse our test file. Save it as `test-data/test.glue` and
run the following in your REPL:

```haskell
Q> readFileUtf8 "test-data/test.glue" >>= parseScript (Filename "test-data/test.glue")
Right
    [ Statement
        ( AssignValue
            ( BindingName
                { unBindingName = "user" }
            )
            ( StringLiteral "pesho" )
        )
    , Statement
        ( AssignValue
            ( BindingName
                { unBindingName = "result" }
            )
            ( ShellCommand
                [ ShellCommandLiteral "ls -l" ] Nothing
            )
        )
    , Statement
        ( AssignValue
            ( BindingName
                { unBindingName = "output" }
            )
            ( ShellCommand
                [ ShellCommandLiteral "ls -l" ] ( Just ShellStandardOut )
            )
        )
    , Statement
        ( AssignValue
            ( BindingName
                { unBindingName = "error" }
            )
            ( ShellCommand
                [ ShellCommandLiteral "ls -l" ] ( Just ShellStandardError )
            )
        )
    , Statement
        ( AssignValue
            ( BindingName
                { unBindingName = "exitCode" }
            )
            ( ShellCommand
                [ ShellCommandLiteral "ls -l" ] ( Just ShellExitCode )
            )
        )
    , Statement
        ( IfStatement
            ( BindingExpression
                ( BindingName
                    { unBindingName = "result" }
                )
            )
            [ Statement
                ( AssignValue
                    ( BindingName
                        { unBindingName = "outputString" }
                    )
                    ( StringLiteral "Success!" )
                )
            ]
            [ Statement
                ( AssignValue
                    ( BindingName
                        { unBindingName = "outputString" }
                    )
                    ( StringLiteral "Failure!" )
                )
            ]
        )
    , Expression
        ( ShellCommand
            [ ShellCommandLiteral "echo "
            , ShellCommandInterpolation
                [ BindingFragment
                    ( BindingName
                        { unBindingName = "outputString" }
                    )
                ]
            ] Nothing
        )
    , Expression
        ( ShellCommand
            [ ShellCommandLiteral "echo "
            , ShellCommandInterpolation
                [ LiteralFragment "Output: "
                , BindingFragment
                    ( BindingName
                        { unBindingName = "output" }
                    )
                , LiteralFragment " | Error: "
                , BindingFragment
                    ( BindingName
                        { unBindingName = "error" }
                    )
                , LiteralFragment " | Exit code: "
                , BindingFragment
                    ( BindingName
                        { unBindingName = "exitCode" }
                    )
                ]
            ] Nothing
        )
    ]
```

### What next?

Our file is parsed. If we wanted to see the result of running this script we'd have to write an
interpreter for the data structures we've parsed this file into. This can be done by walking the
list element by element and evaluating them as you imagine they should.  For an assignment we'd
probably want to have a map that maps a binding name to the result of actually executing the
expression that it's bound to. If it was a literal we'd just set the value to that literal, if it
was a shell command we'd run the shell command and get the result and so on.

## Final words

This was a whirlwind tour of parsing with Megaparsec. Don't worry if you didn't quite get everything
that we did, you can always use what we did here as a reference point when you write your own
parser. In the end this type of thing comes best with continued application and practice.

The code for both the hosts parser and the language parser can be found in
[quanterall/parsing-with-megaparsec](https://github.com/quanterall/parsing-with-megaparsec) and I
hope you'll find it useful.
