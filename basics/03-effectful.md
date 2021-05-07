# "Effectful"

Lots of texts, these materials included, will talk about things being "effectful". So what does that
actually mean?

One way to view it is that an effectful thing can return different values at different times even
when given the same parameters, or cause something to happen externally, perhaps as the main purpose
of running the function.

## IO

`IO` means that the function executing this has the capability to do effectful things. You can view
`IO` as a license to access basically all APIs. Executing in the "IO monad" can be viewed as having
full privileges to do whatever we want, be it reading/writing from/to a disk, talking to the
Internet or "launching the missiles", as people like to joke. For most programs, `IO` is where we
affect or gather data from reality and so it's where most interesting things happen.

## IO ()

`IO ()` is a fairly common type signature for effectful functions. What does it mean to have
something follow `IO` like this? It can be said that types can have arguments. In this particular
case we can say that `IO` has the "type" `IO :: Type -> Type`. The type `IO ()` is therefore `IO`
applied to `()` which produces the type `IO ()`.

Likewise we can also have `IO String` which is `IO` applied to `String`, which produces the type
`IO String`. IO has the "kind" `* -> *` where the asterisks are types. `IO` itself can be seen
as a type constructor in the type system, that requires a type to be passed to it in order to
construct a concrete one.

### Interlude: IO is "higher-kinded"

It can be helpful to draw a parallell to "higher-order functions", i.e. functions that take and/or
return other functions. `IO` (and other types that themselves take type arguments) can be seen as
"higher-order types" that take type arguments in order to return concrete types.

In reality, all types have kinds in Haskell, as we can observe in `ghci`:

```haskell
Q> :kind Int
Int :: *
Q> :kind []
[] :: * -> *
Q> :kind IO
IO :: * -> *
Q> :kind Map      
Map :: * -> * -> *
Q> :kind Set      
Set :: * -> *
Q> :kind Maybe
Maybe :: * -> *
Q> :kind Either
Either :: * -> * -> *
```

The common thread here is that for each type, the amount of asterisks we see are directly related to
how many type arguments the types take. `Int` has zero type arguments and just referring to `Int` is
itself enough to have a concrete type.

In contrast, `[]`, `Set` and `Maybe` take one type argument, so if we say only `[]`, `Set` and
`Maybe` we can see that they still take more arguments to create concrete types. We can still do the
following, however:

```haskell
Q> :kind [Int]
[Int] :: *
Q> :kind Maybe String
Maybe String :: *
Q> :kind Set Float
Set Float :: *
```

Since we've now passed type arguments to these type constructors we're now back to one asterisk,
meaning we have concrete types. With this in mind it's not hard to see why type constructors can be
considered function applications in the type-level.

To provide a complete picture, let's see the same with `Map` and `Either`:

```haskell
Q> :kind Map
Map :: * -> * -> *
Q> :kind Map String
Map String :: * -> *
Q> :kind Map String Int
Map String Int :: *
Q> :kind Either
Either :: * -> * -> *
Q> :kind Either String
Either String :: * -> *
Q> :kind Either String (IO Int)
Either String (IO Int) :: *
```

### Back to `IO ()`

So what is it about `IO ()` that makes it so common in libraries and APIs? `IO` means we have an
essentially arbitrary action, and `()` is the type representing "No interesting return value", which
means that the closest analog we can find in other languages is `void`. Functions that return `IO ()`
are used because they cause something to happen and that's their main purpose.

Examples:

```haskell
-- Create a directory in the file system
System.Directory.createDirectory :: FilePath -> IO ()

-- Remove a directory in the file system
System.Directory.removeDirectory :: FilePath -> IO ()

-- Flush the current GL context
Graphics.Rendering.OpenGL.GL.FlushFinish.flush :: IO ()

-- Set an environment variable to a certain value.
System.Environment.setEnv :: String -> String -> IO ()
```

As we can see, it's not uncommon for these functions to take parameters, but the main thing they
have in common is that they cause something to happen, which is likely the reason we're running them.
These can be the building blocks that our program uses behind the scenes in order to actually do
something in the end.

## IO a

So what happens when we want to use functions that do effectful things but we also want to use
their return values? Well, their function signatures are going to have `IO a` at the end, where `a`
stands in for any type you might be interested in. An example:

```haskell
-- Get the current value of an environment variable.
System.Environment.getEnv :: String -> IO String
```

We can see here that we are passing the function a `String` and getting an `IO String` back. We are
executing "in the IO monad", so this is something effectful that can do basically anything.

Technically speaking, when we have a **value** of type `IO a` we in actuality have an action that
when executed will produce a value of type `a`. When we use `<-` in our code we are running that
action and binding the **result**, the `a` in this case, to the name on the left.

```haskell
import qualified System.Environment as Environment

main :: IO ()
main = do
  dockerFileName <- Environment.getEnv "DOCKERFILE" -- has the type `IO String`
  dockerFileContents <- readFile dockerFileName -- `IO String` again
  putStrLn dockerFileContents -- `IO ()`
```

It's perhaps helpful to draw the analogy to `await` in JavaScript, where we sometimes write our code
"in the `Promise` monad" and so we can do asynchronous things. We unpack these asynchronous values
by using `await` (or `.then()` for people who aren't up-to-date) and when we refer to them in code
the asynchronous nature does not matter in terms of the values they represent.

## What makes `IO` special?

In reality, nothing. `IO` isn't really the bit that's special. Every program you've ever written in
a language that didn't have this concept was always basically running in the `IO` monad, except we
usually do not have access to the actions we execute **as values** and we generally don't talk
about these actions in the type system of whatever language we're using. When you write
`putStrLn "hello"` in Haskell, you are in fact creating a value. Passing that value around is
trivial, so it can be used in other functions. Fundamentally speaking, however, `IO` as "a context
in which we can do whatever we want" is not the part that should jump out at you as new territory.

It's perhaps more interesting that in Haskell we are able to say that certain functions **can't** do
these interesting things; they're only for computing values. This means that we can now definitively,
in our APIs, say that a callback is not able to talk to the network, for example, or do its own
logging.

The following is an illustrative example:

```haskell
maybeReadEvent :: (ByteString -> Maybe Message) -> Socket -> IO (Maybe Message)
maybeReadEvent messageDecoder socket = do
  ...
```

Since our first argument doesn't have the return type `IO (Maybe Message)` the only thing it can do
is either produce nothing from a given byte string, or produce a value of type `Message`. This is a
sensible design choice for a decoding function, and one we can make explicit in our API. Attempting
to do effectful things in this function will lead to using functions like `unsafePerformIO` and
friends, making it clear that one is outside of the realm of reasonable usage.

## Should you avoid effectful things?

It's a bit of a meme that Haskell programmers avoid or dislike effectful things. This is overblown
and in reality nothing useful ever gets done without at some point executing in `IO` or some context
that wraps it. With that in mind, it's still the case that pure functions can be used everywhere,
whereas the possible usage of impure functions will always depend on the context we're in.

Should a function meant to validate a data type execute in `IO`? Probably not. Common sense prevails
here and software is iterative; you will be able to see what can be made pure and thus less
mysterious in time. Making functions pure is not a chore to be done to appease the Haskell gods, but
is mostly a question of removing future questions in future debugging sessions.
