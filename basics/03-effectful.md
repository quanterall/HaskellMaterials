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
something follow `IO` like this? To really get into this we would have to explain the concept of
"kinds", but in order to shed some light on it without going there, it can be said that types can
have arguments. In this particular case we can say that `IO` has the "type" `IO :: Type -> Type`.
The type `IO ()` is therefore `IO` applied to `()` which produces the type `IO ()`.

Likewise we can also have `IO String` which is `IO` applied to `String`, which produces the type
`IO String`, **or any other type** you can imagine. The actual names for these things are "kinds",
as in "IO has the kind `* -> *`" where the asterisks are types, so it takes one type and returns a
total one.

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

## IO t

So what happens when we want to use functions that do effectful things but we also want to use
their return values? Well, their function signatures are going to have `IO t` at the end, where `t`
stands in for any type you might be interested in. An example:

```haskell
-- Get the current value of an environment variable.
System.Environment.getEnv :: String -> IO String
```

We can see here that we are passing the function a `String` and getting an `IO String` back. We are
executing "in the IO monad", so this is something effectful that can do basically anything.

Technically speaking, when we have a **value** of type `IO t` we in actuality have an action that
when executed will produce a value of type `t`. When we use `<-` in our code we are running that
action and binding the **result**, the `t` in this case, to the name on the left.

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
a language that didn't have this concept was always basically running in the `IO` monad. You're able
to do whatever you want whenever you want.

It's perhaps more interesting that in Haskell we are able to say that certain functions **can't** do
these interesting things; they're only for computing values. This means that we can now definitively,
in our APIs, say that a callback is not able to talk to the network, for example, or do its own
logging.

## Should you avoid effectful things?

It's a bit of a meme that Haskell programmers avoid or dislike effectful things. This is overblown
and in reality nothing useful ever gets done without at some point executing in some context that
either wraps or plain runs `IO` expressions.

Should a function meant to validate a data type execute in `IO`? Probably not. Common sense prevails
here and software is iterative; you will be able to see what can be made pure and thus less
mysterious in time. Making functions pure is not a chore to be done to appease the Haskell gods, but
is mostly a question of removing future questions in future debugging sessions.
