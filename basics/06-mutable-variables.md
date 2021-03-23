# Mutable variables

While there is a lot of focus on purity in parts of the Haskell community, Haskell has runtime
support for mutable variables that stretches beyond most programming environments' support. The
difference is that by default things are immutable and we have to opt in to using mutable variables.

It's also important to note that even though purity is desired, one of the few established design
patterns that Haskell has and that is well known to work well for most applications relies on
mutable variables to work.

This is not done because people just love mutable variables, but rather because the alternatives are
sometimes exception-unsafe, harder to use or otherwise have some undesirable quality.

With that in mind we'll look at some types of mutable variables in isolation.

## `IORef`

`IORef` is the most basic type of mutable variable and could be said to work pretty much like a
pointer to a value.

```haskell
main = do
  -- This produces a `IORef Int`, `newIORef` has type `a -> IO (IORef a)
  reference <- newIORef 42

  -- Reading the reference here has type `IORef Int -> IO Int`
  valueOfReference <- readIORef reference

  -- `42`
  print valueOfReference

  -- `writeIORef` sets a new value for the reference
  writeIORef reference 1337
  newValueOfReference <- readIORef reference
  -- `1337`
  print newValueOfReference

  -- We can also call a function on the reference with `modifyIORef`
  modifyIORef reference (\value -> value + 2) -- or just `(+ 2)`
  modifiedValueOfReference <- readIORef reference
  -- `1339`
  print modifiedValueOfReference
```

This is the basic interface of an `IORef` and there are some other utility functions or slightly
different ones that you can also use, but this covers the essential area of the responsibilities an
`IORef` can cover. Note that just like a pointer to a value, modifying **and** reading it from
several threads at once is undesirable because of race conditions. For this purpose you can actually
use `atomicModifyIORef`. This can work for just having one value, but it's likely the case that you
want to use a `TVar` instead in the long run.

## `TVar`

`TVar`s are **transactional variables**. This means that they can be used much like you would use
transactions in a database. The behavior of these is slightly more involved than `IORef`s for
obvious reasons, but what we get out of them is the possibility of race-condition free code that can
be scaled up to arbitrary numbers of green threads.

Transactional functions are executed inside of the `STM` monad. This means that we are working in a
reduced context that doesn't allow `IO`. This guarantee enables our transactional blocks to be
**retried** if we find out that checks that we want to guarantee on their values don't pass. In
order to execute one of these transactional blocks from `IO` we use the function `atomically`:

```haskell
main = do
  -- Either everything in this block succeeds or nothing does; this is the
  -- guarantee we get from `STM`.
  atomically $ do
    transactionalValue <- readTVar transactionalVariable
    -- If this does not hold we will automatically retry the block and it will
    -- only run again if `transactionalVariable` has changed.
    -- Until `transactionalVariable` changes it will block.
    check $ transactionalValue < neededValue
    modifyTVar transactionalVariable (\v -> v + 1)
    writeTVar otherTransactionalVariable $ transactionalValue + 42
```

Here are the types of the different transactional functions above:

```haskell
-- Take an `STM` block and execute it in its entirety to get a result.
atomically :: STM a -> IO a

readTVar :: TVar a -> STM a

-- Runs the given function on the value inside of a TVar.
modifyTVar :: TVar a -> (a -> a) -> STM ()

-- Instead sets the value of the variable to the value given.
writeTVar :: TVar a -> a -> STM ()

-- We can create new `TVar`s with `newTVar` or `newTVarIO`
newTVar :: a -> STM (TVar a)
newTVarIO :: a -> IO (TVar a)
```

We can create transactional variables in `STM` with the `newTVar` function. However, creating a
transactional variable isn't always a transactional action; if a transactional variable is
completely new there is absolutely nothing that can depend on it and it can therefore not affect any
other transactions currently being executed. We can therefore create transactional variables with
the `newTVarIO` function.

### Use of `TVar`s

`TVar`s are great for when you need to have values in your application state that can be interacted
with from several places. How about extensions of this kind of behavior, though? Fortunately `STM`
comes with a few good building blocks for more advanced constructions that could commonly be needed.

`TQueue` is a transactional queue that will automatically block on reading the queue and re-read it
when the variable has changed, meaning we can spawn a thread that will react to all new items in the
queue and otherwise idle, providing the perfect basis for a worker queue.

`TChan` is a transactional channel, much like `TQueue` but has all readers of the channel see each
value being written to it, meaning they will all be able to react to every update.

Both of these are unbounded in size, meaning that unless we read values faster or as fast as we are
writing them we will have issues with memory usage. That is why there are versions of these called
`TBQueue` and `TBChan`, where the "B" stands for bounded. This allows us to specify a maximum size
for them, which provides more of a guarantee that we aren't consuming more and more memory. It also
provides backpressure for the writing side as it will block on a full queue/channel, retrying when/if
the queue/channel changes.

If you require the ability to close your queue or channel, there are also "M" variants that allow
you to close them. When a reader tries to read a value they will get a `Nothing` value back from it
and can then be guaranteed that the queue or channel is closed from then on.

So closable queues and channels are called `TMQueue` & `TMChan`. How about the combination of these
extra guarantees/attributes? `TBMQueue`s and `TBMChan`s. Yes, those are real things. This may seem
ridiculous, but it does provide the ability to get a specific behavior and to guarantee this
behavior in the type system. If something really does need a bounded, closable queue it should
specify that it needs a `TBMQueue`.

For a queue of one or zero elements we can use `TMVar`, which the GHC documentation describes as
follows:

> A TMVar is a synchronising variable, used for communication between concurrent threads. It can be
thought of as a box, which may be empty or full.

This means that we can essentially use it to block immediately from a writing thread when a value
already exists and block immediately from a reading thread when a value does not exist. If it sounds
like this will be fairly slow if we know we're going to have several items that's because it will
be. The upside of this is that you **can** get this very tight blocking behavior and ensure that
threads move in some kind of lockstep.