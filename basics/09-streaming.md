# Streaming

- [Streaming](#streaming)
  - [`Conduit`](#conduit)

Sometimes we want to stream values and are unable to accomplish this with lists or other data
structures. For these situations we want to use something more explicitly made for streaming.

## `Conduit`

One of the primary libraries that we can use for streaming in Haskell is called `conduit`, the other
being `pipes`. In this document we will focus on `conduit`, but feel free to explore the differences
between them on your own. You may find that one of them provides a design that matches your mental
model better.
