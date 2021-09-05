# Await and `<-`

- [Await and `<-`](#await-and--)

In chapter 3 there is a reference made to how `<-` is an awful lot like `await` in TypeScript. In
this file we try to make that comparison clearer by showing them side by side:

We start with the Haskell example, here with extra type annotations to make it abundantly clear what
type each value and function has:

```haskell
import Prelude
import qualified System.Environment as Environment

main :: IO ()
main = do
  dockerFileName :: String <- Environment.getEnv "DOCKERFILE" :: IO String
  dockerFileContents :: String <- readFile dockerFileName :: IO String

  putStrLn dockerFileContents :: IO ()
```

Imagine this TypeScript program that will essentially be the same:

```typescript
async function main(): Promise<void> {
  const dockerFileName: string = await Environment.getEnv("DOCKERFILE"); // Promise<string>
  const dockerFileContents: string = await readFile(dockerFileName); // Promise<string>

  console.log(dockerFileContents);
}
```

When we `await` here, we are saying that the `Promise` will be evaluated, and the value that we bind
to on the left side has no notion of `Promise`-ness in the rest of the code. In our `IO` code we get
the same type of code by using `do` notation and `<-`. The bindings on the left side have no `IO`
attached to them because `<-` instructs Haskell to evaluate the `IO` action and bind the result to
the name on the left side.
