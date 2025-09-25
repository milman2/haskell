# haskell
Haskell for DSL

## GHC (Glasgow Haskell Compiler)

## STack
```
choco install haskell-stack
```

## Haskell Language Server (HLS) for VS Code
```
stack install haskell-language-server
```

## VS Code Extensions
- Haskell
- Haskell Syntax Highlighting
- Haskell GHCi Debug Adapter (optional)

## New Project
```shell
stack new my-haskell-project
cd my-haskell-project
stack setup
stack build
```

## Example
```hs
main :: IO ()
main = putStrLn "Hello, Haskell!"
```

```shell
stack run
```