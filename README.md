# haskell
Haskell for DSL

## Install
### GHC (Glasgow Haskell Compiler)
```shell
curl -sSL https://get.haskellstack.org/ | sh
export PATH="$HOME/.local/bin:$HOME/.stack/bin:$PATH"
```
### STack
```shell
choco install haskell-stack
```

### Haskell Language Server (HLS) for VS Code
```
stack install haskell-language-server
```

### VS Code Extensions
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