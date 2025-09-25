# haskell
Haskell for DSL

## Install
### GHC (Glasgow Haskell Compiler), Stack and Haskell Language Server
```shell
# choco install haskell-stack
curl -sSL https://get.haskellstack.org/ | sh
export PATH="$HOME/.local/bin:$HOME/.stack/bin:$PATH"

# stack install haskell-language-server
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.ghcup/env
ghcup install hls
export PATH="$HOME/.ghcup/bin:$PATH" && haskell-language-server-9.6.7 --version
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