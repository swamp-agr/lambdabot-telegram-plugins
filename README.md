# lambdabot-telegram-plugins

## Description

## Installation

- It is assumed that you are using `cabal-install >= 3.4`. 
- `telegram-lambdabot` was tested with `cabal-install 3.6.2.0` and `GHC 8.10.7`.

1. Install `djinn` to enable `/djinn` command.

```
cd /path/to/projects/dir
git clone https://github.com/augustss/djinn.git
cd djinn
perl -pi -e 's/import LJTFormula/import LJTFormula\nimport Prelude hiding \(\(<>\)\)/g' src/HTypes.hs
cabal install --overwrite-policy=always
```

2. Install `mueval` to enable commands from `eval` plugin. The "core" Haskell plugin with set of a few REPL commands.

```
cd /path/to/projects/dir
git clone https://github.com/gwern/mueval.git
cd mueval
echo 'packages .\n\nwrite-ghc-environment-files: always\n' > cabal.project
cabal build
cabal install --overwrite-policy=always
```

3. Install `hoogle` to enable `/hoogle` command.

```
cd /path/to/projects/dir
git clone https://github.com/ndmitchell/hoogle.git
cd hoogle
cabal install --overwrite-policy=always
hoogle generate
```

It might take a while.

4. Clone this repo. Set up `mueval` env.

```
cd /path/to/projects/dir
git clone https://github.com/swamp-agr/lambdabot-telegram-plugins.git
cd lambdabot-telegram-plugins
cabal install --overwrite-policy=always
cp /path/to/projects/dir/mueval/.ghc* /path/to/projects/dir/lambdabot-telegram-plugins
perl -pi -e 's/.*mueval.*//g' .ghc*
cabal build
```

Add following line into `.ghc*` env file:

```
package-db dist-newstyle/packagedb/ghc-X.Y.Z
```

where `X.Y.Z` is the GHC version.

5. Set up `TELEGRAM_LAMBDABOT_TOKEN` env variable. Don't forget to export it.

6. Run `telegram-lambdabot` from the `lambdabot-telegram-plugins` dir (where `mueval` env was prepared earlier).

FIXME: add `BotFather` related step.

## Usage

## Available instance(s)

## Acknowledgements

- James Cook
- Chris Done
- Lambdabot maintainers
- Telegram Haskell Community
