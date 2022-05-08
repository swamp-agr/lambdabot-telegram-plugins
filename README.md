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

## Usage

Set bot name via `@BotFather` and run:

```
telegram-lambdabot -n <botname>
```

Following commands are available: 

- `/irc` - Send command to Lambdabot.
- `/run` - run <expr>. You have Haskell, 3 seconds and no IO. Go nuts!
- `/let` - let <x> = <e>. Add a binding.
- `/define` - let <x> = <e>. Add a binding.
- `/undefine` - undefine. Reset evaluator local bindings.
- `/check` - check <expr>. You have QuickCheck and 3 seconds. Prove something.
- `/djinn` - djinn <type>. Generates Haskell code from a type.
- `/djinnadd` - djinn-add <expr>. Define a new function type or type synonym.
- `/djinndel` - djinn-del <ident>. Remove a symbol from the environment.
- `/djinnenv` - Show the current djinn environment.
- `/djinnnames` - Show the current djinn environment, compactly.
- `/djinnclr` - Reset the djinn environment.
- `/djinnver` - Show current djinn version.
- `/free` - free <ident>. Generate theorems for free.
- `/index` - index <ident>. Returns the Haskell modules in which <ident> is defined.
- `/hoogle` - hoogle <expr>. Haskell API Search for either names, or types.
- `/instances` - instances <typeclass>. Fetch the instances of a typeclass.
- `/instancesimporting` - instancesimporting [<module> [<module> [<module...]]] <typeclass>. Fetch the instances of a typeclass, importing specified modules first.
- `/pl` - pointless <expr>. Play with pointfree code.
- `/pointy` - pointful <expr>. Make code pointier.
- `/repoint` - pointful <expr>. Make code pointier.
- `/unpointless` - pointful <expr>. Make code pointier.
- `/unpl` - pointful <expr>. Make code pointier.
- `/unpf` - pointful <expr>. Make code pointier.
- `/pretty` - pretty <expr>. Display haskell code in a pretty-printed manner
- `/listmodules` - listmodules. Show available plugins.
- `/list` - list [module|command]. Show commands for [module] or the module providing [command].
- `/echo` - echo <msg>. echo irc protocol string.
- `/uptime` - uptime. Show uptime.
- `/type` - type <expr>. Return the type of a value.
- `/kind` - kind <type>. Return the kind of a type.
- `/undo` - undo <expr>. Translate do notation to Monad operators.
- `/do` - do <expr>. Translate Monad operators to do notation.
- `/unmtl` - unroll mtl monads.
- `/version` - version/source. Report the version and git repo of this bot
- `/help` - help <command>. Ask for help for <command>. Try 'list' for all commands.
- `/src` - src <id>. Display the implementation of a standard function.

## Available instance(s)

## Acknowledgements

- James Cook
- Chris Done
- Lambdabot maintainers
- Telegram Haskell Community
