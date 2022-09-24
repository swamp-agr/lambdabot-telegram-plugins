# Revision history for telegram-lambdabot

## 0.2.1 -- 2022-09-24

* Bump `telegram-bot-simple` and fix runtime when receive `custom_emoji` update (see [#32](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/32));

## 0.2.0 -- 2022-05-12

Implementation phase is finished. Lambdabot might be considered ready with current release.
See README for more details. Here is a history of changes since its creation.

* Fix unicode output (see [#1](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/1));
* Fix unicode input (see [#2](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/2));
* Support `eval` plugin (see [#3](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/2));
* Support `check` plugin, add type class for different commands from different plugins (see [#4](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/4));
* Add maintenance shell scripts (see [#5](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/5));
* Support `djinn` plugin, derive `FromCommand` via `DeriveGeneric` (see [#6](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/6));
* Support `free` plugin (see [#7](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/7));
* Support `haddock` and `hoogle` plugins (see [#8](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/8));
* Support following plugins: `instances`, `more`, `pl`, `pointful`, `pretty`, `system`, `type`, `undo`, `unmtl`, `version`, `help`, `source` (see [#9](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/9));
* Fix errors from `telegram-bot-simple` via bumping its version (see [#10](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/10));
* Fix `sendMessage` to comply with Bot API 5.7 (see [#11](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/11));
* Fix `/cmd@botname` commands (see [#12](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/12));
* Add lambdabot `Pristine.hs`, fix `source` plugin and disable lambdabot's insults (see [#13](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/13));
* Fix empty `/help` command (see [#14](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/14));
* Show two different versions via `/help` command (see [#15](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/15));
* Support multiple sandboxes, remove shared state across chats (see [#16](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/16));
* Remove `more` command and plugin (see [#17](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/17));
* Enable `TypeApplications` GHC extension (see [#18](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/18));
* Reply to user messages and support user edits (see [#19](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/19));
* Fix commands from all plugins except `eval` (see [#20](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/20));
* Extend `/help` message (see [#21](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/21));
* Unescape unicode output from interpreter (see [#22](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/22));
* Render response from bot as `monospace` (see [#26](https://github.com/swamp-agr/lambdabot-telegram-plugins/pulls?page=1&q=is%3Apr+is%3Aclosed));
* Move bot parser helpers to upstream `telegram-bot-simple` package and use them (see [#27](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/27));
* Add haddock annotations to the code (see [#28](https://github.com/swamp-agr/lambdabot-telegram-plugins/pull/28));

## 0.1.0 -- 2022-02-16

* First version. Released on an unsuspecting world.
* Only `/irc` command added.
