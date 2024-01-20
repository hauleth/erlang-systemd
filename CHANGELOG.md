<a name="unreleased"></a>
## [Unreleased]

### Bug Fixes
- typespecs for watchdog/1
- fix dialyzer error for systemd:set_status/1
- **journal_h:** Dialyzer error

### Documentation
- correct the in-doc description of return value for watchdog/1
- fix typos

### Features
- add warning log message if there is no readiness message sent
- send MAINPID message on socket start


<a name="v0.6.2"></a>
## [v0.6.2] - 2021-12-30
### Bug Fixes
- docs for `systemd_journal_h` ([`9979ba5`](https://github.com/hauleth/erlang-systemd/commit/9979ba53c8a0c8d02294cbfb5bf4ba3be4ab4da0))

### Documentation
- add syntax highlighting on systemd example ([`a580cab`](https://github.com/hauleth/erlang-systemd/commit/a580cab9808d0aa8b1ad10fa18409af3c940f1a5))

### Features
- add functions to set custom status ([`9342d36`](https://github.com/hauleth/erlang-systemd/commit/9342d3654981044a3fcbf67fef7fc41cd855b946))


<a name="v0.6.1"></a>
## [v0.6.1] - 2021-05-21
### Bug Fixes
- Add Erlang 24 support ([#29](https://github.com/hauleth/erlang-systemd/issues/29)) ([`2588664`](https://github.com/hauleth/erlang-systemd/commit/2588664af2633ff7b8c3b829eee0bcfc4c324407))
- do not fail on invalid JOURNAL_STREAM content ([`0ce748e`](https://github.com/hauleth/erlang-systemd/commit/0ce748edffcb72bb028733e9ca4707cb30add853))
- add enough to list of required applications ([`937ef70`](https://github.com/hauleth/erlang-systemd/commit/937ef703b4145ee3ad9279018129a9c3a93a9dda))
- **systemd_journal_h:** Support literal field values ([#25](https://github.com/hauleth/erlang-systemd/issues/25)) ([`7a1b9a7`](https://github.com/hauleth/erlang-systemd/commit/7a1b9a7fb2b3e3ec682f9ee9797f9817172a7050))
- **systemd_protocol:** keep empty values in encode_field/2 ([#27](https://github.com/hauleth/erlang-systemd/issues/27)) ([`e732727`](https://github.com/hauleth/erlang-systemd/commit/e732727b0b637eb29e8adc77a4eb46d7ebc0f41a))


<a name="0.6.0"></a>
## [0.6.0] - 2021-03-12
### Bug Fixes
- **journal_h:** use proper check for socket existence ([`20b048e`](https://github.com/hauleth/erlang-systemd/commit/20b048e14c21f74091091cd83c4386b9afeabc2f))
- **kmsg_formatter:** do not append log level on last newline ([`4e52c0f`](https://github.com/hauleth/erlang-systemd/commit/4e52c0f89a08e06b225e40ab9d0ee6f8f605b380))


<a name="0.5.3"></a>
## [0.5.3] - 2021-01-04
### Bug Fixes
- **journal:** use correct name for priority attribute ([`9493d35`](https://github.com/hauleth/erlang-systemd/commit/9493d35f8c99645472836b3e13e5e30372ee3350))


<a name="0.5.2"></a>
## [0.5.2] - 2021-01-04
### Bug Fixes
- **journal:** format message as Unicode-encoded binary ([`1614c7c`](https://github.com/hauleth/erlang-systemd/commit/1614c7cedc592ecbf375149a1e0de6bfd180301c))


<a name="0.5.1"></a>
## [0.5.1] - 2021-01-04
### Bug Fixes
- **journal:** cleanup journal documentation ([`3b6bf5a`](https://github.com/hauleth/erlang-systemd/commit/3b6bf5aafe4bf3e364be4bb54654dcf2c96c9163))

### Documentation
- describe usage in Elixir projects ([`ff67d80`](https://github.com/hauleth/erlang-systemd/commit/ff67d808114e6aaeb6a338357c3b5ca7986040ca))
- fix typo equested -> requested ([`5949441`](https://github.com/hauleth/erlang-systemd/commit/5949441e63fba06c6f8c01c8951ffe81dfd354fe))
- imporve systemd.service example in the README ([`cd1b697`](https://github.com/hauleth/erlang-systemd/commit/cd1b697c8d4326fcec50cf8b85ab57e5d221bd63))
- **fds:** document new functions ([`0e782a6`](https://github.com/hauleth/erlang-systemd/commit/0e782a6635b104b52c49c4027733bf9e5b9b65e1))


<a name="0.5.0"></a>
## [0.5.0] - 2020-03-09
### Bug Fixes
- remove unneeded `handle_info/2` callbacks ([`fa77f8e`](https://github.com/hauleth/erlang-systemd/commit/fa77f8eb04d4cb46e9c8c42c60f9758276b7f0d9))

### Documentation
- update README ([`33cce22`](https://github.com/hauleth/erlang-systemd/commit/33cce223d7d629e86f7ccb8eb739b0c03238c655))
- fix typos in documentation ([`6a40613`](https://github.com/hauleth/erlang-systemd/commit/6a406132a090f029ae5e7174dd03f8674b14fc64))
- **systemd_kmsg_formatter:** add documentation about auto registration ([`4ecc76d`](https://github.com/hauleth/erlang-systemd/commit/4ecc76dbdd280b6942ccb9884cbb8195eb5e02c4))


<a name="0.4.0"></a>
## [0.4.0] - 2020-01-20
### Bug Fixes
- **systemd_stderr_formatter:** prefix all lines instead of only first one ([`1ccd500`](https://github.com/hauleth/erlang-systemd/commit/1ccd5002e7ab9e884a9947c948a46fd3ac93870e))

### Documentation
- add since documentation tags ([`abc7f28`](https://github.com/hauleth/erlang-systemd/commit/abc7f286390f855e9577c1488ea6d954ab8efc87))
- expand documentation on systemd and systemd_journal_formatter ([`f6e323c`](https://github.com/hauleth/erlang-systemd/commit/f6e323c2e764fe9d47dc0f1455af6e10755bc580))
- **systemd:** expand documentation of `unset_env/1` ([`630460e`](https://github.com/hauleth/erlang-systemd/commit/630460e40e0b0b8ff169547ef8737698f2a1db2e))
- **systemd_stderr_formatter:** hide implementation functions ([`f670d1f`](https://github.com/hauleth/erlang-systemd/commit/f670d1f2389a9a5a587f42be8ddc5c61b0cf3562))


<a name="0.3.3"></a>
## [0.3.3] - 2020-01-18
### Bug Fixes
- **systemd_journal_formatter:** check for empty data before building iolist ([`095088e`](https://github.com/hauleth/erlang-systemd/commit/095088eeed308869505bacc5a10af338e6f241b9))
- **systemd_journal_formatter:** again invalid format of multiline messages ([`b8f213a`](https://github.com/hauleth/erlang-systemd/commit/b8f213af5a51a06ca41badbb01669b7c0e64cd08))


<a name="0.3.2"></a>
## [0.3.2] - 2020-01-18
### Bug Fixes
- **systemd_journal_formatter:** improper multiline format ([`2c821e5`](https://github.com/hauleth/erlang-systemd/commit/2c821e5a9d3073fcf84ada2c7cff47f760e2f51e))


<a name="0.3.1"></a>
## [0.3.1] - 2020-01-18
### Bug Fixes
- **systemd_journal_formatter:** handling of unary report_cb return value ([`4a0577b`](https://github.com/hauleth/erlang-systemd/commit/4a0577b42db54bd67c54489b187e842c4638d423))


<a name="0.3.0"></a>
## [0.3.0] - 2020-01-18
### Bug Fixes
- move environment variables extraction to supervisor ([`d889d67`](https://github.com/hauleth/erlang-systemd/commit/d889d6739c6da10fb5d9efb0b7518ba0144b8721))
- **systemd_watchdog:** convert timeout from microseconds to milliseconds ([`fec0e26`](https://github.com/hauleth/erlang-systemd/commit/fec0e26f2bfe964df0c8eb96b1f1f0723add84b5))
- **systemd_watchdog:** use send_after instead of timeouts ([`c85c095`](https://github.com/hauleth/erlang-systemd/commit/c85c09558009bfca9dbb1b4286823539451ea83b))
- **systemd_watchdog:** initial timeout was off ([`d8839b7`](https://github.com/hauleth/erlang-systemd/commit/d8839b7d126a717a2d98bdaeec44b48d48c6b54a))

### Documentation
- add informations about journal loggers ([`20718cc`](https://github.com/hauleth/erlang-systemd/commit/20718cc553270783b476bd9ee08b338e038c4fc3))
- **README:** extend example with more correct systemd service ([`ab3c3fa`](https://github.com/hauleth/erlang-systemd/commit/ab3c3fa85899b9a797197b6d24b55099e0fb71a9))
- **systemd_formatter:** document usage ([`5c3e3ba`](https://github.com/hauleth/erlang-systemd/commit/5c3e3bab79ac4e4bbef558be89192c552ec3ee65))
- **systemd_journal_h:** add warnings about required formatter ([`3e09c7c`](https://github.com/hauleth/erlang-systemd/commit/3e09c7cac642233900bb8e52a9d43888ff5b6f8e))
- **systemd_stderr_formatter:** fix missing tag ([`31f1b60`](https://github.com/hauleth/erlang-systemd/commit/31f1b60ed023f665b16d51a98efce1403db960cf))


<a name="0.1.2"></a>
## [0.1.2] - 2020-01-15
### Bug Fixes
- **systemd_watchdog:** convert time unit from micro to milliseconds ([`2a40a6e`](https://github.com/hauleth/erlang-systemd/commit/2a40a6e30fd5052bbf59ad3524230211e2993693))


<a name="0.2.0"></a>
## [0.2.0] - 2020-01-15
### Bug Fixes
- **systemd_watchdog:** convert timeout from microseconds to milliseconds ([`fec0e26`](https://github.com/hauleth/erlang-systemd/commit/fec0e26f2bfe964df0c8eb96b1f1f0723add84b5))
- **systemd_watchdog:** use send_after instead of timeouts ([`c85c095`](https://github.com/hauleth/erlang-systemd/commit/c85c09558009bfca9dbb1b4286823539451ea83b))
- **systemd_watchdog:** initial timeout was off ([`d8839b7`](https://github.com/hauleth/erlang-systemd/commit/d8839b7d126a717a2d98bdaeec44b48d48c6b54a))


<a name="0.1.1"></a>
## [0.1.1] - 2020-01-15
### Bug Fixes
- **systemd_app:** remove start_phase ([`a9e5905`](https://github.com/hauleth/erlang-systemd/commit/a9e5905f2e00dcb76ae8243d9415e15bb1e3f3ed))
- **systemd_socket:** unused variable ([`1d51b2e`](https://github.com/hauleth/erlang-systemd/commit/1d51b2eeac8a7d13836808a8f71b71480b32541c))
- **systemd_watchdog:** one missed direct call to socket ([`0fce8eb`](https://github.com/hauleth/erlang-systemd/commit/0fce8eb1f3880d10d0c88f1356a7bc894ffeae8d))


<a name="0.1.0"></a>
## 0.1.0 - 2020-01-15

[Unreleased]: https://github.com/hauleth/erlang-systemd/compare/v0.6.2...HEAD
[v0.6.2]: https://github.com/hauleth/erlang-systemd/compare/v0.6.1...v0.6.2
[v0.6.1]: https://github.com/hauleth/erlang-systemd/compare/0.6.0...v0.6.1
[0.6.0]: https://github.com/hauleth/erlang-systemd/compare/0.5.3...0.6.0
[0.5.3]: https://github.com/hauleth/erlang-systemd/compare/0.5.2...0.5.3
[0.5.2]: https://github.com/hauleth/erlang-systemd/compare/0.5.1...0.5.2
[0.5.1]: https://github.com/hauleth/erlang-systemd/compare/0.5.0...0.5.1
[0.5.0]: https://github.com/hauleth/erlang-systemd/compare/0.4.0...0.5.0
[0.4.0]: https://github.com/hauleth/erlang-systemd/compare/0.3.3...0.4.0
[0.3.3]: https://github.com/hauleth/erlang-systemd/compare/0.3.2...0.3.3
[0.3.2]: https://github.com/hauleth/erlang-systemd/compare/0.3.1...0.3.2
[0.3.1]: https://github.com/hauleth/erlang-systemd/compare/0.3.0...0.3.1
[0.3.0]: https://github.com/hauleth/erlang-systemd/compare/0.1.2...0.3.0
[0.1.2]: https://github.com/hauleth/erlang-systemd/compare/0.2.0...0.1.2
[0.2.0]: https://github.com/hauleth/erlang-systemd/compare/0.1.1...0.2.0
[0.1.1]: https://github.com/hauleth/erlang-systemd/compare/0.1.0...0.1.1
