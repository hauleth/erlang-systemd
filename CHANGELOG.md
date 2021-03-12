<a name="0.6.0"></a>
## 0.6.0 (2021-03-12)


#### Features

* **journal_h:**  add overload protection to journal handler ([bc8eebec](bc8eebec))

#### Bug Fixes

* **journal_h:**  use proper check for socket existence ([20b048e1](20b048e1))
* **kmsg_formatter:**  do not append log level on last newline ([4e52c0f8](4e52c0f8))



<a name="0.5.3"></a>
### 0.5.3 (2021-01-04)


#### Bug Fixes

* **journal:**  use correct name for priority attribute ([9493d35f](9493d35f))



<a name="0.5.2"></a>
### 0.5.2 (2021-01-04)


#### Bug Fixes

* **journal:**  format message as Unicode-encoded binary ([1614c7ce](1614c7ce))



<a name="0.5.1"></a>
### 0.5.1 (2021-01-04)


#### Bug Fixes

* **journal:**  cleanup journal documentation ([3b6bf5aa](3b6bf5aa))

#### Features

*   define simpler format for journal messages ([c30ab4b6](c30ab4b6))
* **notify:**  use `socket` instead of `gen_udp` for greater control ([edd778b7](edd778b7))
* **watchdog:**  add support for healthcheck function ([9852ecf8](9852ecf8))



<a name="0.5.0"></a>
## 0.5.0 (2020-03-09)


#### Features

*   add systemd.hrl ([95d3b509](95d3b509))
*   implement rest of systemd daemon features ([f5480152](f5480152))
* **journal:**  merge journal formatter and handler ([5236862c](5236862c))
* **systemd:**
  *  add helper for readiness process child_spec ([6a3571f2](6a3571f2))
  *  add predefined extend_timeout state ([f5e10666](f5e10666))
* **systemd_formatter:**  add systemd formatter ([cea5f8b3](cea5f8b3))
* **systemd_journal_formatter:**
  *  change API to make it more foolproof ([c43fa589](c43fa589))
  *  add formatter for journald ([30ed15d8](30ed15d8))
* **systemd_journal_h:**  create logger handler for systemd's journal ([e760b086](e760b086))
* **systemd_kmsg_formatter:**  automatically set kmsg formatter ([9bf36716](9bf36716))
* **systemd_stderr_formatter:**  rename from systemd_formatter ([f92f92eb](f92f92eb))
* **systemd_watchdog:**
  *  always set timeout from var ([66da44cd](66da44cd))
  *  add scaling to timeouts ([693f7c69](693f7c69))
  *  use `systemd` module for notifications ([241adc30](241adc30))

#### Bug Fixes

*   remove unneeded `handle_info/2` callbacks ([fa77f8eb](fa77f8eb))
*   move environment variables extraction to supervisor ([d889d673](d889d673))
* **systemd_app:**  remove start_phase ([a9e5905f](a9e5905f))
* **systemd_journal_formatter:**
  *  check for empty data before building iolist ([095088ee](095088ee))
  *  again invalid format of multiline messages ([b8f213af](b8f213af))
  *  improper multiline format ([2c821e5a](2c821e5a))
  *  handling of unary report_cb return value ([4a0577b4](4a0577b4))
* **systemd_socket:**  unused variable ([1d51b2ee](1d51b2ee))
* **systemd_stderr_formatter:**  prefix all lines instead of only first one ([1ccd5002](1ccd5002))
* **systemd_watchdog:**
  *  convert timeout from microseconds to milliseconds ([fec0e26f](fec0e26f))
  *  use send_after instead of timeouts ([c85c0955](c85c0955))
  *  initial timeout was off ([d8839b7d](d8839b7d))
  *  one missed direct call to socket ([0fce8eb1](0fce8eb1))



<a name="0.4.0"></a>
## 0.4.0 (2020-01-20)

#### Bug Fixes

* **systemd_stderr_formatter:**  prefix all lines instead of only first one ([1ccd5002](1ccd5002))

#### Features

* **BREAKING CHANGE!** add `systemd:unset_env/1` for unsetting variables ([f61b2fe](f61b2fe))

<a name="0.3.3"></a>
### 0.3.3 (2020-01-18)


#### Bug Fixes

* **systemd_journal_formatter:** again invalid format of multiline messages ([b8f213af](b8f213af))

<a name="0.3.2"></a>
### 0.3.2 (2020-01-18)


#### Bug Fixes

* **systemd_journal_formatter:**  invalid multiline format ([2c821e5](2c821e5))

<a name="0.3.1"></a>
### 0.3.1 (2020-01-18)


#### Bug Fixes

* **systemd_journal_formatter:**  handling of unary report_cb return value ([4a0577b4](4a0577b4))

<a name="0.3.0"></a>
## 0.3.0 (2020-01-18)


#### Bug Fixes

*   move environment variables extraction to supervisor ([d889d673](d889d673))
* **systemd_app:**  remove start_phase ([a9e5905f](a9e5905f))
* **systemd_socket:**  unused variable ([1d51b2ee](1d51b2ee))
* **systemd_watchdog:**
  *  one missed direct call to socket ([0fce8eb1](0fce8eb1))

#### Features

*   add systemd.hrl ([95d3b509](95d3b509))
*   implement rest of systemd daemon features ([f5480152](f5480152))
* **systemd:**  add predefined extend_timeout state ([f5e10666](f5e10666))
* **systemd_formatter:**  add systemd formatter ([cea5f8b3](cea5f8b3))
* **systemd_journal_formatter:**
  *  change API to make it more foolproof ([c43fa589](c43fa589))
  *  add formatter for journald ([30ed15d8](30ed15d8))
* **systemd_journal_h:**  create logger handler for systemd's journal ([e760b086](e760b086))
* **systemd_stderr_formatter:**  rename from systemd_formatter ([f92f92eb](f92f92eb))
* **systemd_watchdog:** use `systemd` module for notifications ([241adc30](241adc30))

<a name="0.2.0"></a>
## 0.2.0 (2020-01-15)


#### Bug Fixes

* **systemd_watchdog:**
  *  convert timeout from microseconds to milliseconds ([fec0e26f](fec0e26f))
  *  use send_after instead of timeouts ([c85c0955](c85c0955))
  *  initial timeout was off ([d8839b7d](d8839b7d))

#### Features

* **systemd_watchdog:**  add scaling to timeouts ([693f7c69](693f7c69))

<a name="0.1.1"></a>
### 0.1.1 (2020-01-15)


#### Bug Fixes

* **systemd_app:**  remove start_phase ([26414a01](26414a01))
* **systemd_socket:**  unused variable ([bdea7215](bdea7215))
* **systemd_watchdog:**  one missed direct call to socket ([86e1100c](86e1100c))
