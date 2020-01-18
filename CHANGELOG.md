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
* **systemd_watchdog:**
  *  add scaling to timeouts ([693f7c69](693f7c69))
  *  use `systemd` module for notifications ([241adc30](241adc30))

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
