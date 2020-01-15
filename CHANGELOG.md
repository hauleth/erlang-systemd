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
