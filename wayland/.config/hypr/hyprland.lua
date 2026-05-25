hl.monitor({
  output = "HDMI-A-2",
  mode = "2560x1440@143.91Hz",
  position = "0x0",
  scale = 1,
})

hl.monitor({
  output = "",
  mode = "preferred",
  position = "auto",
  scale = "auto",
})

require("autostart")
require("layouts/xmonad")
require("bindings")
require("envs")
require("input")
require("looknfeel")

require("apps/1password")
require("apps/bitwarden")
require("apps/browser")
require("apps/hyprshot")
require("apps/jetbrains")
require("apps/localsend")
require("apps/pip")
require("apps/qemu")
require("apps/retroarch")
require("apps/steam")
require("apps/system")
require("apps/terminals")
require("apps/walker")
require("apps/webcam-overlay")
