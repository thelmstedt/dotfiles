local terminal = "alacritty"
local fileManager = "thunar"
local menu = "walker"
local window_switcher = "walker -m windows"
local xmonad = require("layouts/xmonad")

local mainMod = "SUPER"

local function unfloat()
  hl.dispatch(hl.dsp.window.float({ action = "unset" }))
  hl.dispatch(hl.dsp.layout("reset"))
end

hl.bind(mainMod .. " + M", hl.dsp.exit())

hl.bind("SUPER + V", hl.dsp.exec_cmd("cliphist list | rofi -dmenu | cliphist decode | wl-copy"))
hl.bind(mainMod .. " + SHIFT + period", hl.dsp.exec_cmd("1password --ozone-platform-hint=x11 --quick-access"))

hl.bind(mainMod .. " + F1", hl.dsp.layout("tabbed"))
hl.bind(mainMod .. " + F2", hl.dsp.layout("vertical"))
hl.bind(mainMod .. " + F3", hl.dsp.layout("horizontal"))
hl.bind(mainMod .. " + F4", hl.dsp.layout("3col"))
hl.bind(mainMod .. " + F12", hl.dsp.layout("grow"))
hl.bind(mainMod .. " + F11", hl.dsp.layout("shrink"))

hl.bind(mainMod .. " + Tab", function() xmonad.cycle_focus(true) end)
hl.bind(mainMod .. " + SHIFT + Tab", function() xmonad.cycle_focus(false) end)
hl.bind(mainMod .. " + Return", hl.dsp.layout("swapwithmaster"))

hl.bind(mainMod .. " + left", hl.dsp.focus({ direction = "l" }))
hl.bind(mainMod .. " + right", hl.dsp.focus({ direction = "r" }))
hl.bind(mainMod .. " + up", hl.dsp.focus({ direction = "u" }))
hl.bind(mainMod .. " + down", hl.dsp.focus({ direction = "d" }))

hl.bind(mainMod .. " + T", unfloat)

hl.bind(mainMod .. " + W", hl.dsp.focus({ monitor = 0 }))
hl.bind(mainMod .. " + E", hl.dsp.focus({ monitor = 1 }))
hl.bind(mainMod .. " + SHIFT + W", hl.dsp.window.move({ monitor = 0 }))
hl.bind(mainMod .. " + SHIFT + E", hl.dsp.window.move({ monitor = 1 }))

hl.bind(mainMod .. " + backslash", hl.dsp.exec_cmd(menu))
hl.bind(mainMod .. " + SHIFT + backslash", hl.dsp.exec_cmd(window_switcher))
hl.bind(mainMod .. " + SHIFT + V", hl.dsp.exec_cmd("walker -m clipboard"))

hl.bind(mainMod .. " + SHIFT + C", hl.dsp.window.close())
hl.bind(mainMod .. " + SHIFT + Return", hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + SHIFT + N", hl.dsp.exec_cmd(fileManager))

local workspace_keys = {
  { key = "1", workspace = 1 },
  { key = "2", workspace = 2 },
  { key = "3", workspace = 3 },
  { key = "4", workspace = 4 },
  { key = "5", workspace = 5 },
  { key = "6", workspace = 6 },
  { key = "7", workspace = 7 },
  { key = "8", workspace = 8 },
  { key = "9", workspace = 9 },
  { key = "0", workspace = 10 },
  { key = "minus", workspace = 11 },
  { key = "equal", workspace = 12 },
}

for _, item in ipairs(workspace_keys) do
  hl.bind(mainMod .. " + " .. item.key, hl.dsp.focus({ workspace = item.workspace, on_current_monitor = true }))
  hl.bind(mainMod .. " + SHIFT + " .. item.key, hl.dsp.window.move({ workspace = item.workspace, follow = false }))
end

hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.float({ action = "set" }))
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })
hl.bind(mainMod .. " + SHIFT + mouse:273", hl.dsp.window.resize(), { mouse = true })

hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl next"), { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl previous"), { locked = true })
hl.bind("Pause", hl.dsp.exec_cmd("~/bin/mpris_pp.sh"), { locked = true })

hl.bind("SUPER + P", hl.dsp.window.pin())
