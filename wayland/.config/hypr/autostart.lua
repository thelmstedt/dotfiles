hl.on("hyprland.start", function()
  hl.exec_cmd("qs &")
  hl.exec_cmd("jetbrains-toolbox --minimize &")
  hl.exec_cmd("1password --ozone-platform-hint=x11 &")
  hl.exec_cmd("wl-paste --type text --watch cliphist store &")
  hl.exec_cmd("wl-paste --type image --watch cliphist store &")

  -- I think these have to start after hyprland, but I just start it from terminal login. sooooo...
  hl.exec_cmd("systemctl --user restart elephant.service")
  hl.exec_cmd("systemctl --user restart walker.service")
end)
