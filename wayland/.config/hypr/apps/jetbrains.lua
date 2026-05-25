hl.window_rule({
  name = "jetbrains-splash",
  match = {
    class = "^(jetbrains-.*)$",
    title = "^(splash)$",
    float = true,
  },
  tag = "+jetbrains-splash",
  center = true,
  no_focus = true,
  border_size = 0,
})
