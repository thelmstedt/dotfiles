local util = require("layouts/xmonad/util")

local M = {}

M.commands = { "tabbed" }
M.messages = { "tabbed", "focusactive" }
M.needs_focus_recalculate = true

function M.new_state()
  return {}
end

function M.handle(_, command)
  if command == "focusactive" then
    return true
  end

  return false
end

function M.place(ctx, targets, ids, _, helpers)
  local id = helpers.active_id(ctx)
  if not id or not targets[id] then
    id = ids[1]
  end

  for _, target_id_value in ipairs(ids) do
    local target = targets[target_id_value]
    if target then
      if target_id_value == id then
        target:place(ctx.area)
      else
        util.place_hidden(target, ctx.area)
      end
    end
  end
end

return M
