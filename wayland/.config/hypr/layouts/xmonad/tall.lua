local util = require("layouts/xmonad/util")

local M = {}

M.commands = { "vertical", "horizontal" }
M.messages = { "vertical", "horizontal", "ratio <0.1..0.9>", "grow", "shrink" }

function M.new_state()
  return {
    orientation = "vertical",
    ratio = 0.5,
    reflect_x = false,
    reflect_y = false,
  }
end

function M.select(state, command)
  state.orientation = command
end

function M.reset(state)
  local defaults = M.new_state()
  state.orientation = defaults.orientation
  state.ratio = defaults.ratio
  state.reflect_x = defaults.reflect_x
  state.reflect_y = defaults.reflect_y
end

function M.handle(state, command, arg)
  if command == "ratio" then
    state.ratio = util.clamp(tonumber(arg) or state.ratio, 0.1, 0.9)
  elseif command == "grow" then
    state.ratio = util.clamp(state.ratio + 0.03, 0.1, 0.9)
  elseif command == "shrink" then
    state.ratio = util.clamp(state.ratio - 0.03, 0.1, 0.9)
  else
    return false
  end

  return true
end

function M.place(ctx, targets, ids, state)
  local master_id = ids[1]
  local master = targets[master_id]
  if not master then
    return
  end

  if #ids == 1 then
    master:place(ctx.area)
    return
  end

  if state.orientation == "horizontal" then
    local master_side = state.reflect_y and "bottom" or "top"
    local stack_side = state.reflect_y and "top" or "bottom"
    master:place(util.split(ctx, ctx.area, master_side, state.ratio))
    util.place_stack(ctx, targets, { table.unpack(ids, 2) }, util.split(ctx, ctx.area, stack_side, 1 - state.ratio), "horizontal")
  else
    local master_side = state.reflect_x and "right" or "left"
    local stack_side = state.reflect_x and "left" or "right"
    master:place(util.split(ctx, ctx.area, master_side, state.ratio))
    util.place_stack(ctx, targets, { table.unpack(ids, 2) }, util.split(ctx, ctx.area, stack_side, 1 - state.ratio), "vertical")
  end
end

return M
