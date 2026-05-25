local util = require("layouts/xmonad/util")

local M = {}

M.commands = { "threecol", "3col" }
M.messages = { "3col", "ratio <0.1..0.9>", "grow", "shrink" }

function M.new_state()
  return {
    ratio = 1 / 3,
    reflect_x = false,
  }
end

function M.select(state)
  state.ratio = 1 / 3
end

function M.reset(state)
  local defaults = M.new_state()
  state.ratio = defaults.ratio
  state.reflect_x = defaults.reflect_x
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

  local side_ratio = (1 - state.ratio) / 2
  local left = util.split(ctx, ctx.area, "left", side_ratio)
  local right = util.split(ctx, ctx.area, "right", side_ratio)
  local middle = util.split(ctx, util.split(ctx, ctx.area, "right", 1 - side_ratio), "left", state.ratio / (state.ratio + side_ratio))
  local left_stack = {}
  local right_stack = {}

  master:place(middle)

  for i = 2, #ids do
    if (i % 2) == 0 then
      table.insert(left_stack, ids[i])
    else
      table.insert(right_stack, ids[i])
    end
  end

  if state.reflect_x then
    left, right = right, left
  end

  util.place_stack(ctx, targets, left_stack, left, "vertical")
  util.place_stack(ctx, targets, right_stack, right, "vertical")
end

return M
