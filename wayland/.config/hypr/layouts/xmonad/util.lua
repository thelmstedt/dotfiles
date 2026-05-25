local M = {}

function M.clamp(x, min, max)
  return math.max(min, math.min(max, x))
end

function M.split(ctx, area, side, ratio)
  return ctx:split(area, side, ratio)
end

function M.place_stack(ctx, targets, ids, area, orientation)
  local remaining_area = area
  local remaining = #ids

  for i, id in ipairs(ids) do
    local target = targets[id]
    if target then
      if i == #ids then
        target:place(remaining_area)
      elseif orientation == "vertical" then
        target:place(M.split(ctx, remaining_area, "top", 1 / remaining))
        remaining_area = M.split(ctx, remaining_area, "bottom", (remaining - 1) / remaining)
      else
        target:place(M.split(ctx, remaining_area, "left", 1 / remaining))
        remaining_area = M.split(ctx, remaining_area, "right", (remaining - 1) / remaining)
      end
    end

    remaining = remaining - 1
  end
end

function M.place_hidden(target, area)
  target:place({ x = area.x - 100000, y = area.y - 100000, w = 20, h = 20 })
end

return M
