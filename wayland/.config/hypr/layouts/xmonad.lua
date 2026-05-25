local tabbed = require("layouts/xmonad/tabbed")
local tall = require("layouts/xmonad/tall")
local three_col = require("layouts/xmonad/three_col")

local state = {
  workspaces = {},
}

local layout_name = "xmonad"
local layouts = { tall, three_col, tabbed }
local layout_commands = {}
local expected_messages = { "swapwithmaster", "promote", "reset", "fullscreen", "full" }
local tabbed_state_path = "/tmp/xmonad-tabbed-workspaces"
local layout_state_path = "/tmp/xmonad-workspace-layouts"
local order_state_path = "/tmp/xmonad-workspace-order"

for _, layout in ipairs(layouts) do
  for _, command in ipairs(layout.commands or {}) do
    layout_commands[command] = layout
  end

  for _, message in ipairs(layout.messages or {}) do
    table.insert(expected_messages, message)
  end
end

local index_of

local function field(obj, key)
  local ok, value = pcall(function()
    return obj and obj[key]
  end)

  if ok then
    return value
  end
end

local function workspace_object_key(workspace)
  if workspace == nil then
    return nil
  end

  if type(workspace) ~= "table" and type(workspace) ~= "userdata" then
    return tostring(workspace)
  end

  local id = field(workspace, "id")
  if id ~= nil then
    return "id:" .. tostring(id)
  end

  local name = field(workspace, "name")
  if name ~= nil then
    return "name:" .. tostring(name)
  end

  return tostring(workspace)
end

local function window_workspace_key(window)
  return workspace_object_key(field(window, "workspace"))
end

local function workspace_key(ctx)
  local key = workspace_object_key(field(ctx, "workspace"))
  if key then
    return key
  end

  for _, target in ipairs(ctx.targets or {}) do
    key = window_workspace_key(field(target, "window"))
    if key then
      return key
    end
  end

  local area = ctx.area
  return string.format("area:%s:%s:%s:%s", area.x, area.y, area.w, area.h)
end

local function new_workspace_state()
  local layout_state = {}
  for _, layout in ipairs(layouts) do
    layout_state[layout] = layout.new_state and layout.new_state() or {}
  end

  return {
    order = {},
    display_order = {},
    addresses = {},
    active_layout = tall,
    layout_state = layout_state,
  }
end

local function workspace_state(key)
  local ws = state.workspaces[key]
  if not ws then
    ws = new_workspace_state()
    state.workspaces[key] = ws
  end

  return ws
end

local function workspace_id_from_key(key)
  return key and key:match("^id:(.+)$")
end

local function publish_tabbed_workspaces()
  local file = io.open(tabbed_state_path, "w")
  if not file then
    return
  end

  for key, ws in pairs(state.workspaces) do
    local id = workspace_id_from_key(key)
    if id and ws.active_layout == tabbed then
      file:write(id, "\n")
    end
  end

  file:close()
end

local function layout_label(ws)
  if ws.active_layout == tabbed then
    return "TAB"
  elseif ws.active_layout == three_col then
    return "3COL"
  elseif ws.active_layout == tall then
    local tall_state = ws.layout_state[tall]
    return tall_state.orientation == "horizontal" and "HORZ" or "VERT"
  end

  return "XMONAD"
end

local function publish_workspace_layouts()
  local file = io.open(layout_state_path, "w")
  if not file then
    return
  end

  for key, ws in pairs(state.workspaces) do
    local id = workspace_id_from_key(key)
    if id then
      file:write(id, " ", layout_label(ws), "\n")
    end
  end

  file:close()
end

local function publish_workspace_order()
  local file = io.open(order_state_path, "w")
  if not file then
    return
  end

  for key, ws in pairs(state.workspaces) do
    local id = workspace_id_from_key(key)
    if id then
      file:write(id)
      local order = #ws.display_order > 0 and ws.display_order or ws.order
      for _, window_id in ipairs(order) do
        local address = ws.addresses[window_id]
        if address then
          file:write(" ", address)
        end
      end
      file:write("\n")
    end
  end

  file:close()
end

local function publish_workspace_state()
  publish_tabbed_workspaces()
  publish_workspace_layouts()
  publish_workspace_order()
end

local function workspace_for_window(window)
  local key = window_workspace_key(window)
  if key and state.workspaces[key] then
    return state.workspaces[key]
  end

  local stable_id = field(window, "stable_id")
  local id = stable_id and tostring(stable_id)
  for _, ws in pairs(state.workspaces) do
    if index_of(ws.order, id) then
      return ws
    end
  end
end

local function is_xmonad_layout(window)
  local layout = window and window.layout
  return layout and (layout.name == layout_name or layout.name == "lua:" .. layout_name)
end

local function target_id(target)
  local window = target.window
  return window and tostring(window.stable_id) or tostring(target.index)
end

local function target_address(target)
  local window = target.window
  local address = field(window, "address")
  return address and tostring(address)
end

function index_of(tbl, value)
  for i, v in ipairs(tbl) do
    if v == value then
      return i
    end
  end
end

local function active_id(ctx, ws)
  for _, target in ipairs(ctx.targets) do
    local window = target.window
    if window and window.active then
      return target_id(target)
    end
  end

  return ws.order[1]
end

local function sync_order(ctx, ws)
  local present = {}
  local targets = {}
  local addresses = {}

  for _, target in ipairs(ctx.targets) do
    local id = target_id(target)
    present[id] = true
    targets[id] = target
    addresses[id] = target_address(target)
  end

  local old_order = ws.order
  ws.order = {}
  ws.addresses = {}

  for _, id in ipairs(old_order) do
    if present[id] then
      table.insert(ws.order, id)
      ws.addresses[id] = addresses[id]
    end
  end

  for _, target in ipairs(ctx.targets) do
    local id = target_id(target)
    if not index_of(ws.order, id) then
      table.insert(ws.order, id)
      ws.addresses[id] = addresses[id]
    end
  end

  return targets
end

local function reversed_tail(ids)
  local result = { ids[1] }
  for i = #ids, 2, -1 do
    table.insert(result, ids[i])
  end
  return result
end

local function three_col_clockwise_order(ws)
  local left_stack = {}
  local right_stack = {}

  for i = 2, #ws.order do
    if (i % 2) == 0 then
      table.insert(left_stack, ws.order[i])
    else
      table.insert(right_stack, ws.order[i])
    end
  end

  local three_col_state = ws.layout_state[three_col]
  if three_col_state.reflect_x then
    left_stack, right_stack = right_stack, left_stack
  end

  local result = { ws.order[1] }
  for _, id in ipairs(right_stack) do
    table.insert(result, id)
  end
  for i = #left_stack, 1, -1 do
    table.insert(result, left_stack[i])
  end
  return result
end

local function update_display_order(ws)
  if ws.active_layout == tabbed or #ws.order <= 1 then
    ws.display_order = { table.unpack(ws.order) }
    return
  end

  if ws.active_layout == three_col then
    ws.display_order = three_col_clockwise_order(ws)
    return
  end

  local tall_state = ws.layout_state[tall]
  if ws.active_layout == tall then
    if (tall_state.orientation == "vertical" and tall_state.reflect_x)
        or (tall_state.orientation == "horizontal" and not tall_state.reflect_y) then
      ws.display_order = reversed_tail(ws.order)
    else
      ws.display_order = { table.unpack(ws.order) }
    end
    return
  end

  ws.display_order = { table.unpack(ws.order) }
end

local function promote_active(ctx, ws)
  local id = active_id(ctx, ws)
  local i = id and index_of(ws.order, id)
  if not i then
    return
  end

  table.remove(ws.order, i)
  table.insert(ws.order, 1, id)
end

local function helpers(ws)
  return {
    active_id = function(ctx)
      return active_id(ctx, ws)
    end,
  }
end

local function select_layout(ws, layout, command)
  ws.active_layout = layout

  if layout.select then
    layout.select(ws.layout_state[layout], command)
  end

  update_display_order(ws)
  publish_workspace_state()
end

local function reset(ws)
  ws.active_layout = tall

  for _, layout in ipairs(layouts) do
    if layout.reset then
      layout.reset(ws.layout_state[layout])
    end
  end

  update_display_order(ws)
  publish_workspace_state()
end

hl.layout.register("xmonad", {
  recalculate = function(ctx)
    local ws = workspace_state(workspace_key(ctx))
    local targets = sync_order(ctx, ws)

    if #ws.order == 0 then
      return
    end

    ws.active_layout.place(ctx, targets, ws.order, ws.layout_state[ws.active_layout], helpers(ws))

    update_display_order(ws)
    publish_workspace_state()
  end,

  layout_msg = function(ctx, msg)
    local ws = workspace_state(workspace_key(ctx))
    local command, arg = msg:match("^(%S+)%s*(.*)$")

    local selected_layout = layout_commands[command]
    if selected_layout then
      select_layout(ws, selected_layout, command)
    elseif command == "fullscreen" or command == "full" then
      select_layout(ws, tabbed, "tabbed")
    elseif ws.active_layout.handle and ws.active_layout.handle(ws.layout_state[ws.active_layout], command, arg, ctx, helpers(ws)) then
      -- Handled by the active sub-layout.
    elseif command == "swapwithmaster" or command == "promote" then
      promote_active(ctx, ws)
    elseif command == "reset" then
      reset(ws)
    else
      return "xmonad: expected " .. table.concat(expected_messages, ", ")
    end

    update_display_order(ws)
    publish_workspace_state()
    return true
  end,
})

hl.on("window.active", function(window)
  local ws = workspace_for_window(window)
  if not ws or not is_xmonad_layout(window) then
    return
  end

  if ws.active_layout.needs_focus_recalculate then
    hl.dispatch(hl.dsp.layout("focusactive"))
  end
end)

function state.cycle_focus(next)
  local window = hl.get_active_window()
  local ws = workspace_for_window(window)
  if not ws or not is_xmonad_layout(window) then
    hl.dispatch(hl.dsp.window.cycle_next({ next = next }))
    return
  end

  local order = #ws.display_order > 0 and ws.display_order or ws.order
  local id = field(window, "stable_id")
  local current = id and index_of(order, tostring(id))
  if not current or #order == 0 then
    return
  end

  local target = next and (current + 1) or (current - 1)
  if target > #order then
    target = 1
  elseif target < 1 then
    target = #order
  end

  local address = ws.addresses[order[target]]
  if address then
    hl.dispatch(hl.dsp.focus({ window = "address:" .. address }))
  end
end

return state
