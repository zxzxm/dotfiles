---------------------------
-- Default awesome theme --
---------------------------

theme = {}
theme.confdir       = awful.util.getdir("config") .. "/themes/default"

theme.font          = "Arial Bold 8"

theme.bg_normal     = "#202020"
theme.bg_focus      = "#202020"
theme.bg_urgent     = "#FF9800"
theme.bg_minimize   = "#444444"

theme.fg_normal     = "#B8B8B8"
theme.fg_focus      = "#FECF35"
theme.fg_urgent     = "#202020"
theme.fg_minimize   = "#ffffff"

theme.border_width  = "1"
theme.border_normal = "#B8B8B8"
theme.border_focus  = "#303030"
theme.border_marked = "#91231c"

-- color :title_fg,          "#fecf35"
-- color :title_bg,          "#202020"
-- color :title_border,      "#303030"

-- color :focus_bg,          "#202020"
-- color :focus_border,      "#303030"
-- color :focus_fg,          "#fecf35"

-- color :urgent_fg,         "#FF9800"
-- color :urgent_bg,         "#202020"
-- color :urgent_border,     "#303030"

-- color :occupied_fg,       "#b8b8b8"
-- color :occupied_border,   "#303030"
-- color :occupied_bg,       "#202020"

-- color :views_border,      "#303030"
-- color :views_bg,          "#202020"
-- color :views_fg,          "#757575"

-- color :sublets_bg,        "#202020"
-- color :sublets_border,    "#303030"
-- color :sublets_fg,        "#757575"

-- color :client_inactive,   "#202020"
-- color :client_active,     "#303030"

-- color :panel,             "#202020"

-- color :background,        "#3d3d3d"

-- color :stipple,           "#757575"



-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
-- theme.taglist_bg_focus = "#B8B8B8"

theme.tasklist_bg_focus = "#303030"


-- Display the taglist squares
theme.taglist_squares_sel                       = theme.confdir .. "/icons/taglist/squaref_b.png"
theme.taglist_squares_unsel                     = theme.confdir .. "/icons/taglist/square_b.png"

theme.tasklist_floating_icon                    = theme.confdir .. "/icons/tasklist/floatingw.png"

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon                         = theme.confdir .. "/icons/submenu.png"
theme.menu_height                               = "15"
theme.menu_width                                = "100"

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget                               = "#cc0000"

-- Widgets
theme.widget_separator                          = theme.confdir .. "/icons/widgets/separator.png"
theme.widget_date                               = theme.confdir .. "/icons/widgets/time.png"


-- Titlebar
theme.titlebar_close_button_normal              = theme.confdir .. "/icons/titlebar/close_normal.png"
theme.titlebar_close_button_focus               = theme.confdir .. "/icons/titlebar/close_focus.png"

theme.titlebar_ontop_button_normal_inactive     = theme.confdir .. "/icons/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive      = theme.confdir .. "/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = theme.confdir .. "/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active        = theme.confdir .. "/icons/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive    = theme.confdir .. "/icons/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive     = theme.confdir .. "/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = theme.confdir .. "/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active       = theme.confdir .. "/icons/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive  = theme.confdir .. "/icons/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive   = theme.confdir .. "/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = theme.confdir .. "/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active     = theme.confdir .. "/icons/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = theme.confdir .. "/icons/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = theme.confdir .. "/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = theme.confdir .. "/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active    = theme.confdir .. "/icons/titlebar/maximized_focus_active.png"

-- Wallpaper
theme.wallpaper_cmd                             = { "/usr/bin/nitrogen --restore" }

-- Layout icons
theme.layout_fairh                              = theme.confdir .. "/icons/layouts/fairh.png"
theme.layout_fairv                              = theme.confdir .. "/icons/layouts/fairv.png"
theme.layout_floating                           = theme.confdir .. "/icons/layouts/floating.png"
theme.layout_magnifier                          = theme.confdir .. "/icons/layouts/magnifier.png"
theme.layout_max                                = theme.confdir .. "/icons/layouts/max.png"
theme.layout_fullscreen                         = theme.confdir .. "/icons/layouts/fullscreen.png"
theme.layout_tilebottom                         = theme.confdir .. "/icons/layouts/tilebottom.png"
theme.layout_tileleft                           = theme.confdir .. "/icons/layouts/tileleft.png"
theme.layout_tile                               = theme.confdir .. "/icons/layouts/tile.png"
theme.layout_tiletop                            = theme.confdir .. "/icons/layouts/tiletop.png"
theme.layout_spiral                             = theme.confdir .. "/icons/layouts/spiral.png"
theme.layout_dwindle                            = theme.confdir .. "/icons/layouts/dwindle.png"

theme.awesome_icon                              = theme.confdir .. "/icons/submenu.png"

return theme
-- vim: filetype                                =lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:encoding=utf-8:textwidth=80
