
  page_side <- function(subheader_title, side_nav_tabs_independent, side_nav_tabs_independent_icon, sheets_figures, sheets_figures_icon = NULL) {
    #shiny.i18n::usei18n(i18n)
    tagList(
      material_side_nav(
        fixed = FALSE,
        image_source = 'db.png',
        tags$li(tags$div(class="divider")),
        tags$li(tags$a(class="subheader", subheader_title)),
        material_side_nav_tabs(
          side_nav_tabs = side_nav_tabs_independent,
          icons = side_nav_tabs_independent_icon
        )
        # ,
        #
        # material_side_nav_tabs_collapsible(
        #   collapsible_menu_arrow_name = "Dropdown",
        #   side_nav_tabs = sheets_figures,
        #   icons = sheets_figures_icon
        # )
      )
    )
  }
