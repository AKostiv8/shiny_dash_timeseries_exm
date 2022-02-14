library(shiny)
library(shinymaterial)
library(shiny.router)
library(stringr)
library(shinycssloaders)
library(shinyjs)
library(reactable)
library(sparkline)
library(yaml)
library(tidyverse)
library(lubridate)
library(data.table)
library(shinymanager)
library(googlesheets4)
library(jsonlite)
library(plotly)
library(modeltime)
# library(tidymodels) # conflict with shiny.router
library(timetk)

source("utils.R")
source("pages/main_page_schema.R")
source("pages/navbar/cdc.R")
source("pages/navbar/hdn.R")


router <- make_router(
    route("/", page_1_page, page_1_page_server),
    route("hdn", page_2_page, page_2_page_server)
)



# Wrap shinymaterial apps in material_page
ui <- material_page(


    useShinyjs(),
    title = "Forecast | Example",
    primary_theme_color = "#2c3d4f",
    secondary_theme_color = "#797c17",
    right_menu = tagList(
        tags$ul(
            id="nav-mobile", class="right hide-on-med-and-down",
            tags$li(id = "cdc_pageId", class = "active", a(href = route_link("/"), "Sales")),
            tags$li(id = "hdn_pageId", a(href = route_link("/hdn"), "Tickets"))
        )
    ),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$link(rel="icon", href="db.png")
    ),

    # Aside menu content
    uiOutput("sidecontent"),


    router$ui


)

server <- function(input, output, session) {

    router$server(input, output, session)

    observe({
            if (is_page("/")) {
                output$sidecontent <- renderUI({
                    render_material_from_server(
                        tagList(
                            shiny::tags$script('$("#tab1_p1_tab_id a" ).trigger("click");'),
                            page_side(
                                subheader_title = "Sales section",
                                side_nav_tabs_independent = c(
                                    "Predict Forecast Sales" = "tab1_p1",
                                    "Test Forecast Sales" = "tab2_p1",
                                    "Test Accurancy Sales" = "tab3_p1"
                                    ),
                                side_nav_tabs_independent_icon = c("list", "list","list")
                            )
                        )
                    )
                })
                runjs('$( "#cdc_pageId" ).addClass( "active" );')
                runjs('$( "#hdn_pageId" ).removeClass( "active" );')
                runjs('$(".sidenav-trigger").remove();')

                # change_page("side")
            } else if (is_page("hdn")) {
                output$sidecontent <- renderUI({
                    render_material_from_server(
                        tagList(
                            shiny::tags$script('$("#tab1_p2_tab_id a" ).trigger("click");'),
                        page_side(
                            subheader_title = "Tickets section",
                            side_nav_tabs_independent = c(
                                "Predict Forecast Tickets" = "tab1_p2",
                                "Test Forecast Tickets" = "tab2_p2",
                                "Test Accurancy Tickets" = "tab3_p2"
                                ),
                            side_nav_tabs_independent_icon = c("list", "list","list")
                        )
                        )
                    )
                })
                runjs('$( "#hdn_pageId" ).addClass( "active" );')
                runjs('$( "#cdc_pageId" ).removeClass( "active" );')
                runjs('$(".sidenav-trigger").remove();')
            }
    })



}
shinyApp(ui = ui, server = server)
