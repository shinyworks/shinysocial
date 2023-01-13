#' Collect social media contact information
#'
#' A shiny module to collect and normalize a user's social media contact
#' information.
#'
#' @inheritParams .shared-parameters
#' @param twitter,linkedin Logical; whether to include the given site in the ui.
#'   At least one of these values must be `TRUE`.
#'
#' @return A shiny module ui, or the normalized contacts from one.
#' @export
social_contacts_ui <- function(id, twitter = TRUE, linkedin = TRUE) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    if (twitter) {
      shiny::column(
        width = 6,
        offset = 0,
        shiny::textInput(
          ns("twitter"), label = shiny::icon("twitter")
        )
      )
    },
    if (linkedin) {
      shiny::column(
        width = 6,
        offset = 0,
        shiny::textInput(
          ns("linkedin"), label = shiny::icon("linkedin")
        )
      )
    }
  )
}

#' @rdname social_contacts_ui
social_contacts_server <- function(id) {
  contacts <- shiny::reactiveValues()
  shiny::moduleServer(
    id,
    function(input, output, session) {
      contacts$twitter_handle <- shiny::reactive({
        shiny::req(input$twitter)
        twitter_input <- stringr::str_remove_all(
          input$twitter,
          "https|http|:|twitter\\.com|twitter/.|/|@"
        )
        twitter_input
      })
      contacts$linkedin_url <- shiny::reactive({
        shiny::req(input$linkedin)
        linkedin_input <- stringr::str_remove_all(
          input$linkedin,
          "https|:|www\\.linkedin\\.com|linkedin.com|/in|/|@"
        )
        linkedin_input
      })
    }
  )
  return(contacts)
}
