#' Enhanced Data Viewer
#'
#' @description One of the biggest problems of the built-in data viewer is its
#'   limited capability to expand the columns.
#'
#' @param file File to be viewed
#'
#' @export
#'
View <- function(file) {
  data.table::setDTthreads(8)
  requireNamespace(DT)
  requireNamespace(shiny)

  ui <- basicPage(
    DT::DTOutput("mytable")
  )

  server <- function(input, output) {
    output$mytable <- DT::renderDT(file,
      extensions = c("ColReorder", "Responsive", "KeyTable"),
      options = list(
        pageLength = 50,
        lengthMenu = c(5, 20, 50, 100, 200, 1000),
        colReorder = TRUE,
        autoWidth = TRUE,
        language = list(search = "Filter:"),
        scrollX = TRUE,
        keys = TRUE
      )
    )
  }

  shinyApp(ui, server)
}
