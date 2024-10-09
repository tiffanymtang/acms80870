#' Run hierarchical clustering Shiny application
#'
#' @param ... Additional arguments to pass to [shiny::shinyApp()]
#'
#' @returns A shiny application
#'
#' @export
run_app <- function(...) {

  # Load data
  # qa_key <- load_q_and_a_key()
  ling_orig <- load_ling_data()
  zip_df <- load_zip_data()
  ling_df <- merge_ling_zip_data(ling_orig, zip_df)

  title <- "Linguistics Data Survey: Clustering"

  # picker_width <- "97.5%"

  # UI -------------------------------------------------------------------------
  ui <- shinydashboard::dashboardPage(
    title = title,

    # Header -------------------------------------------------------------------
    shinyWrappers::prettyDashboardHeader(
      title = title,
      title_position = "center",
      title_style = list(
        `font-variant` = "small-caps", `font-size` = "26px",
        `font-family` = "'Quattrocento Sans'"
      )
    ),

    # Sidebar ------------------------------------------------------------------
    shinydashboard::dashboardSidebar(disable = TRUE),

    # Body ---------------------------------------------------------------------
    shinydashboard::dashboardBody(
      shinyWrappers::use_pretty_style(),
      bslib::layout_columns(
        shinyWrappers::prettyBox(
          clusteringOptionsUI(1),
          title = "Hierarchical Clustering Results v1"
        ),
        shinyWrappers::prettyBox(
          clusteringOptionsUI(2),
          title = "Hierarchical Clustering Results v2"
        )
      ),
      bslib::layout_columns(
        shinyWrappers::prettyBox(
          clusteringOptionsUI(3),
          title = "Hierarchical Clustering Results v3"
        ),
        shinyWrappers::prettyBox(
          clusteringOptionsUI(4),
          title = "Hierarchical Clustering Results v4"
        )
      )
    )
  )

  # Server ---------------------------------------------------------------------
  server <- function(input, output, session) {
    updateClusteringOptions(1)
    updateClusteringOptions(2)
    updateClusteringOptions(3)
    updateClusteringOptions(4)

    runClustering(1, ling_df)
    runClustering(2, ling_df)
    runClustering(3, ling_df)
    runClustering(4, ling_df)
  }

  shiny::shinyApp(ui, server, ...)
}
