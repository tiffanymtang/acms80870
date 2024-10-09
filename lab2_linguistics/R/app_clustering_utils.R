#' Widgets to set data and clustering options
clusteringOptionsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Data input/cleaning options
    shiny::fluidRow(
      shiny::column(
        4,
        shinyWrappers::radio_group_buttons(
          ns("data_mode"),
          label = "Data Mode",
          choices = c("County-level", "Individual-level")
        )
      ),
      shiny::column(
        4,
        shiny::numericInput(
          ns("min_answers"),
          label = "Min. # of Answers",
          min = 1, max = 67, value = 50, step = 1,
          width = "60%"
        )
      ),
      shiny::column(
        4,
        shinyWrappers::radio_group_buttons(
          ns("dimred_mode"),
          label = "Do Dimension Reduction",
          choices = c("No", "Yes")
        )
      )
    ),
    #' Dimension reduction options
    shiny::conditionalPanel(
      condition = sprintf("input[['%s-dimred_mode']] == 'Yes'", id),
      bslib::layout_column_wrap(
        shinyWrappers::picker_input(
          ns("dimred_method"),
          label = "Dimension Reduction Method",
          choices = c("PCA", "t-SNE", "UMAP"),
          selected = "PCA"
        ),
        shiny::numericInput(
          ns("dimred_n_components"),
          label = "Number of Components",
          min = 2, value = 20, step = 1
        ),
        shiny::conditionalPanel(
          condition = sprintf(
            "input[['%s-dimred_method']] == 't-SNE' || input[['%s-dimred_method']] == 'UMAP'",
            id, id
          ),
          shiny::numericInput(
            ns("dimred_perplexity"),
            label = "Perplexity",
            min = 2, max = 800, value = 30, step = 5
          )
        )
      )
    ),
    # Clustering options
    bslib::layout_column_wrap(
      shinyWrappers::radio_group_buttons(
        ns("cluster_mode"),
        label = "Clustering Method",
        choices = c("K-means", "Hierarchical")
      ),
      # Hierarchical clustering options
      shiny::conditionalPanel(
        condition = sprintf("input[['%s-cluster_mode']] == 'Hierarchical'", id),
        shinyWrappers::picker_input(
          ns("linkage"),
          label = "Linkage Metric",
          choices = c("ward.D", "single", "complete", "average"),
          selected = "ward.D",
          width = "80%"
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input[['%s-cluster_mode']] == 'Hierarchical'", id),
        shinyWrappers::picker_input(
          ns("distance"),
          label = "Distance Metric",
          choices = c("euclidean", "manhattan", "maximum", "canberra", "binary", "minkowski"),
          selected = "euclidean",
          width = "80%"
        )
      )
    ),
    # Specify number of clusters
    shiny::sliderInput(
      ns("k"),
      label = "Number of clusters",
      min = 2, max = 100, value = 2, step = 1,
      animate = shiny::animationOptions(interval = 2000)
    ),
    # Choose plot type
    shinyWrappers::radio_group_buttons(
      ns("plot_mode"),
      label = "Plot Type",
      choices = c("Map", "Tree", "PCA", "t-SNE", "UMAP")
    ),
    # Show plots
    shiny::conditionalPanel(
      condition = sprintf("input[['%s-plot_mode']] == 'Map'", id),
      shinyWrappers::plotUI(ns("map_plot"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input[['%s-plot_mode']] == 'Tree'", id),
      shinyWrappers::plotUI(ns("tree_plot"))
    ),
    shiny::conditionalPanel(
      condition = sprintf(
        "input[['%s-plot_mode']] == 'PCA' || input[['%s-plot_mode']] == 't-SNE' || input[['%s-plot_mode']] == 'UMAP'",
        id, id, id
      ),
      shinyWrappers::plotUI(ns("scatter_plot"))
    )
  )
}


#' Update clustering option widgets
updateClusteringOptions <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      if (input$dimred_method == "PCA") {
        value <- 100
      } else if (input$dimred_method %in% c("t-SNE", "UMAP")) {
        value <- 2
        if (input$dimred_method == "t-SNE") {
          shiny::updateNumericInput(
            session,
            "dimred_perplexity",
            label = "Perplexity",
            min = 2, max = 800, value = 30, step = 5
          )
        } else if (input$dimred_method == "UMAP") {
          shiny::updateNumericInput(
            session,
            "dimred_perplexity",
            label = "# Neighbors",
            min = 2, value = 15, step = 5
          )
        }
      }
      shiny::updateNumericInput(
        session,
        "dimred_n_components",
        min = 2, value = value, step = 1
      )
    })
  })
}


#' Run clustering
runClustering <- function(id, ling_df) {
  shiny::moduleServer(id, function(input, output, session) {
    # get cleaned linguistics data
    cleanLingData <- shiny::reactive({
      shiny::req(input$min_answers, input$data_mode)
      data <- ling_df |>
        remove_samples(min_answers = input$min_answers)
      if (input$data_mode == "County-level") {
        data <- one_hot_ling_data(data, return_matrix = FALSE) |>
          aggregate_survey_response_by_county()
      } else if (input$data_mode == "Individual-level") {
        data <- one_hot_ling_data(data, return_matrix = FALSE)
      }
      data <- data |>
        # need to remove duplicates or else tsne will throw an error
        dplyr::distinct(
          dplyr::across(tidyselect::starts_with("Q")),
          .keep_all = TRUE
        )
    })

    # get data after possibly doing dimension reduction
    getData <- shiny::reactive({
      shiny::req(input$dimred_mode, input$dimred_n_components)
      if (input$dimred_mode == "Yes") {
        if (input$dimred_method == "PCA") {
          X <- doPCA()
        } else if (input$dimred_method == "t-SNE") {
          X <- dotSNE()
        } else if (input$dimred_method == "UMAP") {
          X <- doUMAP()
        }
        X <- X[, 1:input$dimred_n_components, drop = FALSE]
      } else {
        X <- cleanLingData() |>
          get_X_matrix()
      }
      return(X)
    })

    # do dimension reduction wrapper
    doDimensionReduction <- shiny::reactive({
      shiny::req(input$plot_mode %in% c("PCA", "t-SNE", "UMAP"))
      if (input$plot_mode == "PCA") {
        X <- doPCA()
      } else if (input$plot_mode == "t-SNE") {
        X <- dotSNE()
      } else if (input$plot_mode == "UMAP") {
        X <- doUMAP()
      }
      return(X[, 1:2, drop = FALSE])
    })

    # do PCA
    doPCA <- shiny::reactive({
      X <- cleanLingData() |>
        get_X_matrix()
      pca_out <- prcomp(X)
      return(pca_out$x)
    })

    # do tSNE
    dotSNE <- shiny::reactive({
      shiny::req(input$dimred_perplexity, input$dimred_n_components)
      X <- cleanLingData() |>
        get_X_matrix()
      tsne_out <- Rtsne::Rtsne(
        X,
        dims = input$dimred_n_components,
        perplexity = input$dimred_perplexity
      )
      return(tsne_out$Y)
    })

    # do UMAP
    doUMAP <- shiny::reactive({
      shiny::req(input$dimred_perplexity, input$dimred_n_components)
      X <- cleanLingData() |>
        get_X_matrix()
      umap_out <- umap::umap(
        X,
        n_components = input$dimred_n_components,
        n_neighbors = input$dimred_perplexity
      )
      return(umap_out$layout)
    })

    # compute distance matrix for hierarchical clustering
    getHclustDist <- shiny::reactive({
      shiny::req(input$distance)
      X <- getData()
      D <- dist(X, method = input$distance)
      return(D)
    })

    # do hierarchical clustering
    doHclust <- shiny::reactive({
      shiny::req(input$linkage, input$distance)
      X <- getData()
      D <- getHclustDist()
      hclust_out <- vdocs::plot_hclust(
        X, dist_matrix = D, linkage = input$linkage
      )
      return(hclust_out)
    })

    # do kmeans clustering
    doKmeans <- shiny::reactive({
      shiny::req(input$k)
      X <- getData()
      kmeans_out <- stats::kmeans(X, centers = input$k)
      return(kmeans_out)
    })

    # get cluster membership vector
    getClusters <- shiny::reactive({
      shiny::req(input$k)
      if (input$cluster_mode == "Hierarchical") {
        cluster_out <- doHclust()
        clusters <- cutree(cluster_out$hclust, k = input$k)
      } else if (input$cluster_mode == "K-means") {
        cluster_out <- doKmeans()
        clusters <- cluster_out$cluster
      }
      return(clusters)
    })

    ## Map plot ---------------------------------------------------------------
    plot_map <- reactive({
      ling_data <- cleanLingData()
      clusters <- getClusters()
      plt <- plot_dr_map(
        as.numeric(clusters), ling_data, ndim = 1, by_county = TRUE
      ) +
        ggplot2::labs(
          fill = sprintf("%s Clusters", input$cluster_mode),
          color = sprintf("%s Clusters", input$cluster_mode),
          title = ""
        )
    })
    shinyWrappers::plotServer(
      id = "map_plot",
      plot_fun = plot_map,
      plot_options = FALSE,
      modes = "ggplot"
    )

    ## Tree plot --------------------------------------------------------------
    plot_tree <- reactive({
      req(input$cluster_mode == "Hierarchical")
      hclust_out <- doHclust()
      return(hclust_out$plot)
    })
    shinyWrappers::plotServer(
      id = "tree_plot",
      plot_fun = plot_tree,
      plot_options = FALSE,
      modes = "ggplot"
    )

    ## Scatter plot -----------------------------------------------------------
    get_dimred_df <- reactive({
      X <- cleanLingData() |>
        get_X_matrix()
      if (input$plot_mode == "PCA") {
        pca_out <- prcomp(X)
        X <- pca_out$x[, 1:2]
      } else if (input$plot_mode == "t-SNE") {
        if ((input$dimred_mode == "Yes") && (input$dimred_method == "t-SNE")) {
          perplexity <- input$dimred_perplexity
        } else {
          perplexity <- 30
        }
        tsne_out <- Rtsne::Rtsne(X, dims = 2, perplexity = perplexity)
        X <- tsne_out$Y
      } else if (input$plot_mode == "UMAP") {
        if ((input$dimred_mode == "Yes") && (input$dimred_method == "UMAP")) {
          n_neighbors <- input$dimred_perplexity
        } else {
          n_neighbors <- 15
        }
        umap_out <- umap::umap(X, n_components = 2, n_neighbors = n_neighbors)
        X <- umap_out$layout
      }
      return(X)
    })
    plot_cluster_scatter <- reactive({
      X <- get_dimred_df() |>
        as.data.frame() |>
        dplyr::mutate(
          .clusters = getClusters()
        )
      plt <- ggplot2::ggplot(X) +
        ggplot2::aes(
          x = .data[[colnames(X)[1]]],
          y = .data[[colnames(X)[2]]],
          color = .clusters
        ) +
        ggplot2::geom_point() +
        vthemes::scale_color_vmodern(discrete = FALSE) +
        ggplot2::labs(
          color = sprintf("%s Clusters", input$cluster_mode),
          x = sprintf("%s Component 1", input$plot_mode),
          y = sprintf("%s Component 2", input$plot_mode)
        ) +
        vthemes::theme_vmodern(size_preset = "medium")
      return(plt)
    })
    shinyWrappers::plotServer(
      id = "scatter_plot",
      plot_fun = plot_cluster_scatter,
      plot_options = FALSE,
      modes = "ggplot"
    )
  })
}
