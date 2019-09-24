library(DataExplorer)

NOT RUN {
  #############################
  ## Default config file     ##
  ## Copy and edit if needed ##
  #############################
  config <- list(
    "introduce" = list(),
    "plot_str" = list(
      "type" = "diagonal",
      "fontSize" = 35,
      "width" = 1000,
      "margin" = list("left" = 350, "right" = 250)
    ),
    "plot_missing" = list(),
    "plot_histogram" = list(),
    "plot_qq" = list(sampled_rows = 1000L),
    "plot_bar" = list(),
    "plot_correlation" = list("cor_args" = list("use" = "pairwise.complete.obs")),
    "plot_prcomp" = list(),
    "plot_boxplot" = list(),
    "plot_scatterplot" = list(sampled_rows = 1000L)
  )
  
  # Create report
  create_report(iris)
  create_report(airquality, y = "Ozone")
  
  # Load library
  library(ggplot2)
  library(data.table)
  library(rmarkdown)
  
  # Set some missing values
  diamonds2 <- data.table(diamonds)
  for (j in 5:ncol(diamonds2)) {
    set(diamonds2,
        i = sample.int(nrow(diamonds2), sample.int(nrow(diamonds2), 1)),
        j,
        value = NA_integer_)
  }
  
  # Create customized report for diamonds2 dataset
  create_report(
    data = diamonds2,
    output_file = "report.html",
    output_dir = getwd(),
    y = "price",
    config = list(
      "introduce" = list(),
      "plot_missing" = list(),
      "plot_histogram" = list(),
      "plot_qq" = list("by" = "cut", sampled_rows = 1000L),
      "plot_bar" = list("with" = "carat"),
      "plot_correlation" = list("cor_args" = list("use" = "pairwise.complete.obs")),
      "plot_prcomp" = list(),
      "plot_boxplot" = list("by" = "cut")
    ),
    html_document(toc = TRUE, toc_depth = 6, theme = "flatly")
  )
  # }