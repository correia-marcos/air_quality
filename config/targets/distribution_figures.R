# Distribution figures target declarations. Scientific functions live in src/.
list(
  targets::tar_target(kernel_distribution_plots,
    {
      paper_font
      set_paper_theme()
      city_panels <- list("Bogotá" = bogota_outliers, "Mexico City" = cdmx_outliers,
        "São Paulo" = sao_paulo_outliers, "Santiago" = santiago_outliers)
      density_pm10 <- density_pm25 <- list()
      for (year in c(analysis_year, kernel_reference_years)) {
        key <- as.character(year)
        pm25_limit <- if (year == analysis_year) kernel_pm25_limit else
        kernel_pm25_reference_limit
        density_pm10[[key]] <- plot_kernel_density_by_city(
          city_data = city_panels,
          pollutant = "pm10",
          year = year,
          x_max = kernel_pm10_limit,
          city_colours = kernel_city_colours,
          city_linetypes = kernel_city_linetypes,
          fill_alpha = 0,
          legend_position = "bottom")
        density_pm25[[key]] <- plot_kernel_density_by_city(
          city_data = city_panels,
          pollutant = "pm25",
          year = year,
          x_max = pm25_limit,
          city_colours = kernel_city_colours,
          city_linetypes = kernel_city_linetypes,
          fill_alpha = 0,
          legend_position = "bottom")
      }
      exceedance_pm10 <- plot_exceedance_shares(
        city_data = city_panels,
        pollutant = "pm10",
        year = analysis_year,
        legend_position = "bottom")
      exceedance_pm25 <- plot_exceedance_shares(
        city_data = city_panels,
        pollutant = "pm25",
        year = analysis_year,
        legend_position = "bottom")
      list(pm10 = density_pm10, pm25 = density_pm25,
        exceedance = cowplot::plot_grid(exceedance_pm10, exceedance_pm25, nrow = 2))
    }),

  targets::tar_target(figure_kernel_distributions,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "temporal")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (year in c(analysis_year, kernel_reference_years)) {
        key <- as.character(year)
        tag <- if (year == analysis_year) "" else paste0("_", year)
        file <- here::here(out_dir, paste0("all", tag, ".pdf"))
        save_plot_pdf(
          plot_obj = kernel_distribution_plots$pm10[[key]],
          path = file,
          width = 16,
          height = 9,
          dpi = 300,
          bg = NULL,
          limitsize = FALSE)
        files <- c(files, file)
        file <- here::here(out_dir, paste0("all_pm25", tag, ".pdf"))
        save_plot_pdf(
          plot_obj = kernel_distribution_plots$pm25[[key]],
          path = file,
          width = 16,
          height = 9,
          dpi = 300,
          bg = NULL,
          limitsize = FALSE)
        files <- c(files, file)
      }
      file <- here::here(out_dir, "exceedance_shares.pdf")
      save_plot_pdf(
        plot_obj = kernel_distribution_plots$exceedance,
        path = file,
        width = 16,
        height = 12,
        dpi = 300,
        bg = NULL,
        limitsize = FALSE)
      c(files, file)
    }, format = "file")
)
