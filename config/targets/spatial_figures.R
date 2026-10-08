# Spatial figures target declarations. Scientific functions live in src/.
list(
  targets::tar_target(figure_population_density_maps,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "maps")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      file <- here::here(out_dir, "bogota_population_density_map.pdf")
      save_plot_pdf(
        plot_obj = bogota_population_map,
        path = file,
        width = 8,
        height = 8,
        dpi = 300,
        bg = NULL,
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "mexico_population_density_map.pdf")
      save_plot_pdf(
        plot_obj = cdmx_population_map,
        path = file,
        width = 8,
        height = 8,
        dpi = 300,
        bg = NULL,
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "santiago_population_density_map.pdf")
      save_plot_pdf(
        plot_obj = santiago_population_map,
        path = file,
        width = 8,
        height = 8,
        dpi = 300,
        bg = NULL,
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "saopaulo_population_density_map.pdf")
      save_plot_pdf(
        plot_obj = sao_paulo_population_map,
        path = file,
        width = 8,
        height = 8,
        dpi = 300,
        bg = NULL,
        limitsize = FALSE)
      files <- c(files, file)
      files
    }, format = "file"),

  targets::tar_target(figure_pollution_quintile_maps,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "maps")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      file <- here::here(out_dir, "map_bogota_3km.pdf")
      save_plot_pdf(
        plot_obj = bogota_education_map,
        path = file,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "map_mexico_3km.pdf")
      save_plot_pdf(
        plot_obj = cdmx_education_map,
        path = file,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "map_santiago_3km_dc.pdf")
      save_plot_pdf(
        plot_obj = santiago_education_map,
        path = file,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "map_saopaulo_3km.pdf")
      save_plot_pdf(
        plot_obj = sao_paulo_education_map,
        path = file,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      files
    }, format = "file")
)
