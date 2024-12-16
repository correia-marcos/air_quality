# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: Create configuration file for setup of packages and functions used in the project
#
# @Date: Nov 2024
# @author: Marcos

# Load libraries - groundhog increases code reproducibility
library(groundhog)            # You need to have at least R version = 4.3.1

# Loading required packages 
groundhog.library(
  pkg  = c("cowplot",
           "dplyr",
           "ggplot2",
           "ggspatial",
           "rnaturalearth",
           "rnaturalearthdata",
           "sf",
           "terra", 
           "tidyr"),
  date = "2024-05-05")

here::i_am(".gitignore")

# ############################################################################################
# Functions
# ############################################################################################

# Function --------------------------------------------------------------------
# @Arg       : shapefile  is an 'sf' object representing the city boundary
# @Arg       : nc_file    is a string containing the path to a single .nc4 file
#              from the MERRA-2 dataset
# @Arg       : city_name  is a string with the name of the city
# @Output    : A ggplot object representing the map of the city boundary and 
#              MERRA-2 grid cells
# @Purpose   : Creates a spatial plot showing the city's boundary and the overlayed
#              MERRA-2 grid cells from the specified nc_file. This visualization helps 
#              in understanding the spatial extent of the MERRA-2 data relative to 
#              the city's area.
# @Written_on: 10/12/2024
# @Written_by: Marcos Paulo
plot_merra2_grid_city <- function(shapefile, nc_file, city_name) {
  
  # Load the MERRA-2 data
  nc_data <- rast(nc_file)
  
  # Crop the raster to the extent of the shapefile plus a buffer (optional)
  # buffer_distance <- 0.0001  # Degrees, adjust as needed
  # shapefile_buffered <- st_buffer(shapefile, dist = buffer_distance)
  
  # Transform shapefile to match MERRA-2 CRS if necessary
  crs(nc_data) <- "EPSG:4326"
  shapefile_proj <- st_transform(shapefile, crs = crs(nc_data))
  
  # Crop MERRA-2 data to the city's extent for visualization
  nc_data_cropped <- terra::crop(nc_data, vect(shapefile_proj), snap = "out")
  nc_data_masked  <- terra::mask(nc_data_cropped, vect(shapefile_proj))
  
  # Convert raster to polygons to represent grid cells
  grid_cells    <- as.polygons(nc_data_cropped, dissolve = FALSE, values = FALSE)
  grid_cells_sf <- st_as_sf(grid_cells)
  
  # Calculate centroids for grids
  grid_centroids <- st_centroid(grid_cells_sf)
  
  # Create a ggplot
  p <- ggplot() +
    geom_sf(data = grid_cells_sf, fill = NA, color = "navy", size = 0.3) +
    geom_sf(data = shapefile_proj, fill = "grey50", color = "grey10", size = 0.001) +
    geom_sf(data = grid_centroids, shape = 3, fill = "navy", color = "navy", size = 1.5) +
    labs(
      title = paste("MERRA-2 Grid Cells over", city_name),
      x = "Longitude", 
      y = "Latitude"
    ) +
    coord_sf() +
    scale_x_continuous(name = "Longitude", breaks = seq(-180, 180, by = 0.5)) +
    scale_y_continuous(name = "Latitude", breaks = seq(-90, 90, by = 0.5)) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "gray80", size = 14, face = "bold"),
      axis.text = element_text(color = "gray80", size = 12),
      axis.ticks = element_line(color = "gray80"),
      axis.line = element_line(color = "gray80", linewidth = 0.03),
      plot.title = element_text(face = "bold", hjust = 0.5, color = "black", size = 16),
      plot.subtitle = element_text(hjust = 0.5, color = "black"),
      panel.border = element_blank(),
      legend.position = "none",
    )
  
  print(p)
  
  # Return the plot object
  return(p)
}


# Function --------------------------------------------------------------------
# @Arg       : df_list is a named list of dataframes, where each dataframe contains
#              aerosol concentration and PM 2.5 data.
# @Arg       : variable is a string specifying the variable to plot (e.g., "DUSMASS25",
#              "OCSMASS", "pm25_estimate").
# @Arg       : var_label is a string specifying the label for the x-axis and plot title.
#              Defaults to the variable name if not provided.
# @Arg.      : max_x_limit is a number representing the right limit for the x axis
# @Output    : A single density plot comparing the variable across cities.
# @Purpose   : Generate a density plot for a specific aerosol or PM 2.5 concentration
#              across multiple cities, including WHO PM 2.5 guidelines if applicable.
# @Written_on: 13/12/2024
# @Written_by: Marcos Paulo
plot_variable_across_cities <- function(df_list,
                                        variable,
                                        var_label = NULL,
                                        max_y_limit = NULL,
                                        max_x_limit = NULL) {
  
  # Set a default label if none is provided
  if (is.null(var_label)) {
    var_label <- variable
  }
  
  # Ensure all dataframes in the list contain the specified variable
  if (!all(sapply(df_list, function(df) variable %in% names(df)))) {
    stop(paste("All dataframes must contain the variable:", variable))
  }
  
  # Combine dataframes into one with a city identifier
  combined_df <- do.call(rbind, lapply(names(df_list), function(city) {
    df <- df_list[[city]]
    df$City <- city  # Add city name as a new column
    return(df)
  }))
  
  # Ensure the variable is numeric
  combined_df[[variable]] <- as.numeric(combined_df[[variable]])
  
  # Create the density plot
  p <- ggplot(combined_df, aes(x = .data[[variable]], color = City, fill = City)) +
    geom_density(alpha = 0.3, linewidth = 1) +
    labs(
      title = paste("Density Plot of", var_label),
      x = var_label,
      y = "Density"
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal(base_size = 14) +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  # Apply custom x-axis limits if provided
  if (!is.null(max_x_limit)) {
    p <- p + scale_x_continuous(limits = c(NA, max_x_limit))  # Keep default min, set max
  }
  
  # Add WHO lines if the variable is PM 2.5
  if (variable == "pm25_estimate") {
    # Apply custom y-axis limit for the segments
    if (is.null(max_y_limit)){
      # Calculate max_y for the plot to position the lines and labels
      dens <- density(combined_df[[variable]], na.rm = TRUE)
      max_y <- max(dens$y)
    } else {
      max_y <- max_y_limit
    }

    p <- p +
      geom_segment(x = 25,
                   xend = 25,
                   y = 0,
                   yend = max_y,
                   linetype = "dashed",
                   color = "orange",
                   linewidth = 0.5) +
      geom_segment(x = 35,
                   xend = 35,
                   y = 0,
                   yend = max_y,
                   linetype = "dashed",
                   color = "darkred",
                   linewidth = 0.5) +
      annotate("text",
               x = 25.5,
               y = max_y - 0.01,
               label = "IT2",
               color = "orange",
               size = 4,
               hjust = 0) +
      annotate("text",
               x = 35.5,
               y = max_y - 0.01,
               label = "IT1",
               color = "darkred",
               size = 4,
               hjust = 0)
  }
  
  print(p)
  return(p)
}


# Function --------------------------------------------------------------------
# @Arg       : latin_america - An 'sf' object representing Latin America map.
# @Arg       : regions       - A list of 'sf' objects for metropolitan areas 
#                              (e.g., Bogota, ciudad_mexico, etc.).
# @Arg       : region_names  - A vector of city names corresponding to 'regions'.
# @Arg       : outline       - Logical; if TRUE, regions will be outlined.
# @Output    : A high-quality map with scale bar, compass, and customized aesthetics.
# @Purpose   : Produce a publication-ready map highlighting metropolitan regions 
#              over Latin America with optional outlines.
# @Written_on: 15/12/2024
# @Written_by: Marcos Paulo
plot_latin_america_map <- function(latin_america, regions, region_names, outline = TRUE) {
  # Check input validity
  if (length(regions) != length(region_names)) {
    stop("The length of 'regions' and 'region_names' must match.")
  }
  
  # Predefined colors for each region
  region_colors <- c("Bogotá" = "#4C72B0", 
                     "Ciudad de México" = "#DD8452", 
                     "Santiago" = "aquamarine4", 
                     "São Paulo" = "darkred")
  
  # Ensure all regions have the same CRS and add an identifier column
  regions <- lapply(1:length(regions), function(i) {
    region <- regions[[i]]
    region <- st_transform(region, crs = 4326) # Ensure consistent CRS
    region_union <- st_union(region)  # Aggregate into one polygon
    
    # Simplify the geometry to remove internal details
    region_simplified <- st_simplify(region_union, dTolerance = 0.01, preserveTopology = TRUE)
    
    # Convert to sf object with simplified geometry
    region_sf <- st_sf(region_name = region_names[i], geometry = region_simplified)
    
    return(region_sf)
  })
  
  # Combine all regions into a single sf object
  combined_regions <- do.call(rbind, regions)  # Preserve attributes, including 'region_name'
  
  # Determine bounding box for the regions to "zoom" the map
  bbox <- st_bbox(combined_regions)
  xlim <- c(bbox["xmin"] - 2, bbox["xmax"] + 2)  # Add small buffer for aesthetics
  ylim <- c(bbox["ymin"] - 2, bbox["ymax"] + 2)
  
  # Base map: Latin America
  base_map <- ggplot() +
    # Plot Latin America background
    geom_sf(data = st_transform(latin_america, crs = 4326), 
            fill = "gray85", color = "white", size = 0.2) +
    
    # Plot the metropolitan regions
    geom_sf(data = combined_regions, aes(fill = region_name), lwd = 0, alpha = 0.7) +
    
    # Zoom the map to focus on the regions
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    
    # Custom fill colors and legend title
    scale_fill_manual(
      values = region_colors, 
      name = "Metropolitan Regions") +
    
    # Add map labels and aesthetics
    labs(
      title = "Metropolitan Regions in Latin America",
      x = "Longitude", y = "Latitude", fill = "Regions"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 10),
      axis.text = element_text(color = "black"),
      axis.title = element_text(face = "bold", color = "black")
    )
  
  # Add a scale bar and north arrow
  base_map <- base_map +
    # annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           style = north_arrow_fancy_orienteering())
  
  # Print the map
  print(base_map)
  
  # Return the map
  return(base_map)
}


# Function --------------------------------------------------------------------
# @Arg         : df is a data frame containing columns "Date", "Hour", 
#                "DUSMASS25", "OCSMASS", "BCSMASS", "SSSMASS25", "SO4SMASS" and "pm25_estimate".
# @Arg         : city_name is a string representing the city's name.
# @Output      : A list of five ggplot objects, each representing the distribution 
#                of a selected variable for the given city.
# @Purpose     : This function creates five distribution plots (histograms) for 
#                selected variables from the given data frame. The chosen variables are:
#                - DUSMASS25, OCSMASS, BCSMASS, SSSMASS25, and pm25_estimate.
#                For pm25_estimate, two vertical lines are added to indicate WHO guidelines:
#                Interim Target 1 (IT1): 35 µg/m³
#                Interim Target 2 (IT2): 25 µg/m³
# @Written_on  : 13/12/2024
# @Written_by  : Marcos Paulo
plot_city_distributions <- function(df, city_name) {
  
  # Associate variable names with descriptions
  variable_descriptions <- list(
    DUSMASS25 = "Dust Surface Mass Concentration (PM 2.5) in µg/m³",
    OCSMASS = "Organic Carbon Surface Mass Concentration in µg/m³",
    BCSMASS = "Black Carbon Surface Mass Concentration in µg/m³",
    SSSMASS25 = "Sea Salt Surface Mass Concentration (PM 2.5) in µg/m³",
    SO4SMASS = "SO4 Surface Mass Concentration in µg/m³",
    pm25_estimate = "PM 2.5 (MERRA-2) in µg/m³")
  
  # Variables to plot (5 in total)
  vars_to_plot <- names(variable_descriptions)
  
  # Check if required columns exist
  if (!all(vars_to_plot %in% names(df))) {
    stop("The data frame must contain: 
         DUSMASS25, OCSMASS, BCSMASS, SSSMASS25, and pm25_estimate.")
  }
  
  # Certify all columns have the required class
  df[vars_to_plot] <- sapply(df[, vars_to_plot], as.numeric)
  
  # Initialize a list to store plots
  plot_list <- list()
  
  for (var in vars_to_plot) {
    p <- ggplot(df, aes(x = .data[[var]])) +
      geom_density(fill = "chocolate4", color = "black", alpha = 0.5, linewidth = 0.8) +
      labs(
        title = paste(city_name, "-", variable_descriptions[[var]]),
        x = variable_descriptions[[var]],
        y = "Density") +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_text(color = "black", face = "bold"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5))
    
    # If the variable is pm25_estimate, add vertical lines for IT1 and IT2
    if (var == "pm25_estimate") {
      # Calculate density to find the highest point
      dens <- density(df[[var]], na.rm = TRUE)
      max_y <- max(dens$y) 
      max_x <- dens$x[which.max(dens$y)]
      
      # Add vertical lines and labels for WHO limits
      p <- p +
        geom_segment(x = 25, xend = 25, y = 0, yend = max_y, 
                     color = "orange", linetype = "dashed", linewidth = 0.5) +
        geom_segment(x = 35, xend = 35, y = 0, yend = max_y, 
                     color = "darkred", linetype = "dashed", linewidth = 0.5) +
        annotate("text", x = 26.1, y = (max_y - 0.01), label = "IT2", vjust = -1, 
                 color = "orange", size = 3) +
        annotate("text", x = 36.1, y = (max_y - 0.01), label = "IT1", vjust = -1, 
                 color = "darkred", size = 3)
      # +
      # labs(
        #   subtitle = "WHO Interim Targets: IT1 = 35 µg/m³, IT2 = 25 µg/m³")
    }
    
    plot_list[[var]] <- p
  }
  
  # print(plot_list)
  
  return(plot_list)
}


# Function --------------------------------------------------------------------
# @Arg       : plot_list is a list of ggplot objects for a single city.
# @Arg       : city_name is a string specifying the name of the city.
# @Arg       : output_dir is a string specifying the directory to save the PDFs.
# @Output    : Saves a single PDF for the provided plot list.
# @Purpose   : Save each list of plots into a separate PDF efficiently.
# @Written_on: 13/12/2024
# @Written_by: Marcos Paulo
save_plot_list_to_pdf <- function(plot_list, city_name, output_dir) {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Define the output PDF file path
  output_file <- file.path(output_dir, paste0(city_name, "_aerosols.pdf"))
  
  # Open the PDF device
  pdf(file = output_file, width = 16, height = 9)
  
  # Save each plot in the list to the PDF
  for (plot_name in names(plot_list)) {
    print(plot_list[[plot_name]])
  }
  
  # Close the PDF device
  dev.off()
  
  # Print confirmation
  cat("Saved:", output_file, "\n")
}
