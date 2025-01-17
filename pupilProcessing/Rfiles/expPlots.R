data <- read_excel("Data/filteredData.xlsx", sheet = "Analysis (2)")
create_plots <- function(df, period) {
  # Filter the data for the selected period
  filtered_data <- df[df$Period == period, ]
  
  # Ensure there is data to plot
  if (nrow(filtered_data) > 0) {
    # Split the data into subsets
    dot1 <- filtered_data[1, ] # First dot
    dot2 <- if (nrow(filtered_data) > 1) filtered_data[2, ] else NULL # Second dot
    combined <- filtered_data # Both dots
    
    # Determine y-axis limits based on the return of all data points
    y_min <- min(combined$return) * 100 - 20
    y_max <- max(combined$return) * 100 + 20
    
    # Calculate shared breaks for 'Desviación ASG'
    desv_breaks <- round(seq(min(combined$desvSP), max(combined$desvSP), length.out = 2), 2)
    
    # Function to generate individual plots
    generate_plot <- function(data, title) {
      ggplot(data, aes(x = normSP, y = return * 100, label = Ticker)) +
        geom_point(aes(size = desvSP)) +
        geom_text(vjust = -1.5, hjust = 0.5, size = 3) +
        scale_size(
          name = "Desviación\nASG",
          labels = function(x) round(x, 2) # Ensure legend labels are rounded to 2 decimals
        ) +
        scale_x_continuous("Métrica ASG Normalizada", limits = c(0, 1)) +
        scale_y_continuous("Retorno (%)", limits = c(y_min, y_max)) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0.5),
          plot.caption = element_text(size = 10, hjust = 0)
        ) +
        ggtitle(title)
    }
    
    # Create individual plots
    plot1 <- generate_plot(dot1, "")
    plot2 <- if (!is.null(dot2)) generate_plot(dot2, "") else NULL
    plot3 <- generate_plot(combined, "")
    
    # Return the plots as a list
    return(list(plot1, plot2, plot3))
  } else {
    return(NULL)
  }
}


create_plots_comb <- function(df, period) {
  # Filter the data for the selected period
  filtered_data <- df[df$Period == period, ]
  
  # Ensure there is data to plot
  if (nrow(filtered_data) > 0) {
    # Split the data into subsets
    dot1 <- filtered_data[1, ] # First dot
    dot2 <- if (nrow(filtered_data) > 1) filtered_data[2, ] else NULL # Second dot
    combined <- filtered_data # Both dots
    
    # Determine y-axis limits based on the return of all data points
    y_min <- min(combined$return) * 100 - 20
    y_max <- max(combined$return) * 100 + 20
    
    # Calculate shared breaks for 'Desviación ASG'
    desv_breaks <- round(seq(min(combined$ESGPond), max(combined$desvPond), length.out = 2), 2)
    
    # Function to generate individual plots
    generate_plot <- function(data, title) {
      ggplot(data, aes(x = ESGPond, y = return * 100, label = Ticker)) +
        geom_point(aes(size = desvPond)) +
        geom_text(vjust = -1.5, hjust = 0.5, size = 3) +
        scale_size(
          name = "Desviación\nASG",
          labels = function(x) round(x, 2) # Ensure legend labels are rounded to 2 decimals
        ) +
        scale_x_continuous("Métrica ASG Normalizada", limits = c(0, 1)) +
        scale_y_continuous("Retorno (%)", limits = c(y_min, y_max)) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0.5),
          plot.caption = element_text(size = 10, hjust = 0)
        ) +
        ggtitle(title)
    }
    
    # Create individual plots
    plot1 <- generate_plot(dot1, "")
    plot2 <- if (!is.null(dot2)) generate_plot(dot2, "") else NULL
    plot3 <- generate_plot(combined, "")
    
    # Return the plots as a list
    return(list(plot1, plot2, plot3))
  } else {
    return(NULL)
  }
}

create_plot_combined <- function(df, period) {
  # Filter the data for the selected period
  filtered_data <- df[df$Period == period, ]
  
  # Ensure there is data to plot
  if (nrow(filtered_data) > 0) {
    # Split the data into subsets
    dot1 <- filtered_data[1, ] # First dot
    dot2 <- if (nrow(filtered_data) > 1) filtered_data[2, ] else NULL # Second dot
    combined <- filtered_data # Both dots
    
    # Determine y-axis limits based on the return of all data points
    y_min <- min(combined$return) * 100 - 20
    y_max <- max(combined$return) * 100 + 20
    
    # Calculate shared breaks for 'Desviación ASG'
    desv_breaks <- round(seq(min(combined$desvPond), max(combined$desvPond), length.out = 2), 2)
    
    # Function to generate individual plots
    generate_plot <- function(data, title) {
      ggplot(data, aes(x = ESGPond, y = return * 100, label = Ticker)) +
        geom_point(aes(size = desvPond, colour = desvReturn)) +
        scale_colour_gradient(low = "darkred", high = "red", name = "Desviación\nRetorno") +
        scale_size(
          name = "Desviación\nASG",
          breaks = desv_breaks, # Use shared breaks
          labels = function(x) round(x, 2) # Ensure legend labels are rounded to 2 decimals
        ) +
        scale_x_continuous("Promedio Métricas Bloomberg y S&P", limits = c(0, 1)) +
        scale_y_continuous("Retorno (%)", limits = c(y_min, y_max)) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0.5),
          plot.caption = element_text(size = 10, hjust = 0)
        ) +
        ggtitle(title)
    }
    
    # Create individual plots
    plot1 <- generate_plot(dot1, "Graph with First Dot Only")
    plot2 <- if (!is.null(dot2)) generate_plot(dot2, "Graph with Second Dot Only") else NULL
    plot3 <- generate_plot(combined, "Graph with Both Dots")
    
    # Return the plots as a list
    return(list(plot1, plot2, plot3))
  } else {
    return(NULL)
  }
}


periods <- unique(data$Period)
plots <- lapply(periods, function(period) create_plots(data, period))
names(plots) <- periods
plotsCombined <- lapply(periods, function(period) create_plots_comb(data, period))
names(plotsCombined) <- periods





