run_arlequin <- function(file, ld = FALSE) {
   
   arlecore_path <- get_arlecore_path()
   
   if (isTRUE(ld)) {
      ars_file <- normalizePath("./arlequin/arl_run_withLD.ars", winslash = "\\", mustWork = TRUE)
   } else {
      ars_file <- normalizePath("./arlequin/arl_run.ars", winslash = "\\", mustWork = TRUE)
   }
   
   workdir <- dirname(file)
   oldwd <- getwd()
   on.exit(setwd(oldwd), add = TRUE)
   
   setwd(workdir)
   
   status <- system2(
      command = arlecore_path,
      args = c(shQuote(basename(file)), shQuote(ars_file))
   )
   
   if (status != 0) {
      stop("Error running Arlecore: ", status)
   }
   
   return(paste0(workdir, "/", tools::file_path_sans_ext(basename(file)), ".res"))
}


parse_pop_labels <- function(doc) {
   nodes <- XML::getNodeSet(doc, "//pairDistPopLabels")
   
   if (length(nodes) == 0) {
      return(NULL)
   }
   
   xmlText <- XML::xmlValue(nodes[[1]])
   lines <- strsplit(xmlText, "\n")[[1]]
   lines <- trimws(lines)
   lines <- lines[grepl("^[0-9]+\\s*:", lines)]
   
   pop_number <- as.integer(sub("^([0-9]+)\\s*:.*$", "\\1", lines))
   pop_names <- sub("^[0-9]+\\s*:\\s", "", lines)
   
   return(data.frame(PopulationName = pop_number,
                    Population = pop_names,
                    stringsAsFactors = FALSE))
   
}


parse_pop_diversity <- function(doc){
   pop_label <- parse_pop_labels(doc)
   
   if (is.null(pop_label)) {
      stop("Could not find <pairDistPopLabels> in XML file.")
   }
   
   group_nodes <- XML::getNodeSet(doc, "//A[contains(@NAME, '_group')]")
   
   results <- list()
   
   for (i in seq_along(group_nodes)) {
      current_node <- group_nodes[[i]]
      group_name <- XML::xmlGetAttr(current_node, "NAME")
      
      group_num <- as.integer(
         sub(".*_group([0-9]+)$", "\\1", group_name)
      )
      
      pop_index <- group_num + 1
      population <- pop_label$Population[pop_index]
      node <- XML::getSibling(current_node, after = TRUE)
      standard_txt <- NULL
      hw_txt <- NULL
      
      while (!is.null(node)) {
         if (XML::xmlName(node) == "A") {
            next_name <- XML::xmlGetAttr(node, "NAME", default = "")
            
            if (grepl("_group[0-9]+$", next_name)) {
               break
            }
         }
         
         if (XML::xmlName(node) == "data") {
            txt <- XML::xmlValue(node)
            
            if (grepl("Standard diversity indices", txt, fixed = TRUE)) {
               standard_txt <- txt
            }
            
            if (grepl("Hardy-Weinberg equilibrium", txt, fixed = TRUE)) {
               hw_txt <- txt
            }
         }
         
         node <- XML::getSibling(node, after = TRUE)
      }
      
      standard_node <- NULL
      node <- XML::getSibling(current_node, after = TRUE)
      found_standard <- FALSE
      
      while (!is.null(node)) {
         if (XML::xmlName(node) == "A") {
            next_name <- XML::xmlGetAttr(node, "NAME", default = "")
            
            if (grepl("_group[0-9]+$", next_name)){
               break
            }
         }
         
         if (XML::xmlName(node) == "data") {
            txt <- XML::xmlValue(node)
            
            if (grepl("Standard diversity indices", txt, fixed = TRUE)) {
               found_standard = TRUE
            }
            
            if (found_standard && grepl("Locus#", txt, fixed = TRUE)) {
               standard_node <- node
               break
            }
         }
         
         node <- XML::getSibling(node, after = TRUE)
      }
      
      hw_node <- NULL
      node <- XML::getSibling(current_node, after = TRUE)
      found_hw <- FALSE
      
      while (!is.null(node)) {
         if (XML::xmlName(node) == "A") {
            next_name <- XML::xmlGetAttr(node, "NAME", default = "")
            
            if (grepl("_group[0-9]+$", next_name)) {
               break
            }
         }
         
         if (XML::xmlName(node) == "data") {
            txt <- XML::xmlValue(node)
            
            if (grepl("Hardy-Weinberg equilibrium", txt, fixed = TRUE)) {
               found_hw <- TRUE
            } 
            
            if (found_hw && grepl("P-value", txt, fixed = TRUE)) {
               hw_node <- node
               break
            }
         }
         
         node <- XML::getSibling(node, after = TRUE)
      }
      
      standard <- NULL
      
      if (!is.null(standard_node)) {
         txt <- XML::xmlValue(standard_node)
         lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
         
         locus_lines <- grep(
            paste0(
            "^[[:space:]]*[0-9]+",
            "[[:space:]]+[0-9]+",
            "[[:space:]]+[0-9]+",
            "[[:space:]]+[-+0-9.]+",
            "[[:space:]]+[-+0-9.]+",
            "[[:space:]]*$"
            ), lines, value = TRUE
         )
         
         if (length(locus_lines) > 0) {
            parsed <- strsplit(trimws(locus_lines), "[[:space:]]+")
            parsed <- do.call(rbind, parsed)
            standard <- data.frame(
               Population = population,
               Locus = as.integer(parsed[, 1]),
               GeneCopies = as.numeric(parsed[, 2]),
               NumAlleles = as.numeric(parsed[, 3]),
               ObsHet = as.numeric(parsed[, 4]),
               ExpHet = as.numeric(parsed[, 5]),
               stringsAsFactors = FALSE
            )
         }
      }
      
      hw <- NULL
      
      if (!is.null(hw_node)) {
         txt <- XML::xmlValue(hw_node)
         lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
         
         hw_lines <- grep(
            paste0(
               "^[[:space:]]*[0-9]+",
               "[[:space:]]+[0-9]+",
               "[[:space:]]+[-+0-9.]+",
               "[[:space:]]+[-+0-9.]+",
               "[[:space:]]+[-+0-9.]+",
               "[[:space:]]+[-+0-9.]+",
               "[[:space:]]+[0-9]+",
               "[[:space:]]*$"
            ), lines, value = TRUE
         )
         
         if (length(hw_lines) > 0){
            parsed <- strsplit(trimws(hw_lines), "[[:space:]]+")
            parsed <- do.call(rbind, parsed)
            
            hw <- data.frame(
               Locus = as.integer(parsed[, 1]),
               HW_Genot = as.numeric(parsed[, 2]),
               HW_ObsHet = as.numeric(parsed[, 3]),
               HW_ExpHet = as.numeric(parsed[, 4]),
               HW_Pvalue = as.numeric(parsed[, 5]),
               HW_SD = as.numeric(parsed[, 6]),
               HW_StepsDone = as.numeric(parsed[, 7]),
               stringsAsFactors = FALSE
            )
         }
      }
      
      if (!is.null(standard) && !is.null(hw)) {
         combined <- merge(standard, hw, by = "Locus", all.x = TRUE, sort = FALSE)
         
         combined$Population <- population
         
         combined <- combined[
            ,
            c("Population", "Locus", "GeneCopies", "NumAlleles", "ObsHet",
              "ExpHet", "HW_Genot", "HW_ObsHet", "HW_ExpHet", "HW_Pvalue", "HW_SD", "HW_StepsDone")
         ]
         
         results[[length(results) + 1]] <- combined
      } else if (!is.null(standard)) {
         results[[length(results) + 1]] <- standard
      } else if (!is.null(hw)) {
         hw$Population <- population
         hw$PopulationNumber <- pop_index
         results[[length(results) + 1]] <- hw
      }
   }
   
   if (length(results) == 0) {
      return(NULL)
   }

   do.call(rbind, results)
   
}

parse_ld <- function(doc) {
   pop_label <- parse_pop_labels(doc)
   
   if (is.null(pop_label)) {
      stop("Could not find <pairDistPopLabels> in XML file.")
   }
   
   group_nodes <- XML::getNodeSet(doc, "//A[contains(@NAME, '_group')]")
   
   results <- list()
   
   for (i in seq_along(group_nodes)) {
      current_node <- group_nodes[[i]]
      group_name <- XML::xmlGetAttr(current_node, "NAME", default = "")
      
      group_num <- as.integer(sub(".*_group([0-9]+)$", "\\1", group_name))
      pop_index <- group_num + 1
      population <- pop_label$Population[pop_index]
      
      node <- XML::getSibling(current_node, after = TRUE)
      ld_node <- NULL
      #found_ld <- FALSE
      
      while (!is.null(node)) {
         if (XML::xmlName(node) == "A") {
            next_name <- XML::xmlGetAttr(node, "NAME", default = "")
            
            if (grepl("_group[0-9]+$", next_name)) {
               break
            }
         }
         
         if (XML::xmlName(node) == "data") {
            txt <- XML::xmlValue(node)
            if (grepl("Table of significant linkage disequilibrium", txt, fixed = TRUE)) {
               ld_node <- node
               break
            }
         }
         
         node <- XML::getSibling(node, after = TRUE)
      }
      
      if (is.null(ld_node)) {
         next
      }
      
      txt <- XML::xmlValue(ld_node)
      lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
      ld_lines <- grep(
         "^[[:space:]]*[0-9]+[[:space:]]*\\|", lines, value = TRUE
      )
      
      ld_results <- list()
      for (line in ld_lines) {
         section <- strsplit(line, "\\|", fixed = FALSE)[[1]]
         
         if (length(section) < 2) { 
            next
         }
         
         locus1 <- as.integer(trimws(section[1]))
         value_string <- paste(section[-1], collapse = "")
         values <- strsplit(trimws(value_string), "[[:space:]]+")[[1]]
         values <- values[nzchar(values)]
         
         locus2 <- seq_along(values) - 1
         
         ld_results[[length(ld_results) + 1]] <- data.frame(
            Population = population,
            Locus1 = locus1,
            Locus2 = locus2,
            LD = values,
            stringsAsFactors = FALSE
         )
      }
      
      if (length(ld_results) > 0) {
         population_ld <- do.call(rbind, ld_results)
         population_ld <- population_ld[
            population_ld$Locus1 != population_ld$Locus2 & 
               population_ld$LD == "+",
            ,
            drop = FALSE
         ]
         
         results[[length(results) +1]] <- population_ld
      }
   }
   
   do.call(rbind, results)
}


plot_heatmap_arlecore <- function(long_data, pop_labels, legend_name = "Value") {
   pop_names <- pop_labels$Population
   
   heatmap_data <- long_data
   colnames(heatmap_data) <- c("Pop1", "Pop2", "Value")
   
   heatmap_data$Pop1 <- pop_names[
      match(heatmap_data$Pop1, LETTERS[seq_along(pop_names)])
   ]
   
   heatmap_data$Pop2 <- pop_names[
      match(heatmap_data$Pop2, LETTERS[seq_along(pop_names)])
   ]
   
   heatmap_data$Value <- as.numeric(heatmap_data$Value)
   
   heatmap_data$Pop1 <- factor(heatmap_data$Pop1, levels = pop_names)
   heatmap_data$Pop2 <- factor(heatmap_data$Pop2, levels = pop_names)
   
   heatmap_data$hover <- paste0("<b>", 
                                heatmap_data$Pop1, 
                                " x ", 
                                heatmap_data$Pop2, 
                                "</b>", "<br>", 
                                legend_name, ": ", 
                                sprintf("%.3f", heatmap_data$Value))
   p <- plotly::plot_ly(data = heatmap_data,
                   x = ~Pop1,
                   y = ~Pop2,
                   z = ~Value,
                   text = ~hover,
                   hoverinfo = "text",
                   type = "heatmap",
                   colorscale = "Viridis",
                   colorbar = list(title = legend_name))
   
   p <- p %>% plotly::add_trace(
      data = heatmap_data,
      x = ~Pop1,
      y = ~Pop2,
      text = ~sprintf("%.3f", Value),
      type = "scatter",
      mode = "text",
      textposition = "middle center",
      hoverinfo = "skip",
      showlegend = FALSE
   )
   p %>% plotly::layout(xaxis = list(title = NULL, side = "bottom"),
                     yaxis = list(title = NULL))
      
}

plot_pairwise_data_prep <- function(pairwise_matrix, pop_labels) {
   pop_names <- pop_labels$Population
   nei <- pairwise_matrix[[1]]$data
   between <- pairwise_matrix[[2]]$data
   within <- pairwise_matrix[[3]]$data
   
   make_long <- function(mat, type) {
      df <- as.data.frame(as.table(as.matrix(mat)))
      colnames(df) <- c("row", "col", "value")
      df$row <- as.numeric(df$row)
      df$col <- as.numeric(df$col)
      
      df <- df[!is.na(df$value), ]
      df$x <- pop_names[df$col]
      df$y <- pop_names[df$row]
      
      df$type <- type
      df
   }
   
   nei_long <- make_long(nei, "Nei's distance")
   between_long <- make_long(between, "Between populations")
   within_long <- make_long(within, "Within populations")
   
   return(list(nei = nei_long,
               between = between_long,
               within = within_long))
}

plot_pairwise_heatmap <- function(data, pop_labels) {
   pop_names <- pop_labels$Population
   heatmap_data <- dplyr::bind_rows(data)
   heatmap_data$x <- factor(heatmap_data$x, levels = pop_names)
   heatmap_data$y <- factor(heatmap_data$y, levels = pop_names)
   
   p <- ggplot(heatmap_data,
          aes(x = x, y = y, fill = value, text = paste0("Population 1: ", x, "<br>Population 2: ", y, "<br>Value: ", sprintf("%.3f", value))
              ))+
      geom_tile(color = "white",
                linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.1f", value)), size = 3) +
      facet_wrap(~type, nrow = 1) +
      scale_fill_viridis_c(name = "Value", na.value = "white") +
      coord_fixed() +
      labs(x = NULL, y = NULL, title = "Ave. Number of Pairwise Differences") +
      theme_minimal(base_size = 11) +
      theme(
         panel.grid = element_blank(),
         axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
         axis.text.y = element_text(),
         legend.position = "right",
         strip.text = element_text(face = "bold")
      )
   
   plotly::ggplotly(p,
                    tooltip = "text") %>%
      plotly::layout(hoverlabel = list(align = ""))
}

plot_pairwise_heatmap_overlap <- function(data, pop_labels) {
   pop_names <- pop_labels$Population
   nei_long <- data[[1]]
   between_long <- data[[2]]
   within_long <- data[[3]]
   
   nei_long$x <- factor(nei_long$x, levels = pop_names)
   nei_long$y <- factor(nei_long$y, levels = pop_names)
   
   between_long$x <- factor(between_long$x, levels = pop_names)
   between_long$y <- factor(between_long$y, levels = pop_names)
   
   within_long$x <- factor(within_long$x, levels = pop_names)
   within_long$y <- factor(within_long$y, levels = pop_names)
   
   ggplot()+
      geom_tile(data = nei_long,
                aes(x = x, y = y, fill = value),
                color = "white",
                linewidth = 0.5) +
      scale_fill_gradient(
         name = "Nei's distance",
         low = "white",
         high = "blue",
         na.value = "white"
      ) +
      ggnewscale::new_scale_fill() +
      geom_tile(data = between_long,
                aes(x = x, y = y, fill = value),
                color = "white",
                linewidth = 0.5) +
      scale_fill_gradient(
         name = "Between populations",
         low = "white",
         high = "green",
         na.value = "white"
      ) +
      ggnewscale::new_scale_fill() +
      geom_tile(data = within_long,
                aes(x = x, y = y, fill = value),
                color = "white",
                linewidth = 0.5) +
      scale_fill_gradient(
         name = "Within populations",
         low = "white",
         high = "orange",
         na.value = "white"
      ) +
      geom_text(data = nei_long,
                aes(x = x, y = y, label = sprintf("%.1f", value)),
                size = 3) +
      geom_text(data = between_long,
                aes(x = x, y = y, label = sprintf("%.1f", value)),
                size = 3) +
      geom_text(data = within_long,
                aes(x = x, y = y, label = sprintf("%.1f", value)),
                size = 3) +
      coord_fixed() +
      labs(x = NULL, y = NULL, title = "Ave. Number of Pairwise Differences (Breakdown)") +
      theme_minimal(base_size = 11) +
      theme(
         panel.grid = element_blank(),
         axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
         axis.text.y = element_text(),
         legend.position = "right"
      )
}

parse_sections_arlequin <- function(doc, tag, fun) {
   nodes <- XML::getNodeSet(doc, paste0("//", tag))
   if (length(nodes) == 0) {
      return(NULL)
   }
   
   results <- lapply(nodes, function(node) {
      xmlText <- XML::xmlValue(node)
      timeAttr <- XML::xmlGetAttr(node, "time")
      fun(xmlText = xmlText, timeAttr =  timeAttr)
   })
   
   unlist(results, recursive = FALSE)
}

parse_arlequin_report <- function(doc) {
   sections <- list()

   #sections <- c(sections,
   #              parse_sections_arlequin(doc, "sumNumAlleles", sumNumAllelesFunction))
   
   sections <- c(sections,
                 parse_sections_arlequin(doc, "sumExpHeterozygosity", sumExpectedHeterozygosity))
   
   #sections <- c(sections,
   #              parse_sections_arlequin(doc, "sumThetaH", sumThetaHFunction))
   
   sections <- c(sections,
                 parse_sections_arlequin(doc, "PairFstMat", pairFstMatrix))
   
   sections <- c(sections,
                 parse_sections_arlequin(doc, "coancestryCoefficients", coancestryCoeff))
   
   sections <- c(sections,
                 parse_sections_arlequin(doc, "pairwiseDifferenceMatrix", pairwiseDiffMatrix))
   
   #sections <- c(sections,
   #              parse_sections_arlequin(doc, "PairFstPvalMat", fStat_Pvalues_Func))
   
   population_diversity <- parse_pop_diversity(doc)
   
   if (!is.null(population_diversity)){
      sections <- c(sections,
                    list(
                       list(
                          type = "population_diversity",
                          title = "Population Diversity and HWE",
                          time = Sys.time(),
                          data = population_diversity
                       )
                    ))
   }
   
   sections <- Filter(Negate(is.null), sections)
   list(sections = sections)
   
}
