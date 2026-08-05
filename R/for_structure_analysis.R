# ======================================
# R function to export a genind object in STRUCTURE format
#
# Tom Jenkins t.l.jenkins@exeter.ac.uk
#
# July 2018
#
# ======================================

# data: genind object
# file: file name to write
# pops: whether to include population info
# markers: whether to include marker info
# unix: export as a unix text file (windows text file default)
# Function is flexible with regards to ploidy, although genotypes are
# considered to be unambiguous.
# Missing data must be recorded as NA.
# SNP data must be biallelic with all alleles present.

# Example use:
# library(adegenet)
# ind = as.character(paste("ind_", seq(1:100), sep=""))
# pop = as.character(c(rep("pop1",25), rep("pop2",25), rep("pop3",25), rep("pop4",25)))
# loci = list(c("AA","AC","CC"), c("GG","GC","CC"), c("TT","TA","AA"), c("CC","CT","TT"))
# loci = sample(loci, 100, replace=T)
# loci = lapply(loci, sample, size=100, replace=TRUE)
# geno = as.data.frame(loci, col.names= .genlab("loc",100))
# data = df2genind(geno, ploidy=2, ind.names=ind, pop=pop, sep="")
# genind2structure(data, file="example_structure.str")

# Convert Windows text file to Unix text file in Linux
# awk '{ sub("\r$", ""); print }' winfile.txt > unixfile.txt

#' Genind to STRUCTURE
#'
#' @param input The input as a dataframe.
#' 
#' @returns A dataframe containing information from the input file.
#'
#' @importFrom tidyselect everything
#'
#' @keywords internal
genind2structure2 <- function(data, file = "", pops = TRUE, markers = TRUE, unix = FALSE) {
   ## Check input file a genind object
   if (!"genind" %in% class(data)) {
      warning("Function was designed for genind objects.")
   }
   
   ## Check adegenet, miscTools and stringr are installed
   if (!require(adegenet)) {
      install.packages("adegenet")
   }
   if (!require(miscTools)) {
      install.packages("miscTools")
   }
   if (!require(stringr)) {
      install.packages("stringr")
   }
   
   
   # ---------------- #
   #
   # Preamble
   #
   # ---------------- #
   
   ## Ploidy
   ploid <- max(data$ploidy)
   
   ## Number of individuals
   ind <- nInd(data)
   
   ## Create dataframe containing individual labels
   ## Number of duplicated labels depends on the ploidy of the dataset
   df <- data.frame(ind = rep(indNames(data), each = ploid))
   
   ## Locus names
   loci <- locNames(data)
   
   
   # ---------------- #
   #
   # Population IDs
   #
   # ---------------- #
   
   if (pops) {
      ## Create dataframe containing individual labels
      ## Number of duplicated labels depends on the ploidy of the dataset
      df.pop <- data.frame(pop = rep(as.numeric(data$pop), each = ploid))
      
      ## Add population IDs to dataframe
      df$Pop <- df.pop$pop
   }
   
   # ---------------- #
   #
   # Process genotypes
   #
   # ---------------- #
   
   ## Add columns for genotypes
   df <- cbind(df, matrix(-9, # -9 codes for missing data
                          nrow = dim(df)[1],
                          ncol = nLoc(data),
                          dimnames = list(NULL, loci)
   ))
   
   ## Loop through dataset to extract genotypes
   for (L in loci) {
      thesedata <- data$tab[, grep(paste("^", L, "\\.", sep = ""), dimnames(data$tab)[[2]])] # dataotypes by locus
      al <- 1:dim(thesedata)[2] # numbered alleles
      for (s in 1:ind) {
         if (all(!is.na(thesedata[s, ]))) {
            tabrows <- (1:dim(df)[1])[df[[1]] == indNames(data)[s]] # index of rows in output to write to
            tabrows <- tabrows[1:sum(thesedata[s, ])] # subset if this is lower ploidy than max ploidy
            df[tabrows, L] <- rep(al, times = thesedata[s, ])
         }
      }
   }
   
   
   # ---------------- #
   #
   # Marker IDs
   #
   # ---------------- #
   
   if (markers) {
      ## Add a row at the top containing loci names
      df <- as.data.frame(insertRow(as.matrix(df), 1, c(loci, "", "")))
   }
   
   # ---------------- #
   #
   # Export file
   #
   # ---------------- #
   
   ## Export dataframe
   write.table(df,
               file = file, sep = "\t", quote = FALSE,
               row.names = FALSE, col.names = FALSE
   )
   
   ## Export dataframe as unix text file
   if (unix) {
      output_file <- file(file, open = "wb")
      write.table(df,
                  file = output_file, sep = "\t", quote = FALSE,
                  row.names = FALSE, col.names = FALSE
      )
      close(output_file)
   }
}

#====================== Adapted from the dartR package
#' Convert genind object to STRUCTURE-compatible format
#'
#' @keywords internal
to_structure <- function(genind_obj,
                         include_pop = TRUE) {
   # out_path <- file.path(dir, file)
   # Get basic info
   ind <- adegenet::indNames(genind_obj)
   pop <- if (include_pop) as.character(genind_obj@pop) else rep(1, length(ind))
   ploidy <- max(genind_obj@ploidy)
   loci <- adegenet::locNames(genind_obj)
   n_loc <- length(loci)
   
   # Convert allele matrix to integer format
   allele_matrix <- matrix(-9, nrow = length(ind), ncol = n_loc * ploidy)
   colnames(allele_matrix) <- paste(rep(loci, each = ploidy), paste0(".a", 1:ploidy), sep = "")
   
   for (i in seq_along(ind)) {
      for (j in seq_along(loci)) {
         allele_values <- genind_obj@tab[i, grep(paste0("^", loci[j], "\\."), colnames(genind_obj@tab))]
         allele_values[is.na(allele_values)] <- -9
         allele_matrix[i, ((j - 1) * ploidy + 1):(j * ploidy)] <- allele_values
      }
   }
   
   final_data <- data.frame(ID = ind, POP = pop, allele_matrix, stringsAsFactors = FALSE)
   return(final_data)
   
   # write.table(final_data, file = out_path, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)
   # return(out_path)
}

#' Parse matrix files
#' Sections commented out were revised.
#'
#' @keywords internal
.structureParseQmat <- function(q.mat.txt,
                                pops) {
   q.mat.txt <- sub("[*]+", "", q.mat.txt)
   q.mat.txt <- sub("[(]", "", q.mat.txt)
   q.mat.txt <- sub("[)]", "", q.mat.txt)
   q.mat.txt <- sub("[|][ ]+$", "", q.mat.txt)
   cols1to4 <- c("row", "id", "pct.miss", "orig.pop")
   
   strsplit(q.mat.txt, " ") %>%
      purrr::map(function(q) {
         q <- q[!q %in% c("", " ", ":")] %>%
            as.character() %>%
            rbind() %>%
            as.data.frame(stringsAsFactors = FALSE)
         
         stats::setNames(
            q,
            c(
               cols1to4,
               paste("Group", 1:(ncol(q) - 4), sep = ".")
            )
         )
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate_at(dplyr::vars(
         "row",
         "pct.miss",
         "orig.pop",
         dplyr::starts_with("Group.")
      ), as.numeric) %>%
      dplyr::mutate(orig.pop = if (!is.null(pops)) {
         pops[.data$orig.pop]
      } else {
         .data$orig.pop
      })
}


#' Read STRUCTURE file
#' Sections commented out were revised.
#'
#' @keywords internal
structureRead <- function(file,
                          pops = NULL) {
   if (!file.exists(file)) {
      stop(error(paste("the file '", file, "' can't be found.",
                       sep = ""
      )))
   }
   
   result <- scan(file, "character", quiet = TRUE)
   loc <- grep("Estimated", result,
               ignore.case = FALSE,
               value = FALSE
   )
   est.ln.prob <- as.numeric(result[loc[1] + 6])
   loc <- grep("likelihood", result,
               ignore.case = FALSE,
               value = FALSE
   )
   mean.lnL <- as.numeric(result[loc[1] + 2])
   var.lnL <- as.numeric(result[loc[2] + 2])
   loc <- grep("MAXPOPS", result, value = F)
   maxpops <- result[loc]
   maxpops <- sub("MAXPOPS=", "", maxpops)
   maxpops <- as.integer(sub(",", "", maxpops))
   loc <- grep("GENSBACK", result, value = F)
   gensback <- result[loc]
   gensback <- sub("GENSBACK=", "", gensback)
   gensback <- as.integer(sub(",", "", gensback))
   smry <- c(
      k = maxpops, est.ln.prob = est.ln.prob, mean.lnL = mean.lnL,
      var.lnL = var.lnL
   )
   result <- scan(file, "character",
                  sep = "\n",
                  quiet = TRUE
   )
   first <- grep("(%Miss)", result, value = FALSE) + 1
   last <- grep("Estimated Allele", result, value = FALSE) -
      1
   tbl.txt <- result[first:last]
   tbl.txt <- sub("[*]+", "", tbl.txt)
   tbl.txt <- sub("[(]", "", tbl.txt)
   tbl.txt <- sub("[)]", "", tbl.txt)
   tbl.txt <- sub("[|][ ]+$", "", tbl.txt)
   prior.lines <- grep("[|]", tbl.txt)
   
   no.prior <- if (length(prior.lines) < length(tbl.txt)) {
      no.prior.q.txt <- if (length(prior.lines) == 0) {
         tbl.txt
      } else {
         tbl.txt[-prior.lines]
      }
      .structureParseQmat(no.prior.q.txt, pops)
   } else {
      NULL
   }
   
   # Initial section's error: no function to return from, jumping to top level
   # Resolved by wrapping in a function 'maxpops_check'
   maxpops_check <- function(maxpops, smry, no.prior) {
      if (maxpops == 1) {
         no.prior$row <- NULL
         return(list(summary = smry, q.mat = no.prior, prior.anc = NULL))
      }
   }
   
   has.prior <- if (length(prior.lines) > 0) {
      prior.txt <- strsplit(tbl.txt[prior.lines], "[|]")
      prior.q.txt <- unlist(lapply(prior.txt, function(x) x[1]))
      df <- .structureParseQmat(prior.q.txt, pops)
      
      prior.anc <- purrr::map(prior.txt, function(x) {
         anc.mat <- matrix(NA, nrow = maxpops, ncol = gensback + 1)
         rownames(anc.mat) <- paste("Pop", 1:nrow(anc.mat), sep = ".")
         colnames(anc.mat) <- paste("Gen", 0:gensback, sep = ".")
         
         x <- sapply(strsplit(x[-1], "\\s|[:]"), function(y) {
            y <- y[y != ""]
            y[-1]
         })
         
         for (i in 1:ncol(x)) {
            pop <- as.numeric(x[1, i])
            anc.mat[pop, ] <- as.numeric(x[-1, i])
         }
         anc.mat
      }) %>% stats::setNames(df$id)
      prob.mat <- t(sapply(1:nrow(df), function(i) {
         pop.probs <- rowSums(prior.anc[[i]])
         pop.probs[is.na(pop.probs)] <- df$Group.1[i]
         pop.probs
      }))
      colnames(prob.mat) <- paste("Group", 1:ncol(prob.mat),
                                  sep = "."
      )
      df$Group.1 <- NULL
      df <- cbind(df, prob.mat)
      list(df = df, prior.anc = prior.anc)
   } else {
      NULL
   }
   
   has.prior.df <- if (is.null(has.prior)) {
      NULL
   } else {
      has.prior$df
   }
   
   q.mat <- rbind(no.prior, has.prior.df)
   q.mat <- q.mat[order(q.mat$id), ] # removed q.mat$row since row is nonexistent
   # q.mat$row <- NULL
   rownames(q.mat) <- NULL
   
   # Initial single-line code did not work, revised as ff::
   subset <- q.mat[, -(1:3)]
   subset <- as.data.frame(lapply(subset, as.numeric))
   normalized <- subset / rowSums(subset)
   q.mat <- cbind(q.mat[, 1:3], normalized)
   
   # q.mat[, -c(1:3)] <- t(apply(q.mat[, -c(1:3)], 1, function(i) i/sum(i)))
   
   prior.anc <- if (is.null(has.prior)) {
      NULL
   } else {
      has.prior$prior.anc
   }
   
   list(summary = smry, q.mat = q.mat, prior.anc = prior.anc)
}


#' Parse matrix files
#' Sections commented out were revised.
#'
#' @keywords internal
utils.structure.evanno <- function(sr, plot = TRUE) {
   if (!"structure.result" %in% class(sr)) {
      stop(error("'sr' is not a result from 'structure.run'."))
   }
   k.tbl <- table(sapply(sr, function(x) x$summary["k"]))
   
   
   # f (length(k.tbl) < 3)
   # stop(error("must have at least two values of k."))
   sr.smry <- t(sapply(sr, function(x) x$summary))
   ln.k <- tapply(sr.smry[, "est.ln.prob"], sr.smry[, "k"], mean)
   sd.ln.k <- tapply(sr.smry[, "est.ln.prob"], sr.smry[, "k"], sd)
   
   ln.pk <- diff(ln.k)
   ln.ppk <- abs(diff(ln.pk))
   delta.k <- sapply(2:(length(ln.k) - 1), function(i) {
      abs(ln.k[i + 1] - (2 * ln.k[i]) + ln.k[i - 1]) / sd.ln.k[i]
   })
   
   # df <- data.frame(k = as.numeric(names(ln.k)),
   #                 reps = as.numeric(table(sr.smry[, "k"])),
   #                 mean.ln.k = as.numeric(ln.k),
   #                 sd.ln.k = as.numeric(sd.ln.k),
   #                 ln.pk = c(NA, ln.pk),
   #                 ln.ppk = c(NA, ln.ppk, NA),
   #                 delta.k = c(NA, delta.k, NA))
   
   # Expected number of K values
   n.k <- length(ln.k)
   
   # Safe padding function
   pad <- function(x, len) {
      x <- as.numeric(x)
      if (length(x) < len) {
         return(c(x, rep(NA, len - length(x))))
      }
      return(x[1:len])
   }
   
   df <- data.frame(
      k = as.numeric(names(ln.k)),
      reps = as.numeric(table(sr.smry[, "k"])),
      mean.ln.k = pad(ln.k, n.k),
      sd.ln.k = pad(sd.ln.k, n.k),
      ln.pk = pad(ln.pk, n.k),
      ln.ppk = pad(ln.ppk, n.k),
      delta.k = pad(delta.k, n.k)
   )
   
   df$k <- as.numeric(as.character(df$k))
   
   rownames(df) <- NULL
   df$sd.min <- df$mean.ln.k - df$sd.ln.k
   df$sd.max <- df$mean.ln.k + df$sd.ln.k
   plot.list <- list(
      mean.ln.k =
         ggplot2::ggplot(
            df,
            ggplot2::aes(
               x = k,
               y = mean.ln.k
            )
         ) +
         ggplot2::ylab("mean LnP(K)") +
         ggplot2::geom_segment(ggplot2::aes(
            x = k,
            xend = k,
            y = sd.min,
            yend = sd.max
         )),
      ln.pk = ggplot2::ggplot(
         df[!is.na(df$ln.pk), ],
         ggplot2::aes(
            x = k,
            y = ln.pk
         )
      ) +
         ggplot2::ylab("LnP'(K)"),
      ln.ppk = ggplot2::ggplot(
         df[!is.na(df$ln.ppk), ],
         ggplot2::aes(
            x = k,
            y = ln.ppk
         )
      ) +
         ggplot2::ylab("LnP''(K)")
   )
   
   if (!all(is.na(df$delta.k))) {
      plot.list$delta.k <- ggplot2::ggplot(df[!is.na(df$delta.k), ], ggplot2::aes(x = k, y = delta.k)) +
         ggplot2::ylab(expression(Delta(K)))
   }
   for (i in 1:length(plot.list)) {
      plot.list[[i]] <- plot.list[[i]] + ggplot2::geom_line() +
         ggplot2::geom_point(
            fill = "white", shape = 21,
            size = 3
         ) + ggplot2::xlim(c(1, max(df$k))) +
         ggplot2::theme(axis.title.x = ggplot2::element_blank())
   }
   if (plot) {
      p <- plot.list %>% purrr::map(function(x) {
         ggplot2::ggplot_gtable(ggplot2::ggplot_build(x))
      })
      maxWidth <- do.call(grid::unit.pmax, purrr::map(p, function(x) x$widths[2:3]))
      for (i in 1:length(p)) p[[i]]$widths[2:3] <- maxWidth
      p$bottom <- "K"
      p$ncol <- 2
      do.call(gridExtra::grid.arrange, p)
   }
   df$sd.min <- df$sd.max <- NULL
   invisible(list(df = df, plots = plot.list))
}


#' Run STRUCTURE analysis
#'
#' @keywords internal
running_structure <- function(input_file,
                              k.range,
                              num.k.rep,
                              burnin,
                              numreps,
                              noadmix = FALSE,
                              phased = FALSE,
                              ploidy = NULL,
                              linkage = FALSE,
                              alpha_value = NULL,
                              structure_path = structure_path,
                              output_dir = dir) {
   # Validate K range
   if (length(k.range) < 2) stop("Provide at least two K values for Evanno analysis.")
   if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
   
   numinds <- length(readLines(input_file))
   numloci <- (ncol(read.table(input_file, header = FALSE, sep = " ")) - 2) / 2
   
   base_label <- tools::file_path_sans_ext(basename(input_file))
   
   rep.df <- expand.grid(rep = 1:num.k.rep, k = k.range)
   rep.df$run <- paste0(base_label, ".k", rep.df$k, ".r", rep.df$rep)
   
   input_file <- normalizePath(input_file)
   structure_path <- normalizePath(structure_path)
   
   
   out_files <- lapply(1:nrow(rep.df), function(run_label) {
      out_path <- file.path(output_dir, rep.df[run_label, "run"])
      
      mainparams <- file.path(output_dir, "mainparams")
      extraparams <- file.path(output_dir, "extraparams")
      
      if (is.null(ploidy)) {
         ploidy <- "2"
      } else {
         ploidy <- ploidy
      }
      
      adm_burnin <- if (burnin < 500) burnin else 500
      
      for_mainparam <- paste("#define", c(
         paste("MAXPOPS", rep.df[run_label, "k"]),
         # paste("ADMBURNIN", adm_burnin),
         paste("BURNIN", burnin),
         paste("NUMREPS", numreps),
         paste("INFILE", input_file),
         paste("NUMINDS", numinds),
         paste("NUMLOCI", numloci),
         paste("PLOIDY", ploidy),
         "MISSING -9",
         "ONEROWPERIND 1",
         "LABEL 1",
         "POPDATA 1",
         "POPFLAG 0",
         "LOCDATA 0",
         "PHENOTYPE 0",
         "EXTRACOLS 0",
         "MARKERNAMES 0",
         "RECESSIVEALLELES 0",
         "MAPDISTANCES 0",
         paste("PHASED", ifelse(phased, 1, 0)),
         "MARKOVPHASE 0",
         "NOTAMBIGUOUS -999"
      ))
      writeLines(for_mainparam, con = mainparams)
      
      # alpha = 1/max(k.range)
      
      for_extraparam <- paste("#define", c(
         paste("NOADMIX", ifelse(noadmix, 1, 0)),
         paste("LINKAGE", ifelse(linkage, 1, 0)),
         # paste("ADMBURNIN", adm_burnin),
         paste("ADMBURNIN", max(0, as.integer(burnin / 2))),
         "USEPOPINFO 0",
         "FREQSCORR 1",
         "ONEFST 0",
         "INFERALPHA 1",
         "POPALPHAS 0",
         paste("ALPHA", alpha_value),
         "INFERLAMBDA 0",
         "POPSPECIFICLAMBDA 0",
         "LAMBDA 1.0",
         "FPRIORMEAN 0.01",
         "FPRIORSD 0.05",
         "UNIFPRIORALPHA 1",
         "ALPHAMAX 10.0",
         "ALPHAPRIORA 1.0",
         "ALPHAPRIORB 2.0",
         "LOG10RMIN -4.0",
         "LOG10RMAX 1.0",
         # "LOG10RSTAT -2.0",
         "GENSBACK 2",
         "MIGRPRIOR 0.01",
         "PFROMPOPFLAGONLY 0",
         "LOCISPOP 1",
         "LOCPRIORINIT 1.0",
         "MAXLOCPRIOR 20.0",
         "PRINTNET 1",
         "PRINTLAMBDA 1",
         "PRINTQSUM 1",
         "SITEBYSITE 0",
         "PRINTQHAT 0",
         "UPDATEFREQ 100",
         "PRINTLIKES 0",
         "INTERMEDSAVE 0",
         "ECHODATA 1",
         "ANCESTDIST 0",
         "NUMBOXES 1000",
         "ANCESTPINT 0.90",
         "COMPUTEPROB 1",
         "ALPHAPROPSD 0.025",
         "STARTATPOPINFO 0",
         "RANDOMIZE 0",
         "SEED 2245",
         "METROFREQ 10",
         "REPORTHITRATE 0"
      ))
      writeLines(for_extraparam, con = extraparams)
      
      message("mainparams written to: ", mainparams, " | Exists: ", file.exists(mainparams))
      message("extraparams written to: ", extraparams, " | Exists: ", file.exists(extraparams))
      message("Using mainparams: ", normalizePath(mainparams))
      message("Using extraparams: ", normalizePath(extraparams))
      
      
      # added 15 August 2025
      cmd_args <- c(
         "-i",
         input_file,
         "-K", rep.df[run_label, "k"],
         "-m", normalizePath(mainparams),
         "-e", normalizePath(extraparams),
         "-o", out_path
      )
      
      # message("Running STRUCTURE: ", cmd)
      message("Running STRUCTURE with args: ", paste(cmd_args, collapse = " "))
      log <- tryCatch(system2(structure_path, args = cmd_args, stdout = TRUE, stderr = TRUE),
                      error = function(e) e$message
      )
      
      writeLines(log, paste0(out_path, "_log.txt"))
      
      final_out <- paste0(out_path, "_f")
      if (!file.exists(final_out) && file.exists(out_path)) final_out <- out_path
      final_out <- if (file.exists(paste0(out_path, "_f"))) paste0(out_path, "_f") else out_path
      if (!file.exists(final_out)) {
         warning("Missing output file for run: ", run_label)
         return(NULL)
      }
      
      result <- tryCatch(structureRead(final_out), error = function(e) {
         warning("Failed to parse STRUCTURE output: ", run_label)
         return(NULL)
      })
      result$label <- run_label
      result
   })
   
   run.result <- Filter(Negate(is.null), out_files)
   message("Successful STRUCTURE runs: ", length(run.result))
   names(run.result) <- sapply(run.result, `[[`, "label")
   class(run.result) <- c("structure.result", class(run.result))
   
   if (length(run.result) < 3) stop("Evanno analysis needs at least three successful runs.")
   
   ev <- utils.structure.evanno(run.result, plot = FALSE)
   
   #
   lapply(names(ev$plots), function(pname) {
      plot_obj <- ev$plots[[pname]]
      png_path <- file.path(output_dir, paste0("Evanno_", pname, ".png"))
      grDevices::png(filename = png_path, width = 1600, height = 1200, res = 200)
      print(plot_obj)
      grDevices::dev.off()
      message("Saved PNG plot: ", png_path)
   })
   
   # if (plot.out && "delta.k" %in% names(ev$plots)) {
   suppressMessages(print(ev$plots$delta.k))
   
   
   return(list(
      results = run.result,
      evanno = ev,
      plot.paths = list.files(output_dir, full.names = TRUE)
   ))
}


#' Calculate Q matrices from STRUCTURE v2.3.4 results
#' 
#' @param dir The directory containing the STRUCTURE results.
#'
#' @returns directory containing the q matrices files.
#' 
#' @export
#' @examples
#' generate_ind_files("./structure_res")
generate_ind_files <- function(dir) {
   output <- list.files(path = dir, pattern = "\\_f$", full.names = TRUE)
   
   output_list <- lapply(output, function(filepath) {
      lines <- readLines(filepath)
      
      start <- grep("Inferred ancestry of individuals", lines) + 2
      end <- grep("^Estimated Allele Frequencies in each cluster", lines)[1] - 1
      
      qmatrices <- lines[start:end]
      
      qmatrices_data <- do.call(rbind, lapply(qmatrices, function(line) {
         return(paste0(strsplit(trimws(line), split = "\\s+")[[1]], sep = " "))
      }))
      return(as.data.frame(qmatrices_data))
   })
   
   names(output_list) <- basename(output)
   return(output_list)
}


# for using CLUMPP
# first combine the files with the same K
combine_ind_files <- function(dir, k.value = 2) {
   ind_result <- tempfile(pattern = paste0("combined_", k.value, "_"), fileext = ".ind")
   
   f_files <- list.files(dir, full.names = TRUE)
   k_pattern <- paste0("k", k.value)
   k_files <- f_files[grepl(k_pattern, f_files)]
   
   if (length(k_files) == 0) {
      stop("No .ind files given K.")
   }
   
   runs <- lapply(k_files, read.table, header = FALSE)
   combined_runs <- dplyr::bind_rows(runs)
   print(dim(combined_runs))
   print(head(combined_runs))
   
   write.table(combined_runs,
               file = ind_result,
               quote = FALSE,
               row.names = FALSE,
               col.names = FALSE)
   
   R <- length(k_files)
   C <- nrow(runs[[1]])
   pops <- levels(factor(combined_runs[[4]]))
   ids <- levels(factor(combined_runs[[2]]))
   
   return(list(file = ind_result,
               R_value = R,
               C_value = C,
               pops = pops,
               ids = ids))
}

structureExportQmat <- function(sr, output.dir = ".", prefix = "run") {
   if (!inherits(sr, "structure.result")) {
      stop("'sr' must be a structure.result object.")
   }
   
   out.files <- character(length(sr))
   
   for (i in seq_along(sr)) {
      q.mat <- sr[[i]]$q.mat
      
      fname <- if (!is.null(sr[[i]]$label)) {
         paste0(sr[[i]]$label, ".indfile")
      } else {
         paste0(prefix, "_", i, ".indfile")
      }
      
      outfile <- file.path(output.dir, fname)
      
      utils::write.table(
         q.mat,
         file = outfile,
         sep = "\t",
         quote = FALSE,
         row.names = FALSE,
         col.names = FALSE
      )
      
      out.files[i] <- outfile
      
   }
   invisible(out.files)
}


structureImport <- function(files, pops = NULL) {
   if (length(files) == 0) {
      stop("No STRUCTURE files found.")
   }
   
   if (is.null(pops)) {
      pops <- structure_get_populations(files[1])
   }
   
    sr <- lapply(files, function(f) {
       result <- strataG::structureRead(f, pops = pops)
       
       result$files <- f
       result$label <- basename(f)
       
       result
    })
    
    names(sr) <- vapply(sr, function(x) x$label, character(1))
    sr <- sr[order(
       as.numeric(sub(".*k([0-9]+).*", "\\1", names(sr))),
       as.numeric(sub(".*r([0-9]+).*", "\\1", names(sr)))
    )]
    class(sr) <- c("structure.result", "list")

    return(sr)
}

structure_get_populations <- function(file){
   txt <- readLines(file)
   
   first <- grep("(%Miss)", txt, fixed = TRUE) + 1
   last <- grep("Estimated Allele", txt, fixed = TRUE) - 1
   
   if (length(first) == 0 || length(last) == 0) {
      stop("Cannot find STRUCTURE Q matrix in _f files.")
   }
   
   q.txt <- txt[first:last]
   
   # same as structureRead()
   q.txt <- sub("[\\*]+", "", q.txt)
   q.txt <- sub("[(]", "", q.txt)
   q.txt <- sub("[)]", "", q.txt)
   q.txt <- sub("[|][ ]+$", "", q.txt)
   
   pops <- vapply(strsplit(q.txt, "\\s+"), function(x){
      x <- x[x != ""]
      x[4]
   }, character(1))
   
   return(unique(pops))
}