run_arlequin <- function(file, ld = FALSE) {
   
   arlecore_path <- get_arlecore_path()
   
   if (isTRUE(ld)) {
      ars_file <- normalizePath("./arlequin/arl_run_withLD.ars", winslash = "\\", mustWork = TRUE)
   } else {
      ars_file <- normalizePath("./arlequin/arl_run.ars", winslash = "\\", mustWork = TRUE)
   }
   
   print(ars_file)
   print(file.exists(ars_file))
   
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

parse_sections_arlequin <- function(doc, tag, fun) {
   nodes <- XML::getNodeSet(doc, paste0("//", tag))
   if (length(nodes) == 0) {
      return(NULL)
   }
   
   lapply(nodes, function(node) {
      xmlText <- XML::xmlValue(node)
      timeAttr <- XML::xmlGetAttr(node, "time")
      fun(xmlText = xmlText, timeAttr =  timeAttr)
   })
}

parse_arlequin_report <- function(doc) {
   sections <- list()

   sections <- c(sections,
                 parse_sections_arlequin(doc, "sumNumAlleles", sumNumAllelesFunction))
   
   sections <- c(sections,
                 parse_sections_arlequin(doc, "sumExpectedHeterozygosity", sumExpectedHeterozygosity))
   
   sections <- c(sections,
                 parse_sections_arlequin(doc, "sumThetaH", sumThetaHFunction))
   
   sections <- c(sections,
                 parse_sections_arlequin(doc, "pairFstMatrix", pairFstMatrix))
   
   sections <- c(sections,
                 parse_sections_arlequin(doc, "coancestryCoeff", coancestryCoeff))
   
   sections <- c(sections,
                 parse_sections_arlequin(doc, "pairwiseDiff", pairwiseDiffMatrix))
   
   sections <- c(sections,
                 parse_sections_arlequin(doc, "fStat_Pvalues", fStat_Pvalues_Func))
   
   sections <- Filter(Negate(is.null), sections)
   list(sections = sections)
   
}