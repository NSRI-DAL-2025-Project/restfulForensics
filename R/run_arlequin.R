run_arlequin <- function(file, ld = FALSE) {
   
   arlecore_path <- get_arlecore_path()
   
   if (isTRUE(ld)) {
      ars_file <- "arlequin/arl_run_withLD.ars"
   } else {
      ars_file <- "arlequin/arl_run.ars"
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
   
   return(paste0(workdir, tools::file_path_sans_ext(basename(file)), ".res"))
}

extract_arlecore_results <- function(dir) {
   
   # load the directory and check for the xml document
   # fixed naman na arp_file ang name ng directory
   data_files <- list.files(path = dir,
                            pattern = "arp_file.xml",
                            full.names = TRUE)
   
   
   
   
}