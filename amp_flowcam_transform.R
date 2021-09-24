#' AMP flowcam data transformation
#'
#' @param x Flowcam summary data file (.xlsx)
#' @param output_dir Where to save output files
#'
#' @return
#' @export
#'
#' @examples

amp_flowcam_transform <- function(x, output_dir = "./") {
  filename <- sub("\\...*$", "", basename(x))
  # red in data from xlsx file
  flowcam_file <-
    suppressMessages(readxl::read_excel(x, col_names = FALSE,))
  
  # find end of metadata stats
  end_stats <-
    grep(pattern = "======== End Metadata Statistics ========", x = flowcam_file$...1)
  
  # find start of particle properties
  start_particle_property <-
    grep(pattern = "======== Particle Property Statistics ========", x = flowcam_file$...1)
  
  # get column names for stats metadata
  flowcam_stats_col_names <- unlist(flowcam_file[3, 1:8])
  flowcam_stats <- flowcam_file[5:end_stats - 1, 1:8]
  # set column names
  colnames(flowcam_stats) <- flowcam_stats_col_names
  flowcam_stats[flowcam_stats == "--"] <- NA
  write.csv(
    flowcam_stats,
    row.names = FALSE,
    file = paste0(output_dir, filename, "_stats.csv"),
    na = ""
  )
  
  flowcam_particle_col_names <-
    unlist(flowcam_file[start_particle_property + 2, ])
  flowcam_particle_properties <-
    flowcam_file[start_particle_property + 3:nrow(flowcam_file), ]
  colnames(flowcam_particle_properties) <-
    flowcam_particle_col_names
  
  write.csv(
    flowcam_particle_properties,
    row.names = FALSE,
    file = paste0(output_dir, filename, "_particleProperties.csv")
  )
}