save_output <- function(output,
                        scenario,
                        param_set,
                        ss_set){

  output_file_name <- create_output_file_name(
    scenario = scenario,
    param_set = param_set,
    ss_set = ss_set
  )

  output_folder <- file.path(
    getwd(),
    "results",
    scenario
  )
  output_file_path <- file.path(output_folder, output_file_name)

  testit::assert(is.character(output_file_name))
  message(
    paste0("Trying to save ", output_file_name, " to ", output_file_path, "\n")
  )
  save(output, file = output_file_path)

  if (file.exists(output_file_path)) {
    message(paste0("Saved ", output_file_name, " to ", output_file_path, "\n"))
  } else {
    warning(paste0(
      "File not found. Saving to ",
      output_file_path,
      " likely failed.\n")
    )
  }
}


create_output_file_name <- function(scenario,
                                    param_set,
                                    ss_set) {

  output_file_name <- paste0(
    scenario,
    "_param_set_",
    param_set,
    "_ss_",
    ss_set,
    ".RData"
  )


  return(output_file_name)
}


check_create_folders <- function(scenario,
                                 save_output) {

  if (!save_output) {
    message("Returning results to object, no I/O used.\n")
    return()
  }

  output_folder <- file.path(
    getwd(),
    "results",
    scenario
  )

  if (!dir.exists(output_folder)) {
    message(paste0(
      output_folder,
      " folder not found, attempting to create it.\n")
    )
    dir.create(output_folder, recursive = TRUE)
    message("Created folder successfully.")
  } else if (!dir.exists(output_folder)) {
    stop(paste0(
      "Tried creating: ",
      output_folder,
      "Folder still not found, aborting"
    ))
  } else {
    message(output_folder, " folder found. No creation needed.\n")
  }
}
