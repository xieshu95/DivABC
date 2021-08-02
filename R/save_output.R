save_output <- function(output,
                        param_space_name,
                        param_set,
                        rep){

  output_file_name <- create_output_file_name(
    param_space_name = param_space_name,
    param_set = param_set,
    rep = rep
  )

  output_folder <- file.path(
    getwd(),
    "results",
    param_space_name
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


create_output_file_name <- function(param_space_name,
                                    param_set,
                                    rep) {

  output_file_name <- paste0(
    param_space_name,
    "_param_set_",
    param_set,
    "_rep",
    rep,
    ".RData"
  )


  return(output_file_name)
}


check_create_folders <- function(param_space_name,
                                 save_output) {

  if (!save_output) {
    message("Returning results to object, no I/O used.\n")
    return()
  }

  output_folder <- file.path(
    getwd(),
    "results",
    param_space_name
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
