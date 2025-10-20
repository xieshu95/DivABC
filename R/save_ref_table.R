
save_ref_table <- function(ref_df,
                           id){

  output_file_name <- create_ref_name(
    id = id
  )

  output_folder <- file.path(
    getwd(),
    "results",
    "ref_tables"
  )
  output_file_path <- file.path(output_folder, output_file_name)

  testit::assert(is.character(output_file_name))
  message(
    paste0("Trying to save ", output_file_name, " to ", output_file_path, "\n")
  )
  save(ref_df, file = output_file_path)

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


create_ref_name <- function(id) {

  output_file_name <- paste0(
    "ref_df_",
    id,
    ".RData"
  )

  return(output_file_name)
}

