args <- commandArgs(TRUE)

id <- as.numeric(args[1])
save_output <- TRUE
metadata <- paste0("This is id", id)

library(DivABC)

create_ref_table_musse(
  save_output = save_output,
  id = as.numeric(args[1])
)