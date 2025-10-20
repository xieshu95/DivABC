args <- commandArgs(TRUE)

id <- as.numeric(args[1])
save_output <- TRUE
metadata <- paste0("This is id", id)

library(DivABC)

create_ref_table(
  save_output = save_output,
  id = id
)