utils::globalVariables(c("errorMessage"))
.datatable.aware <- TRUE
# Use GMT, never BST
Sys.setenv(TZ = "GMT")

"%!in%" <- Negate("%in%")
