style_JSON <- function(notebook, styleit = TRUE) {
    for (i in seq_along(notebook$cells)) {
        cell <- notebook$cells[[i]]

        if (cell$cell_type == "code") {
            execution_count <- cell$execution_count
            if (is.null(execution_count)) {
                execution_count <- NULL  # Explicitly set to NULL to write as JSON null
            }

            src <- cell$source
            src_oneline <- paste0(src, collapse = "")

            styled_code <- tryCatch({
                if (styleit) {
                  st <- styler::style_text(src_oneline)
                  st_string <- paste0(sapply(st, toString), collapse = "\n")
                } else {
                  st_string <- src_oneline
                }
                st_string
            }, error = function(e) {
                message("Error styling code: ", e$message)
            })

            notebook$cells[[i]]$source <- list(styled_code)

            # Restore the execution_count field
            notebook$cells[[i]]$execution_count <- execution_count
        }
    }
    return(notebook)
}
