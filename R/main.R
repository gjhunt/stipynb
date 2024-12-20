#' Style the notebook using the `styler` package.
#' @param notebook_file the notebook file name
#' @param overwrite overwrite existing ipynb file (default: FALSE)
#' @param verbose print information about styling (default: TRUE)
#' @param save save the styled file (default: TRUE)
#' @export
style_nb <- function(notebook_file, overwrite = FALSE, verbose = TRUE, save = TRUE) {

    nb <- tools::file_path_sans_ext(notebook_file)

    notebook_path <- glue::glue("{nb}.ipynb")

    if (overwrite) {
        new_notebook_path <- notebook_path
    } else {
        new_notebook_path <- glue::glue("{nb}_styled.ipynb")
    }

    if (!file.exists(notebook_path))
        stop(glue::glue("{notebook_path} does not exist."))

    original_notebook <- jsonlite::fromJSON(notebook_path, simplifyVector = FALSE)

    styled_nb <- style_JSON(original_notebook, styleit = TRUE)
    unstyled_nb <- style_JSON(original_notebook, styleit = FALSE)

    original_hash <- digest::digest(unstyled_nb)
    styled_hash <- digest::digest(styled_nb)

    if (verbose) {
        cat(paste0("Original file hash: ", original_hash, "\n"))
        cat(paste0("Styled file hash: ", styled_hash, "\n"))
    }

    if ((original_hash != styled_hash) & save) {
        # Write the styled notebook back to the original file
        jsonlite::write_json(styled_nb, new_notebook_path, auto_unbox = TRUE, pretty = TRUE,
            null = "null")
        if (verbose)
            cat("Notebook updated and saved to", new_notebook_path, "\n")
    } else {
        if (verbose)
            cat("No changes were made to the notebook.\n")
    }

    return(list(original_hash = original_hash, styled_hash = styled_hash, json = styled_nb))
}
