#' value_box_theme
#'
#' this function exists in bslib. but somehow, it doesn't works.
#' @import bslib
#' @export
value_box_theme <- function(name = NULL, bg = NULL, fg = NULL) {
  if (is.null(name)) {
    if (is.null(bg)) {
      name <- "default"
    } else {
      # don't warn if we can't get a contrast color, `bg` might be valid
      # CSS but not something sass can compute on
      fg <- fg %||% suppressWarnings(get_color_contrast(bg))
    }

    return(new_value_box_theme(name, bg, fg))
  }

  if (!rlang::is_string(name)) {
    rlang::abort('`theme` must be a single value, e.g. "primary", "danger", "purple", etc.')
  }

  if (!grepl("^(text|bg)-", name)) {
    name <- paste0("bg-", name)
  }

  new_value_box_theme(name, bg, fg)
}

#' new_value_box_theme
#'
#' this function exists in bslib. but somehow, it doesn't works.
#' @import bslib
#' @export

new_value_box_theme <- function(class = NULL, bg = NULL, fg = NULL) {
  structure(
    list(
      class = class,
      fg = fg,
      bg = bg
    ),
    class = "bslib_value_box_theme"
  )
}
