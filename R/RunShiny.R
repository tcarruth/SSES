#' Run a Shiny Application
#'
#' \code{Shiny} runs one of the Shiny Applications that are included in the package
#'
#' @param app The name of the Shiny application to run.
#' @references Modified from Deal Attali's code: \url{http://deanattali.com/2015/04/21/r-package-shiny-app/}
#' @importFrom utils install.packages installed.packages
#' @export
Shiny <- function(app) {
  temp <- try(class(app), silent=TRUE)

  if (class(temp) == "try-error") app <- deparse(substitute(app))
  Apps <- list.files(system.file("shiny_apps", package = "ABTMSE"))
  validAppMsg <- paste0("Valid examples are:\n '", paste(Apps, collapse = "', '"), "'")
  if (missing(app) || !nzchar(app) || !app %in% Apps) {
    stop(
      'Please run `Shiny()` with a valid Shiny app',
      validAppMsg,
      call. = FALSE)
  }
  appDir <- system.file("shiny_apps", app, package = "ABTMSE")
  shiny::runApp(appDir, display.mode = "normal",launch.browser = TRUE)
}
