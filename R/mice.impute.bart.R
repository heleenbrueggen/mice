#' A short description of your function
#' 
#' @aliases mice.impute.bart
#' @inheritParams mice.impute.pmm
#' @param \dots Other named arguments passed down to
#' \code{dbarts::bart()}.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @details 
#' @note 
#' @author
#' @references 
#' @family univariate imputation functions
#' @keywords datagen
#' @examples
#' \dontrun{
#' imp <- mice(nhanes, meth = "bart", ntree = 200)
#' plot(imp)
#' }
#' @export

mice.impute.bart <- function(y, ry, x, wy = NULL, use.matcher = FALSE, donors = 5L, ...) {
    install.on.demand("dbarts", ...)
    if (is.null(wy)) {
        wy <- !ry
    }

    # Parameter estimates
    fit <- dbarts::bart(x, y, keeptrees = TRUE, verbose = FALSE)

    yhatobs <- fitted(fit, type = "ev", sample = "train")[ry]
    yhatmis <- fitted(fit, type = "ev", sample = "train")[wy]

    # Find donors
    if (use.matcher) {
        idx <- matcher(yhatobs, yhatmis, k = donors)
    } else {
        idx <- matchindex(yhatobs, yhatmis, donors)
    }

    return(y[ry][idx])
}