#' A short description of your function
#' 
#' @aliases mice.impute.2l.rbart
#' @inheritParams mice.impute.pmm
#' @param type Vector of length \code{ncol(x)} identifying random and class
#' variables.  Random variables are identified by a '2'. The class variable
#' (only one is allowed) is coded as '-2'. Fixed effects are indicated by
#' a '1'.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}.
#' @details 
#' @note 
#' @author 
#' @references 
#' @family
#' @keywords 
#' @export
#' @examples

mice.impute.2l.rbart <- function(y, ry, x, wy = NULL, type, use.matcher = FALSE, donors = 5L, ...) {
    install.on.demand("dbarts", ...)
    if (is.null(wy)) {
        wy <- !ry
    }

    clust <- names(type[type == -2])
    effects <- names(type[type != -2])
    X <- x[, effects, drop = FALSE]

    model <- paste0(
        "y ~ ", paste0(colnames(X), collapse = " + ")
    )

    fit <- dbarts::rbart_vi(formula = formula(model), group.by = clust, data = data.frame(y, x), verbose = FALSE, n.threads = 1, n.samples = 500L, n.burn = 500L, ...)

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