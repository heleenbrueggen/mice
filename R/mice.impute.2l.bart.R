#' A short description of your function
#' 
#' @aliases mice.impute.2l.bart
#' @inheritParams mice.impute.pmm
#' @param type Vector of length \code{ncol(x)} identifying random and class
#' variables.  Random variables are identified by a '2'. The class variable
#' (only one is allowed) is coded as '-2'. Fixed effects are indicated by
#' a '1'.
#' @param intercept Logical determining whether the intercept is automatically
#' added.
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

mice.impute.2l.bart <- function(y, ry, x, wy = NULL, type, intercept = TRUE, use.matcher = FALSE, donors = 5L, ...) {
    install.on.demand("stan4bart", ...)
    if (is.null(wy)) {
        wy <- !ry
    }

    if (intercept) {
        x <- cbind(1, as.matrix(x))
        type <- c(2, type)
        names(type)[1] <- colnames(x)[1] <- "(Intercept)"
    }

    clust <- names(type[type == -2])
    rande <- names(type[type == 2])
    fixe <- names(type[type > 0])

    lev <- unique(x[, clust])

    X <- x[, fixe, drop = FALSE]
    Z <- x[, rande, drop = FALSE]
    xobs <- x[ry, , drop = FALSE]
    yobs <- y[ry]
    Xobs <- X[ry, , drop = FALSE]
    Zobs <- Z[ry, , drop = FALSE]

    # create formula
    fr <- ifelse(length(rande) > 1,
        paste("+ ( 1 +", paste(rande[-1L], collapse = "+")),
        "+ ( 1 "
    )
    randmodel <- paste(
        "yobs ~ bart(", paste(fixe[-1L], collapse = "+"), ")",
        fr, "|", clust, ")"
    )

    suppressWarnings(fit <- try(
    stan4bart::stan4bart(formula(randmodel),
      data = data.frame(yobs, xobs),
      verbose = -1,
      ...
    ),
    silent = TRUE
  ))
  if (inherits(fit, "try-error")) {
    warning("stan4bart does not run. Simplify imputation model")
    return(y[wy])
  }

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