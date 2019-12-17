#' @title Summarizing mlms result
#' @description summary method for class "mlms"
#' @param x an object of class "mlms", usually, a result of a call to \code{
#' link{fit_mlms}}
#' @return
#' The function summary.mlms returns a list of result organized from a given
#' mlms object.
#' \describe{
#'   \item{bivars}{The p-value matrix for bivariate correlation between any
#'   variable from \code{X} and \code{Z}.}
#'   \item{covars}{The p-value matrix for covariates in the complete model.}
#'   \item{models}{Combinded model coefficients and statistics for both
#'   unadjusted and adjusted for each x variable}
#' }
#' @importFrom magrittr `%>%`
#' @import dplyr
#' @export
#' @examples
#' data(growth)
#' X = growth[,1:3]
#' design = model.matrix(~treatment, data = growth)
#' Z = growth[,5:10]
#' fit = fit_mlms(X, design, Z, coef = "treatmentLNS")
#' res = summary(fit)
summary.mlms = function(x, ...){
    bivars = x$bivars
    covars = x$covars
    unadjusted = cbind(x$unadjusted.model, x$confint[,1:2], df = x$df.residual[,1]) %>%
        as.data.frame()
    unadjusted$var = rownames(unadjusted)
    unadjusted$model = "unadjusted"
    unadjusted = unadjusted[,c(8,9,1,2,5,6,7,3,4)]
    colnames(unadjusted) = gsub("unadjusted", "", colnames(unadjusted))
    adjusted = cbind(x$adjusted.model, x$confint[,3:4], df = x$df.residual[,2]) %>%
        as.data.frame()
    adjusted$var = rownames(adjusted)
    adjusted$model = "adjusted"
    adjusted = adjusted[,c(8,9,1,2,5,6,7,3,4)]
    colnames(adjusted) = gsub("adjusted", "", colnames(adjusted))
    models = rbind(unadjusted, adjusted) %>%
        mutate(
            model = factor(model) %>% relevel(ref = "unadjusted"),
            var = factor(var, levels = unique(var))
        ) %>%
        arrange(var, model)
    structure(
        list(
            bivars = bivars,
            covars = covars,
            models = models
        ),
        class = "summary.mlms"
    )
}

#' @export
export = function(x, ...) UseMethod("export", x)

#' @title export summary mlms to excel
#' @description export summary mlms to excel files
#' @param x summary.mlms returned by \code{\link{summary.mlms}}
#' @param file character. The file path to export
#' @export
#' @examples
#' data(growth)
#' X = growth[,1:3]
#' design = model.matrix(~treatment, data = growth)
#' Z = growth[,5:10]
#' fit = fit_mlms(X, design, Z, coef = "treatmentLNS")
#' res = summary(fit)
#' export(res, "growth.xlsx")
export.summary.mlms = function(x, file){
    if(!requireNamespace("openxlsx")){
        stop("Please install openxlsx using:\ninstall.packages(\"openxlsx\")")
    }
    wb = openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "MainEffects")
    openxlsx::writeData(wb, "MainEffects", x$models)

    openxlsx::addWorksheet(wb, "ModelSelection")
    bivar = as.data.frame(x$bivars)
    bivar$pvalgroup = ""
    bivar$pvalgroup[1] = "Bivariate"
    bivar$var = rownames(bivar)
    n = ncol(bivar)
    bivar = bivar[,c(n-1, n, seq_len(n-2))]
    covar = as.data.frame(x$covars)
    covar$pvalgroup = ""
    covar$pvalgroup[1] = "Covariate"
    covar$var = rownames(covar)
    n = ncol(covar)
    covar = covar[,c(n-1, n, seq_len(n-2))]
    model_select = rbind(bivar, covar)
    openxlsx::writeData(wb, "ModelSelection", model_select)

    openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}
