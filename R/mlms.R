#' @title fit mixed linear model selection
#' @description This function fits a linear model regression between any
#' variable in \code{data.frame} X and the design matrix, with the
#' \code{data.frame} Z as covariates.
#' @details Each potential covariate in Z is frist tested whether it is
#' correlated with any variable in X. The result of this step is a \code{matrix}
#' of p values in the slot \code{bivar}. For any variable in X, only the
#' covariants in Z with a p-value < 0.1 is used to adjust the variance in the
#' complete model. Next, the covariants are tested whether they are significant
#' in the complete model. The result of this step is in the slot \code{covar}.
#' Finally, the complete model is fit to test the effect of the target
#' coefficient. The result is in the slot \code{fits}
#' @param X data.frame
#' @param design matrix. Output from \code{\link{model.matrix}}. Number of rows
#' must equal to that of X.
#' @param Z data.frame. Number of rows must equal to that of X.
#' @param coef character. The target coefficient to test.
#' @param alternative character. The length must be either 1 or the same as the
#' number of columns in X.
#' @examples
#' data(growth)
#' X = growth[,1:3]
#' design = model.matrix(~treatment, data = growth)
#' Z = growth[,5:10]
#' fit_mlms(X, design, Z, coef = "treatmentLNS")
#' @export
fit_mlms = function(X, design, Z, coef, alternative="two.sided"){
    stopifnot(nrow(X) == nrow(design))
    stopifnot(nrow(X) == nrow(Z))
    stopifnot(is.character(coef))
    stopifnot(coef %in% colnames(design))

    if(length(alternative) == 1){
        if(!alternative %in% c("two.sided", "greater", "less")){
            stop("alternative must be either \"two.sided\", \"less\", or \"greater\"")
        }
        alternative = rep(alternative, ncol(X))
    } else {
        if(length(alternative) != ncol(X)){
            stop("The length of alternative must be either 1 or the same as column number in X")
        }
        if(any(!alternative %in% c("two.sided", "greater", "less"))){
            stop("alternative must be either \"two.sided\", \"less\", or \"greater\"")
        }
    }

    if(any(!is.numeric(Z))) {
        Z = make_dummy_variables(Z)
        cat("Categorical variables are converted to dummy variables\n")
    }

    bivars = bivar(X, Z)
    covars = covar(X, design, Z, bivars)
    fit1 = .fit_unadjusted(X, design, coef, alternative)
    fit2 = .fit_select_model(X, design, Z, bivars, coef, alternative)
    df.residual = cbind(unadjusted = fit1$df.residual, adjusted = fit2$df.residual)
    confint = cbind(fit1$confint, fit2$confint)
    colnames(confint) = paste (c(rep("unadjusted", 2), rep("adjusted", 2)),
                               colnames(confint))

    structure(
        list(
            bivars           = bivars,
            covars           = covars,
            unadjusted.model = fit1$coefficients,
            adjusted.model   = fit2$coefficients,
            df.residual      = df.residual,
            confint          = confint
        ),
        class = "mlms"
    )
}

#' @title Calculate bivariate correlation
#' @description Calculate the p-values of Pearson's correlation between any
#' combination of variables from data.frame X and Y
#' @param X data.frame
#' @param Y data.frame
bivar = function(X, Y){
    lapply(X, function(x){
        sapply(Y, function(y){
            cor.test(x, y)$p.value
        })
    }) %>%
        do.call(rbind, .)
}

#' @title Calculate covariants
#' @description Calculate the p-values of the variable Z as a covariate in the
#' complete model. Only variables that are significantly correlated with the x
#' variabel in X is tested.
#' @param X data.frame
#' @param design matrix
#' @param Z data.frame
#' @param bivars matrix returned by \code{\link{bivar}}
#' @importFrom magrittr `%>%`
#' @keywords internal
covar = function(X, design, Z, bivars) {
    lapply(colnames(X), function(x_var){
        x = X[, x_var]
        d = cbind(design, Z[, bivars[x_var,] < 0.1, drop = FALSE]) %>%
            as.matrix()
        fit = lm(x ~ d + 0) %>% summary %>% coef
        rownames(fit) = gsub("^d", "", rownames(fit))
        res = sapply(colnames(bivars), function(z){
            if(z %in% rownames(fit)){
                return(fit[z, "Pr(>|t|)"])
            }
            return(NA)
        })
    }) %>%
        do.call(rbind, .) %>%
        `rownames<-`(colnames(X))
}

#' @importFrom magrittr `%>%`
#' @keywords internal
.fit_select_model = function(X, design, Z, bivars, coef, alternative){
    res = lapply(seq_len(ncol(X)), function(i){
        x_var = colnames(X)[i]
        x = X[, x_var]
        d = cbind(design, Z[, bivars[x_var,] < 0.1, drop = FALSE]) %>%
            as.matrix()
        mod = lm(x ~ d + 0)
        ci = confint(mod)[paste0("d", coef),]
        names(ci) = paste("CI", names(ci))
        df.residual = mod$df.residual
        coefficients = summary(mod)$coefficients[paste0("d", coef),]
        if(alternative[i] != "two.sided"){
            lower_tail = alternative[i] == "less"
            coefficients[4] = pt(coefficients[3], df.residual, lower.tail = lower_tail)
        }
        return(list(
            "coefficients" = coefficients,
            "confint"      = ci,
            "df.residual"  = df.residual
        ))
    })
    coefficients = lapply(res, function(x) x$coefficients) %>%
        do.call(rbind, .) %>% `rownames<-`(colnames(X))
    df.residual = sapply(res, function(x) x$df.residual) %>%
        `names<-`(colnames(X))
    confint = lapply(res, function(x) x$confint) %>%
        do.call(rbind, .) %>% `rownames<-`(colnames(X))
    return(list(
        "coefficients" = coefficients,
        "confint"      = confint,
        "df.residual"  = df.residual
    ))
}

#' @keywords internal
#' @importFrom magrittr `%>%`
.fit_unadjusted = function(X, design, coef, alternative) {
    res = lapply(seq_len(ncol(X)), function(i){
        x_var = colnames(X)[i]
        x = X[, x_var]
        mod = lm(x ~ design + 0)
        ci = confint(mod)[paste0("design", coef),]
        names(ci) = paste("CI", names(ci))
        df.residual = mod$df.residual
        coefficients = summary(mod)$coefficients[paste0("design", coef),]
        if(alternative[i] != "two.sided"){
            lower_tail = alternative[i] == "less"
            coefficients[4] = pt(coefficients[3], df.residual, lower.tail = lower_tail)
        }
        return(list(
            "coefficients" = coefficients,
            "confint"      = ci,
            "df.residual"  = df.residual
        ))
    })
    coefficients = lapply(res, function(x) x$coefficients) %>%
        do.call(rbind, .) %>% `rownames<-`(colnames(X))
    df.residual = sapply(res, function(x) x$df.residual) %>%
        `names<-`(colnames(X))
    confint = lapply(res, function(x) x$confint) %>%
        do.call(rbind, .) %>% `rownames<-`(colnames(X))
    return(list(
        "coefficients" = coefficients,
        "confint"      = confint,
        "df.residual"  = df.residual
    ))
}


#' @title make dummy variables
#' @description convert any categorical variables into dummy variables in a data
#' frame
#' @param x data.frame
#' @return data.frame
#' @export
#' @examples
#' df = make_dummy_variables(iris)
make_dummy_variables = function(x){
    out = NULL
    for(var in colnames(x)){
        col = x[[var]]
        if(!is.numeric(col)){
            col = as.factor(col)
            if(length(levels(col)) == 2){
                col = as.numeric(col) - 1
                out = cbind(out, col)
                colnames(out)[ncol(out)] = var
            } else {
                dummys = sapply(levels(col)[-1], function(l){
                    ifelse(col == l, 1, 0)
                })
                colnames(dummys) = paste0(var, ":", levels(col)[-1])
                out = as.data.frame(cbind(out, dummys))
            }
        } else {
            out = cbind(out, col)
            colnames(out)[ncol(out)] = var
        }
    }
    return(as.data.frame(out))
}
