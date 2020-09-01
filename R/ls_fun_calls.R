# List functions in R call
#
# @param x an R call or list of R calls
#
# @return Character, all functions in an R call
#
# @examples
# ls_fun_calls(quote(lm(mpg ~ cyl, mtcars)))
ls_fun_calls <- function(x) {
  if (is.function(x)) {
    c(ls_fun_calls(formals(x)), ls_fun_calls(body(x)))
  }
  else if (is.call(x)) {
    pkg <- NA
    fname <- as.character(x[[1]])
    ex <- eval(x[[1]])
    if (is.primitive(ex)){
      pkg <- "base"
    } else {
      ee <- environment(ex)
      if (isBaseNamespace(ee)){
        # Not allowed to getNamespaceInfo from base
        pkg <- "base"
      } else {
        pkg <- paste0(getNamespaceInfo(ee, "spec")[["name"]])
      }
    }
    ch <- unlist(lapply(x[-1], ls_fun_calls), use.names = TRUE, recursive = FALSE)
    r <- list(c(pkg = pkg, fun = fname))
    if (is.null(ch)){
      r
    } else {
      append(r, ch)
    }
  }
}
