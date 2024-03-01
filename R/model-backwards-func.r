# Automated backwards deletion procedure
# ------------------------------------------------------------------------------
model_backwards.func <- function(model, scope, remove_p = p_val_b, outfile){
  sink(outfile)
  testCovs <- scope
  removeLevel = remove_p
  tempFit <- model
  step <- 0
  continue <- TRUE
  sumdf <- data.frame()
  # Stepwise backwards deletion with Chisq test on deviance
  while(continue==TRUE){
    step <- step + 1
    # Find least significant covariate
    if(length(testCovs)>0){
      x <- drop1(tempFit, scope = testCovs, test = "Chisq")
      cat(paste0("\n---  Backwards Selection Step: ", step, "  ---"))
      x.relabel <- x
      x.relabel.row.names <- sapply(X = rownames(x = x.relabel), FUN = function(.){
        if(length(x = grep(pattern = paste(names(x = c(full.con.t.es, exposureCov)), collapse = "|"), x = .)) > 0){
          repl.text <- convert_express_latex.func(
            text_to_convert = as.character(
              unname(obj = c(full.con.t.es))[
                names(x = c(full.con.t.es)) ==
                  grep(
                    pattern = paste(names(x = c(full.con.t.es)), collapse = "|"),
                    x = grep(
                      pattern = paste(names(x = c(full.con.t.es)), collapse = "|"),
                      x = ., value = T
                    ), # grep
                    value = T
                  ) # grep
              ] # unname(obj = c(full.con.t.es))
            ) # as.character
          ) # convert_express_latex.func
          repl.text <- c(repl.text,
                         as.character(
                           unname(obj = c(exposureCov))[
                             names(x = c(exposureCov)) ==
                               grep(
                                 pattern = paste(names(x = c(exposureCov)), collapse = "|"),
                                 x = grep(
                                   pattern = paste(names(x = c(exposureCov)), collapse = "|"),
                                   x = ., value = T
                                 ), # grep
                                 value = T
                               ) # grep
                           ] # unname(obj = c(exposureCov))
                         ) # as.character
          ) # c
          repl.text <- gsub(pattern = "~", replacement = " ", x = repl.text)
          repl.text <- gsub(pattern = "\\[|\\]", replacement = "", x = repl.text)
          . <- gsub(pattern = paste(names(x = c(full.con.t.es, exposureCov)), collapse = "|"), replacement = repl.text, x = .)
        } else{ # if
          . <- .
        } # else
      } # function(.)
      ) # sapply
      rownames(x = x) <- x.relabel.row.names
      x$`Pr(>Chi)` <- formatC(x = x$`Pr(>Chi)`, digits = 4, format = "f")
      max_p <- max(x$`Pr(>Chi)`[2:nrow(x)])
      x$`Pr(>Chi)`[x$`Pr(>Chi)` == "0.0000"] <- "<0.0001"
      x[c("Deviance", "AIC", "LRT")] <- formatC(x = unlist(x = x[c("Deviance", "AIC", "LRT")]), digits = 2, format = "f")
      print(knitr::kable(x))
      # Determine if p-value greater than the pre-chosen p-level;
      # then remove corresponding term.
      if(max_p > removeLevel){
        removeVar <- row.names(x[which(x$`Pr(>Chi)` == max_p),])
        x.add <- x[which(x$`Pr(>Chi)` == max_p),]
        cat(sprintf("\n---  Removing %1s. P Value = %1.6s  ---\n", removeVar, max_p))
        # Add deletion per step in summary table
        sumdf <- rbind(sumdf, cbind(x.add, Step = step))
        newMod <- paste(".~.-", names(x.relabel.row.names)[unname(obj = x.relabel.row.names) == removeVar])
        tempFit <- update(tempFit, newMod)
        testCovs <- testCovs[which(testCovs != names(x.relabel.row.names)[unname(obj = x.relabel.row.names) == removeVar])]
      }else{
        cat(paste0("\n---  No Effects with P Value >", removeLevel, "  ---\n"))
        continue = FALSE
      }
    }else
      continue = FALSE
  }
  cat(paste0("\n---  Summary of Backward Elimination  ---"))
  print(knitr::kable(sumdf))
  sink()
  return(tempFit)
} # logistic_backwards.func
