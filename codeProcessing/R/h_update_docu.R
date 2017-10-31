## Function that compares the existing documentation with the optimal, and
## reorders / replaces as necessary.

# Function to update existing documentation -------------------------------

#' [Helper] Update documentation as necessary
#'
#' This function calls for the standard form of the documentation (given the
#'   current documentation), and then compares it to the existing documentation.
#'   If it's different (or if the existing documentation is missing), it'll be
#'   updated in fxdoc$MCB.
#'
#' This function makes no changes to disk.
#'
#' @param fxdoc [\code{\link{FunctionDocu}}] :: Current state of function
#'   documentation
#'
#' @return [\code{\link{FunctionDocu}}] :: Updated function documentation
#'
#' @export
#'
update_stored_docu = function(fxdoc) {

  files_changed = rep(FALSE, max(fxdoc$fxtable$fileID))

  for (ID in seq_along(fxdoc$fxinfo)) {
    fileID = fxdoc$fxtable$fileID[ID]
    todo = paste(
      "## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (",
      fxdoc$fxinfo[[ID]]$fxname,")", sep = "")

    ID_fxs = setdiff(which(fxdoc$fxtable$fileID == fileID), ID)
    ## Find all functions listed in this same file, ignoring current function
    line_before_fx = fxdoc$fxtable$fx_start[ID] - 1

    ## Update documentation, and update fx_df.
    ## Depending on the case -- fx_df needs to have doc_start, doc_end,
    ## fx_start, status updated.
    ## Also, depending on the case, fxdoc$MCB has the code updated (replacing
    ## docu_cur with docu_new in fxdoc$fxinfo)
    diff_nrow_0 = function(x,y) {
      return(ifelse(is.null(x), 0, nrow(x)) - ifelse(is.null(y), 0, nrow(y)))
    }
    lines_added = diff_nrow_0(fxdoc$fxinfo[[ID]]$docu_new,
                              fxdoc$fxinfo[[ID]]$docu_cur)

    if (is.null(fxdoc$fxinfo[[ID]]$docu_cur)) {
      # If no current documentation
      if (is.null(fxdoc$fxinfo[[ID]]$docu_new)) {
        # Want no documentation -> do nothing
        fxdoc$fxtable$status[ID] = "no_change:no_docu"

      } else { # Add in new documentation
        fxdoc$fxtable$status[ID] = "add_docu"; files_changed[fileID] = TRUE
        fxdoc$MCB$code[[fileID]] = insert_codelines(
          fxdoc$MCB$code[[fileID]],
          c(todo, fxdoc$fxinfo[[ID]]$docu_new$Value),
          fxdoc$fxtable$fx_start[ID])
        fxdoc$fxtable$doc_start[ID] = fxdoc$fxtable$fx_start[ID] + 1
        fxdoc$fxtable$doc_end[ID] = fxdoc$fxtable$fx_start[ID] - 1
        lines_added = lines_added + 1

      }
    } else { # Else, current documentation exists
      if (is.null(fxdoc$fxinfo[[ID]]$docu_new)) { # Want to remove documentation
        fxdoc$fxtable$status[ID] = "remove_docu"; files_changed[fileID] = TRUE
        fxdoc$MCB$code[[fileID]] = replace_codelines(
          fxdoc$MCB$code[[fileID]],
          NULL, fxdoc$fxtable$doc_start[ID], fxdoc$fxtable$doc_end[ID])
        fxdoc$fxtable$doc_end[ID] = NA; fxdoc$fxtable$doc_start[ID] = NA

      } else if (lines_added != 0 || any(
        fxdoc$fxinfo[[ID]]$docu_new$Value != fxdoc$fxinfo[[ID]]$docu_cur$Value)
      ) { # Documentation doesn't match
        fxdoc$fxtable$status[ID] = "update_docu"; files_changed[fileID] = TRUE
        fxdoc$MCB$code[[fileID]] = replace_codelines(
          fxdoc$MCB$code[[fileID]], c(todo, fxdoc$fxinfo[[ID]]$docu_new$Value),
          fxdoc$fxtable$doc_start[ID], fxdoc$fxtable$doc_end[ID])
        fxdoc$fxtable$doc_start[ID] = fxdoc$fxtable$doc_start[ID] + 1
        lines_added = lines_added + 1

      } else {
        fxdoc$fxtable$status[ID] = "no_change:docu_exists"
      }
    }

    if (lines_added != 0) {
      ## Adjust locations for this specific function
      fxdoc$fxtable$doc_end[ID] = fxdoc$fxtable$doc_end[ID] + lines_added
      fxdoc$fxtable$fx_start[ID] = fxdoc$fxtable$fx_start[ID] + lines_added

      ## Adjust locations for remaining functions in the file
      for(rs in ID_fxs) {
        for (cs in which(colnames(fxdoc$fxtable) %in%
                         c("doc_start", "doc_end", "fx_start"))) {
          if (!is.na(fxdoc$fxtable[rs,cs]) &&
              fxdoc$fxtable[rs,cs] >= line_before_fx) {
            fxdoc$fxtable[rs,cs] = fxdoc$fxtable[rs,cs] + lines_added
          }
        }
      }
    }
  }

  fxdoc$files_changed = files_changed
  return(fxdoc)
}


# Function to compute mode of text ----------------------------------------


#' Compute "mode" of text
#'
#' This function computes the most frequently occuring element of a character
#'   vector (it works exactly like Mode on an integer vector). In addition, it's
#'   possible to specify certain exact elements to ignore, by specifying
#'   \code{ignore_type}
#'
#' @param v [vector-char] :: Input vector whose mode is desired
#' @param ignore [vector-char; DEFAULT = "temp"] :: Exact elements to ignore
#'   when computing mode
#'
#' @return [char] :: Mode of \code{v}. Returns char(0) if no non-ignored
#'   elements.
#'
#' @export
#'
Mode_text = function(v, ignore = "temp") {
  t = v[!(v %in% ignore)]
  if (length(t) == 0) { return(char(0)) }
  tab = table(t)
  return(names(tab)[which(tab == max(tab))[1]])
}

