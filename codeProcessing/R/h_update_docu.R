

# Function to update existing documentation -------------------------------


## TODO [Improvement]: Make object (class) type for storage of output data elements?

#' [Helper] Update documentation as necessary
#' 
#' This function calls for the standard form of the documentation (given the current documentation), and then compares it to the existing documentation. If it's different (or if the existing documentation is missing), it'll be updated in MCB.
#' 
#' This function makes no changes to disk. 
#' 
#' @param fxdf [dataframe] :: function information output from \code{\link{zhdp_extractDocu}}
#' @param fxlist [list] :: function information output from \code{\link{zhdp_extractDocu}}
#' @param MCB [\code{\link{MatchedCodebase}}] :: Stored code information (and information about matches, but this is unreliable, since the code might be altered by previous calls to this function)
#' 
#' @return [list] :: Contains updated function information. Entries of the list are: \code{fx_df}, \code{fx_list}, \code{files_changed}, \code{MCB} \cr
#' fx_df -- [list] :: Updated output of \code{\link{zhdp_extractDocu}} \cr
#' fx_list -- [list] :: Updated output of \code{\link{zhdp_extractDocu}} \cr
#' files_changed -- [vector-int] :: A vector indicating which in MCB were updated \cr
#' MCB -- [\code{\link{MatchedCodebase}}] :: Updated source code
#' 
#' @export
#' 
write_updated_docu = function(fxdf, fxlist, MCB) {
  ## function that updates the documentation if necessary (comparing the existing docu to the 'standard' one.)  
  
  files_changed = rep(FALSE, max(fxdf$fileID))
  
  for (ID in seq_along(fxlist)) {
    fileID = fxdf$fileID[ID]
    todo = paste("## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (",fxlist[[ID]]$fxname,")", sep = "")
    
    ID_fxs = setdiff(which(fxdf$fileID == fileID), ID) ## Find all functions listed in this same file, ignoring current function
    line_before_fx = fxdf$fx_start[ID] - 1
    
    ## Update documentation, and update fx_df. 
    ## Depending on the case -- fx_df needs to have doc_start, doc_end, fx_start, status updated. 
    ## Also, depending on the case, MCB has the code updated (replacing docu_cur with docu_new in fxlist)
    diff_nrow_0 = function(x,y) { return(ifelse(is.null(x), 0, nrow(x)) - ifelse(is.null(y), 0, nrow(y))) }
    lines_added = diff_nrow_0(fxlist[[ID]]$docu_new, fxlist[[ID]]$docu_cur)
    
    if (is.null(fxlist[[ID]]$docu_cur)) { # If no current documentation
      if (is.null(fxlist[[ID]]$docu_new)) { # Want no documentation -> do nothing
        fxdf$status[ID] = "no_change:no_docu"
        
      } else { # Add in new documentation
        fxdf$status[ID] = "add_docu"; files_changed[fileID] = TRUE
        MCB$code[[fileID]] = insert_codelines(MCB$code[[fileID]], c(todo, fxlist[[ID]]$docu_new$Value), fxdf$fx_start[ID])
        fxdf$doc_start[ID] = fxdf$fx_start[ID] + 1; fxdf$doc_end[ID] = fxdf$fx_start[ID] - 1
        lines_added = lines_added + 1
        
      }
    } else { # Else, current documentation exists
      if (is.null(fxlist[[ID]]$docu_new)) { # Want to remove documentation
        fxdf$status[ID] = "remove_docu"; files_changed[fileID] = TRUE
        MCB$code[[fileID]] = replace_codelines(MCB$code[[fileID]], NULL, fxdf$doc_start[ID], fxdf$doc_end[ID])
        fxdf$doc_end[ID] = NA; fxdf$doc_start[ID] = NA
        
      } else if (lines_added != 0 || any(fxlist[[ID]]$docu_new$Value != fxlist[[ID]]$docu_cur$Value)) { # Documentation doesn't match
        fxdf$status[ID] = "update_docu"; files_changed[fileID] = TRUE
        MCB$code[[fileID]] = replace_codelines(MCB$code[[fileID]], c(todo, fxlist[[ID]]$docu_new$Value), fxdf$doc_start[ID], fxdf$doc_end[ID])
        fxdf$doc_start[ID] = fxdf$doc_start[ID] + 1
        lines_added = lines_added + 1
        
      } else {
        fxdf$status[ID] = "no_change:docu_exists"
      }
    }
    
    if (lines_added != 0) {
      ## Adjust locations for this specific function
      fxdf$doc_end[ID] = fxdf$doc_end[ID] + lines_added; fxdf$fx_start[ID] = fxdf$fx_start[ID] + lines_added
      
      ## Adjust locations for remaining functions in the file
      for(rs in ID_fxs) {
        for (cs in which(colnames(fxdf) %in% c("doc_start", "doc_end", "fx_start"))) {
          if (!is.na(fxdf[rs,cs]) && fxdf[rs,cs] >= line_before_fx) { fxdf[rs,cs] = fxdf[rs,cs] + lines_added }
        }
      }
    }  
  } 
  
  return(list(fx_df = fxdf, fx_list = fxlist, files_changed = files_changed, MCB = MCB))
}


# Function to compute mode of text ----------------------------------------


#' Compute "mode" of text
#' 
#' This function computes the most frequently occuring element of a character vector (it works exactly like Mode on an integer vector). In addition, it's possible to specify certain exact elements to ignore, by specifying \code{ignore_type}
#' 
#' @param v [vector-char] :: Input vector whose mode is desired
#' @param ignore [vector-char; DEFAULT = "temp"] :: Exact elements to ignore when computing mode
#' 
#' @return [char] :: Mode of \code{v}. Returns char(0) if no non-ignored elements. 
#' 
#' @export
#' 
Mode_text = function(v, ignore = "temp") {
  t = v[!(v %in% ignore)]
  if (length(t) == 0) { return(char(0)) }  
  tab = table(t)
  return(names(tab)[which(tab == max(tab))[1]])
}

