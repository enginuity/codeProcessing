# R code processing package (codeProcessing)

A collection of developer tools for R, by providing a set of tools to aid with
code refactoring and exploration. It contains the following features: 

* Search/replace/add comments to match text that match certain regex patterns.
  * This can also generate a report listing all changed aspects. 
* Create a color-coded dependency tree/graph for the functions in an R package
or an arbitrary collection of code files.
* Generate a TODO report (looking for TODO's or other similar structures/regex's
in the code). 
* Aid with creating Roxygen2 templates:
  * Add placeholders for undocumented parameters.
  * Replace documentation for a certain parameter across multiple functions
  (though this probably should be replaced by inherit parameter calls).
  * Reorder parameters in documentation automatically to match function order.

NOTE: Some uses of this package can cause file changes (intentionally). Do not
use this package on directories without proper version control. 


## Version Plan

* Version 0.1.3 -- General cleanup
  * Some refactoring to streamline things.
  * Add/fix documentation.
  
* Version 0.1.4 -- Fix dependency plotting
  * Fix any issues in dependency plotting, create a static version of the
  dependency plot that does not use the graphviz package.
  
* Version 0.1.5 -- Create addins
  * Create addins for search/replace.
  * Create addin for documentation processing.
  * Create addin for dependency visualization.


## Changelog
**Version 0.1.2 2015/07/15** 
* Lots of bug fixes and general refactoring

**Version 0.1.1 2015/03/02**
* Remove DEFAULT_FD behavior, defaults to working directory if FD is not input.
* Add `stringr` as an import, as opposed to calling `library(stringr)`.
