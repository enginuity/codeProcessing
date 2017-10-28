
# Package Documentation Rules

## Parameter listing in function documentation: 
@param param-name [<type>] :: <documentation>
Within [], use: 
* , will separate information about parameters: eg.
* LEN = (length of vector) or
* DIM = (dimensions of matrix, etc.)
* ; will separate information about values the parameter takes on
* DEFAULT = (default value) or
* ALLOWED = (possibly a vector of allowed values)

Example:
[vector-int, LEN = 2; DEFAULT = c(1,2)] :: some description

## Class documentation (S3 classes)
An \itemize with items as follows:
\item <name> -- [<mode description as above>] :: <documentation>
