# Roxygen2 Documentation Workflow Guide for metamet Package

This guide explains how to add, manage, and generate roxygen2
documentation for the metamet package.

## Overview

**Roxygen2** is an R documentation system that lets you write
documentation comments directly in your R code and automatically
generates `.Rd` files and updates the `NAMESPACE` file.

### Key Concepts

- **Documentation comments** start with `##'` (with a space after `#`)
- **Tags** like `@param`, `@return`, `@export` control documentation
  generation
- **[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)**
  or
  **[`roxygen2::roxygenise()`](https://roxygen2.r-lib.org/reference/roxygenize.html)**
  regenerates all documentation files

------------------------------------------------------------------------

## 1. Understanding Roxygen Tags

### Common Tags Used in metamet

| Tag | Purpose | Example |
|----|----|----|
| `@param` | Document function parameters | `@param dt A data.table containing observations` |
| `@return` | Describe what the function returns | `@return A metamet object` |
| `@details` | Provide detailed information | `@details The function adds a site column...` |
| `@examples` | Provide usage examples | `@examples mm <- metamet(...)` |
| `@export` | Make function available to package users | `@export` |
| `@keywords internal` | Mark as internal-use only | `@keywords internal` |
| `@noRd` | Don’t create `.Rd` file (for internal functions) | `@noRd` |
| `@rdname` | Group multiple functions in one `.Rd` file | `@rdname metamet` |
| `@seealso` | Link to related functions | `@seealso \\code{\\link{join}}` |
| `@importFrom` | Document imported functions | `@importFrom data.table .SD` |

------------------------------------------------------------------------

## 2. Structure of a Roxygen Documentation Block

\`\`\`r \##’ Brief description (one line) \##’ \##’ Detailed description
(optional, multiple lines allowed). \##’ Explain what the function does
and how it works. \##’ \##’ @param param_name Description of the
parameter. \##’ @param another_param Description of another parameter.
\##’ \##’ @return Description of what the function returns. \##’ \##’
@details \##’ Additional implementation details, notes, or
considerations. \##’ \##’ @seealso \##’ for related functionality \##’
\##’ @examples \##’ \# Example 1: Basic usage \##’ result \<-
my_function(x = 1) \##’ \##’ \# Example 2: More advanced usage \##’
result \<- my_function(x = c(1, 2, 3)) \##’ \##’ @export \##’ @keywords
internal my_function \<- function(param1, param2 = NULL) { \# Function
body }
