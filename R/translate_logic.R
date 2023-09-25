#' Translate REDCap branching logic into R expressions
#'
#' @description
#' Translate REDCap branching logic into expressions evaluable in R. E.g.
#'
#' | __Translation step__ | __Example expression__ |
#' | -------------------- | -------------- |
#' | 0. Initial REDCap logic | `[over_18]='1' and [signed_consent]<>''` |
#' | 1. Translate to R | `over_18 == '1' & signed_consent != ''` |
#' | 2. (Optional) Swap values/labels&nbsp;&nbsp; | `over_18 == 'Yes' & signed_consent != ''` |
#' | 3. (Optional) Use is.na | `over_18 == 'Yes' & !is.na(signed_consent)` |
#' | 4. (Optional) Use %in% | `over_18 %in% 'Yes' & !is.na(signed_consent)` |
#'
#' @param x A character vector of REDCap branching logic statements
#' @param use_value_labs Logical indicating whether to replace factor option
#'   values with labels (based on the mapping defined in `meta_factors`).
#'   E.g.:\cr
#'   `y == '1'` becomes `y == 'Yes'`
#' @param use_header_labs Logical indicating whether to use labels instead of
#'   variable names as column names (based on the mapping defined in
#'   `meta_dictionary`).
#'   E.g.:\cr
#'   `age >= 18` becomes `"Participant's age" >= 18`
#' @param use_is_na Logical indicating whether to replace REDCap-style tests for
#'   missingness with [`is.na`]. E.g.:\cr
#'   `y == ""` becomes `is.na(y)`\cr
#'   `y != ""` becomes `!is.na(y)`
#' @param use_in Logical indicating whether to replace instances of [`==`] and
#'   [`!=`] associated with factor-type variables (as defined in `meta_factors`)
#'   with [`%in%`]. E.g.:\cr
#'   `y == 'Yes'` becomes `y %in% 'Yes'`\cr
#'   `y != 'Yes'` becomes `!y %in% 'Yes'`
#' @param drop_redundant Logical indicating whether to simplify expressions by
#'   removing redundant components from expressions that test both for equality
#'   and inequality with the same variable. E.g.:\cr
#'  `var == "X" & var != ""` becomes `var == "X"`
#' @param meta_factors A data frame containing variable names (column
#'   `field_name`) and corresponding values (column `value`) and labels (column
#'   `label`) for factor-type variables. Fetch with [`meta_factors`]. Only
#'   needed if either `use_value_labs` or `use_in` are `TRUE`.
#' @param meta_dictionary A data frame containing variable names (column
#'   `field_name`) and labels (column `field_label`). Fetch with
#'   [`meta_dictionary`]. Only needed if `use_header_labs` is `TRUE`.
#' @param on_error What to do if one or more elements of statements can't be
#'   translated into valid R expressions? Options are "ignore" (return `NA` for
#'   relevant elements), "warn" (return `NA` for relevant elements and give
#'   warning), or "fail" (throw error). Defaults to "warn".
#'
#' @return
#' A character vector of R-style expressions
#'
#' @examples
#' # normally would fetch factor metadata with redcap::meta_factors(), but here
#' # we'll create a simple example by hand
#' df_factors <- data.frame(
#'   field_name = c("head_household", "head_household"),
#'   value = c("0", "1"),
#'   label = c("No", "Yes"),
#'   stringsAsFactors = FALSE
#' )
#'
#' redcap_logic <- "head_household=1 and age<>\"\""
#' translate_logic(redcap_logic, meta_factors = df_factors)
#'
#' @export translate_logic
translate_logic <- function(x,
                            use_value_labs = TRUE,
                            use_header_labs = FALSE,
                            use_is_na = TRUE,
                            use_in = TRUE,
                            drop_redundant = FALSE,
                            meta_factors = NULL,
                            meta_dictionary = NULL,
                            on_error = "warn") {

  # validate args
  on_error <- match.arg(on_error, c("ignore", "warn", "fail"))

  if (use_value_labs & is.null(meta_factors)) {
    stop("If use_value_labs is TRUE, meta_factors must be specified", call. = FALSE)
  }
  if (use_in & is.null(meta_factors)) {
    stop("If use_in is TRUE, meta_factors must be specified", call. = FALSE)
  }
  if (use_header_labs & is.null(meta_dictionary)) {
    stop("If use_header_labs is TRUE, meta_dictionary must be specified", call. = FALSE)
  }

  # translate
  out <- vapply(
    X = x,
    FUN = translate_logic_,
    FUN.VALUE = "",
    USE.NAMES = FALSE,
    use_value_labs = use_value_labs,
    use_header_labs = use_header_labs,
    use_is_na = use_is_na,
    use_in = use_in,
    drop_redundant = drop_redundant,
    meta_factors = meta_factors,
    meta_dictionary = meta_dictionary
  )

  # check for elements that couldn't be translated
  x_fail <- x[!is.na(x) & is.na(out)]

  x_fail_msg <- paste(
    "The following statements could not be translated:\n",
    paste(x_fail, collapse = "\n")
  )

  if (length(x_fail) > 0 & on_error %in% c("warn", "fail")) {
    if (on_error == "warn") {
      warning(x_fail_msg, call. = FALSE)
    } else {
      stop(x_fail_msg, call. = FALSE)
    }
  }

  # return
  out
}


#' Non-vectorized version of translate_logic
#'
#' @noRd
translate_logic_ <- function(x,
                             use_value_labs,
                             use_header_labs,
                             use_is_na,
                             use_in,
                             drop_redundant,
                             meta_factors,
                             meta_dictionary) {

  if (!is.na(x)) {
    x_prep <- try(translate_prep(x), silent = TRUE)

    if (!"try-error" %in% class(x_prep)) {

      if (drop_redundant) {
        x_prep <- drop_redundant_traverse(x_prep)
      }

      x_out <- translate_traverse(
        x_prep,
        use_value_labs = use_value_labs,
        use_header_labs = use_header_labs,
        use_is_na = use_is_na,
        use_in = use_in,
        meta_factors = meta_factors,
        meta_dictionary = meta_dictionary
      )

      x_out <- deparse1(x_out)

    } else {
      x_out <- NA_character_
    }
  } else {
    x_out <- NA_character_
  }

  x_out
}


#' Initial gsub step for translating REDCap-style expressions to R
#'
#' @param x A call returned by [`str2lang()`]
#'
#' @noRd
translate_prep <- function(x) {
  x <- gsub("\\n", " ", x)
  x <- gsub("\\[\\w+\\](?=\\[)", "", x, perl = TRUE)
  x <- gsub("[[:space:]]+or[[:space:]]+", " | ", x, ignore.case = TRUE)
  x <- gsub("[[:space:]]+and[[:space:]]+", " & ", x, ignore.case = TRUE)
  x <- gsub("([a-z,0-9,_])\\((?<=\\()(.*?)(?=\\))\\)", "\\1___\\2", x, perl = TRUE)
  x <- gsub("[[:space:]]*(?<![><])=[[:space:]]*", " == ", x, perl = TRUE)
  x <- gsub("[[:space:]]*<>[[:space:]]*", " != ", x)
  x <- gsub("[[:space:]]*>(?![=])[[:space:]]*", " > ", x, perl = TRUE)
  x <- gsub("[[:space:]]*<(?![=])[[:space:]]*", " < ", x, perl = TRUE)
  x <- gsub("\\[event\\-name\\]", "[redcap_event_name]", x)
  x <- gsub("([[]|[]])", "", x)
  x <- gsub("\"", "\'", x)
  str2lang(x)
}


#' @noRd
drop_redundant_traverse <- function(x) {

  if (is_expr_lowest_drop(x)) {
    x <- drop_redundant(x)
  } else {
    for (i in seq_len(length(x))) {
      if (is_expr_lowest_drop(x[[i]])) {
        x[[i]] <- drop_redundant(x[[i]])
      } else {
        x[[i]] <- drop_redundant_traverse(x[[i]])
      }
    }
  }

  x
}


#' Recursive function used to traverse expressions from top-down to break into
#' binary components (3 terms or fewer) that can be passed to
#' translate_options(), translate_missing(), and translate_equals()
#'
#' @noRd
translate_traverse <- function(x,
                               use_value_labs,
                               use_header_labs,
                               use_is_na,
                               use_in,
                               meta_factors,
                               meta_dictionary) {

  if (is_expr_lowest(x)) {
    if (use_value_labs)  x <- translate_options(x, meta_factors)
    if (use_in)          x <- translate_equals(x, meta_factors$field_name, use_is_na)
    if (use_header_labs) x <- translate_names(x, meta_dictionary)
    if (use_is_na)       x <- translate_missing(x)
  } else {
    for (i in seq_len(length(x))) {
      if (is_expr_lowest(x[[i]])) {
        if (use_value_labs)  x[[i]] <- translate_options(x[[i]], meta_factors)
        if (use_in)          x[[i]] <- translate_equals(x[[i]], meta_factors$field_name, use_is_na)
        if (use_header_labs) x[[i]] <- translate_names(x[[i]], meta_dictionary)
        if (use_is_na)       x[[i]] <- translate_missing(x[[i]])
      } else {
        x[[i]] <- translate_traverse(
          x[[i]],
          use_value_labs = use_value_labs,
          use_header_labs = use_header_labs,
          use_is_na = use_is_na,
          use_in = use_in,
          meta_factors = meta_factors,
          meta_dictionary = meta_dictionary
        )
      }
    }
  }

  x
}


#' Swap factor-variable options (e.g. '0'/'1') with labels (e.g. 'Yes'/'No'),
#' given a binary expression and metadata dictionary
#'
#' @description
#' Searches a binary expression for variables/options matching a metadata
#' dictionary, and replaces options with labels.
#'
#' E.g. head_of_household == "1" -> head_of_household == "Yes"
#'
#' @param x A call returned by str2lang()
#' @param meta_factors The long-form metadata dictionary for factor-type variables
#'   (see [`meta_factors`])
#'
#' @noRd
translate_options <- function(x, meta_factors) {

  # check if has factor-type var
  var_factor <- intersect(all.vars(x), meta_factors$field_name)
  has_factor <- length(var_factor) == 1L

  if (has_factor) {

    vals_factor <- meta_factors$value[meta_factors$field_name %in% var_factor]
    labs_factor <- meta_factors$label[meta_factors$field_name %in% var_factor]

    x_vec <- vapply(x, as.character, "")
    i_val <- which(x_vec %in% vals_factor)

    if (length(i_val) > 1L) {
      stop("Mutliple factor-type values found in lowest-level expression")
    } else if (length(i_val) == 1L) {
      lab_swap <- labs_factor[vals_factor == x_vec[[i_val]]]
      x[[i_val]] <- lab_swap
    }
  }

  x
}



#' Swap variable names (e.g. 'ahead_household') with labels (e.g. 'Are you the
#' head of the household ?')
#'
#' @description
#' Searches a binary expression for variables matching a metadata dictionary,
#' and replaces variable names with labels.
#'
#' E.g.:
#' - ahead_household == "Yes"
#' becomes...
#' - "[Are you the head of the household ?]" == "Yes"
#'
#' @param x A call returned by str2lang()
#' @param dict The metadata dictionary (see [`meta_dictionary`])
#'
#' @noRd
translate_names <- function(x, dict) {

  vars_in_dict <- intersect(all.vars(x), dict$field_name)

  if (length(vars_in_dict) > 0) {

    for (i in seq_along(vars_in_dict)) {
      var_label <- dict$field_label[dict$field_name %in% vars_in_dict[i]]
      var_position <- which(as.character(x) %in% vars_in_dict[i])
      x[[var_position]] <- enclose(var_label, "[", "]")
    }
  }

  x
}



#' Replace REDCap-style tests for missing value with is.na()
#'
#' @description
#' var == "" -> is.na(var)
#' var != "" -> !is.na(var)
#'
#' @param x A call returned by str2lang()
#'
#' @noRd
translate_missing <- function(x) {

  # Search expression for:
  # - a variable
  # - a REDCap-style missing value ("")
  # - an equal or not-equal sign ("==" | "!=")
  is_var <- untick(as.character(x)) %in% all.vars(x)
  is_missing <- as.character(x) %in% ""
  is_equal <- as.character(x) %in% "=="
  is_not_equal <- as.character(x) %in% "!="

  # If expression has the three required components, translate to is.na()
  if (any(is_var) & any(is_missing) & (any(is_equal) | any(is_not_equal))) {
    x <- paste0("is.na(", x[[which(is_var)]], ")")
    if (any(is_not_equal)) x <- paste0("!", x)
    x <- str2lang(x)
  }

  x
}



#' Replace == with %in%
#'
#' @description
#' x == "Yes" -> x %in% "Yes"
#'
#' @param x A call returned by str2lang()
#' @param vars_factor A character vector of names of factor-type variables
#'
#' @noRd
translate_equals <- function(x, vars_factor, use_is_na) {

  # Search expression for:
  # - a factor-type variable
  # - an equal or not-equal sign ("==" | "!=")
  has_factor <- any(all.vars(x) %in% vars_factor)
  is_equal <- as.character(x) %in% "=="
  is_not_equal <- as.character(x) %in% "!="

  # if use_is_na is TRUE, wait to replace with is.na(var) rather than var %in% ""
  skip_missing <- use_is_na & any(as.character(x) %in% "")

  # If expression has the required components, translate
  if (!skip_missing & has_factor & (any(is_equal) | any(is_not_equal))) {

    x[[which(is_equal | is_not_equal)]] <- as.symbol("%in%")
    if (any(is_not_equal)) x <- str2lang(paste0("!", deparse(x)))
  }

  x
}



#' Test whether an expression is binary (has at most 3 atomic terms)
#'
#' @param x A call returned by str2lang()
#'
#' @noRd
is_expr_lowest <- function(x) {
   all(lengths(as.list(x)) == 1L) & length(x) <= 3L
}



#' @noRd
drop_redundant <- function(x) {

  x_vec <- as.character(x)
  x_list <- as.list(x)

  has_and <-  x_vec[1] == "&"
  is_equal <- vapply(x_list, any_equal, FALSE)
  is_not_missing <- vapply(x_list, any_not_missing, FALSE)

  if (has_and & any(is_equal) & any(is_not_missing)) {

    i_equal <- which(is_equal)
    i_not_missing <- which(is_not_missing)

    same_var <- all.vars(x_list[[i_equal]]) == all.vars(x_list[[i_not_missing]])

    if (same_var) {
      x <- x[[i_equal]]
    }
  }

  x
}


#' @noRd
is_expr_lowest_drop <- function(x) {
  length(x) <= 3 & n_operators(x) <= 1
}


#' @noRd
#' @importFrom utils getParseData
n_operators <- function(x) {
  x_deparse <- deparse1(x)
  n <- if (nchar(x_deparse) == 1) { # bit of a hack
    0
  } else {
    sum(getParseData(parse(text = x_deparse))$token %in% c("OR", "AND"))
  }
  n
}


#' @noRd
any_equal <- function(x) {
  any(as.character(x) %in% "==")
}


#' @noRd
any_not_missing <- function(x) {
  any(as.character(x) %in% "") & any(as.character(x) %in% "!=")
}

