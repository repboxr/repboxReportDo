df_to_plain_str = function(
  df,
  long_nchar = 10,
  exclude_vars = character(0),
  max_string_length = NULL,
  max_string_length_by_var = NULL,
  max_width = 1000
) {
  if (is.null(df) || NROW(df) == 0) {
    return("")
  }

  df = as.data.frame(df, stringsAsFactors = FALSE)

  if (length(exclude_vars) > 0) {
    df = df[, !names(df) %in% exclude_vars, drop = FALSE]
  }

  if (NCOL(df) == 0) {
    return("")
  }

  if (NROW(df) > 1) {
    return(df_to_plain_table_str(df, max_width = max_width))
  }

  vals = vapply(df, df_to_plain_chr, character(1))
  is_long = vapply(seq_along(df), function(i) {
    is.character(df[[i]]) &&
      !is.na(stringi::stri_length(vals[[i]])) &&
      stringi::stri_length(vals[[i]]) > long_nchar
  }, logical(1))
  names(is_long) = names(df)

  vals[is_long] = df_to_plain_shorten_vals(
    vals = vals[is_long],
    max_string_length = max_string_length,
    max_string_length_by_var = max_string_length_by_var
  )

  short_names = names(df)[!is_long]
  long_names = names(df)[is_long]

  parts = character(0)

  if (length(short_names) > 0) {
    parts = c(parts, paste0(short_names, "=", vals[short_names], collapse = ", "))
  }

  if (length(long_names) > 0) {
    long_parts = vapply(long_names, function(name) {
      paste0(name, ":\n", vals[[name]])
    }, character(1))

    if (length(parts) > 0) {
      parts = c(parts, "")
    }

    parts = c(parts, paste0(long_parts, collapse = "\n\n"))
  }

  paste0(parts, collapse = "\n")
}


df_to_plain_table_str = function(df, max_width = 1000) {
  old_width = getOption("width")
  options(width = max_width)
  on.exit(options(width = old_width), add = TRUE)

  paste0(capture.output(print(as.data.frame(df), row.names = FALSE, right = FALSE)), collapse = "\n")
}


df_to_plain_chr = function(x) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }

  if (is.list(x) && !is.data.frame(x)) {
    x = unlist(x, recursive = TRUE, use.names = FALSE)
  }

  if (length(x) == 0) {
    return("")
  }

  x = as.character(x[1])

  if (is.na(x)) {
    return("NA")
  }

  x
}


df_to_plain_shorten_vals = function(
  vals,
  max_string_length = NULL,
  max_string_length_by_var = NULL
) {
  if (length(vals) == 0) {
    return(vals)
  }

  max_len = rep(NA_integer_, length(vals))
  names(max_len) = names(vals)

  if (!is.null(max_string_length) && length(max_string_length) > 0) {
    max_len[] = suppressWarnings(as.integer(max_string_length[1]))
  }

  if (!is.null(max_string_length_by_var) && length(max_string_length_by_var) > 0) {
    var_max = unlist(max_string_length_by_var, use.names = TRUE)
    var_max = suppressWarnings(as.integer(var_max))
    var_max = var_max[!is.na(var_max) & nzchar(names(var_max))]

    common = intersect(names(vals), names(var_max))
    if (length(common) > 0) {
      max_len[common] = var_max[common]
    }
  }

  needs_shortening = !is.na(max_len) &
    max_len >= 0L &
    stringi::stri_length(vals) > max_len

  if (!any(needs_shortening)) {
    return(vals)
  }

  vals[needs_shortening] = vapply(names(vals)[needs_shortening], function(name) {
    len = max_len[[name]]

    if (len <= 3L) {
      return(stringi::stri_sub(vals[[name]], 1L, len))
    }

    paste0(stringi::stri_sub(vals[[name]], 1L, len - 3L), "...")
  }, character(1))

  vals
}
