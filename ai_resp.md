Implemented as two function replacements in `R/rrd_html.R`. The relevant code currently builds the regcheck title from `rrd_html_regcheck_flags()` and returns `"sb vs rb coef/se mismatch"` for the target issue type, so the clean change is to add the two new fields to `flags` and enrich only that title branch. 

!MODIFICATION rrd_html_regcheck_flags in R/rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_regcheck_flags"
description = "Add the new regcheck parcel fields `rb_sb_share_coeff_same` and `sb_num_coef` to the flags used for regcheck issue titles."
------------------------------------------------------------------------------------------------------------------------------------------

```r
rrd_html_regcheck_flags = function(row) {
  restore.point("rrd_html_regcheck_flags")

  get_bool = function(name, default = FALSE) {
    if (!name %in% names(row)) return(default)
    val = row[[name]][1]
    if (is.na(val)) return(default)
    isTRUE(as.logical(val))
  }

  get_chr = function(name) {
    if (!name %in% names(row)) return("")
    val = as.character(row[[name]][1])
    if (is.na(val)) "" else val
  }

  get_num = function(name, default = NA_real_) {
    if (!name %in% names(row)) return(default)
    val = suppressWarnings(as.numeric(row[[name]][1]))
    if (is.na(val)) return(default)
    val
  }

  get_int = function(name, default = NA_integer_) {
    if (!name %in% names(row)) return(default)
    val = suppressWarnings(as.integer(row[[name]][1]))
    if (is.na(val)) return(default)
    val
  }

  list(
    so = get_bool("so_did_run"),
    sb = get_bool("sb_did_run"),
    rb = get_bool("rb_did_run"),
    so_raw = get_bool("so_raw_did_run", default = get_bool("so_did_run")),
    sb_raw = get_bool("sb_raw_did_run", default = get_bool("sb_did_run")),
    sb_so_same = get_bool("sb_so_identical", default = NA),
    rb_sb_coef_same = get_bool("rb_sb_coef_same", default = NA),
    rb_sb_se_same = get_bool("rb_sb_se_same", default = NA),
    rb_sb_share_coeff_same = get_num("rb_sb_share_coeff_same"),
    sb_num_coef = get_int("sb_num_coef"),
    problem = get_chr("problem"),
    comment = get_chr("comment")
  )
}
```

!END_MODIFICATION rrd_html_regcheck_flags in R/rrd_html.R

!MODIFICATION rrd_html_regcheck_title in R/rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_regcheck_title"
description = "For `sb vs rb coef/se mismatch` titles, append the percent of coefficients that differ and the number of sb coefficients when those fields are available."
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
rrd_html_regcheck_title = function(flags) {
  restore.point("rrd_html_regcheck_title")

  missing = c(
    if (!isTRUE(flags$so)) "so" else character(0),
    if (!isTRUE(flags$sb)) "sb" else character(0),
    if (!isTRUE(flags$rb)) "rb" else character(0)
  )

  if (length(missing) > 0) {
    title = paste0(rrd_html_join_words(missing), " missing")
    if (!isTRUE(flags$sb)) {
        if (isTRUE(flags$sb_raw)) {
           title = paste0(title, " (sb_raw exists)")
        } else {
           title = paste0(title, " (sb and sb_raw missing)")
        }
    }
    return(title)
  }

  coef_bad = identical(flags$rb_sb_coef_same, FALSE)
  se_bad = identical(flags$rb_sb_se_same, FALSE)

  if (coef_bad && se_bad) {
    title = "sb vs rb coef/se mismatch"

    share_same = suppressWarnings(as.numeric(flags$rb_sb_share_coeff_same))
    sb_num_coef = suppressWarnings(as.integer(flags$sb_num_coef))

    details = character(0)

    if (!is.na(share_same)) {
      share_not_same = 100 * (1 - share_same)
      digits = if (abs(share_not_same) < 10) 1L else 0L
      share_not_same_txt = formatC(share_not_same, digits = digits, format = "f")
      details = c(details, paste0(share_not_same_txt, "% coeffs differ"))
    }

    if (!is.na(sb_num_coef)) {
      details = c(details, paste0(sb_num_coef, " sb coefs"))
    }

    if (length(details) > 0) {
      title = paste0(title, " (", paste0(details, collapse = ", "), ")")
    }

    return(title)
  }
  if (coef_bad) return("sb vs rb coef mismatch")
  if (se_bad) return("sb vs rb SE mismatch")

  if (identical(flags$sb_so_same, FALSE)) {
    return("sb vs so mismatch")
  }

  if (nzchar(flags$problem)) {
    return(flags$problem)
  }

  "Regression check issue"
}
```

!END_MODIFICATION rrd_html_regcheck_title in R/rrd_html.R
