
# Custom output hook for removing prefix ----------------------------------

default_output_hook = knitr::knit_hooks$get("output")

knitr::knit_hooks$set(output = function(x, options) {
  comment = knitr::opts_current$get("comment")

  if (is.na(comment)) comment = ""

  can_null = grepl(paste0(comment, "\\s*\\[\\d?\\]"), x, perl = TRUE)
  do_null = isTRUE(knitr::opts_current$get("null_prefix"))

  if(can_null && do_null) {
    align_index = regexpr("\\]", x)[1] - 1
    re = paste0("^.{", align_index, "}\\]")
    rep = comment
    x = gsub(re, rep, x)
    re = paste0("\\\n.{", align_index, "}\\]")
    rep = paste0("\n", comment )
    x = gsub(re, rep, x)
  }

  default_output_hook(x, options)
})
