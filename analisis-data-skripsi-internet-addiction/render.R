if (FALSE) {
  library(rmarkdown)
  
  # simple invocation
  render("internetaddiction.Rmd", pdf_document())
  
  # specify an option for latex engine
  render("internetaddiction.Rmd", pdf_document(latex_engine = "lualatex"))
  
  # add a table of contents and pass an option to pandoc
  render("internetaddiction.Rmd", pdf_document(toc = TRUE, "--listings"))
}
