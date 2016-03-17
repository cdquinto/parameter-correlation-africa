library(knitr)
library(markdown)

reportFileHtml<-"report_consuelo_export.html"
reportFileMd<-"report_consuelo_export.md"

knit(input = "report_consuelo2.Rmd", output= reportFileMd)
markdownToHTML(file=reportFileMd, output=reportFileHtml)