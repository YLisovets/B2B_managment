library(lubridate)

rmarkdown::render("План_неделя.Rmd", output_file = paste0("Планирование/План_на_неделю_", week(today())))

rmarkdown::render("План_факт.Rmd", output_file = paste0("Планирование/План_факт_неделя_", week(today() - weeks(1))))
