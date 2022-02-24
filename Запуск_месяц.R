library(lubridate)

rmarkdown::render("RFM_анализ.Rmd", output_file = paste0("RFM/RFM_анализ_", (month(as_datetime(floor_date(today(), unit = "month")) - months(1), label = TRUE, abbr = FALSE))))

rmarkdown::render("Прогноз_на_месяц.Rmd", output_file = paste0("Планирование/Прогноз_", (month(as_datetime(floor_date(today(), unit = "month")), label = TRUE))))
