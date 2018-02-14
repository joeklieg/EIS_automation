library(tidyverse)
library(readxl)

avg_med <- read_xls("~/Desktop/ENG_data_group/school_salaries/salaries.xls", sheet = 1)
deciles <- read_xls("~/Desktop/ENG_data_group/school_salaries/salaries.xls", sheet = 2)
quartiles <- read_xls("~/Desktop/ENG_data_group/school_salaries/salaries.xls", sheet = 3)
institutions <- read_xls("~/Desktop/ENG_data_group/school_salaries/salaries.xls", sheet = 4)

n <- c("department", "assistant", "associate", "full")

avg <- avg_med[3:12, 1:4]
med <- avg_med[3:12, c(1, 6:8)]
upp_dec <- deciles[3:12, 1:4]
low_dec <- deciles[3:12, c(1, 6:8)]
upp_quart <- quartiles[3:12, 1:4]
low_quart <- quartiles[3:12 , c(1, 6:8)]

colnames(avg) <- n
colnames(med) <- n
colnames(upp_dec) <- n
colnames(low_dec) <- n
colnames(upp_quart) <- n
colnames(low_quart) <- n

avg[avg == "X"] <- NA
med[med == "X"] <- NA
upp_dec[upp_dec == "X"] <- NA
low_dec[low_dec == "X"] <- NA
upp_quart[upp_quart == "X"] <- NA
low_quart[low_quart == "X"] <- NA

salary_gather <- function(x) {
        gather(x, prof_rank, salary, -department)
}

avg <- salary_gather(avg)
med <- salary_gather(med)
upp_dec <- salary_gather(upp_dec)
low_dec <- salary_gather(low_dec)
upp_quart <- salary_gather(upp_quart)
low_quart <- salary_gather(low_quart)

avg$metric <- "average"
med$metric <- "median"
upp_dec$metric <- "upper_decile"
low_dec$metric <- "lower_decile"
upp_quart$metric <- "upper_quartile"
low_quart$metric <- "lower_quartile"

decile_vis <- bind_rows(avg, med, upp_dec, low_dec)
decile_vis$salary <- as.numeric(decile_vis$salary)
decile_vis$prof_rank <- as.factor(decile_vis$prof_rank)
decile_vis$metric <- as.factor(decile_vis$metric)
decile_vis$metric <- factor(decile_vis$metric,
                            levels = c("upper_decile", "average", "median", "lower_decile"))




quartile_vis <- bind_rows(avg, med, upp_quart, low_quart)
quartile_vis$salary <- as.numeric(quartile_vis$salary)
quartile_vis$prof_rank <- as.factor(quartile_vis$prof_rank)
quartile_vis$metric <- as.factor(quartile_vis$metric)
quartile_vis$metric <- factor(quartile_vis$metric,
                              levels = c("upper_quartile", "average", "median", "lower_quartile"))

total_vis <- bind_rows(decile_vis, quartile_vis)

write_csv(decile_vis, "~/Desktop/ENG_data_group/school_salaries/salaries_interactive/decile_data")
write_csv(quartile_vis, "~/Desktop/ENG_data_group/school_salaries/salaries_interactive/quartile_data")
write_csv(total_vis, "~/Desktop/ENG_data_group/school_salaries/salaries_interactive/total_data")

str(total_vis)

dec <- ggplot(decile_vis, aes(department, salary, color = metric)) + geom_point()
        dec + facet_grid(. ~ prof_rank) +
                labs(color = "Salary Metric") +
                scale_y_discrete(breaks = pretty(decile_vis$salary, n = 10)) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                labs(title = "Engineering Salaries by Decile")

quar <- ggplot(quartile_vis, aes(department, salary, color = metric)) + geom_point()
        quar + facet_grid(. ~ prof_rank) +
                labs(color = "Salary Metric") +
                scale_y_discrete(breaks = pretty(quartile_vis$salary, n = 10)) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                labs(title = "Engineering Salaries by Quartile")
