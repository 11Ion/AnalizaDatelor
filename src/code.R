library(ggplot2)
library(dplyr)

data <- read.csv("ds_salaries.csv")
colors <- c("#283593", "#E53935", "#009688", "#9C27B0", "#FFB300")
str(data)


# Grafic - Distribuția Salarială pe ani.
ggplot(data, aes(x = factor(work_year), y = salary, fill = factor(work_year))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(x = "An", y = "Salariul în USD", title = "Distribuția Salarială pe An") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  guides(fill = guide_legend(title = "Ani"))

# Identificarea factorilor care influențează variația salariilor.

# calc v. medie pentru pentru fiecare nivel de experienta
average_salary_by_experience <- data %>%
  group_by(experience_level) %>%
  summarize(mean_salary = mean(salary_in_usd))

# Grafic - Salariul Mediu pe Nivel de Experiență
ggplot(average_salary_by_experience, aes(x = experience_level, y = mean_salary, group = 1)) +
  geom_line(color = "black", size = 0.7) +
  geom_point(color = "#283593", size = 3) +
  labs(x = "Nivel de experiență", y = "Salariul mediu în USD", title = "Salariul mediu pe nivel de experiență")

# Impactul muncii la distanță

filtered_data <- data %>%
  filter(remote_ratio %in% c(0, 100)) %>%
  select(remote_ratio, salary_in_usd)

average_salaries <- filtered_data %>%
  group_by(remote_ratio) %>%
  summarize(mean_salary = mean(salary_in_usd))

average_salaries$remote_ratio <- ifelse(average_salaries$remote_ratio == 100, "La distanță", "La birou")

pie_chart <- ggplot(average_salaries, aes(x = 1, y = mean_salary, fill = remote_ratio, label = sprintf("%.2f USD", mean_salary))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f USD", mean_salary), vjust = ifelse(remote_ratio == "La Distanță", -1, 1.6)), size = 4, position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Influența muncii la distanță", fill = "Tipul muncii") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("La birou" = "#009688", "La distanță" = "#283593"), labels = c("La birou", "La distanță"))

print(pie_chart)

# Influența locației asupra salariului
ggplot(data, aes(x = company_location, y = salary_in_usd, fill = company_location)) +
  geom_boxplot() +
  labs(x = "Locație", y = "Salariul în USD", title = "Influența Locației asupra Salariului") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  guides(fill = guide_legend(title = "Locație"))

# Influența mărimii companiei asupra salariului
ggplot(data, aes(x = company_size, y = salary_in_usd, fill = company_size)) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  labs(x = "Mărimea Companiei", y = "Salariul în USD", title = "Influența Mărimii Companiei asupra Salariului") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  guides(fill = guide_legend(title = "Mărimea Companiei"))

