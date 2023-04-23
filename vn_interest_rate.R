if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(here, rio,
               tidyverse, lubridate, reshape2, scales, xts,
               janitor,
               cowplot,
               extrafont)

loadfonts(device = "win")

policy_rates_file <- here(Sys.getenv("USERPROFILE"),
                  "CloudDrive/My Drive/data", "vn_money_market",
                  "vn_policy_rates.xlsx")

interbank_file <- here(Sys.getenv("USERPROFILE"),
                   "CloudDrive/My Drive/data", "vn_money_market",
                   "vn_interbank.xlsx")

refinance_df <- import(policy_rates_file, which = "refinance")
discount_df <- import(policy_rates_file, which = "discount")
interbank_df <- import(interbank_file) %>% 
  janitor::clean_names()

on_df <- interbank_df %>% 
  filter(ky_han == "ON") %>% 
  select(c(date, lai_suat_percent))

date_seq <- seq(min(on_df$date, refinance_df$date, discount_df$date),
                max(on_df$date, refinance_df$date, discount_df$date), by = "day")

weekdays_seq <- weekdays(date_seq)
date_seq <- date_seq[!(weekdays_seq == "Saturday" | weekdays_seq == "Sunday")]

merged_df <- data.frame(date = date_seq) %>% 
  merge(on_df, by = "date", all = TRUE) %>% 
  merge(refinance_df, by = "date", all = TRUE) %>% 
  merge(discount_df, by = "date", all = TRUE)

filled_df <- merged_df %>% 
  fill(discount, refinance, .direction = "down")

melted_df <- filled_df %>% 
  reshape2::melt(id.var="date")

theme_set(theme_minimal())

to_plot <- melted_df %>% filter(date >= as.Date("2022-01-01"))

plot1 <- ggplot(to_plot,
       aes(x = date, y = value, col = variable)) +
  geom_line() +
  theme(text = element_text(size=14,  family="serif"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_color_brewer(palette="Set1", #color palette
                     breaks=c("refinance", #select order of labels name
                              "discount",
                              "lai_suat_percent"),
                     labels=c("Tái cấp vốn", #rename labels
                              "Tái chiết khấu",
                              "Liên ngân hàng qua đêm")) +
  labs(title = str_c("Lãi suất điều hành và lãi suất liên ngân hàng ",
                     strftime(min(to_plot$date), "%Y"),
                     " - ", strftime(max(to_plot$date), "%Y")),
       subtitle = str_c("%/năm"),
       caption = str_c("Nguồn: NHNN"))

ggsave(path = "img",
       filename = "vn_interest_rates.png",
       plot = plot1,
       width = 17, height = 13,
       units = "cm", dpi = 300,
       bg = "white")
