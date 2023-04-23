if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(here, rio,
               tidyverse, lubridate, reshape2, scales, xts,
               janitor,
               cowplot,
               extrafont)

loadfonts(device = "win")

#Data location
data_file1 <- here(Sys.getenv("USERPROFILE"),
                   "CloudDrive/My Drive/data", "vn_fx", "vn_fxM.xlsx")
data_file2 <- here(Sys.getenv("USERPROFILE"),
                   "CloudDrive/My Drive/data", "vn_fx", "vn_neerM.xlsx")
data_file3 <- here(Sys.getenv("USERPROFILE"),
                   "CloudDrive/My Drive/data", "vn_fx", "vn_reerM.xlsx")

fx <- import(data_file1) %>% 
  janitor::clean_names()
neer <- import(data_file2) %>% 
  janitor::clean_names()
reer <- import(data_file3) %>% 
  janitor::clean_names()

df <- fx %>% 
  merge(neer, all = TRUE) %>% 
  merge(reer, all = TRUE)

df <- df %>% mutate(sbv = 1/sbv)

df_to_plot <- df[df$date >= as.Date("2022-01-01"),]

test <- df_to_plot %>%
  select(-date) %>% 
  xts(order.by = df_to_plot$date)

x1 <- coredata(test)

a <- t(apply(x1, 1, function(x) x/x1[1,]*100))
b <- data.frame(date = as.Date(floor_date(index(test), "month")), a)

df_melted <- b %>% 
  select(date, sbv, neer, reer) %>% 
  reshape2::melt(id.var = "date")

theme_set(theme_minimal())


plot <- ggplot(df_melted, aes(x = date, y = value, col = variable, shape = variable)) +
  geom_path() +
  geom_point() +
  scale_x_date(labels = date_format("%m/%Y"), expand=c(0.05,0.05)) +
  theme(text=element_text(size=14,  family="serif"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_color_brewer(palette="Set1", #color palette
                     breaks=c("sbv", #select order of labels name
                              "reer",
                              "neer"),
                     labels=c("Tỷ giá trung tâm", #rename labels
                              "REER",
                              "NEER")) +
  scale_shape(breaks = c("sbv", "reer", "neer"),
              labels = c("Tỷ giá trung tâm", "REER", "NEER")) +
  labs(title = str_c("Tỷ giá VND ",
                     strftime(min(df_melted$date), "%m/%Y"),
                     " - ", strftime(max(df_melted$date), "%m/%Y")),
       subtitle = "Chỉ số, bình quân các ngày trong tháng, tháng 1/2021 = 100",
       caption = str_c("Nguồn: NHNN, JP Morgan, Ban NCĐP tổng hợp",
                       "\nChú thích: Chỉ số tăng nghĩa là VND lên giá và ngược lại"))

ggsave(path = "img",
       filename = "vn_fx.png",
       plot = plot,
       width = 17, height = 13,
       units = "cm", dpi = 300,
       bg = "white")
