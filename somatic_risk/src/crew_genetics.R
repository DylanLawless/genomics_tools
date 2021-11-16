library(rbokeh)
library(dplyr)

# import ----
df <- 
  read.table(file="../data/crew_genetics.csv", 
             header = TRUE, 
             sep = ",", 
             stringsAsFactors = FALSE)

# summary and clean ----
df <- df %>% group_by(crew_member) %>%
  mutate(cumulative_risk=cumsum(variant_risk))

df$crew_member <- as.character(df$crew_member)
df$variant_risk <- round(df$variant_risk, digits = 3)
df$CI_min <- round(df$CI_min, digits = 2)
df$CI_max <- round(df$CI_max, digits = 2)
df$crtitial_care <- as.character(df$crtitial_care)


# plot 1 ----
plot1 <- 
  figure(#width = 800, height = 600
    legend_location = "top_right"
  ) %>%
  ly_points(x=year, y=variant_risk, data = df, 
            size = 20, color = "black", alpha =1, glyph = crtitial_care,
            legend = FALSE) %>%
  ly_points(x=year, y=variant_risk, data = df,
            size = 10,
           color = crew_member, alpha = .5, glyph = 21,
            hover = list(crew_member, year, variant_risk, CI_min, CI_max, crtitial_care)) %>%
  
  ly_segments(year, CI_max, year, CI_min,  
              data = df, color = crew_member, width = 2)%>%
    x_axis(label = "Year") %>%
    y_axis(label = "New variant risk") %>%
  x_range(c(0, 200)) 

plot1

# Set a treatment criteria ----
events <- data.frame(when=as.numeric(c("1.5", "2")),
                       what=c("Pre-emptive", "Critical care"))

# print the cum risk at time of a criticial care incident 
df$cum_at_crit <- ifelse(df$crtitial_care ==10, df$cumulative_risk,
                     ifelse(df$crtitial_care < 10, NA))

# plot 2 ----
plot2 <-
figure() %>%
  ly_points(x=year, y=cum_at_crit, data = df, 
            size = 20, color = "black", alpha =1, glyph = crtitial_care, legend = FALSE) %>%
  ly_lines(x=year, y=cumulative_risk, group = crew_member, 
           color = crew_member,
           data = df,
           legend = FALSE,
           hover = list(year, cumulative_risk)) %>%
  x_axis(label = "Year") %>%
  y_axis(label = "Cumulative risk") %>%
  ly_points(x=year, y=cumulative_risk, data = df,
            color = crew_member, glyph = crew_member,
            hover = list(year, variant_risk, cumulative_risk),
            legend = FALSE) %>%
ly_text(y=events$when, x=30, color=c("red", "blue"),
        text=events$what, align="right", font_size="7pt") %>%
  ly_abline(h=events$when, color=c("red", "blue"), 
            type=2, alpha=1/4) 
plot2


df$is_critical <- (!is.na(df$crtitial_care))

library(ggplot2)
library(plotly)

p1 <- df %>% 
  ggplot( aes(x=year, y=variant_risk, group = is_critical)) +
  geom_errorbar(aes(ymin=CI_min, ymax=CI_max),  width = 1, alpha = .2) +
  geom_point(aes(size = variant_risk, fill = crew_member), shape= 21)

ggplotly(p1)

p2 <- df %>% 
  ggplot( aes(x=year, y=cumulative_risk, group = is_critical) ) +
  geom_point(aes(size = variant_risk,
                 fill=crew_member
                 )) +
  geom_line( aes(color=crew_member), alpha = 0.4)+
  geom_hline(linetype="dotted", yintercept= 1.5, color= "blue") +
  geom_hline(linetype="dotted", yintercept= 2, color= "red")

ggplotly(p2)


fill=alpha(crew_member, 0.2), stroke=0.3



# Output html ----
#rbokeh2html(plot1, file = "../output/plot1.html",
#            pretty = FALSE,
#            secure = TRUE)
#
#rbokeh2html(plot2, file = "../output/plot2.html",
#            pretty = FALSE,
#            secure = TRUE)



