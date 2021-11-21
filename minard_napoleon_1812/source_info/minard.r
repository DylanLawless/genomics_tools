source("~/Documents/ggplot/ggplot/load.r")

troops <- read.table("troops.txt", header=T)
cities <- read.table("cities.txt", header=T)
temps <- read.table("temps.txt", header=T)
temps$date <- as.Date(strptime(temps$date,"%d%b%Y"))

# library(maps)
# borders <- data.frame(map("world", xlim=c(10,50), ylim=c(40, 80), plot=F)[c("x","y")])

xlim <- scale_x_continuous(limits = c(24, 39))

ggplot(cities, aes(x = long, y = lat)) + 
geom_path(
  aes(size = survivors, colour = direction, group = group), 
  data=troops
) + 
geom_point() + 
geom_text(aes(label = city), hjust=0, vjust=1, size=4) + 
scale_size(to = c(1, 10)) + 
scale_colour_manual(values = c("grey50","red")) +
xlim




ggsave(file = "march.pdf", width=16, height=4)

qplot(long, temp, data=temps, geom="line") + 
geom_text(aes(label = paste(day, month)), vjust=1) + xlim

ggsave(file = "temps.pdf", width=16, height=4)