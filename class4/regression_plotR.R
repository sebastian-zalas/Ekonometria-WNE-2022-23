### Linear Regression plot ###
#install.packages("ggplot2")
library(ggplot2)

# cleaning
rm(list = ls())
set.seed(1234)

# working directory - set you preferred folder
# setwd()

#Generate the independent variable and the error
x1 = rnorm(30, 1, 2)
error = rnorm(30, 0, 10)

#Generate dependent variable
y = 1 + 2*x1 + error

#Data frame
data = cbind(x1, y)
data = as.data.frame(data)

#estimate the model
model = lm(y ~ x1)

# calculate residuals and predicted values
res <- signif(residuals(model), 5)
y_hat <- predict(model)

#prepare plot
plot1 = ggplot(data, aes(x=x1, y=y)) +
        geom_point(alpha=5, shape=18, colour="darkorange2", size = 5)
plot1 = plot1 + theme_bw() + ylab("y (zmienna objaśniana)") + 
        xlab("x1 (zmienna objaśniająca)") + 
        theme(legend.position = 'bottom', 
              legend.direction = "horizontal", 
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12))
plot1

plot2 = ggplot(data, aes(x=x1, y=y)) +
        geom_point(alpha=5, shape=18, colour="darkorange2", size = 5) +
        geom_smooth(se= F , method=lm, color = "aquamarine4", size=1.1)
plot2 = plot2 + theme_bw() + ylab("y (zmienna objaśniana)") + 
        xlab("x1 (zmienna objaśniająca)") + 
        theme(legend.position = 'bottom', 
              legend.direction = "horizontal", 
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12))
plot2

plot3 = ggplot(data, aes(x=x1, y=y)) +
        geom_point(alpha=5, shape=18, colour="darkorange2", size = 5) +
        geom_smooth(se= F , method=lm, color = "aquamarine4", size=1.1)+
        geom_segment(aes(x = x1, y = y, xend = x1, yend = y_hat), color="brown1", size=1, data = data)
plot3 = plot3 + theme_bw() + ylab("y (zmienna objaśniana)") + 
        xlab("x1 (zmienna objaśniająca)") + 
        theme(legend.position = 'bottom', 
              legend.direction = "horizontal", 
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12))
plot1
plot2
plot3

#save plot
png(filename="class4_regression_plot1.png", width = 800, height = 480)
plot1
dev.off()

png(filename="class4_regression_plot2.png", width = 800, height = 480)
plot2
dev.off()

png(filename="class4_regression_plot3.png", width = 800, height = 480)
plot3
dev.off()
