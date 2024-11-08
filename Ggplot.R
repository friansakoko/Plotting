#Basic Plot----
plot(x = 1:10,
     y = 1:10,
     xlab = "X Axis label",
     ylab = "Y Axis label",
     main = "Main Title")


#Basic Color----
data("longley")

plot(x = longley$GNP, 
     y = longley$Unemployed, 
     col = "red",     
     pch = 16,         
     main = "GNP vs Unemployment with Custom Colors by Year",
     xlab = "Gross National Product (GNP)", 
     ylab = "Number of Unemployed")

#Basic Rainbow----
data("longley")

colors <- rainbow(length(unique(longley$Year)))[as.factor(longley$Year)]

plot(x = longley$GNP, 
     y = longley$Unemployed, 
     col = colors,     
     pch = 16,         
     main = "GNP vs Unemployment with Color by Year",
     xlab = "Gross National Product (GNP)", 
     ylab = "Number of Unemployed")

#Basic Histogram----
data("longley")

hist(x = longley$GNP,
     main = "GNP Distribution in the Longley Dataset",
     xlab = "Gross National Product (GNP)",
     ylab = "Frequency",
     breaks = 20,         # 20 bins
     xlim = c(min(longley$GNP), max(longley$GNP)),  
     col = "lightblue",   
     border = "darkblue") 

#Basic Barplot----
library(yarrr)
data("longley")

longley$YearGroup <- ifelse(longley$Year < 1955, "Before 1955", "1955 and After")

pirateplot(formula = Employed ~ YearGroup,
           data = longley,
           main = "Pirate Plot of Employment by Year Group",
           xlab = "Year Group",
           ylab = "Number of Employed",
           pal = "info",         # Color palette
           bean.f.o = .6,        # Bean fill opacity
           point.o = .8)         # Point opacity

#Instal Yarr Package----

install.packages("yarr")

#Basic Pirateplot----
library(yarrr)

data("longley")

longley$YearGroup <- ifelse(longley$Year < 1955, "Before 1955", "1955 and After")
longley$YearGroup <- factor(longley$YearGroup, levels = c("Before 1955", "1955 and After"))

# Create a pirate plot with a theme
pirateplot(formula = Employed ~ YearGroup,
           data = longley,
           theme = 4,                 
           main = "Employment by Year Group",
           xlab = "Year Group",
           ylab = "Number of Employed",
           pal = "info") 

#Basic abline vertical and horizontal----
data("longley")

plot(x = longley$GNP, 
     y = longley$Unemployed, 
     main = "Scatter Plot of GNP vs. Unemployed with Regression Line",
     xlab = "Gross National Product (GNP)", 
     ylab = "Number of Unemployed",
     pch = 16,                  # Shape of the points
     col = "darkblue")          # Color of the points

abline(h = 450, col = "red", lwd = 2, lty = 2)  
abline(h = 200, col = "red", lwd = 2, lty = 2)  

abline(v = 250, col = "green", lwd = 2, lty = 3)  
abline(v = 550, col = "green", lwd = 2, lty = 3)  

#Basic abline regresi----
data("longley")

plot(x = longley$GNP, 
     y = longley$Unemployed, 
     main = "Scatter Plot of GNP vs. Unemployed with Regression Line",
     xlab = "Gross National Product (GNP)", 
     ylab = "Number of Unemployed",
     pch = 16,                  # Shape of the points
     col = "darkblue")          # Color of the points

model <- lm(Unemployed ~ GNP, data = longley)
abline(model, col = "red", lwd = 2)  # lwd sets line thickness

#Basic grid and text----
data("longley")

plot(x = longley$GNP, 
     y = longley$Unemployed, 
     main = "Scatter Plot of GNP vs. Unemployed with Regression Line",
     xlab = "Gross National Product (GNP)", 
     ylab = "Number of Unemployed",
     pch = 16,                  # Shape of the points
     col = "darkblue")          # Color of the points

model <- lm(Unemployed ~ GNP, data = longley)
abline(model, col = "red", lwd = 2)  # lwd sets line thickness

grid()              #add grid

text(x = 450,       #add text
     y = 350,
     labels = "Regresi",
     pos =3
)

#Basic legend----
data("longley")

plot(x = longley$GNP, 
     y = longley$Unemployed, 
     main = "Scatter Plot of GNP vs. Unemployed with Regression Line",
     xlab = "Gross National Product (GNP)", 
     ylab = "Number of Unemployed",
     pch = 16,                  # Shape of the points
     col = "darkblue")          # Color of the points

model <- lm(Unemployed ~ GNP, data = longley)
abline(model, col = "red", lwd = 2)  # lwd sets line thickness

grid()

legend("topleft",                               
       legend = c("Data Points", "Regression Line"), 
       col = c("darkblue", "red"),       # Colors for each element
       pch = c(16, NA),                  # Point shapes; NA for no symbol
       lty = c(NA, 1),                   # Line types; 1 solid, 2 dashed
       lwd = c(NA, 2),                   # Line widths
       bty = "1",
       box.col = 'lightgray',
       box.lwd = 2)                               

#Basic legend 2----
data("longley")

plot(x = longley$GNP, 
     y = longley$Unemployed, 
     main = "Scatter Plot of GNP vs. Unemployed with Regression Line",
     xlab = "Gross National Product (GNP)", 
     ylab = "Number of Unemployed",
     pch = 16,                  # Shape of the points
     col = "darkblue")          # Color of the points

model <- lm(Unemployed ~ GNP, data = longley)
abline(model, col = "red", lwd = 2)  # lwd sets line thickness
abline(h = 450, col = "red", lwd = 2, lty = 2)  # Red dashed line

grid()

# Add a legend
legend("topright",                               # Position of the legend
       legend = c("Data Points", "Regression Line", "Horizontal Line"), # Text labels
       col = c("darkblue", "red", "red"),       # Colors for each element
       pch = c(16, NA, NA),                     # Point shapes; NA for no symbol
       lty = c(NA, 1, 2),                       # Line types; 1 for solid, 2 for dashed
       lwd = c(NA, 2, 2),                       # Line widths
       bty = "o")                               # Box around the legend

#Install ggplot ----

install.packages("ggplot2")

#Gggplot: Table ----

install.packages("ISLR")

library(ISLR)
data(Credit)
View(Credit)

#GG layer 1 : Data ----
library(ggplot2)
library(ISLR)
data(Credit)

ggplot(data = Credit)

#GG layer 2 : Aesthetic ----

library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student))

#GG layer 3 : Geometric ----

library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point()     #Geometric layer

#GG layer 4 : Facets ----

library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  facet_wrap(~ Student)    #facet layer

#GG layer 5 : Statistics ----

library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  facet_wrap(~ Student)+
  geom_smooth(method = "lm", se = TRUE)       #Statistic layer

#GG layer 6 : Coordinates ----

library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  facet_wrap(~ Student)+
  geom_smooth(method = "lm", se = TRUE) +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 2500))    #Coordinates layer

#GG layer 7 : Theme ----

library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Student) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 2000)) +
  theme_minimal()  #Theme layer 

#ggplot Basic: Aesthetic----

library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Income, 
                   y = Balance, 
                   color = Married, 
                   size = Education,
                   shape = Gender,
                   alpha = Rating
                   )) +
  geom_point()

#ggplot Basic: Geometric geom_point----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Income, 
                   y = Balance, 
                   #color = Married, 
                   #size = Education,
                   #shape = Gender,
                   #alpha = Rating
                  )) +
  geom_point(color = "red",
             #size = 10,
             #shape = 17,
             alpha = 0.5,
             )

#ggplot Basic: Geometric geom_line----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Income, 
                   y = Gender, 
                   color = Student, 
                   #size = Education,
                   #shape = Gender,
                   #alpha = Rating
                   )) +
    geom_line(
              #color = "red",
              size = 2,
              #shape = 5,
              #alpha = 0.5,
              )

#ggplot Basic: Geometric geom_boxplot----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Student, 
                   y = Income,
                   color = Married, 
                   fill = Married
)) +
  geom_boxplot()

#ggplot Basic: Geometric geom_violinplot----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Student, 
                   y = Income,
                   color = Married, 
                   fill = Married
)) +
  geom_violin(trim = FALSE)

#ggplot Basic: Geometric geom_histogram----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Income, 
                   color = Married, 
                   fill = Married
                   )) +
  geom_histogram(bins = 50, alpha = 0.6)

#ggplot Basic: Geometric geom_densityplot----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Income, 
                   color = Married, 
                   fill = Married
)) +
  geom_density()


#ggplot Basic: Geometric geom_bar----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Student, y = Balance, fill = Student)) + 
  geom_bar(stat = "identity")

#ggplot Advance: facet_wrap----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Student, 
                   y = Income, 
                   color = Student
                   )) +
  geom_boxplot() +
  facet_wrap(~interaction(Married,Gender) 
            )
#ggplot Advance: facet_wrap histogram----

library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Income, 
                   fill = Student,
                   )) +
  geom_histogram(bins = 30, Alpha = 0.7) +      
  facet_wrap(~interaction(Married,Gender)
             )

#ggplot Advance: Statistic geom_smooth 1----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  facet_wrap(~ Student)+
  geom_smooth(method = "gam",    #lm, loess, gam
              se = TRUE)

#ggplot Advance: Statistic geom_smooth 2----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  facet_wrap(~ Student)+
  geom_smooth(method = "gam",    #lm, loess, gam
              se = TRUE)+
  geom_smooth(method = "lm")

#ggplot Advance: Statistic geom_density_2d----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  facet_wrap(~ Student)+
  geom_density_2d()

#ggplot Advance: Statistic geom_quantile----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  facet_wrap(~ Student)+
  geom_quantile(quantiles = c(0.25, 0.5, 0.75))

#ggplot Advance: Coordinate reverse----
library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  facet_wrap(~ Student)+
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_reverse()

#ggplot Advance: Coordinate coor_polar----
library(ggplot2)
library(ISLR)
data(Credit)
names(Credit)

ggplot(Credit, aes(x = Student, y = Balance, fill = Student)) + 
  geom_bar(stat = "identity")+
  coord_polar(theta = "y")

#ggplot Advance: Theme Axis----
library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +
  facet_wrap(~ Student) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 2000)) +
  theme(
    panel.background = element_rect(fill = "white"),  # Background color
    # Axis titles
    axis.title.x = element_text(size = 19),  # x-axis title font size
    axis.title.y = element_text(size = 16),  # y-axis title font size
    # Axis text
    axis.text.x = element_text(size = 10),  # x-axis text font size
    axis.text.y = element_text(size = 15)  # y-axis text font size
  )
#ggplot Advance: Theme Grid----
library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +
  facet_wrap(~ Student) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 2000)) +
  theme(
    panel.background = element_rect(fill = "white"),  # Background color
    # Grid lines
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),  
    panel.grid.minor = element_line(),  # Minor gridlines
    
  )

#ggplot Advance: Theme Legend 1----
library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +
  theme(
    legend.position = ("top"),  # inside: c(0.2, 0.8), outside: ("top")(bottom")
    legend.text = element_text(size = 15),  
    legend.title = element_text(size = 16, face = "bold"),  #"bold","italic", 
    legend.background = element_rect(fill = "white", color = "black",linetype = "dashed"),
    #legend.background = element_blank() #removes the background (box)
  )
#ggplot Advance: Theme Legend 2----
library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", aes(color = "Regression Line")) +
  theme(
    legend.position = "top",  # Position of the legend
    legend.text = element_text(size = 10),  
    legend.title = element_text(size = 10, face = "bold"),  
    legend.background = element_rect(fill = "white", color = "black", linetype = "dashed")
  ) +
  labs(
    color = "Legenda"  # Set the title of the legend
  ) +
  scale_color_manual(values = c("blue", "red", "green"),  # Add one more color for regression line
                     labels = c("Student", "No Student", "Regression Line")
  ) 

#ggplot Advance: Theme Annotate----
library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +
  theme() +
  annotate("text", 
           x = 100, y = 1000,  # Position of the text
           label = "Top Secret",  # The text you want to add
           size = 20, color = "black",  # Text size and color
           fontface = "bold", # Text style
           angle = 45,
           alpha = 0.2
           )  

#ggplot Advance: Theme Symbol----

library(ggplot2)
library(ISLR)
data(Credit)
head(Credit)

ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +
  theme() +
  labs(
    x = expression("Income ("~mu~")"),  # ("~sigma^2~"),("~mu~")
    y = expression("Balance "(a[down]^up))  # subscript
  )
