# Clean environment ------------------------------------------------------

rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(dplyr)
library(dts.quality)
library(ggplot2)
library(equivalence)

# Data in ----------------------------------------------------------------
data.in <- read_excel("PVAL01 comparisons.xlsx", skip = 2)
colnames(data.in)[4] <- "A"  #Without_Light"
colnames(data.in)[5] <- "B" #With_Light"

# Create differences ----------------------------------------------------
data.in$light.difference = data.in$B - data.in$A

data.in2 <- data.in %>%
  filter(light.difference <0.2)


# TOST -------------------------------------------------------------------

t_test_df <- data.frame(
        ttest = numeric(),
        TOST = numeric()
)

for (i in 1:10) {

data1 <- data.in2[1:(i+1),]

r1 <- t.test(data1$A, data1$B)
t_test_df[i,1] = r1$p.value

r2 <- tost(data1$A, data1$B, 1)
t_test_df[i,2] = r2$tost.p.value
}

t_test_df$row_n <- as.numeric(rownames(t_test_df))

plot2t <- ggplot(t_test_df, aes(row_n)) +
        geom_point(aes(y=ttest), size=4, shape=21, colour = "black", fill = "cornflowerblue") +
        geom_point(aes(y=TOST), size=4, shape=21, colour = "darkgreen", fill = "beige") +
        geom_hline(yintercept = 0.05, lty=2,col = "red")
plot2t

r1
r2
