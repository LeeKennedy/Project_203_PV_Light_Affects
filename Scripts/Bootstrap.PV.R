# Clean environment ------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(ggplot2)
library(dplyr)

# Seed data --------------------------------------------------------------
x_bar1 <- 1
x_sd1 <- 0.01
#---------------
x_bar2 <- 1.02
x_sd2 <- 0.01
#---------------
Accept <- 0.01

# Populate database -----------------------------------------------------

A <- rep(rnorm(10000, x_bar1, x_sd1))
B <- rep(rnorm(10000, x_bar2, x_sd2))
Diff <- B-A

bs_df <- data.frame(cbind(A,B,Diff))

# Plot histogram --------------------------------------------------------
bs_plot <- ggplot(bs_df, aes(x=Diff)) +
        geom_histogram(fill = "beige", colour = "darkgreen", bins = 50) +
        geom_vline(xintercept = Accept, lty = 2, col = "Red")
        
bs_plot
