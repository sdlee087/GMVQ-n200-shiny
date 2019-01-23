#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

##Preliminaries
x4 <- read.csv("x-n200.csv")
r4 <- read.csv("r-n200.csv")

ns <- nrow(r4)/2
ds <- ncol(r4)

K <- 1/r4[seq(2,2*ns,by=2),]
mu <- rbind(r4[seq(1,2*ns,by=2),]*K,x4)

mus <- c()
Ks <- c()
for(i in 1:ds) mus[i] <- paste0("mu",i)
for(i in 1:ds) Ks[i] <- paste0("K",i)
colnames(mu) <- mus
colnames(K) <- Ks

loglambda <- seq(0,-10, by = -0.2)

mu_tib <- as_tibble(mu)
K_tib <- as_tibble(K)
mu_tib <- mu_tib %>% add_column(loglambda = c(seq(0, -10, by=-0.2), -10.5))
K_tib <- K_tib %>% add_column(loglambda = seq(0, -10, by=-0.2))

mu_tib <- mu_tib %>% gather(key = "index", value = "mu", -loglambda) %>% mutate(index = factor(index, levels = mus))
K_tib <- K_tib %>% gather(key = "index", value = "K", -loglambda) %>% mutate(index = factor(index, levels = Ks))

shinyServer(function(input, output) {
  output$muPlot <- renderPlot({
    mu_tib %>% dplyr::filter(abs(loglambda - input$target) < 1e-4) %>% ggplot(aes(x=mu)) + geom_histogram(binwidth = 0.5) + coord_cartesian(xlim = c(-6, 6), ylim = c(0,120) )
  })
  output$KPlot <- renderPlot({
    K_tib %>% dplyr::filter(abs(loglambda - input$target) < 1e-4) %>% ggplot(aes(x=K)) + geom_histogram(binwidth = 0.5) + coord_cartesian(xlim = c(0, 12), ylim = c(0,120))
  })
  
})
