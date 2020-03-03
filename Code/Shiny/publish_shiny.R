install.packages('rsconnect')
library(rsconnect)
install.packages('lattice')
library(lattice)

rsconnect::setAccountInfo(name='ceciliasr', token='C1FAF8FF24FE0A80B987D4C2CC7C6FD4', secret='bYgYfvP2Z8NziH1bAmFpMycJbDKwHHXYTTEBX6uh')
rsconnect::deployApp("c:/TFM/Code/Shiny")
