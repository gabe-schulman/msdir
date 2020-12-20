## Read in the data
library(readr)
library(magrittr)
source("oop_code.R")
## Load any other packages that you may need to execute your code
library(dplyr)
library(tidyr)

#create longitudinal data object

make_LD <- function(data, num_subs = 10) {
  structure(list(data = data, num_subs = num_subs), class = "LongitudinalData")
}

print.LongitudinalData <- function(x) paste("Longitudinal data with", x$num_subs, "subjects")

#create subject object that takes longitudinal data as input and has method to output info on subjects

subject <- function(ld, sub) {
  if (sub %in% ld$data$id) {
    ld$data %<>% filter(id == sub)
    structure(list(data = ld$data, sub = sub), class = "subject")
  }
}

print.subject <- function(s) paste("Subject ID:", s$sub)

summary.subject <- function(s) {
  s[[1]] %>% 
    group_by(visit, room) %>%
    summarize(value = mean(value)) %>%
    spread(room, value)
}

#create visit object that can filter by visit

visit <- function(subject, vis) {
  subject[[1]] %<>% filter(visit == vis)
  structure(list(data = subject[[1]], vis = vis, subject = subject[[2]]), class = "visit")
}

#create room object that can filter information by room

room <- function(visit, room) {
  visit$data %<>% filter(room == room)
  structure(list(data = visit$data, room = room, sub = visit$subject, vis = visit$vis), class = "room")
}

print.room <- function(room) {
  cat("ID:", room$sub, "\nVisit:", room$vis, "\nRoom:", room$room)
}

summary.room <- function(room) {
  summary(room[[1]]$value)
}

data <- read_csv("data/MIE.csv")
#output for text file
sink("oop_output.txt")
x <- make_LD(data)
print(class(x))
print(x)

## Subject 10 doesn't exist
out <- subject(x, 10)
print(out)

out <- subject(x, 14)
print(out)

out <- subject(x, 54) %>% summary
print(out)

out <- subject(x, 14) %>% summary
print(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)

## Show a summary of the pollutant values
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)
sink()
