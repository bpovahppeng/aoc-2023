# Front matter and prepping input for analysis----

library(tidyverse)
library(sf)
library(terra)

#reads in input as a 140 length character vector (140 characters in each element)
input = readLines('input/input_3.txt') 

## prep a function that converts a character vector into a matrix
convert_to_matrix = function(input_text){
  input_text %>% 
    str_split('') %>% 
    unlist() %>% 
    array(dim = c(length(input_text), str_length(input_text[[1]]))) %>%
    t()
}

#do conversion
input_matrix = input %>% 
  convert_to_matrix()

## find the special characters in the input, and escape them for use in regex
special_characters = input %>% 
  str_split('') %>% 
  unlist() %>% 
  .[. != ''] %>% 
  unique() %>% 
  str_subset('[^\\d|\\.]') %>% 
  str_replace_all('(\\[|\\]|\\(|\\)|\\{|\\}|\\^|\\$|\\*|\\+|\\?|\\.)', '\\\\\\1') %>% 
  paste0(collapse = '|')

## convert input into a raster
input_raster = rast(input_matrix)

## expand the y extent of the raster to lengthen each cell vertically
# this is the trick that lets us solve this problem spatially.
# if we don't expand the y size of each cell, then we won't be able to differentiate between horizontally
# and vertically adjacent numbers when combining adjacent cells into multi-digit numbers
ext(input_raster) = c(0, str_length(input[[1]]), 0, length(input) * 1.2)

plot(input_raster)

## make a lookup table of the encoded values and their decoded values
encoding = 
  data.frame(
    encoded = as.vector(values(input_raster)),
    decoded = values(input_raster, dataframe = TRUE) %>% pull(1) %>% as.character()
  ) %>% 
  distinct() %>% 
  arrange(encoded) %>% 
  mutate(
    is_number = str_detect(decoded, '\\d'),
    is_special = str_detect(decoded, special_characters),
    is_period = str_detect(decoded, '\\.')
  )

## create a polygon layer with the numeric cells from the input----
number_raster = clamp( #extract the numeric cells from the input using the lookup table
  input_raster, 
  lower = min(encoding$encoded[encoding$is_number]), 
  upper = max(encoding$encoded[encoding$is_number]),
  values = FALSE
  )

plot(number_raster)

number_vector = as.polygons(number_raster, aggregate = FALSE) %>% #convert the numeric cells into individual polygons
  st_as_sf() %>% 
  mutate(digit_id = row_number()) #add a unique id to each polygon, in the order they appear in the input

plot(number_vector['lyr.1'])

## create a polygon layer with the special character cells from the input----
special_raster = ifel( #extract the special character cells from the input, with NA for non-special characters
  input_raster %in% encoding$decoded[encoding$is_special], 
  input_raster, 
  NA
)

plot(special_raster)

special_vector = as.polygons(special_raster, aggregate = FALSE) %>% #convert to vector
  st_as_sf() %>% 
  left_join(encoding, by = c('lyr.1' = 'encoded')) #join to lookup table to get the decoded values

plot(special_vector['decoded'])

## make a buffer around the special characters to be able to sense adjacency
special_buffer = special_vector %>% 
  st_buffer(0.5) %>% 
  mutate(
    part_id = row_number() #add unique ids for each special character for grouping later
  )

plot(special_buffer['decoded'])

## collapse the numbers together such that horizontally adjacent numbers are in the same polygon, concatenated----
# from centroids of cells, buffer and dissolve
number_bubbles = st_centroid(number_vector) %>% 
  st_buffer(0.55) %>% #this buffer extends outside the number cells horizontally, but not vertically
  st_union() %>% #dissolve into a single polygon
  st_cast('POLYGON') %>% #split multi-part polygons into single-part polygons
  st_as_sf() %>% 
  mutate(number_id = row_number()) #add unique ids for each number for grouping

# join the number bubbles to the number vector for combining digits into multi-digit numbers
numbers = number_bubbles %>% 
  st_join(number_vector) %>% #join to number vector to get digit and digit ids
  arrange(digit_id) %>% #sort by digit id to make sure digits are in the right order
  group_by(number_id) %>% 
  summarize( #collapse digits into a single number
    number = as.numeric(paste0(lyr.1, collapse = '')),
    #the next two lines are just for troubleshooting
    numbers = list(lyr.1),
    digits = list(digit_id),
  )

plot(number_bubbles)

plot(numbers['number'])

# Part 1----

#intersect the numbers with the special buffer to get the numbers that are adjacent to special characters
part_numbers = numbers %>% 
  st_intersection(special_buffer)

#get rid of duplicates (some numbers may be adjacent to multiple special characters)
numbers_to_sum = part_numbers %>%
  group_by(number_id) %>% 
  summarize(
    number = number[1]
  )

plot(numbers_to_sum['number'])

part_1_answer = sum(numbers_to_sum$number) #527446

# Part 2----

#from the part numbers, we can apply some rules to get the gear ratios only
gear_ratios = part_numbers %>% 
  filter(
    .by = part_id, #group by unique special character
    decoded == '*', #only keep the numbers that are next to an asterisk
    n() == 2 #only keep the groups of numbers that have exactly two numbers next to each asterisk
  ) %>% 
  st_drop_geometry() %>% 
  summarize(
    .by = part_id,
    gear_ratio = number[1] * number[2] #multiply the two numbers together to get the gear ratio
  )

part_2_answer = sum(gear_ratios$gear_ratio) #73201705
