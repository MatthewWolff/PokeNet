library(dplyr)
library(caTools)
library(neuralnet)
library(parallel)

# Customizable Constants?
input_dir <- "~/projects/pokemon/raw_data/"
output_dir <- "~/projects/pokemon/" # must end with a "/"

USE_TYPE <- TRUE
type_matchup <- "type_matchup"

sample_sizes_to_test <- seq(1000, 2000, by=1000)

#####################################################################
# Data Cleaning/Transforming Methods 

#' @param binary_data a vector of values that can be coerced to boolean
#' @return a vector of -1 and 1's
binarize_categorical <- function(binary_data)
  sapply(as.logical(binary_data), function(x) if(x) 1 else -1)

# encodes types as an array of 1's and 0's
#' @param type1 pokemon's primary type
#' @param type2 pokemon's secondary type (can be NA)
#' @return an array of 0's and 1's representing a pokemon's type(s)
#' @note reference TYPE_LIST to translate
encode_type <- function(type1, type2) { # NOTE: unused
  uniq <- c(type1, type2) %>% .[!is.na(.)] %>% unique %>% sort
  type_num <- 0:length(uniq) %>% `names<-`(c("NA", uniq)) # NA is type 0
  
  modify_type_array <- function(combined_type_string) { 
    types <- strsplit(combined_type_string, " ") %>% unlist
    type_array <- integer(length(uniq))
    type_array[type_num[types]] <- 1 # if NA, won't get set
    type_array 
  }
  encoded_types <- paste(type1, type2) %>% lapply(modify_type_array)
  encoded_types
}

#' @param type_list1 pokemon 1's encoded type
#' @param type_list2 pokemon 2's encoded type
#' @return who will win and to what degree (negative indicates pokemon 1)
determine_type_advantage <- function(type_list1, type_list2) {
  type_effect <- function(type_list) type_chart[type_list[1], type_list[2]] # helper
  
  p1_types <- type_list1 %>% sapply(as.logical) %>% TYPE_LIST[.]
  p2_types <- type_list2 %>% sapply(as.logical) %>% TYPE_LIST[.]
  
  get_matchup_effectiveness <- function(types1, type2) 
    expand.grid(types1, type2) %>% split(seq(nrow(.))) %>% # cartesian product
    lapply(unlist) %>% lapply(as.character) %>% # re-format
    lapply(type_effect) %>% unlist %>% unname # feed into type_chart
  
  # sum up their possible matchups and determine who has better typing
  p1_attacking <- get_matchup_effectiveness(p1_types, p2_types) %>% sum
  p2_attacking <- get_matchup_effectiveness(p2_types, p1_types) %>% sum
  
  if (p1_attacking == p2_attacking) type_factor <- 0
  else type_factor <- if_else(p1_attacking > p2_attacking, -1, 1)
  type_factor*pmax(p1_attacking, p2_attacking) %>% `names<-`(type_matchup)
}

#'@example type_chart["Fire", "Water"] -> 0.5  # not effective!
type_chart ="
   Normal      1.0 2.0 1.0 1.0 1.0 1.0 1.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
   Fighting    1.0 1.0 2.0 1.0 1.0 0.5 0.5 1.0 1.0 1.0 1.0 1.0 1.0 2.0 1.0 1.0 0.5 2.0
   Flying      1.0 0.5 1.0 1.0 0.0 2.0 0.5 1.0 1.0 1.0 1.0 0.5 2.0 1.0 2.0 1.0 1.0 1.0
   Poison      1.0 0.5 1.0 0.5 2.0 1.0 0.5 1.0 1.0 1.0 1.0 0.5 1.0 2.0 1.0 1.0 1.0 0.5
   Ground      1.0 1.0 1.0 0.5 1.0 0.5 1.0 1.0 1.0 1.0 2.0 2.0 0.0 1.0 2.0 1.0 1.0 1.0
   Rock        0.5 2.0 0.5 0.5 2.0 1.0 1.0 1.0 2.0 0.5 2.0 2.0 1.0 1.0 1.0 1.0 1.0 1.0
   Bug         1.0 0.5 2.0 1.0 0.5 2.0 1.0 1.0 1.0 2.0 1.0 0.5 1.0 1.0 1.0 1.0 1.0 1.0
   Ghost       0.0 0.0 1.0 0.5 1.0 1.0 0.5 2.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 2.0 1.0
   Steel       0.5 2.0 0.5 0.0 2.0 0.5 0.5 1.0 0.5 2.0 1.0 0.5 1.0 0.5 0.5 0.5 1.0 0.5
   Fire        1.0 1.0 1.0 1.0 2.0 2.0 0.5 1.0 0.5 0.5 2.0 0.5 1.0 1.0 0.5 1.0 1.0 0.5
   Water       1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.5 0.5 0.5 2.0 2.0 1.0 0.5 1.0 1.0 1.0
   Grass       1.0 1.0 2.0 2.0 0.5 1.0 2.0 1.0 1.0 2.0 0.5 0.5 0.5 1.0 2.0 1.0 1.0 1.0
   Electric    1.0 1.0 0.5 1.0 2.0 1.0 1.0 1.0 0.5 1.0 1.0 1.0 0.5 1.0 1.0 1.0 1.0 1.0
   Psychic     1.0 0.5 1.0 1.0 1.0 1.0 2.0 2.0 1.0 1.0 1.0 1.0 1.0 0.5 1.0 1.0 2.0 1.0
   Ice         1.0 2.0 1.0 1.0 1.0 2.0 1.0 1.0 2.0 2.0 1.0 1.0 1.0 1.0 0.5 1.0 1.0 1.0
   Dragon      1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.5 0.5 0.5 0.5 1.0 2.0 2.0 1.0 2.0
   Dark        1.0 2.0 1.0 1.0 1.0 1.0 2.0 0.5 1.0 1.0 1.0 1.0 1.0 0.0 1.0 1.0 0.5 2.0
   Fairy       1.0 0.5 1.0 2.0 1.0 1.0 0.5 1.0 2.0 1.0 1.0 1.0 1.0 1.0 1.0 0.0 0.5 1.0" %>% 
  strsplit("\n") %>% sapply(strsplit, " ") %>% .[-1]  %>% sapply(function(x) x[which(x != "")]) %>%
  as_tibble %>% `colnames<-`(.[1,]) %>% .[-1,] %>% mutate_all(as.numeric) %>% `rownames<-`(names(.)) 

#####################################################################
# Input Ingesting Methods (turning two pokemon IDs into NN data)

#' @param pairs accepts a list of pairs of pokemon IDs
#' @param how_many how many of the pairs to expand 
#' @return a large data frame containing the stats of the pair of pokemon
make_battle <- function(pairs, how_many="ALL", use_type=F){
  if(how_many == "ALL") how_many = dim(pairs)[1]
  
  battles <- 1:how_many %>% lapply(function(x){
    battle <- as.numeric(pairs[x,])
    p1 <- stripped[battle[1],] 
    p2 <- stripped[battle[2],]
    p1.type <- pn[battle[1], "Type"] %>% unlist %>% unname
    p2.type <- pn[battle[2], "Type"] %>% unlist %>% unname
    if(use_type) {
      c(p1, p2, determine_type_advantage(p1.type, p2.type)) 
    } else {
      c(p1, p2) 
    }
  })
  
  pokemon_stats <- stripped
  pokemon1_cols <- paste0(names(pokemon_stats), 1)
  pokemon2_cols <- paste0(names(pokemon_stats), 2)
  
  if(use_type) {
    dat <- as.data.frame(matrix(unlist(battles), nrow=length(unlist(battles[1])))) %>% t %>%
      `colnames<-`(c(pokemon1_cols, pokemon2_cols, type_matchup)) %>% `rownames<-`(NULL)
    dat[,type_matchup] <- scale(dat[,type_matchup]) # normalize the new column
  } else {
    dat <- as.data.frame(matrix(unlist(battles), nrow=length(unlist(battles[1])))) %>% t %>%
      `colnames<-`(c(pokemon1_cols, pokemon2_cols)) %>% `rownames<-`(NULL)
  }
  
  dat
}

#' @param battles a dataframe of pokemon pair statistics, with no winner
#' @param how_many the number of rows of winners to add as a column
#' @return a new dataframe that has a winner column in front!
add_winner <- function(battles, how_many){
  winners <- as.data.frame(unname(train[3] %>% unlist %>% .[1:how_many]))
  colnames(winners) <- c("Winner")
  cbind(winners, battles)
}

#' @param how_many the number of winners to check
#' @return a vector of winner outcomes... i think. it's been a while since I wrote this
check_winners <- function(how_many){
  test[3] %>% unlist %>% .[1:how_many]
}

#####################################################################
# Neural Net Methods

#' @param sampling_size how many rows of the training set to use
#' @return a neural net that has been trained with the specified sampling_size
generate_neural_net <- function(sampling_size){
  dat <- train %>% make_battle(sampling_size, use_type=USE_TYPE) %>% add_winner(sampling_size)
  
  ## select features and turn into formula
  feats <- names(dat[,-1]) # drop "Winner" or else it'll know who wins LOL
  f <- paste(feats, collapse=" + ") %>% paste("Winner ~", .) %>% as.formula
  
  ## run
  ptm <- proc.time()
  nn <- neuralnet(f, dat, hidden=c(20, 20, 20), linear.output=FALSE)
  time_taken <- proc.time() - ptm
  
  ## Check out the neural net
  # plot(nn)
  result <- paste("Net with sample of", sampling_size,"took:", time_taken[3], "seconds.\n")
  write(result, paste0(output_dir, "performance.txt"), append = TRUE)
  print(result)
  nn
}

#' @param nn a neural net that can determine the winner, given 2 pokemon IDs
#' @return the size of training samples it had and the accuracy it could predict with
test_neural_net <- function(nn){
  testing_size = dim(test)[1]
  testing <- make_battle(test, testing_size, use_type = USE_TYPE)
  # apply the neural net to some tests
  predicted <- compute(nn, testing)
  predicted$net.result <- sapply(predicted$net.result, round, digits=0)
  acc <- sum((predicted$net.result == check_winners(testing_size)))/testing_size
  cat("Neural Net of training size", length(nn$response),"\n\tAccuracy: ", acc*100, "%\n")
}

#' @param sample_sizes_to_test a vector of sample sizes with which to train a neural network
#' @return a series of neural nets, trained to the specified degrees
parallel_train_nets <- function(sample_sizes_to_test) {
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores, type="FORK")
  nn <- cl %>% parLapply(sample_sizes_to_test, function(x) generate_neural_net(x))
  stopCluster(cl)
  nn
}

#' @param nn the neural nets to test - they will be checked for validity (random corruption happens? lol)
#' @return None. Output will be produced in stdout
test_nets <- function(nn) {
  valid_net <-  13 # if something screws up and it's invalid, toss it out ig lol
  sapply(nn, function(x) {if(length(x) == valid_net) test_neural_net(x)}) %>% invisible
}

#####################################################################
### MAIN EXECUTION
pokemon <- read.csv(paste0(input_dir,"pokemon.csv"), na.strings=c("", NA), stringsAsFactors = F) %>% 
  rename(
    ID     = X.,
    Type1  = Type.1,
    Type2  = Type.2,
    Sp.Atk = Sp..Atk,
    Sp.Def = Sp..Def,
    Gen    = Generation
  ) %>% as.tibble
pn <- pokemon  %>%
  mutate( # Judge by their best offensive stat
    Power     = pmax(Attack, Sp.Atk) 
  ) %>% 
  mutate( # normalize all numerics
    HP        = as.numeric(scale(HP)), 
    Attack    = as.numeric(scale(Attack)), # assuming stats are normally distributed... 
    Defense   = as.numeric(scale(Defense)), # qqnorm looks okay, so w/e
    Sp.Atk    = as.numeric(scale(Sp.Atk)),
    Sp.Def    = as.numeric(scale(Sp.Def)),
    Speed     = as.numeric(scale(Speed)),
    Power     = as.numeric(scale(Power)),
    Type      = encode_type(Type1, Type2)
  ) %>%
  mutate( # convert categorical variables to numerics
    Legendary = binarize_categorical(Legendary)
  )
TYPE_LIST <- c(pn$Type1, pn$Type2) %>% unique %>% sort 

set.seed(101)
training_data <- 
  paste0(input_dir,"combats.csv") %>% read.csv %>%
  mutate(Winner = as.numeric(First_pokemon != Winner)) # must be numeric outcome

## pull apart data into training and testing
stripped <- pn %>% select(-c(ID, Name, Type1, Type2, Type, Gen))
split <- training_data$Winner %>% sample.split(SplitRatio = 0.20) # only leaves 10,000 training recs
train <- training_data %>% subset(split == TRUE) # but we don't run more than 8,000 at a time ^, so
test  <- training_data %>% subset(split == FALSE)

## parallelize neural net generation
nn <- sample_sizes_to_test %>% parallel_train_nets()

## test the nets. 
test_nets(nn)

## Save results?
# save(nn, file=paste0(output, "neural_nets.rds")) # R object version
# dput(nn, "~/Projects/Pokemon/neural_net_object.r") # text version

## Train neural net on data for which we do not have results
# nn <- generate_neural_net(sampling_size = 1000)

# tests <- read.csv(paste0(input, "tests.csv")) %>% make_battle() # prep data
# predicted <- compute(nn, tests)
# winners <- predicted$net.result %> sapply(round, digits=0) + 1 # R-indexing starts at 1 smh
# winning_pokemon <- as.data.frame(unlist(sapply(1:dim(tests)[1], function(x) tests[x,][winners[x]])))
# names(winning_pokemon) <- "Winner"
# write.csv(winning_pokemon, file=paste0(output, "winner.csv"))
