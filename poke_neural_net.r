library(dplyr)
library(caTools)
library(neuralnet)
library(parallel)

input <- "~/projects/pokemon/raw_data/"
output <- "~/projects/pokemon/" # must end with a "/"

#####################################################################
#' @param categorical a vector of categorical values that have levels
#' @return 1-of-(C-1) effects-coding style list of vectors
numerify_categorical <- function(categorical){
  uniq <- unique(categorical[!is.na(categorical)]) # remove NA's for the love of god
  categorical <- factor(categorical, levels = uniq) 
  sorter <- lapply(uniq, function(x) {
    rtn <- integer(length(uniq))
    rtn[x] <- 1
    if(as.numeric(x) == length(rtn))
      rep(-1, length(rtn))
    else
      rtn
  })
  names(sorter) <- uniq
  return(unname(sorter[categorical]))
}

#' @param binary_data a vector of values that can be coerced to boolean
#' @return a vector of -1 and 1's
binarize_categorical <- function(binary_data)
  sapply(as.logical(binary_data), function(x) if(x) 1 else -1)

enumerate_categorical <- function(categorical){
  unlist(sapply(categorical, function(x) 
    ifelse(is.na(x), NA, which(levels(categorical) == x))
  ))
}
#####################################################################
#' @param x the index within the metadata list of the list to be stored
#' @return None
store_metadata <- function(x)
  write.csv(
    t(as.data.frame(metalists[[x]])), 
    paste0(output,"normalized/", names(metalists[x]),".csv"),
    row.names = FALSE, na = "")

#####################################################################
#' @param list the main list to store
#' @param metalists columns from the main list that are separate data.frames
#' @return None
store <- function(list, metalists){
  dir.create(paste0(output,"normalized/"), showWarnings = FALSE)
  write.csv(list, paste0(output,"normalized/","main.csv"), row.names = FALSE)
  invisible(lapply(1:length(metalists), store_metadata))
}

#####################################################################
#' @param pairs accepts a list of pairs of pokemon IDs
#' @param how_many how many of the pairs to expand 
#' @return a large data frame containing the stats of the pair of pokemon
make_battle <- function(pairs, how_many){
  if(how_many == "ALL")
    how_many = dim(pairs)[1]
  battles <- lapply(1:how_many, function(x){
    battle <- as.numeric(pairs[x,])
    p1 <- raw[battle[1],]
    p2 <- raw[battle[2],]
    rtn <- c(p1, p2)
  })
  col_names <- c(paste0(names(raw), 1), paste0(names(raw), 2))
  dat <- as.data.frame(matrix(unlist(battles), nrow=length(unlist(battles[1]))))
  dat <- t(dat)
  colnames(dat) <- col_names
  rownames(dat) <- NULL
  return(dat)
}
#' @param battles a dataframe of pokemon pair statistics
#' @param how_many the number of winners to add as a column
#' @return a new dataframe that has a winner column in front!
add_winner <- function(battles, how_many){
  winners <- as.data.frame(unname(unlist(train[3])[1:how_many]))
  colnames(winners) <- c("Winner")
  cbind(winners, battles)
}
#' @param how_many the number of winners to check
#' @return a vector of winner outcomes... i think. it's been a while since I wrote this
check_winners <- function(how_many){
  return(unname(unlist(test[3])[1:how_many]))
}
#####################################################################
generate_neural_net <- function(training_set, sampling_size){
  dat <- make_battle(training_set, sampling_size)
  dat <- add_winner(dat, sampling_size)
  # select features and turn into formula
  feats <- c(paste0(names(raw), 1), paste0(names(raw), 2))
  f <- paste(feats, collapse=' + ')
  f <- paste('Winner ~', f)
  f <- as.formula(f)  # Convert to formula
  
  # run
  ptm <- proc.time()
  nn <- neuralnet(f, dat, hidden=c(10, 10, 10), linear.output=FALSE)
  time_taken <- proc.time() - ptm
  
  # Check out the neural net
  # plot(nn)
  result <- paste("Net with sample of", sampling_size,"took:", time_taken[3], "seconds.\n")
  write(result, paste0(output, "performance.txt"), append = TRUE)
  print(result)
  return(nn)
}

test_neural_net <- function(nn){
  testing_size = dim(test)[1]
  testing <- make_battle(test, testing_size)
  # apply the neural net to some tests
  predicted <- compute(nn, testing)
  predicted$net.result <- sapply(predicted$net.result, round, digits=0)
  acc <- sum((predicted$net.result == check_winners(testing_size)))/testing_size
  cat("Neural Net of training size",length(nn$response),"\n\tAccuracy: ", acc*100, "%\n")
}
#####################################################################

# normalizing data
pokemon <- read.csv(paste0(input,"pokemon.csv"), na.strings=c("", NA))
pn <- pokemon %>% 
  rename(ID = X.,
         Type1 = Type.1,
         Type2 = Type.2,
         Sp.Atk = Sp..Atk,
         Sp.Def = Sp..Def,
         Gen = Generation) %>%
  mutate(Power = pmax(Attack, Sp.Atk)) %>% # Judge by their best offensive stat
  mutate(HP = as.numeric(scale(HP)), # normalize all numerics
         Attack = as.numeric(scale(Attack)), # assuming stats are normally distributed... 
         Defense = as.numeric(scale(Defense)), # qqnorm looks okay, so w/e
         Sp.Atk = as.numeric(scale(Sp.Atk)),
         Sp.Def = as.numeric(scale(Sp.Def)),
         Speed = as.numeric(scale(Speed)),
         Power = as.numeric(scale(Power))) %>%
  mutate(Type1 = numerify_categorical(Type1), # take care of categorical variables
         Type2 = numerify_categorical(Type2),
         Gen = numerify_categorical(Gen),
         Legendary = binarize_categorical(Legendary)) %>% # convert binary to numeric
  mutate(Type2 = lapply(Type2, function(x) ifelse(!is.null(x), x, NA))) # NULL -> NA

# determine which lists need to be separately stored
lists <- pn[which(!sapply(pn, class) == "list")] # this will grab the lists and ignore lists of lists (they're bad!)
# metalists <- pn[which(sapply(pn, class) == "list")]
# store(lists, metalists) 

set.seed(101)
training_data <- read.csv(paste0(input,"combats.csv")) %>%
  mutate(Winner = as.numeric(First_pokemon != Winner)) # numeric outcome

# pull apart data into training and testing
raw <- lists %>% select(-c(ID, Name))
split <- sample.split(training_data$Winner, SplitRatio = 0.20) # only leaves 10,000 training recs
train <- subset(training_data, split == TRUE) # but we don't run more than 8,000 at a time, so
test <- subset(training_data, split == FALSE)

# parallelize neural net generation
# no_cores <- detectCores() - 1
# cl <- makeCluster(no_cores, type="FORK")
# nn <- parLapply(cl, seq(5000, 5000, by=1000), 
#                 function(x) generate_neural_net(train, x))
# # print results && kill cluster
# valid_net <-  13
# invisible(sapply(nn, function(x) {if(length(x) == valid_net) test_neural_net(x)}))
# stopCluster(cl)
# save(nn, file=paste0(output, "neural_nets.rds")) # R object version
# dput(nn, "~/Projects/Pokemon/neural_net_object.r") # text version
                        
# Train neural net
nn <- generate_neural_net(train, sampling_size = 3000)

# Generate results with it
tests <- read.csv(paste0(input, "tests.csv"))
tests <- make_battle(tests, "ALL")
predicted <- compute(nn, tests)
winners <-  sapply(predicted$net.result, round, digits=0) + 1 # turn into the index
winning_pokemon <- as.data.frame(unlist(sapply(1:dim(tests)[1], function(x) tests[x,][winners[x]])))
names(winning_pokemon) <- "Winner"
write.csv(winning_pokemon, file=paste0(output, "winner.csv"))
