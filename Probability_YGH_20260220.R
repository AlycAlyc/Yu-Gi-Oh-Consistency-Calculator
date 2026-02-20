###### Yu-Gi-Oh! Consistency calculator (v0.8)
###### Author: Alyc Hu
###### Date: 2026/Feb/20
###### Purpose: to calculate the consistency of a deck by simulating opening 
###### hands. Consistency is defined as opening with hands that allows for at
###### least one line of play.

###### IMPORTANT NOTE: this version of the code do not support card search with
###### punctuation, diacritics, and certain special characters (e.g., &). 
###### Please remove these characters when type in the full card name.
###### Example: The fallen & the virtuous --> The fallen the virtuous

################################################################################
###### Version update note (v0.8): Test for "dead draw" combos.           ######
###### Example: Pre-prep is a dead card if drawn with Mirror              ######
################################################################################
# Load in required packages
library(openxlsx)
library(tidyr)
library(dplyr)
library(httr)
library(rvest)
library(xml2)
library(stringr)
library(gdata)
# Set up work directory
setwd("/Users/yangchenghu/Desktop/Yugioh")

### 0. Set up the deck data.frame
# The deck needs to first be input into an Excel file with the following variables
# a. Card name
# b. Card category (monster, spell, trap)
# c. Card quantity
# d. Function (Engine, Draw, Handtrap, Boardbreaker, Brick)
# Then, put your combos in the second worksheet within the same Excel file
# Note, you don't need to input a two- or three-card combo if it contains
# any one-card starter.
### 1. Input data.frame
deck.name <- c("YGO_Mitsurugi.xlsx")
My.deck <- openxlsx::read.xlsx(deck.name, sheet="Deck_list")
Full.combo <- openxlsx::read.xlsx(deck.name, sheet="Full_combo")
Half.combo <- openxlsx::read.xlsx(deck.name, sheet="Half_combo")
Dead.draw <- openxlsx::read.xlsx(deck.name, sheet="Dead_draw")

# Read in full card info from the Konami website
# Add new columns to store the information
temp.frame <- setNames(data.frame(matrix(ncol = 7, 
                                         nrow = nrow(My.deck))), 
                       c("Card.Name.System", "Attribute", "Type", "Level", "ATK", "DEF", "Effect"))
My.deck.wf <- cbind(My.deck,temp.frame)

for (k in 1:nrow(My.deck.wf)){   
  search.card <- My.deck[k,]
  search.card.name <- My.deck[k,c("Card.Name")]
      # Submit search request
      url <- paste0("https://www.db.yugioh-card.com/yugiohdb/card_search.action?ope=1&keyword=",gsub(" ","+",search.card.name))
      response <- httr::GET(url)
      webpage <- read_html(httr::content(response, "text", encoding = "UTF-8"))
      # Each card in the results is in an element with class ".t_row"
      entry <- webpage %>% html_nodes(".t_row") %>% .[[1]]
      # Built-in error message
      if (length(entry)==0) {
        stop(paste0("No search results found for ", search.card.name,". Please check the spelling before running the code again."))
      }
      # Check the card category to determine the type of search is required
      if (search.card$Card.Type %in% c("Monster","monster","MONSTER")) {
        # Full card name
        My.deck.wf[k,c("Card.Name.System")] <- entry %>%
          html_node(".card_name") %>%
          html_text(trim = TRUE)
        # Extract attribute
        My.deck.wf[k,c("Attribute")] <- entry %>%
          html_node(xpath = ".//span[contains(@class,'attribute')]") %>%
          html_text(trim = TRUE)
        # Extract type
        My.deck.wf[k,c("Type")] <- entry %>%
          html_node(xpath = ".//span[contains(@class,'card_info_species_and_other_item')]") %>%
          html_text(trim = TRUE) %>% 
          gsub("[\n\t]+", "",.)
        # Level
        My.deck.wf[k,c("Level")] <- entry %>%
          html_node(xpath = ".//*[contains(text(),'Level')]") %>%
          html_text(trim = TRUE) %>% 
          gsub("\\D", "",.) %>%
          as.numeric(.)
        # Extract ATK / DEF
        My.deck.wf[k,c("ATK")] <- entry %>%
          html_node(xpath = ".//*[contains(text(),'ATK')]") %>%
          html_text(trim = TRUE) %>%
          gsub("[^0-9]", "", .) %>%
          as.numeric(.)
        My.deck.wf[k,c("DEF")] <- entry %>%
          html_node(xpath = ".//*[contains(text(),'DEF')]") %>%
          html_text(trim = TRUE) %>%
          gsub("[^0-9]", "", .) %>%
          as.numeric(.)
        # Extract effect (in case cards to to find certain names mentioned)
        My.deck.wf[k,c("Effect")] <- entry %>%
          html_node(xpath = ".//dd[contains(@class,'box_card_text c_text flex_1')]") %>%
          html_text(trim = TRUE)
      }
      
      if (!(search.card$Card.Type %in% c("Monster","monster","MONSTER"))) {
        # Full card name
        My.deck.wf[k,c("Card.Name.System")] <- entry %>%
          html_node(".card_name") %>%
          html_text(trim = TRUE)
        # Extract attribute
        My.deck.wf[k,c("Attribute")] <- entry %>%
          html_node(xpath = ".//span[contains(@class,'box_card_attribute')]") %>%
          html_text(trim = TRUE)
        # Extract type
        My.deck.wf[k,c("Type")] <- entry %>%
          html_node(xpath = ".//span[contains(@class,'box_card_effect')]") %>%
          html_text(trim = TRUE) %>% 
          gsub("[\n\t]+", "",.)
        # Extract effect (in case cards to to find certain names mentioned)
        My.deck.wf[k,c("Effect")] <- entry %>%
          html_node(xpath = ".//dd[contains(@class,'box_card_text c_text flex_1')]") %>%
          html_text(trim = TRUE)
      }     
}

My.deck.lf.wf <- tidyr::uncount(My.deck.wf, weights = Card.Quantity)

### 2. Simulations
# Create an empty matrix to store the output
runs <- 10000
# This part is for potential draw/consistency cards.
draw.class.one <- c("Pot of Extravagance", "Pot of Prosperity")
draw.class.two <- c("Radiant Typhoon Vision")
full.count.temp.s <- 0
half.count.temp.s <- 0
draw_lock <- 0
# Column 1 for combo, column 2 for handtrap count, 3 for boardbreaker, 4 for bricks, 5 for half board prob, and 6 for dead draw count
temp.results <- matrix(,nrow = runs, ncol = 6)
sample.deck.count <- 1:nrow(My.deck.lf.wf)
for (a in 1:runs){
  # 2.1 Generate a sample hand
  sample.hand.count <- sample(sample.deck.count, 5, replace=FALSE)
  sample.hand <- My.deck.lf.wf[sample.hand.count,]

  # 2.2 Check if the sample hand contains any consistency cards
  # There are two classes of draw cards: ones to activate immediately, and ones
  # that can be activated later if you don't have combos
  # 2.2.1 Class one draw cards (2.2.2 occurs after the first combo check)
  sample.deck.count.rest <- setdiff(seq_len(nrow(My.deck.lf.wf)), sample.hand.count)
  if (sum(sample.hand$Card.Name.System %in% draw.class.one)>=1) {
    # Pot of Extravagance
    if (sum(sample.hand$Card.Name.System=="Pot of Extravagance")>=1) {
      sample.hand.add.count <- sample(sample.deck.count.rest, 2, replace=FALSE)
      sample.hand.add <- My.deck.lf.wf[sample.hand.add.count,]
      sample.hand <- rbind(sample.hand, sample.hand.add)
      row_index_to_remove <- which.max(sample.hand$Card.Name.System == "Pot of Extravagance")
      sample.hand <- sample.hand %>%
        slice(-row_index_to_remove)
      draw_lock <- 1
    }
    # Pot of Prosperity
    if (sum(sample.hand$Card.Name.System=="Pot of Prosperity")>=1 & sum(sample.hand$Card.Name.System %in% draw.class.two)<1) {
      # First part of the effect: flip (draw) top 6 cards
      sample.hand.add.count <- sample(sample.deck.count.rest, 6, replace=FALSE)
      sample.hand.add <- My.deck.lf.wf[sample.hand.add.count,]
      # Examine if adding any of these 6 cards can make the hand combo-able
      full.count.temp <- c()
      half.count.temp <- c()
      for (f in 1:nrow(sample.hand.add)) {
      sample.hand.add.test <- sample.hand.add[f,]
      sample.hand.temp <- rbind(sample.hand, sample.hand.add.test)
      row_index_to_remove <- which.max(sample.hand.temp$Card.Name == "Pot of Prosperity")
      sample.hand.temp2 <- sample.hand.temp %>%
        slice(-row_index_to_remove)
      # Check the combo status after adding card f
      sample.hand.temp2.vector <- c(sample.hand.temp2$Card.Name)
      temp.check <- data.frame(full.count.temp = numeric(0),
                               half.count.temp = numeric(0))
      for (b in 1:nrow(Full.combo)){
        sample.full.combo.vector <- unname(unlist(Full.combo[b,]))
        sample.full.combo.vector.2 <- sample.full.combo.vector[!is.na(sample.full.combo.vector)]
        full.count.temp <- c(full.count.temp, all(sample.full.combo.vector.2 %in% sample.hand.vector))
      }
      for (c in 1:nrow(Half.combo)){
        sample.half.combo.vector <- unname(unlist(Half.combo[c,]))
        sample.half.combo.vector.2 <- sample.half.combo.vector[!is.na(sample.half.combo.vector)]
        half.count.temp <-  c(half.count.temp, all(sample.half.combo.vector.2 %in% sample.hand.vector))
      }
      # If more than one of the six cards make combo, this sum should be larger than one.
      full.count.temp.s <- full.count.temp.s + sum(full.count.temp)  
      half.count.temp.s <- half.count.temp.s + sum(half.count.temp)
      }
      draw_lock <- 1
    }
  }
  # 2.3 Check if it combos
  sample.hand.vector <- c(sample.hand$Card.Name) 
  full.count <- c()
  half.count <- c()
  dead.count <- c()
  # If the sample hand contains a dead draw combination, remove the dead card
  # before examining the combo.
  for (k in 1:nrow(Dead.draw)) {
    sample.dead.draw.vector <- unname(unlist(Dead.draw[k,]))
    if (all(sample.dead.draw.vector %in% sample.hand.vector)==T) {
      sample.hand.vector.2 <- sample.hand.vector[sample.hand.vector != sample.dead.draw.vector[1]]
      dead.count <- dead.count + 1
    }
    if (all(sample.dead.draw.vector %in% sample.hand.vector)==F) {
      sample.hand.vector.2 <- sample.hand.vector
      dead.count <- dead.count + 0
    }
  }
  for (b in 1:nrow(Full.combo)){
    sample.full.combo.vector <- unname(unlist(Full.combo[b,]))
    sample.full.combo.vector.2 <- sample.full.combo.vector[!is.na(sample.full.combo.vector)]
    full.count <- c(full.count, all(sample.full.combo.vector.2 %in% sample.hand.vector.2))
  }
  for (c in 1:nrow(Half.combo)){
    sample.half.combo.vector <- unname(unlist(Half.combo[c,]))
    sample.half.combo.vector.2 <- sample.half.combo.vector[!is.na(sample.half.combo.vector)]
    half.count <-  c(half.count, all(sample.half.combo.vector.2 %in% sample.hand.vector.2))
  }
  full.count.s <- sum(full.count)  
  half.count.s <- sum(half.count) 
  combo.count <- ifelse((full.count.s >= 1 | full.count.temp.s >=1) , T,
                        ifelse((half.count.s >= 1 | half.count.temp.s >=1),T,F)
  )
  # 2.2.3 Class two draw cards  
  # Radiant Typhoon Vision
  # Logic:
  # a. If you already have combos, don't draw
  # b. If you don't have any line, draw (assume you always activate it in draw phase & not prioritize it over pot of prosperity)
  # b-1. After drawing, if you have combos
  # b-1-1. Discard a RT or Quick-Play so you still have combos after resolving the card
  # b-1-2. Discard the RT or Quick-Play drew if you don't have anything else to discard (even if the card you drew allows for combos)
  # b-2-1. Discard a random RT or Quick-Play if still don't have any line
  # b-2-2. Discard your entire hand if you don't have any RT or Quick-Play to discard
  if (sum(sample.hand$Card.Name.System %in% draw.class.two)>=1){
    if (combo.count==0 & sum(sample.hand$Card.Name.System=="Radiant Typhoon Vision") >=1 & ((sum(sample.hand$Card.Name.System %in% "Pot of Prosperity") < 1)|sum(sample.hand$Type =="Quick-Play")>=2)) {
      draw_lock <- draw_lock + 1
    # First part of the effect: draw 2 cards
      sample.hand.add.count <- sample(sample.deck.count.rest, 2, replace=FALSE)
      sample.hand.add <- My.deck.lf.wf[sample.hand.add.count,]
      sample.hand <- rbind(sample.hand, sample.hand.add)
      row_index_to_remove <- which.max(sample.hand$Card.Name == "Radiant Typhoon Vision")
      sample.hand.post.draw <- sample.hand %>%
        slice(-row_index_to_remove)
    # Examine if the hand contains any combo before deciding what to discard
      sample.hand.post.draw.vector <- c(sample.hand.post.draw$Card.Name)
      full.count.post.draw <- c()
      half.count.post.draw <- c()
      dead.count.post.draw <- c()
      post.draw.check <- data.frame(full.count.post.draw = numeric(0),
                                    half.count.post.draw = numeric(0))
      for (k in 1:nrow(Dead.draw)) {
        sample.dead.draw.vector <- unname(unlist(Dead.draw[k,]))
        if (all(sample.dead.draw.vector %in% sample.hand.post.draw.vector)==T) {
          sample.hand.post.draw.vector.2 <- sample.hand.post.draw.vector[sample.hand.post.draw.vector != sample.dead.draw.vector[1]]
          dead.count <- dead.count + 1
        }
        if (all(sample.dead.draw.vector %in% sample.hand.post.draw.vector)==F) {
          sample.hand.post.draw.vector.2 <- sample.hand.post.draw.vector
          dead.count <- dead.count + 0
        }
      }
      for (b in 1:nrow(Full.combo)){
        sample.full.combo.vector <- unname(unlist(Full.combo[b,]))
        sample.full.combo.vector.2 <- sample.full.combo.vector[!is.na(sample.full.combo.vector)]
        full.count.post.draw <- c(full.count.post.draw, all(sample.full.combo.vector.2 %in% sample.hand.post.draw.vector.2))
        post.draw.check[b,c("full.count.post.draw")] <- all(sample.full.combo.vector.2 %in% sample.hand.post.draw.vector.2)
      }
      for (c in 1:nrow(Half.combo)){
        sample.half.combo.vector <- unname(unlist(Half.combo[c,]))
        sample.half.combo.vector.2 <- sample.half.combo.vector[!is.na(sample.half.combo.vector)]
        half.count.post.draw <-  c(half.count.post.draw, all(sample.half.combo.vector.2 %in% sample.hand.post.draw.vector.2))
        post.draw.check[c,c("half.count.post.draw")] <- all(sample.half.combo.vector.2 %in% sample.hand.post.draw.vector.2)
      }
      post.draw.check.full <- gdata::cbindX(Full.combo, post.draw.check)
      post.draw.check.half <- gdata::cbindX(Half.combo, post.draw.check)     
      full.count.s2 <- full.count.s + sum(full.count.post.draw)  
      half.count.s2 <- half.count.s + sum(half.count.post.draw) 
    # Second part of the effect: discard a quick-play or an RT card (or the entire hand if no RT nor quick-play)
    # b-1-1. Discard a RT or Quick-Play so you still have combos after resolving the card
    # b-1-2. Discard the RT or Quick-Play drew if you don't have anything else to discard (even if the card you drew allows for combos)
    if (full.count.s2 >= 1) {
      post.draw.check.full.sub <- post.draw.check.full %>%
        filter(grepl(1, full.count.post.draw))
      for (d in 1: nrow(post.draw.check.full.sub)) {
        sample.full.combo.vector.post <- unname(unlist(post.draw.check.full.sub[d,1:3]))
        sample.full.combo.vector.post2 <- sample.full.combo.vector.post[!is.na(sample.full.combo.vector.post)] 
        pattern <- paste(sample.full.combo.vector.post2, collapse = "|")
        sample.hand.post.draw2 <- sample.hand.post.draw %>%
          filter(!grepl(pattern, Card.Name, ignore.case = TRUE))
    # After removing the combo piece from the sample hand, if there is still other RT or quick-play in hand, then the
    # hand counts as having combo. Otherwise, the sample hand does not have a combo.
        if (sum(sample.hand.post.draw2$Type %in% c("Quick-Play"))>=1 | (sum(sample.hand.post.draw2$Card.Name %in% c("Radiant Typhoon"))>=1)) {
          full.count.s <- full.count.s + 1
                  sample.hand <- sample.hand.post.draw2 %>%
          {
            matches <- str_detect(.$Card.Name, regex("Radiant Typhoon", ignore_case = TRUE)) | .$Type == "Quick-Play"
            if (sum(matches) == 0) return(.)
            slice(., -sample(which(matches), 1))
          }
        }
        if (sum(sample.hand.post.draw2$Type %in% c("Quick-Play")) + (sum(sample.hand.post.draw2$Card.Name %in% c("Radiant Typhoon")) < 1)) {
          full.count.s <- full.count.s + 0        
        }
        }
      }
      # Same logic applies to half combos
      if (half.count.s2 >= 1 & full.count.s2 == 0) {
        post.draw.check.half.sub <- post.draw.check.half %>%
          filter(grepl(1, half.count.post.draw))
        for (e in 1: nrow(post.draw.check.half.sub)) {
          sample.half.combo.vector.post <- unname(unlist(post.draw.check.half.sub[e,1:2]))
          sample.half.combo.vector.post2 <- sample.half.combo.vector.post[!is.na(sample.half.combo.vector.post)] 
          pattern <- paste(sample.half.combo.vector.post2, collapse = "|")
          sample.hand.post.draw2 <- sample.hand.post.draw %>%
            filter(!grepl(pattern, Card.Name, ignore.case = TRUE))
          # After removing the combo piece from the sample hand, if there is still other RT or quick-play in hand, then the
          # hand counts as having combo. Otherwise, the sample hand does not have a combo.
          if (sum(sample.hand.post.draw2$Type %in% c("Quick-Play"))>=1 | (sum(sample.hand.post.draw2$Card.Name %in% c("Radiant Typhoon"))>=1)) {
            half.count.s <- half.count.s + 1
            sample.hand <- sample.hand.post.draw2 %>%
              {
                matches <- str_detect(.$Card.Name, regex("Radiant Typhoon", ignore_case = TRUE)) | .$Type == "Quick-Play"
                if (sum(matches) == 0) return(.)
                slice(., -sample(which(matches), 1))
              }
          }
          if (sum(sample.hand.post.draw2$Type %in% c("Quick-Play")) + (sum(sample.hand.post.draw2$Card.Name %in% c("Radiant Typhoon")) < 1)) {
            half.count.s <- half.count.s + 0
            sample.hand <- sample.hand.post.draw %>%
              {
                matches <- str_detect(.$Card.Name, regex("Radiant Typhoon", ignore_case = TRUE)) | .$Type == "Quick-Play"
                if (sum(matches) == 0) return(.)
                slice(., -sample(which(matches), 1))
              }
          }
        }
      } 
      # If no combo, discard the whole hand
      if (half.count.s2 == 0 & full.count.s2 == 0) {
        sample.hand <- data.frame(Function = character(0))
      }
    }

      combo.count <- ifelse(full.count.s >= 1, T,
                            ifelse(half.count.s >= 1,T,F)
      )   
    }
  
  # 2.4 Handtrap/Boardbreaker counts
  count.handrtrap <- sum(sample.hand$Function == "Handtrap")
  count.boardbreaker <- sum(sample.hand$Function == "Boardbreaker")
  # 2.5 Brick counts
  count.brick <- sum(sample.hand$Function == "Brick")
  # 2.6 store the results
  temp.results[a,1] <- combo.count
  temp.results[a,2] <- count.handrtrap
  temp.results[a,3] <- count.boardbreaker  
  temp.results[a,4] <- count.brick
  temp.results[a,5] <- ifelse(full.count.s==0 & half.count.s >= 1,T,F)
}
### 3. Results
combo.prob <- round((sum(temp.results[,1])/runs)*100,2)
half.prob <- round((sum(temp.results[,5])/runs)*100,2)
avg.handtrap <- round(sum(temp.results[,2])/runs, 2)
avg.boardbreaker <- round(sum(temp.results[,3])/runs, 2)
avg.brick <- round(sum(temp.results[,4])/runs, 2)
print(paste0("After ",runs," times of simulation, the probability of your deck opens with a playable hand is ", combo.prob, "% (including ", half.prob,"% chance of building a half board). Also, you are expected to open with ", avg.handtrap, " handtraps, ", avg.boardbreaker," board-breakers, and ", avg.brick, " bricks."))
