library(data.table)
cards = data.table(id = as.integer(c(1,2,3,4,5,6,7, 8)), 
	type = c("Treasure", "Treasure", "Treasure", "Victory", "Victory", "Victory", "Curse", "Action"),
	sub_type = c("", "", "", "", "", "", "", ""),
	name = c("Copper", "Silver", "Gold", "Estate", "Duchy", "Province", "Curse", "Smithy") ,
	price = c(0,3,6,2,5,8,0,4),
	income = c(1,2,3,0,0,0,0,0),
	vp = c(0,0,0,1,3,6,0,0),
	actions = c(0,0,0,0,0,0,0,0),
	cards = c(0,0,0,0,0,0,0,3),
	buys = c(0,0,0,0,0,0,0,0),
	key = "id"
	)

strategy_gold = list(name = "Treasures only",
  action_priorities = c(),
  buy_priorities = c("Province","Gold","Silver"),
  max_buys = c(NA,NA,NA)
)

strategy_smithy = list(name = "Smithy",
  action_priorities = c("Smithy"),
  buy_priorities = c("Province","Gold","Smithy","Silver"),
  max_buys = c(NA,NA,1,NA)
)

initial_deck = as.integer(c(rep(1,7), rep(4,3)))
initial_hand = c()
initial_discard  = c()

shuffle = function(){
  deck <<- sample(deck)
}

draw_hand = function(){
  if (length(deck) >= 5){
    hand <<- deck[1:5]
    deck <<- deck[-(1:5)] 
  } else {
  hand <<- deck
  deck <<- c()
  discard_to_deck()
  draw(5 - length(hand)) 
  } 
}

draw = function(n){
 if (length(deck) >= n){
    hand <<- c(hand,deck[1:n])
    deck <<- deck[-(1:n)] 
  } else {
    hand <<- deck
    if (length(discard) > 0 ) {
      discard_to_deck()
      draw(n - length(hand)) 
    }
  }
}

discard_to_deck = function(){
  deck <<- c(deck, discard)
  discard <<- c()
  shuffle()
}

play_hand = function(){
  actions = 1
  buys = 1
  income = 0
  hand1 = data.table(id = hand, key = "id")
  hand1 = cards[hand1]

  # Play action cards 
  do while(actions > 0 & hand[type == "Action"]) {
    action_cards =  hand1[type == "Action"]
  }  
  
  play_action()

  # Buy cards
  income = income + hand1[type == "Treasure", sum(income)]
  hand <<- c(hand, buy_cards(income, buys))
  
  # Discards
  discard <<- c(discard, hand)
  hand <<- c() 
}

buy_cards = function(coins, buys){
  new_cards = c()
  priorities = data.table(strategy$buy_priorities, key = "id")
  finish = F
  deck_all = data.table(id = c(deck, hand, discard))
  deck_all = deck_all[,.N,by = "id"]
  setkeyv(deck_all, "id")
  priorities = deck_all[priorities][order(priority)]
  priorities[is.na(N), N:=0]
  while (!finish & buys > 0) {
    # buy_priorities are sorted already, so we always need to find the first affordable card
    # might have a bug when buying 2 same cards on the same turn
    buy = priorities [price<=coins & (is.na(max_buys) | N < max_buys)][1]
    if (!is.na(buy$id)) {
      coins = coins - buy$price
      buys = buys - 1
	new_cards = c(new_cards, buy$id)
    } else {
      finish = T
    }
  }
  return(new_cards)
}

use_strategy = function(strat){
  cards1 = data.table(cards, key = "name")
  buy_priorities = data.table(name = strat$buy_priorities, priority = seq_len(length(strat$buy_priorities)),
	max_buys = strat$max_buys,  key = "name")
  strat[["buy_priorities"]] = cards1[buy_priorities][order(priority)]
  return(strat)
}

calculate_vp = function(){
  discard_to_deck()
  deck1 = data.table(id = deck, key = "id")
  deck1 = cards[deck1]
  return(sum(deck1[type == "Victory"]$vp))
}

