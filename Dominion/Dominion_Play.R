strat = strategy_gold

## Initial hand

results = data.table(strategy = character(), turns = integer(), vp = integer())
for (i in 1:1000) {

deck = initial_deck
hand = initial_hand
discard = initial_discard
turn = 0
strategy = use_strategy(strat)
#print(paste("Current strategy: ", strategy$name, sep = ""))

shuffle()
## The game ends after 20 turns or after getting 4 Provinces
while (sum(c(deck,discard)==6)< 4) { #turn <20 & 
  turn = turn + 1
  draw_hand()
  print(paste("Turn ", turn, 
	". Deck: ", paste(deck,collapse = ","), 
	". Hand: ", paste(hand,collapse = ","), 
	". Discard: ", paste(discard,collapse = ","), 
	sep = "", collapse = ""))
  play_hand()
}

vps = calculate_vp()
vps

results = rbind(results, data.table(strategy = strategy$name, turns = turn, vp = vps))
}

results
#hist(results$vp)
#hist(results$turn, freq = T)
plot(density(results$turn), main = strategy$name)
summary = results[,.N, by = "turns"][order(turns)]
barplot(summary$N, names.arg=summary$turns, space = 0, 
	main = strategy$name)
median(results$turn)
mean(results$turn)
