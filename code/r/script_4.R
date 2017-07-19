

## see how 

fav <- subset(master, line < 0)
und <- subset(master, line > 0)

table(fav$p + fav$line > fav$pA)
ddply(fav, 'season', function(x) {
  table(x$p + x$line > x$pA)
})

table(und$p + und$line > und$pA)
ddply(und, 'season', function(x) {
  table(x$p + x$line > x$pA)
})

# 100 + (+4.5) > 103
# 120 + (+4.5) > 103
# 100 + (-4.5) > 103
# 105 + (-4.5) > 103
# 120 + (-4.5) > 103

