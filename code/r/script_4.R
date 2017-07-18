


## predict that team with higher 2-pointer FG percentage SMA will win

## predict that team with higher 3-pointer FG percentage SMA will win

## predict that team with higher dRb SMA will win

## predict that team with higher ast SMA will win

## predict that team with higher rb SMA will win

## predict that team with higher blk SMA will win

## predict that team with higher FTA SMA will win

## predict that team with lower mean age will win



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

