[x^2 | x <- [1..5]]

[(x,y) | x <- [1..3], y <- [4,5]]

[(x,y) | y <- [4,5], x <- [1..3]]

[(x,y) | x <- [1..3], y <- [x..3]]

concat' [[1,2,3],[4,5],[6]]

[x | x <- [1..10], even x]

factors 15

zip [1..5] [11..15]
pair [1..10]

sorted [1..10]
sorted [1,3,2]

positions 3 [1,2,3,4,3,5]
positions 0 [1,0,0,1,0,1,1,0]

length "abc"
take 4 "abcdefghi"
zip "abc" [1..3]

lowers "sSkdsjSd"
lowers "Haskell"
