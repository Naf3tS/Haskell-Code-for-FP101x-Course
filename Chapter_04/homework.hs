[(x,y) | x <- [1,2,3], y <- [4,5,6]]

[z | z <- [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]] -- a
concat[[[(x,y)] | y <- [4,5,6]] | x <- [1,2,3]] -- b
-- concat[(x,y) | y <- [4,5,6]] | x <- [1,2,3] -- c
concat[[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]