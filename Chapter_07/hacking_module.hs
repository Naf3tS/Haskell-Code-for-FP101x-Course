-- Hacking around with let:
aaa = let   y = 1+2
            z = 4+6
            in y+z

aaa' = y+z
    where   y = 1+2
            z = 4+6

x (+*+) y = x - y