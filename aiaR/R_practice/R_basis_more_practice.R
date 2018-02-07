# http://www.hmwu.idv.tw/web/R/R-exercise-2018v1.pdf
# 1.5 
# (a) ⽤ rep 指令造出以下數列:
#   1 1 1 1 1 2 2 2 2 3 3 3 4 4 5
# (b) ⽤ rev 和 sequence 指令造出以下數列:
#   1 2 3 4 5 6 2 3 4 5 6 3 4 5 6 4 5 6 5 6 6
a = c(rep(1,5),rep(2,4),rep(3,3),rep(4,2),rep(5,1))

b = c()
for (i in 0:5){
  b <- append(b, setdiff(sequence(6),sequence(i)))
}
b

# 1.6 產⽣數列:
# (a) ⽤ rep 指令造出以下數列:
#   "A" "A" "A" "A" "A" "B" "B" "B" "B" "C" "C" "C" "D" "D" "E"
# (b) ⽤ seq, c 指令造出以下數列:
#   "b" "d" "f" "h" "j" "l" "n" "p" "r" "t" "v" "x" "z" "a" "c" "e" "g"
# "i" "k" "m" "o" "q" "s" "u" "w" "y"
a = c(rep('A',5),rep('B',4),rep('C',3),rep('D',2),rep('E',1))
a
b = c("b","d","f","h","j","l","n","p","r","t","v","x","z","a","c","e","g","i","k","m","o","q","s","u","w","y")
b
