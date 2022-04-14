--oblig 3
--Petter Tobias Madsen
--pma045
import Data.Char

next :: Int -> Int --bytter mellom AI og player
next 1 = 2
next 2 = 1
type Board = [Int]

initialNim :: Int-> Board --start brettet for nim
initialNim size = reverse [1..size]

initialChomp :: Int -> Board --start bretter for chomp
initialChomp size = replicate size size

finished :: Board -> Bool --ser og brettet er fredig
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool -- ser om trekket ditt er gyldig
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board -- oppdaterer brettet når du gjør et trekk
move board row num = [update r n | (r, n) <- zip [1..] board]
    where update r n = if r == row then n - num else n
    
putRow :: Int -> Int -> IO () --legger inn radene på brettet
putRow row num = do putStr (show row)
                    putStr " "
                    putStrLn (concat (replicate num "* "))


endString :: Int -> Int -> String -> String--lagger inn konolenomerene
endString r 0 str = str
endString r min str = str ++  (show (r-min)) ++ endString (r) (min -1) str

putBoard :: Int -> Board -> IO ()--legger inn brettet
putBoard r [] = do putStr (" "++endString (r) (r-1) (" "))
                   newline
putBoard r (n:ns)= do putRow r n
                      putBoard (r+1) ns
                         
getGame :: String -> IO String--henter inputen fra user
getGame prompt = do putStr prompt
                    xs <- getLine
                    newline
                    if length xs == 2 || length xs >3 then --user-en kan bare inputen enten 1 char eller 3 char. 1 for ?/q og 3 for num input eks. 2 3
                       do putStrLn "Error: invalid digit"
                          getGame prompt
                    else if head xs == 'c' || head xs == 'q' || head xs == 'n' || head xs == '?' || isDigit (head xs) then
                            return (xs)
                    else
                       do putStrLn "Error: Invalid digit."
                          getGame prompt
                           
newline :: IO ()--newline
newline = putChar '\n'

aiChomp :: Board -> Int -> Int--chmop sin AI
aiChomp board num = if head board == 0 then --finner en rad som ikke tom for strjerner og fjerner 1 stjerne derifra
                        aiChomp (tail board) (num +1)
                    else 
                        num 

intToBinary :: Int -> [Int] -> [Int]--gjør et tall om til binary av det tallet 
intToBinary 0 listNum = 0 : listNum
intToBinary 1 listNum = 1 : listNum
intToBinary num listNum= if (num `mod` 2) == 0 then
                            intToBinary (num `div` 2) (0 : listNum)
                         else 
                            intToBinary (num `div` 2) (1 : listNum)
                            
binaryToInt :: [Int] -> Int -> Int--gjør et binary tall om til et vanelig tall
binaryToInt [] num= num
binaryToInt binarynum num= if head binarynum == 1 then
                              binaryToInt (tail binarynum) (num + (2^((length binarynum)-1)))
                           else 
                              binaryToInt (tail binarynum) (num)

boardInBinary :: [Int] -> [[Int]] -> [[Int]]--gjør alle tallene i board om til tall i binary.  forklarer nim AI helt ned på doc.
boardInBinary [] binarylist =  init binarylist
boardInBinary board binarylist= boardInBinary (init board) ((intToBinary (last board) []:binarylist))

nimAiDel2:: Board -> Int -> Int -> (Int,Int)--tester å fjerne en og en stjerne fra den største raden i board helt til nimsumen av board er balanseret
nimAiDel2 board amount index = if amount<=(board!!index) then
                            if (binaryToInt (nimsum (boardInBinary ((take (index) (board)) ++ (((board!!index)-amount):(drop (index+1) board))) ([[]])) ([])) (0)) == 0 then
                                ((index+1), amount)
                            else 
                                nimAiDel2 board (amount-1) index
                          else nimAiDel2 board (amount-1) index
                                                       

biggestNum:: Board -> Int -> Int -> Int -> (Int)--gir ut indexen til største tall i board
biggestNum [] num index indexH = (indexH)
biggestNum board num index indexH= if (head board)>num then
                                biggestNum (tail board) (head board) (index+1) (index)
                             else (biggestNum (tail board) (num) (index+1)) (indexH)








nimAi :: Board -> Board -> Board -> Int -> Int-> (Int, Int)-- se nederst på doc  
nimAi board boardcopy boardcopy2 amount index=   if (head board) == 0 then--hvis første elemente i board er null. gjør han nimAi igjen uten det elementet
                                                    nimAi (tail board) (tail boardcopy) (boardcopy2)  (amount) (index +1)
                                                 else if (length board) == 1 then -- hvis lengden av board er 1 så tar den bare alle stjerne i den raden. da vinner den
                                                         (index,(head board))
                                                 else if (length board) == 2 then-- hvis lengden av board er 2 så vil den ta nok stjerner fra den største raden slik at randene har like mange stjerner
                                                         if (head board) > (last board) then
                                                            ( index,(head board)-(last board))
                                                          else
                                                             ((index+1),(last board)-(head board))
                                                 else if (amount==0) then --hvis amount(mengende stjerne så må fjernes for å balansere nimsum boarded) er 0 fjerner den første stjerne den har lov til å fjerne
                                                         ((aiChomp board 1), (1))
                                                 else if (boardcopy == []) then --hvis boardcopy=[] så er amount større en den største rad-en i board da går den til nimAidel2
                                                    nimAiDel2 boardcopy2 amount (biggestNum boardcopy2 0 0 0)
                                                 else 
                                                    if (head boardcopy) < amount then--hvis første elemente i boardcopy er mindre enn amount så prøver den neste element i boardcopy helt til boardcopy er tom
                                                        nimAi (board) (tail boardcopy) (boardcopy2) (amount) (index+1)
                                                    else 
                                                       (index,(amount))--hvis første element i boardcopy er større eller likt amount gir den tilbake index av det element i hovedbrettet sammen med amount.

nimsum :: [[Int]] -> [Int]-> [Int]--skaffer nimsumen av board
nimsum boardbinary []= nimsum (tail boardbinary) (head boardbinary)
nimsum [] nims = (nims)
nimsum boardbinary nims = if head boardbinary == [] then
                            nimsum (tail boardbinary) nims 
                       else if length (head boardbinary) > length nims then--hvis binarytallet har flere digits enn nims få blir de tallen flyttet over til nims. helt til det bare er like mange digits i binarytaller som det var i nims i starten
                            nimsum  ((drop ((length (head boardbinary))-(length nims)) (head boardbinary)):(tail (boardbinary))) ((take ((length (head boardbinary))-(length nims)) (head boardbinary))++(nims))
                       else if (head (head boardbinary) == 0) && (head (drop ((length nims) - (length (head boardbinary))) nims)== 0) then --hvis det er 0 i binarytallet og nims ved samme index så skal det tallet blir 0
                            nimsum (((tail(head boardbinary)):(tail(boardbinary)))) (nims)
                       else if (head (head boardbinary) == 1) && (head (drop ((length nims) - (length (head boardbinary))) nims)) == 1 then--hvis det er 1 i binarytallet og nims ved samme index så skal det tallet blir 0
                            nimsum (((tail(head boardbinary)):(tail(boardbinary)))) ((take ((length nims) - (length (head boardbinary))) (nims) ) ++ 0:(drop ((length nims) - (length (head boardbinary))+1)(nims))) 
                       else--hvis det er 1 binarytallet og 0 i nims eller omvendt ved samme index så skal tallet i den index i nims bli 1
                            nimsum (((tail(head boardbinary)):(tail(boardbinary)))) ((take ((length nims) - (length (head boardbinary))) (nims) ) ++ 1:(drop ((length nims) - (length (head boardbinary))+1)(nims))) 
                             
spillet :: Board -> Int -> Char -> IO()--selve spillet
spillet board player game =
    do putStrLn ("-----------------------------")--lager et skiller mellom hver runde
       newline
       putBoard 1 board
       if finished board then --ser om spillet er ferdig. hvi det er så ser den hvilket spill som ble spiltt og hvem som vant.
          if game == 'n' then--nim
             do newline
                if next player == 1 then
                   do putStr ("You ")
                      putStrLn (" Won")
                      spill--tar deg tilbake til start menyen
                else 
                   do putStr ("AI ")
                      putStrLn ("Won")
                      spill
          else--chomp
             do newline
                if next player == 1 then
                   do putStr ("AI ")
                      putStrLn (" Won")
                      spill
                else 
                   do putStr ("You ")
                      putStrLn ("Won")
                      spill
       else--hvis spillet ikke er ferdig looper den trekk frem til spillet er ferdig
          do newline 
             if player == 1  then--spilleren sin tur
                do putStrLn ("Your turn")
                   input <- getGame "enter r a / ? / q: "--tar inputen din
                   if head input == '?' then--hvis inputen er ? gir den deg en tutorial utifra hvilket spill du spiller
                      if game == 'n' then
                          do putStrLn "nim tutorial"
                             putStrLn "in nim you are to pick a rownumber (r) and a amount of stars (a) you wish to remove from that row. for example:1 2"
                             putStrLn "you cant remove more stars form the row then are desplyed next to the row number"
                             putStrLn "the goal is to be the person that removes the last star from the board"
                             putStrLn "you can only remove stars from one row each turn"
                             spillet (board) (player) game
                       else 
                          do putStrLn "chomp tutorial"
                             putStrLn "in chomp you are to pick a rownumber (r) and a amount of stars (a) you wish to remove from that row. for example:1 2"
                             putStrLn "you cant remove more stars form the row then are desplyed on the row number"
                             putStrLn "the goal is to not be the person that removes the last star from the board"
                             putStrLn "you can only remove stars from one row each turn"
                             spillet (board) (player) game
                   else if head input == 'q' then--q tar det til start menyen hvor du kan velge hvilket spill du vil spille eller å slutte helt
                           do putStr "quit to start menu"
                              spill
                   else if isDigit (head input) && isDigit (head (drop 2 input)) && (digitToInt(head input))<=(length board) then--tar tall input og ser om det movet er gyldig
                           if valid board (digitToInt (head input)) (digitToInt (head (drop 2 input))) then
                              spillet (move board (digitToInt (head input)) (digitToInt (head (drop 2 input)))) (next player) game--gjør movet
                           else
                              do newline
                                 putStrLn "Error: Invalid move."--error om movet ikke er gyldig eller det er feil input
                                 spillet (board) (player) game
                    else do newline
                            putStrLn ("Error invalid move")
                            spillet (board) (player) game
             else --Ai sin tur
                do putStrLn "AI turn"
                   if game == 'n' then --gjør nimAi
                      do newline
                         putStr ("AI did move index: ")
                         putStr (show(fst(nimAi (board) (board) (board) (binaryToInt (nimsum (boardInBinary (board) ([[]])) ([])) (0)) (1))))
                         putStr ("  amount: ")
                         putStr (show(snd(nimAi (board) (board) (board) (binaryToInt (nimsum (boardInBinary (board) ([[]])) ([])) (0)) (1))))
                         newline
                         spillet (move board ((fst(nimAi (board) (board) (board) (binaryToInt (nimsum (boardInBinary (board) ([[]])) ([])) (0)) (1)))) ((snd(nimAi (board) (board) (board) (binaryToInt (nimsum (boardInBinary (board) ([[]])) ([])) (0)) (1))))) (next player) game
                    else--gjør chomeAi
                       do newline
                          putStr ("AI did move ")
                          putStr (show (aiChomp (board) (1)))
                          putStr (" 1")
                          newline
                          spillet (move board (aiChomp (board) (1)) (1)) (next player) game

spill :: IO()
spill = do newline
           l <-getGame "n(im) x / c(homp) x/ q(uit): "--tar spill eller quit request
           if length l == 1 || length l == 3 then
              do newline
                 if head l == 'n' && isDigit(head (drop 2 l)) then --gir nim med fra 1-9 størrelse
                    do spillet (initialNim (digitToInt (head (drop 2 l)))) 1 'n'
                 else if head l == 'c' && isDigit(head (drop 2 l)) then--gri chompe med 1-9 størrelse
                         do spillet (initialChomp (digitToInt (head (drop 2 l)))) 1 'c'
                 else if head l == 'q' then --går ut av spillet helt
                         do putStr "quit \n"
                 else
                    do putStr "error on input" 
                       spill
           else
              do putStr "error on input" 
                 spill
    
--den optimale strategien for nim er å gjøre bretter balansert i form at nimsum =0 etter ditt trekk. måten du gjør det på er at du xor alle radene på brettet
--da vil du få ut en nimsum. hvis numsumen ikke 0 og det er over 2 rader med stjerner på brettet så vil du
--fjerne nimsum (gjort om til vaneligtall) fra en rad som har minst den megden stjerner i seg. da vil nimsumen av det nye brette bli 0 som er det du vil ha
--hvis nimsumen (gjort om til vaneligtall) igjen er strørre enn det største tall på brettet går den til AIdel2.
--i AIDel2 tester den å fjerne en og en stjerne fra den raden med mest stjerner i seg og ser om nimsumen etter det er lik 0.
--hvis den er det så vil den gjøre et move med den mengende stjerner som måtte fjernes i testen sammen med index-en til den raden med flest stjerner.
--dermed blir brettet balansert.
--når det er 2 rader igjen vil du balansere radene slik at det er like mange stjerner igjen i begge radene.
--hvis brette allereder er balanseret(nimsum=0) når din tur starter må du bare satse på at motstanderen gjør en feil.
--hvis brettet ikke er balansert når din tur starter vil du altid kunne gjøre et trekk som kan balansere det.
--hvis brettet er balansert vil neste trekk altid gjøre det ubalansert igjen. 


       