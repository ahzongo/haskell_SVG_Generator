import Prelude hiding (Left, Right)
import Control.Monad (forever)
import System.Exit (exitSuccess)

{--
	Pour ce projet, on a eu l'idée de transormer toutes les instructions en seulement une suite d'instructions Forward, Left et Rigth avant de l'associer au crayon et de générer le code svg

--}
-- Definition des type Instruction Crayon
data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction] deriving (Show , Read)
data Crayon = Crayon { absc :: Float, ord :: Float , dir :: Float } deriving (Show ,Read )
type ListInstruction = [Instruction]

-- La fonction eval permet de retourner les Instructions dans des listes d'instructions
eval :: Instruction -> ListInstruction -> ListInstruction
eval (Forward e1) lst = (Forward e1):lst 
eval (Right e1) lst = (Right e1):lst
eval (Left e1) lst = (Left e1):lst
eval (Repeat e1 e2) lst = reverse (parserepeat (take e1 (repeat e2)) lst)

parse :: String -> ListInstruction
parse string = read string :: ListInstruction

--La fonction decharge permet d'ajouter le contenu d'une liste dans une autre. Ce qui permettra d'avoir toutes les instructions dans une seule liste
decharge :: ListInstruction -> ListInstruction -> ListInstruction
decharge e1 e2 = case e1 of
                  [] -> e2
                  (x:xs) -> decharge xs (x:e2)
-- parserepeat permet de transformer une liste de liste d'instuction en une seule liste d'instruction. Nous avons écrit cette fonction surtout pour gerer l'évaluation de l'instruction Repeat avec la combinaison take-repeat de haskell qui renvoie une liste de liste d'instruction                
parserepeat :: [ListInstruction] -> ListInstruction -> ListInstruction
parserepeat e1 e2 = case e1 of
               [] -> e2
               (x:xs) -> parserepeat xs (decharge (reverse (traduct x [])) e2)
               

--la fonction traduct permet voir une liste d'instruction sans l'instruction Repeat. Peut importe la liste donnée en paramètre, elle retourne une liste de Foarward , Left et Right
-- Nous avons opté pour une pareil solution car c'est plus simple de modoliser les déplacements avec les 3 instructions plutôt qu'avec un Repeat
traduct :: ListInstruction -> ListInstruction -> ListInstruction
traduct e lst = case e of 
                 [] -> lst
                 (x:xs) -> traduct xs (decharge (eval x []) lst)

-- domodif permet de modéliser le déplacement du crayon. En fonction de l'instruction et du crayon de départ ,elle renvoie un crayon ayant subi l'instruction
                
domodif :: Instruction -> Crayon -> Crayon
domodif (Forward e) pen = Crayon ((absc pen)+nb*cos ((dir pen)*pi/180)) ((ord pen)+nb*sin ((dir pen)*pi/180)) (dir pen) where nb= fromIntegral e :: Float
domodif (Left e) pen = Crayon (absc pen) (ord pen) ((fromIntegral e :: Float)+(dir pen))
domodif (Right e) pen = Crayon (absc pen) (ord pen) (-(fromIntegral e :: Float)+(dir pen))
domodif (Repeat e1 e2) pen = pen
-- showcrayon renvoie, avec deux crayons en paramètre, la ligne de code svg correspondant
showcrayon :: Crayon -> Crayon -> String
showcrayon c1 c2 = "<line x1= \""++show (absc c1) ++"\" y1= \""++show(ord c1)++"\" x2= \""++show (absc c2) ++"\" y2= \""++show(ord c2)++"\" stroke= \"red\""++"/>"
                       
{-- Vu que dans le code svg on ajoute pas les lignes correspondant aux changements d'angles, il nous semblait nécessaire de créer une fonction qui en fonction des instructions
decide si on doit ajouter une ligne de code svg.
C'est le rôle de la fonction conditionsvg.
--}
conditionsvg :: Instruction -> Crayon -> Crayon -> [String] -> [String]
conditionsvg (Forward e) c1 c2 lst = (showcrayon c1 c2):lst
conditionsvg (Left e) c1 c2 lst = lst
conditionsvg (Right e) c1 c2 lst = lst
conditionsvg (Repeat e i) c1 c2 lst= lst

--logoskell2svg permet avec une liste d'instruction et un crayon, genère la liste des lignes de code svg               
logoskell2svg :: ListInstruction -> Crayon -> [String] -> [String]
logoskell2svg i c lst = case (reverse (traduct i [])) of 
                 [] -> lst
                 (x:xs) -> logoskell2svg xs a (conditionsvg x c a lst) where a = domodif x c
                 

-- outputStrLst est la fonction qui permettra l'affichage du code svg sur la sortie standard                         
outputStrLst :: [String] -> IO()
outputStrLst lst = case lst of
                    [] -> putStrLn "</svg>"
                    (x:xs) -> do
                     putStrLn x
                     outputStrLst xs

main = do
  line <- getLine
  if null line
    then exitSuccess
    else outputStrLst (reverse(logoskell2svg (parse line) (Crayon 100 100 0) ["<title>Exemple</title>","<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"200\" height=\"200\">","<?xml version=\"1.0\" encoding=\"utf-8\"?>"]))                                
            
                                    

