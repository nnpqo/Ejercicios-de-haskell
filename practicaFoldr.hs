--I. Definir en términos de foldr:
--Una función que reciba una lista y devuelva la productoria de sus elementos
productoria:: [Int] -> Int
productoria x = foldr (*) 1 x 
--map
