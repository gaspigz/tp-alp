{-# LANGUAGE GADTs #-}

module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty 

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v x s = M.insert v x s 

-- Evalúa un programa en el estado vacío 
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm c s = case c of
  Skip ->  c :!: s

  Let var e -> let (n :!: s') = evalExp e s  
               in Skip :!: update var n s'

  Seq Skip c2 ->  c2 :!: s 
  Seq c1   c2 -> let (c1':!: s') = stepComm c1 s
               in Seq c1' c2 :!: s'

  IfThenElse b c1 c2 -> let (b1 :!: s') = evalExp b s
                        in if b1 then c1 :!: s'
                                 else c2 :!: s'

  RepeatUntil c b -> Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s
    

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp c s = case c of

  Const n -> n :!: s 
  
  Var   v ->   lookfor v s :!: s    
  
  UMinus exp -> let (n :!: s') = evalExp exp s
                in -n :!: s'

  Plus   x y -> let (n1 :!: s' ) = evalExp x s
                    (n2 :!: s'') = evalExp y s'
                in   n1+n2 :!: s''

  Minus  x y -> let (n1 :!: s') = evalExp x s 
                    (n2 :!:s'') = evalExp y s'
                in   n1 - n2 :!: s'' 

  Times  x y -> let (n1 :!:  s') = evalExp x s 
                    (n2 :!: s'') = evalExp y s'
                in   n1 * n2 :!: s'' 

  Div    x y -> let (n1 :!: s' ) = evalExp x s
                    (n2 :!: s'') = evalExp y s'

                in  (n1 `div` n2) :!: s''   

  VarInc  v ->  let n = lookfor v s 
                    s' = update v (n+1) s 
                in (n+1) :!: s' 
                
  
  BTrue -> True :!: s 
  
  BFalse -> False :!: s 

  Lt x y ->  let (n1 :!: s') = evalExp x s
                 (n2 :!: s'') = evalExp y s'
             in if n1 < n2 then True :!: s''
                           else False :!: s''
  
  Gt x y -> let (n1 :!: s') = evalExp x s
                (n2 :!: s'') = evalExp y s'
             in if n1 > n2 then True :!: s''
                           else False :!: s''
  
  And x y -> let (b1 :!: s') = evalExp x s
                 (b2 :!: s'') = evalExp y s'
             in if b1 then (if b2 then True :!: s'' else False :!: s'')
                     else False :!: s''

  Or x y -> let (b1 :!: s') = evalExp x s
                (b2 :!: s'') = evalExp y s'
            in if b1 then True :!: s'' else (if b2 then True :!: s'' else False :!: s'')
                

  Not x -> let (b1 :!: s') = evalExp x s
           in if b1 then False :!: s' else True :!: s'
 
  Eq x y -> let (n1 :!: s') = evalExp x s
                (n2 :!: s'') = evalExp y s'
            in  n1 == n2 :!: s''

  NEq x y -> let (n1 :!: s') = evalExp x s
                 (n2 :!: s'') = evalExp y s'
             in  n1 /= n2 :!: s'' 



