{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
-- Completar la definición
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v (s, _) = case M.lookup v s of
                Just n  -> Right n
                Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v n (m, tr) = (M.insert v n m, tr)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace str (s, tr) = (s, tr ++ str ++ " ")

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip st = Right (Skip :!: st) 

stepComm (Let var e) st = do
  (n :!: st') <- evalExp e st
  let st'' = update var n st'
  let st''' = addTrace ("Let " ++ var ++ " " ++ show n) st''
  return (Skip :!: st''')

stepComm (Seq Skip c) s = Right (c :!: s)
stepComm (Seq c1 c2) s = do
  (c1' :!: s') <- stepComm c1 s
  return (Seq c1' c2 :!: s') 

stepComm (IfThenElse b c1 c2) s = do
  (cond :!: s') <- evalExp b s
  if cond then return (c1 :!: s') 
       else return (c2 :!: s')

stepComm (RepeatUntil c e) s = do
  return (Seq c (IfThenElse e Skip (RepeatUntil c e)) :!: s)


-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) st = Right (n :!: st)

evalExp (Var v) st = do
  n <- lookfor v st
  return (n :!: st)

evalExp (UMinus e) st = do 
  (n :!: st') <- evalExp e st 
  return (-n :!: st')

evalExp (Plus   x y) s = do
  (n1 :!: s' ) <- evalExp x s
  (n2 :!: s'') <- evalExp y s'
  Right (n1 + n2 :!: s'')

evalExp (Minus  x y) s = do
  (n1 :!: s') <- evalExp x s
  (n2 :!:s'') <- evalExp y s'
  Right (n1 - n2 :!: s'')

evalExp (Times  x y) s = do
  (n1 :!:  s') <- evalExp x s
  (n2 :!: s'') <- evalExp y s'
  Right (n1 * n2 :!: s'')

evalExp (Div x y) s = do
  (n1 :!: s' ) <- evalExp x s
  (n2 :!: s'') <- evalExp y s'
  if n2 == 0 then Left DivByZero
               else Right ((n1 `div` n2) :!: s'')

evalExp (VarInc v) st = do
  n <- lookfor v st 
  let n' = n + 1 
      st' = update v n' st 
      st'' = addTrace ("Let " ++ v ++ " " ++ show n') st'
  return (n' :!: st'')


evalExp BTrue s = Right (True :!: s)

evalExp BFalse s = Right (False :!: s)

evalExp (Lt x y) s =  do
  (n1 :!: s') <- evalExp x s
  (n2 :!: s'') <- evalExp y s'
  Right (if n1 < n2 then True :!: s''
                    else False :!: s'')
evalExp (Gt x y) s =  do
  (n1 :!: s') <- evalExp x s
  (n2 :!: s'') <- evalExp y s'
  Right (if n1 > n2 then True :!: s''
                    else False :!: s'')

evalExp (And x y) s =  do
  (n1 :!: s') <- evalExp x s
  (n2 :!: s'') <- evalExp y s'
  Right (if n1 && n2 then True :!: s''
                    else False :!: s'')

evalExp (Or x y) s =  do
  (n1 :!: s') <- evalExp x s
  (n2 :!: s'') <- evalExp y s'
  Right (if n1 || n2 then True :!: s''
                    else False :!: s'')

evalExp (Not x) s = do
  (b1 :!: s') <- evalExp x s
  Right (if b1 then False :!: s'
               else True :!: s')

evalExp (Eq x y) s =  do
  (n1 :!: s') <- evalExp x s
  (n2 :!: s'') <- evalExp y s'
  Right (if n1 == n2 then True :!: s''
                    else False :!: s'')

evalExp (NEq x y) s =  do
  (n1 :!: s') <- evalExp x s
  (n2 :!: s'') <- evalExp y s'
  Right (if n1 /= n2 then True :!: s''
                     else False :!: s'')


