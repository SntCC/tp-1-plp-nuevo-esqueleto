{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use first" #-}
module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma
--import Data.ByteString (cons)
import Data.Function (const)
import Util
-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)


-- recrExpr :: ... anotar el tipo ...
recrExpr :: (Float -> b) -> (Float -> Float -> b) -> (Expr -> b -> Expr -> b -> b) -> (Expr -> b -> Expr -> b -> b) -> (Expr -> b -> Expr -> b -> b) -> (Expr -> b -> Expr -> b -> b) -> Expr -> b
recrExpr casoConst casoRango casoSuma casoResta casoMult casoDiv expr =
  case expr of
    Const f -> casoConst f
    Rango f1 f2 -> casoRango f1 f2
    Suma expr1 expr2 -> casoSuma expr1 (rec expr1) expr2 (rec expr2)
    Resta expr1 expr2 -> casoResta expr1 (rec expr1) expr2 (rec expr2)
    Mult expr1 expr2 -> casoMult expr1 (rec expr1) expr2 (rec expr2)
    Div expr1 expr2 -> casoDiv expr1 (rec expr1) expr2 (rec expr2)
  where rec = recrExpr casoConst casoRango casoSuma casoResta casoMult casoDiv


-- foldExpr :: ... anotar el tipo ...
--          cb const,    cb rango,       suma,              resta,           mult,            div
foldExpr :: (Float -> b) -> (Float -> Float -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr -> b
foldExpr casoConst casoRango casoSuma casoResta casoMult casoDiv expr =
  case expr of
    Const f -> casoConst f
    Rango f1 f2 -> casoRango f1 f2
    Suma expr1 expr2 -> casoSuma (rec expr1) (rec expr2)
    Resta expr1 expr2 -> casoResta (rec expr1) (rec expr2)
    Mult expr1 expr2 -> casoMult (rec expr1) (rec expr2)
    Div expr1 expr2 -> casoDiv (rec expr1) (rec expr2)
  where rec = foldExpr casoConst casoRango casoSuma casoResta casoMult casoDiv

-- | Evaluar expresiones dado un generador de números aleatorios
-- recordatorio: type G a = Gen -> (a, Gen)
      --Expr -> Gen -> (a,Gen)
eval :: Expr -> G Float
eval  = foldExpr (,) (\x y g-> dameUno (x,y) g) (generarValorYgenerador (+)) (generarValorYgenerador (-))
                                                            (generarValorYgenerador (*)) (generarValorYgenerador  (/))
generarValorYgenerador :: (Float->Float->Float)->G Float->G Float -> Gen-> (Float,Gen)
generarValorYgenerador f x y g  = let (a,g1)= x g
                                      (b,g2)= y g1
                                  in (f a b,g2)



-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
                          --(Gen -> (Float, Gen)) --(Gen -> (Histograma, Gen))

armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = (histograma m (rango95 (fst muestras)) (fst muestras), snd muestras)
                  where muestras = muestra f n g

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.

evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.

mostrar :: Expr -> String
mostrar = recrExpr show rango (\x recx y recy ->  maybeParen (esSuma x && esSuma y ) (suma recx recy))
                              (\x recx y recy -> resta (maybeParen (not (esConstORango x)) recx) (maybeParen (not (esConstORango y)) recy))
                              (\x recx y recy -> maybeParen (not( esMult x && esMult y) ) (mult recx recy))
                              (\x recx y recy -> div (maybeParen (not (esConstORango x)) recx) (maybeParen (not (esConstORango y)) recy))
              where rango x y = show x++" ~ "++show y; --constructores de strings
                    suma recx recy= recx++" + "++recy ;--constructores de strings
                    resta recx recy= recx++" - "++recy;--constructores de strings
                    div recx recy =  recx ++" / "++recy;--constructores de strings
                    mult recx recy = recx ++ " * " ++recy;--constructores de strings
                    esConstORango a= constructor a ==CEConst|| constructor a==CERango --bool de comparacion de maybe
                    esSuma a=constructor a == CESuma 
                    esMult a = constructor a == CEMult 

pepe= casilleros (agregar 2 (vacio 3 (0, 6)))== [ Casillero infinitoNegativo 0.0 0 0.0, Casillero 0.0 2.0 0 0.0,Casillero 2.0 4.0 1 100.0,Casillero 4.0 6.0 0 0.0, Casillero 6.0 infinitoPositivo 0 0.0]


data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
