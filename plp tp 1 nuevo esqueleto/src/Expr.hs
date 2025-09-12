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
{-lgtm-}
-- WHAT THE FUCK IS THIS SHIT
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
{-lgtm-}

{-
RECORDATORIO:
Definamos una función foldAB que abstraiga el esquema de
recursión estructural sobre árboles binarios.

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin Nil = cNil
foldAB cNil cBin (Bin i r d) =
  cBin (foldAB cNil cBin i) r (foldAB cNil cBin d)
-}

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
eval :: Expr -> G Float
eval expr g = recrExpr (\x -> (x,g)) (\x y  -> dameUno (x,y) g) (res (+)) (res (-)) ( res (*)) (res (/)) expr
  where res f expr1 rec1 expr2 rec2= (f (fst rec1) (fst (eval expr2 (snd rec1))),snd rec2)
-- Explicar que concha estamos haciendo aca...

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
mostrar = foldExpr show (\x y-> show x ++ " ~ " ++ show y ) (\recx recy -> recx ++ " + "++ recy) (\recx recy -> recx ++ " - "++ recy)
                        (\recx recy ->"("++ recx ++ " * "++ recy ++ ")") (\recx recy -> "("++recx ++ " / "++ recy++ ")")                                        

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
