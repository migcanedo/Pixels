{-|
Module      : Pixels
Descripcion : Representacion de caracteres ASCII en mapa de Bits.
Autores     : Miguel Canedo 13-10214    (2018)
              Andres Buelvas 13-10184 (2018)
Fuente     : https://github.com/migcanedo/ProyectoHaskell


-}

module Pixels 
    (   
        --* Tipo.
        Pixel,
        --* Funciones.
        font,
        pixelsToString,
        pixelListToPixels,
        pixelListToString,
        concatPixels,
        up,
        down,
        left,
        right,
        upsideDown,
        backwards,
        negative
    ) where

import Data.List (transpose, intercalate)


-----------------------------------------------------------------
-----------------------------------------------------------------
--                        Tipo Pixel                           --
-----------------------------------------------------------------
-----------------------------------------------------------------

{- | Alias del tipo [String] que sera utilizado para contener el mapa de bits
de tamaño 7x5, donde el valor 1 se representa por un '*' (Asterisco) y 
el valor cero por un ' ' (Espacio en blanco).
-}
type Pixel = [String]


-----------------------------------------------------------------
-----------------------------------------------------------------
--               Representaciones en el Mapa.                  --
-----------------------------------------------------------------
-----------------------------------------------------------------
fontBitmap =
  [
    [ 0x00, 0x00, 0x00, 0x00, 0x00 ], --  (space)
    [ 0x00, 0x00, 0x5F, 0x00, 0x00 ], --  !
    [ 0x00, 0x07, 0x00, 0x07, 0x00 ], --  "
    [ 0x14, 0x7F, 0x14, 0x7F, 0x14 ], --  #
    [ 0x24, 0x2A, 0x7F, 0x2A, 0x12 ], --  $
    [ 0x23, 0x13, 0x08, 0x64, 0x62 ], --  %
    [ 0x36, 0x49, 0x55, 0x22, 0x50 ], --  &
    [ 0x00, 0x05, 0x03, 0x00, 0x00 ], --  '
    [ 0x00, 0x1C, 0x22, 0x41, 0x00 ], --  (
    [ 0x00, 0x41, 0x22, 0x1C, 0x00 ], --  )
    [ 0x08, 0x2A, 0x1C, 0x2A, 0x08 ], --  *
    [ 0x08, 0x08, 0x3E, 0x08, 0x08 ], --  +
    [ 0x00, 0x50, 0x30, 0x00, 0x00 ], --  ,
    [ 0x08, 0x08, 0x08, 0x08, 0x08 ], --  -
    [ 0x00, 0x60, 0x60, 0x00, 0x00 ], --  .
    [ 0x20, 0x10, 0x08, 0x04, 0x02 ], --  /
    [ 0x3E, 0x51, 0x49, 0x45, 0x3E ], --  0
    [ 0x00, 0x42, 0x7F, 0x40, 0x00 ], --  1
    [ 0x42, 0x61, 0x51, 0x49, 0x46 ], --  2
    [ 0x21, 0x41, 0x45, 0x4B, 0x31 ], --  3
    [ 0x18, 0x14, 0x12, 0x7F, 0x10 ], --  4
    [ 0x27, 0x45, 0x45, 0x45, 0x39 ], --  5
    [ 0x3C, 0x4A, 0x49, 0x49, 0x30 ], --  6
    [ 0x01, 0x71, 0x09, 0x05, 0x03 ], --  7
    [ 0x36, 0x49, 0x49, 0x49, 0x36 ], --  8
    [ 0x06, 0x49, 0x49, 0x29, 0x1E ], --  9
    [ 0x00, 0x36, 0x36, 0x00, 0x00 ], --  :
    [ 0x00, 0x56, 0x36, 0x00, 0x00 ], --  ;
    [ 0x00, 0x08, 0x14, 0x22, 0x41 ], --  <
    [ 0x14, 0x14, 0x14, 0x14, 0x14 ], --  =
    [ 0x41, 0x22, 0x14, 0x08, 0x00 ], --  >
    [ 0x02, 0x01, 0x51, 0x09, 0x06 ], --  ?
    [ 0x32, 0x49, 0x79, 0x41, 0x3E ], --  @
    [ 0x7E, 0x11, 0x11, 0x11, 0x7E ], --  A
    [ 0x7F, 0x49, 0x49, 0x49, 0x36 ], --  B
    [ 0x3E, 0x41, 0x41, 0x41, 0x22 ], --  C
    [ 0x7F, 0x41, 0x41, 0x22, 0x1C ], --  D
    [ 0x7F, 0x49, 0x49, 0x49, 0x41 ], --  E
    [ 0x7F, 0x09, 0x09, 0x01, 0x01 ], --  F
    [ 0x3E, 0x41, 0x41, 0x51, 0x32 ], --  G
    [ 0x7F, 0x08, 0x08, 0x08, 0x7F ], --  H
    [ 0x00, 0x41, 0x7F, 0x41, 0x00 ], --  I
    [ 0x20, 0x40, 0x41, 0x3F, 0x01 ], --  J
    [ 0x7F, 0x08, 0x14, 0x22, 0x41 ], --  K
    [ 0x7F, 0x40, 0x40, 0x40, 0x40 ], --  L
    [ 0x7F, 0x02, 0x04, 0x02, 0x7F ], --  M
    [ 0x7F, 0x04, 0x08, 0x10, 0x7F ], --  N
    [ 0x3E, 0x41, 0x41, 0x41, 0x3E ], --  O
    [ 0x7F, 0x09, 0x09, 0x09, 0x06 ], --  P
    [ 0x3E, 0x41, 0x51, 0x21, 0x5E ], --  Q
    [ 0x7F, 0x09, 0x19, 0x29, 0x46 ], --  R
    [ 0x46, 0x49, 0x49, 0x49, 0x31 ], --  S
    [ 0x01, 0x01, 0x7F, 0x01, 0x01 ], --  T
    [ 0x3F, 0x40, 0x40, 0x40, 0x3F ], --  U
    [ 0x1F, 0x20, 0x40, 0x20, 0x1F ], --  V
    [ 0x7F, 0x20, 0x18, 0x20, 0x7F ], --  W
    [ 0x63, 0x14, 0x08, 0x14, 0x63 ], --  X
    [ 0x03, 0x04, 0x78, 0x04, 0x03 ], --  Y
    [ 0x61, 0x51, 0x49, 0x45, 0x43 ], --  Z
    [ 0x00, 0x00, 0x7F, 0x41, 0x41 ], --  [
    [ 0x02, 0x04, 0x08, 0x10, 0x20 ], --  \
    [ 0x41, 0x41, 0x7F, 0x00, 0x00 ], --  ]
    [ 0x04, 0x02, 0x01, 0x02, 0x04 ], --  ^
    [ 0x40, 0x40, 0x40, 0x40, 0x40 ], --  _
    [ 0x00, 0x01, 0x02, 0x04, 0x00 ], --  `
    [ 0x20, 0x54, 0x54, 0x54, 0x78 ], --  a
    [ 0x7F, 0x48, 0x44, 0x44, 0x38 ], --  b
    [ 0x38, 0x44, 0x44, 0x44, 0x20 ], --  c
    [ 0x38, 0x44, 0x44, 0x48, 0x7F ], --  d
    [ 0x38, 0x54, 0x54, 0x54, 0x18 ], --  e
    [ 0x08, 0x7E, 0x09, 0x01, 0x02 ], --  f
    [ 0x08, 0x14, 0x54, 0x54, 0x3C ], --  g
    [ 0x7F, 0x08, 0x04, 0x04, 0x78 ], --  h
    [ 0x00, 0x44, 0x7D, 0x40, 0x00 ], --  i
    [ 0x20, 0x40, 0x44, 0x3D, 0x00 ], --  j
    [ 0x00, 0x7F, 0x10, 0x28, 0x44 ], --  k
    [ 0x00, 0x41, 0x7F, 0x40, 0x00 ], --  l
    [ 0x7C, 0x04, 0x18, 0x04, 0x78 ], --  m
    [ 0x7C, 0x08, 0x04, 0x04, 0x78 ], --  n
    [ 0x38, 0x44, 0x44, 0x44, 0x38 ], --  o
    [ 0x7C, 0x14, 0x14, 0x14, 0x08 ], --  p
    [ 0x08, 0x14, 0x14, 0x18, 0x7C ], --  q
    [ 0x7C, 0x08, 0x04, 0x04, 0x08 ], --  r
    [ 0x48, 0x54, 0x54, 0x54, 0x20 ], --  s
    [ 0x04, 0x3F, 0x44, 0x40, 0x20 ], --  t
    [ 0x3C, 0x40, 0x40, 0x20, 0x7C ], --  u
    [ 0x1C, 0x20, 0x40, 0x20, 0x1C ], --  v
    [ 0x3C, 0x40, 0x30, 0x40, 0x3C ], --  w
    [ 0x44, 0x28, 0x10, 0x28, 0x44 ], --  x
    [ 0x0C, 0x50, 0x50, 0x50, 0x3C ], --  y
    [ 0x44, 0x64, 0x54, 0x4C, 0x44 ], --  z
    [ 0x00, 0x08, 0x36, 0x41, 0x00 ], --  {
    [ 0x00, 0x00, 0x7F, 0x00, 0x00 ], --  |
    [ 0x00, 0x41, 0x36, 0x08, 0x00 ]  --  }
  ]

-----------------------------------------------------------------
-----------------------------------------------------------------
--                  Funciones Auxiliares.                      --
-----------------------------------------------------------------
-----------------------------------------------------------------
{-
    Función auxiliar  de la función 'font' que dado un String de 
    tamaño menor que 7, le agrega al comienzo del mismo tanto 
    espacios en blanco necesite para que su tamaño sea exactamente 7.
-}
config :: String -> String
config xs = if (n < 7) then replicate (7 - n) ' ' ++ xs else xs
    where n = length xs

{-
    Función auxiliar de la función font que dado un nùmero entero,
    calcula su Representacion Binaria de tal forma que el 1 es 
    representado por '*' y el 0 por ' '.
-}
hexToBin :: Integer -> String
hexToBin 0 = ""
hexToBin n 
    | n `mod` 2 == 0 = hexToBin (n `div` 2) ++ " "
    | otherwise = hexToBin (n `div` 2) ++ "*"

-----------------------------------------------------------------
-----------------------------------------------------------------
--                  Funciones del Modulo.                      --
-----------------------------------------------------------------
-----------------------------------------------------------------
{- | Función que dado un caracter, devuelve el Pixel que representa 
    al mismo. En caso de que dicho caracter no sea un carcater ASCII,
    la función devolverá un Pixel con todos los bits encendidos.

-}
font :: Char -> Pixel
font l 
  | (asciiValue >= 32 && asciiValue <= 125) = rotarl $ map (config . hexToBin) (fontBitmap !! (asciiValue - 32))
  | otherwise = rotarl $ map hexToBin [0x7F, 0x7F, 0x7F, 0x7F, 0x7F]
  where asciiValue = fromEnum l
        rotarl = reverse . transpose

{- | Función que dado un Pixel, devuelve un String donde separa cada 
    fila del Pixel por un salto de linea ('\n').
-}
pixelsToString :: Pixel -> String
pixelsToString = concat . map (++"\n")

{- | Función que dada una lista de Pixels, devuelve un Pixel único 
    que contenga a todos los Pixels de la lista separanadolos por una 
    fila vacia.
-}
pixelListToPixels :: [Pixel] -> Pixel
pixelListToPixels = intercalate [""]

{- | Función que dada una lista de Pixels, devuelve un String donde 
    separa cada fila del Pixel por un salto de linea ('\n').
-}
pixelListToString :: [Pixel] -> String
pixelListToString = concat . map pixelsToString

{- | Función que dada una lista de Pixels, devuleve un Pixel único con
    todos los Pixels de la lista concatenados de forma Horizontal.
-}
concatPixels :: [Pixel] -> Pixel
concatPixels =  map concat . transpose

{- | Función que dado un String, devuelve un Pixel que es construido 
    mediante la concatenación horizontal de los caracteres, dejando 
    un espacio en balcon entre ellos.
-}
messageToPixels :: String -> Pixel
messageToPixels = map (intercalate " ") . transpose . map font 

{- | Función que dado un Pixel, devuelve un Pixel con todas sus filas 
    desplazadas una posición hacia arriba, mandando la primera fila a 
    la última posición.
-}
up :: Pixel -> Pixel
up xs = tail xs ++ [head xs]

{- | Función que dado un Pixel, devuelve un Pixel con todas sus filas 
    desplazadas una posición hacia abajo, mandando la última fila a 
    la primera posición.
-}
down :: Pixel -> Pixel
down xs = last xs : init xs

{- | Función que dado un Pixel, devuelve un Pixel con todas sus columnas
    desplazadas una posición hacia izquierda, mandando la primera columna 
    a la última posición.
-}
left :: Pixel -> Pixel
left = map f
  where f xs = tail xs ++ [head xs]

{- | Función que dado un Pixel, devuelve un Pixel con todas sus columnas 
    desplazas una posición hacia derecha, mandando la última columna 
    a la primera posición.
-}
right :: Pixel -> Pixel
right = map f
  where f xs = last xs : init xs

--| Función que dado un Pixel, devuelve el reverso vertical de ese Pixel.
upsideDown :: Pixel -> Pixel
upsideDown = reverse

--| Función que dado un Pixel, devuelve el reverso horizontal de ese Pixel.
backwards :: Pixel -> Pixel
backwards = map reverse

{- | Función que dado un Pixel, devuelve un Pixel donde cada bit será el 
    opuesto al original.
-}
negative :: Pixel -> Pixel
negative = map $ map inv
  where inv = (\p -> if (p == '*') then ' ' else '*')