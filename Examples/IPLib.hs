{-# OPTIONS_GHC -Wall #-}
module Examples.IPLib  where

import FrontEnd.Core

type Pixel = (Exp Word32,(Exp Word32,Exp Word32))

mkPixel :: Exp Word32 -> Exp Word32 -> Exp Word32 -> Pixel
mkPixel r g b = (r , (g , b))

red :: Pixel -> Exp Word32
red pixel = fst pixel

green :: Pixel -> Exp Word32
green pixel = fst (snd pixel)

blue :: Pixel -> Exp Word32
blue pixel =  snd (snd pixel)

type Image = Vec (Vec Pixel)

mkImage :: Exp Word32 -> Exp Word32 ->
           (Exp Word32 -> Exp Word32 -> Pixel) -> Image
mkImage height width ixf =
  Vec height (\ i ->
    Vec width (\ j -> ixf i j))

heightImage :: Image -> Exp Word32
heightImage image = lenVec image

widthImage :: Image -> Exp Word32
widthImage image = lenVec (indVec image 0)

getPixel :: Image -> Exp Word32 -> Exp Word32 -> Pixel
getPixel  vec i j = indVec (indVec vec i) j

vecToImage :: Exp Word32 -> Exp Word32 -> Vec (Exp Word32) -> Image
vecToImage height width as =
                 mkImage height width (\ i j ->
                     mkPixel (indVec as ((j+i*width)*3))
                             (indVec as ((j+i*width)*3+1))
                             (indVec as ((j+i*width)*3+2)))

imageToVec :: Image -> Vec (Exp Word32)
imageToVec image = let height = heightImage image
                       width  = widthImage  image
                   in  Vec (height * width * 3)
                           (\ ii -> let i = divE (divE ii 3) width
                                        j = modE (divE ii 3) width
                                        p = getPixel image i j
                                    in  ((modE ii 3) ==. 0) ?
                                         (red   p
                                         , (modE ii 3 ==. 1) ?
                                            (green p ,
                                             blue  p)))

compileImageProcessor :: String -> (Image -> Image) -> IO ()
compileImageProcessor name f =
  makeIP
    (compile
       (\ imageVec ->
           let l = sqrtE (divE (lenVec imageVec) 3)
           in  imageToVec (f (vecToImage l l imageVec))))
       name
