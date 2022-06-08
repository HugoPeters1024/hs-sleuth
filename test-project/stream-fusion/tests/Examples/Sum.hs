import Data.List

main = print . sum . map read . lines =<< getContents

