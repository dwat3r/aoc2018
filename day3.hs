import Text.Regex.Applicative

data Data = Data {
	id :: Int,
	xoff :: Int,
	yoff :: Int,
	xsize :: Int,
	ysize :: Int
	}

withInput f = readFile "day3.txt" 
				>>= parseInput
				>>= f
	where
		parseInput = . lines

