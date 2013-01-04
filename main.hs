{-# Language ViewPatterns, FlexibleInstances #-}

import Control.Monad
import Control.Arrow

import Control.Concurrent

import System.IO
import System.Process
import System.Random
import System.Locale (defaultTimeLocale)

import Data.Time

import qualified Data.ByteString.Lazy as B

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import System.Directory

appF :: (a -> b) -> a -> a -> (b, b)
appF f a b = (f a, f b)

configDir = ".nback"
dataBase = "userData"
dataPath = "/" ++ configDir ++ "/" ++ dataBase

lettersDir = "ress/"

letters = [
	"c.wav"
	,"h.wav"
	,"k.wav"
	,"l.wav"
	,"q.wav"
	,"r.wav"
	,"s.wav"
	,"t.wav"
	]

positionGrid code preStr
	| code <= 9 && code >= 0 = 
		let code' = code - 1 in
		concat . map ((++ "\n") . (preStr ++)) $
			takeRep 4 (row (calc 0 2 code'))
			++ [sep]
			++ takeRep 4 (row (calc 3 5 code'))
			++ [sep]
			++ takeRep 4 (row (calc 6 8 code'))
	| otherwise = []
	where	calc min max a = if a < min || a > max then 3 else a `mod` 3
		takeRep a = take a . repeat
		pat a b c = takeRep 8 a ++ "|" ++ takeRep 8 b ++ "|" ++ takeRep 8 c
		row 0 = pat '#' ' ' ' '
		row 1 = pat ' ' '#' ' '
		row 2 = pat ' ' ' ' '#'
		row _ = pat ' ' ' ' ' '
		sep = "----------------------------"

nr a b = if null a then [] else a ++ b

data Outputs a = Simple a | Complex (a, a) | None
	deriving (Ord, Eq)

{-instance Show a => Show (Outputs a) where
	show (Simple a) = show a
	show (Complex (a, b)) = "(" ++ show a ++ ", " ++ show b ++ ")"
-}

instance Show (Outputs Bool) where
	show (Simple a) = bool2StrInt a
	show (Complex (a, b)) = "(" ++ bool2StrInt a ++ ", " ++ bool2StrInt b ++ ")"

instance Show (Outputs [Bool]) where
	show (Simple a) = foldl (\a b -> nr a "," ++ (show . Simple) b) [] a
	show (Complex a) = (foldl (\a b -> nr a "," ++ (show . Complex) b) [] . uncurry zip) a

instance Show (Outputs Int) where
	show (Simple a) = show a
	show (Complex (a, b)) = "(" ++ show a ++ ", " ++ show b ++ ")"

instance Show (Outputs String) where
	show (Simple a) = show a
	show (Complex (a, b)) = "(" ++ show a ++ ", " ++ show b ++ ")"

bool2StrInt :: Bool -> String
bool2StrInt True = "1"
bool2StrInt False = "0"

instance Functor Outputs where
	fmap f (Simple a) = Simple $ f a
	fmap f (Complex (a,b)) = Complex (f a, f b)
	fmap f None = None

instance Binary a => Binary (Outputs a) where
	put (Simple x) = putWord8 1 >> put x
	put (Complex x) = putWord8 2 >> put x
	put None = error "Put: outputs type None ain't supported"
	get = do
		w <- getWord8
		case w of
			1 -> liftM Simple get
			2 -> liftM Complex get
			_ -> return None

fOutputs f (Simple a) (Simple b) = Simple $ f a b
fOutputs f (Complex (a,b)) (Complex (c,d)) = Complex $ (f a c, f b d)
fOutputs _ _ _ = error "Incorrect use of the function"

addNumOutputs = fOutputs (+)
addLstOutputs = fOutputs (++)

fromSimple (Simple a) = a
fromComplex (Complex a) = a

gameHigh :: GameType -> ([Int] -> a) -> Outputs a
gameHigh (Audio lst) f = Simple $ f $ map snd lst
gameHigh (Position lst) f = Simple $ f lst
gameHigh (Dual lst) f = Complex (f (map (snd . fst) lst), f (map snd lst))

--					date
--						nbackLevel
--							perRoundAmount
--								percent
data Stats = Stats	GameType	UTCTime	Int	Int	(Outputs Rational)

instance Binary UTCTime where
	put a = put $ show a
	get = liftM read get

instance Show Stats where
	show (Stats gType date level amount note) = formatTime defaultTimeLocale "%B %d %Y  %H:%M:%S" date 
		++ " : (" 
		++ show gType ++ " " ++ show level ++ "-back, " ++ show amount ++ " permutations)"
		++ " " ++ filter (/= '"') (show (fmap ((++ "%") . show . fromEnum) note))

instance Binary Stats where
	put (Stats a b c d e) = put a >> put b >> put c >> put d >> put e
	get = liftM5 Stats get get get get get

data GameType = Audio [(String, Int)] | Position [Int] | Dual [((String, Int), Int)] | GEmpty

instance Show GameType where
	show (Audio _) = "Audio"
	show (Position _) = "Position"
	show (Dual _) = "Dual (Audio & Position)"

instance Binary GameType where
	put (Audio _) = putWord8 1
	put (Position _) = putWord8 2
	put (Dual _) = putWord8 3
	get = do
		w <- getWord8
		case w of
			1 -> return $ Audio undefined
			2 -> return $ Position undefined
			3 -> return $ Dual undefined

data GameConfig = GameConfig {
	gameType :: GameType
--	nbackLevel
	,gameLevel :: Int
--	perRoundAmount
	,gameRoundAmount :: Int
--	delay
	,gameDelay :: Rational
	,gameStats :: [Stats]
}

instance Binary GameConfig where
	put (GameConfig a b c d e) = put a >> put b >> put c >> put d >> put e
	get = liftM5 GameConfig get get get get get

pickRandomLetter = (((lettersDir ++) . (letters !!)) &&& id) . (`mod` length letters)

pickRandomPosition = ([1 .. 9] !!) . (`mod` 9)

nbackLevel :: Int
nbackLevel = 3

perRoundAmount :: Int
perRoundAmount = 10

--in milliseconds
delay :: Rational
delay = 3000 * 10 ^^ (-3)

defaultGameConfig = GameConfig (Audio undefined) nbackLevel perRoundAmount delay []

solveRound level = flip slave 0
	where	slave [] result = result
		slave (x:xs) result = if length xs > (level - 1) && x == xs !! (level - 1)
					then slave xs $ result + 1
					else slave xs result

solveRound2 level = flip slave [] . reverse
	where	slave [] result = result
		slave (x:xs) result =
			if length xs > (level - 1) && x == xs !! (level - 1)
				then slave xs $ True : result
				else slave xs $ False : result

randomList 0 g result = (result, g)
randomList n g result =
	let (r, g') = random g in
	randomList (n - 1) g' $ r : result

populateRound f n = map f . take n

preRound gType level permutations delay = do
	runCommand "clear" >>= waitForProcess
	putStrLn []
	putStrLn $ show gType ++ " " ++ show level ++ "-Back, " ++ show permutations ++ " permutations"
	putStrLn $ "Delay : " ++ show delay
	putStrLn []
	putStrLn ("The goal of this training is to practice short term memory.\n"
				++ "To do this, within " ++ show permutations ++ " permutations \n"
				++ "you must detect duplications that were exactly\n"
				++ "on the past " ++ show level ++ " permutation" ++ (if level > 1 then "s" else []) ++ " (inclusive) "
				++ "from the current permutation.\n"
				++ "Example, on 2-back, you have n - k - n, which is a match.\n"
				++ "on 3-back, you have : n - k - l - n, which is a match.\n"
				++ "etc."
		)
	putStrLn []
	putStrLn $ case gType of
		(Audio _) -> "During the round, when you think there is a\n"
				++ "sound that was played back in the correct sequence, just press any key.\n"
				++ "Otherwise, don't press anything\n"
		(Position _) -> "During the round, when you think there is a\n"
				++ "position that was back in the correct sequence, just press any key.\n"
				++ "Otherwise, don't press anything\n"
		(Dual _) -> "During the round, when you think there is a\n"
				++ "position or/and a sound that was back in the correct sequence\n"
				++ "you have to press `l' (location) for the position and `a' (audio) for the sound.\n"
				++ "It is possible that both are at the same time.\n"
				++ "Otherwise, don't press anything\n"
	putStrLn "Your result will be shown after the round."
	putStrLn []
	putStrLn "Press Any Key to Continue"
	getChar
	runCommand "clear" >>= waitForProcess
	putStrLn "Get Ready, the round is about to start in :"
	threadDelay 1000000
	putStrLn "5"
	threadDelay 1000000
	putStrLn "4"
	threadDelay 1000000
	putStrLn "3"
	threadDelay 1000000
	putStrLn "2"
	threadDelay 1000000
	putStrLn "1"
	threadDelay 1000000

stimulus (Audio ((x, _):xs)) = do
	blackhole <- openFile "/dev/null" WriteMode
	createProcess (proc "aplay" [x]) {std_out = UseHandle blackhole, std_err = UseHandle blackhole}
	putStrLn "\n\n\n\n\n"
	putStrLn $ positionGrid 5 "\t\t\t\t\t\t"
	if not $ null xs
		then return $ Audio xs
		else return $ Audio []
stimulus (Position (x:xs)) = do
	putStrLn "\n\n\n\n\n"
	putStrLn $ positionGrid x "\t\t\t\t\t\t"
	if not $ null xs
		then return $ Position xs
		else return $ Position []
stimulus (Dual (((x, _), y):xs)) = do
	blackhole <- openFile "/dev/null" WriteMode
	createProcess (proc "aplay" [x]) {std_out = UseHandle blackhole, std_err = UseHandle blackhole}
	putStrLn "\n\n\n\n\n"
	putStrLn $ positionGrid y "\t\t\t\t\t\t"
	if not $ null xs
		then return $ Dual xs
		else return $ Dual []

purgeStdin = do
	inputAvail <- hReady stdin
	if inputAvail
		then getChar >> purgeStdin
		else return ()

nbackRound _ (Audio []) result = return result
nbackRound _ (Position []) result = return result
nbackRound _ (Dual []) result = return result
nbackRound delay lst result = do
	let output = case lst of 
			(Dual _) -> Complex (False, False)
			_ -> Simple False
	let properResult = case result of
				None -> case lst of
						(Dual _) -> Complex ([], [])
						_ -> Simple []
				_ -> result
					
	purgeStdin -- this avoids any previous inputs to pollute our stdin, in case say the user pressed more than once.
	runCommand "clear" >>= waitForProcess
	let delay' = fromEnum ((delay / 2) * (10 ^ 3))
	blackhole <- openFile "/dev/null" WriteMode
	sTime <- getCurrentTime

	next <- stimulus lst

	inputAvail <- hWaitForInput stdin delay'
	result' <- handleInput output

	--putStrLn $ show result'
	eTime <- getCurrentTime
	let diffTime = diffUTCTime eTime sTime
	(if diffTime < fromRational (delay / 2)
		then threadDelay $ fromEnum $ ((fromRational (delay / 2)) - diffTime) / (10 ^ 6)
		else return ()
		)
	runCommand "clear" >>= waitForProcess
	putStrLn "\n\n\n\n\n"
	putStrLn $ positionGrid 0 "\t\t\t\t\t\t"

	--putStrLn $ show result'

	inputAvail <- hWaitForInput stdin delay'
	result2 <- handleInput result'

	--putStrLn $ show result2

	eTime <- getCurrentTime
	let diffTime = diffUTCTime eTime sTime
	(if diffTime < fromRational delay
		then threadDelay $ fromEnum $ ((fromRational delay) - diffTime) / (10 ^ 6)
		else return ()
		)
	nbackRound delay next (properResult `addLstOutputs` fmap (: []) result2)
	where	handleInput (Simple result) = do
			inputAvail <- hReady stdin
			if inputAvail
				then getChar >> return (Simple True)
				else return $ Simple result 
		handleInput (Complex result) = do
			inputAvail <- hReady stdin
			if inputAvail
				then do
					c <- getChar

					case c of
						'a' -> return $ Complex (True, snd result)
						'l' -> return $ Complex (fst result, True)
						_ -> return $ Complex result
				else return $ Complex result

percent :: Int -> Int -> Rational
percent a b = (toRational a / toRational b) * 100

-- generates a round which has a proper, minimum amount
-- of entries.
genProperRound :: GameType -> Int -> Int -> Int -> IO GameType
genProperRound gtype level permutations min = do
	gen <- getStdGen
	let (result, gen') = populate gtype gen
	setStdGen gen'
	checkMin result
	where	populate (Audio _) = ((Audio . populateRound pickRandomLetter permutations . fst) &&& snd) . flip (randomList permutations) []
		populate (Position _) = ((Position . populateRound pickRandomPosition permutations . fst) &&& snd) . flip (randomList permutations) []
		populate (Dual _) = \gen ->
			let 
				(rands, gen') = randomList permutations gen []
				(rands2, gen'') = randomList permutations gen' []
			in
			(Dual (zip (populateRound pickRandomLetter permutations rands) (populateRound pickRandomPosition permutations rands2)), gen'')

		checkMin (Audio a) = if gameHigh (Audio a) (solveRound level) < Simple min
						then genProperRound gtype level permutations min
						else return $ Audio a
		checkMin (Position a) = if gameHigh (Position a) (solveRound level) < Simple min
						then genProperRound gtype level permutations min
						else return $ Position a
		checkMin (Dual elem) = case gameHigh (Dual elem) (solveRound level) of
						Complex (a, b) -> do
							(Audio aud) <- if a < min
									then genProperRound (Audio undefined) level permutations min
									else return $ Audio $ fst $ unzip elem
							(Position pos) <- if b < min
										then genProperRound (Position undefined) level permutations min
										else return $ Position $ snd $ unzip elem
							return $ Dual $ zip aud pos

execRound :: GameType -> Int -> Int -> Rational -> IO (Outputs Rational)
execRound gType level permutations delay = do
	preRound gType level permutations delay
	runCommand "clear" >>= waitForProcess
	putStrLn "The round has started!"
	round <- genProperRound gType level permutations 2

	sTime <- getCurrentTime

	-- This is the core of the round
	result <- nbackRound delay round None

	eTime <- result `seq` getCurrentTime

	let interim = fOutputs zip result (gameHigh round giveSolve)
	let goodAttempts = fmap (foldl (\a (b, c) -> a + if b == c && b == True then 1 else 0) 0) interim
	let attempts = fmap (length . filter (== True)) result
	let badAttempts = fOutputs (-) attempts goodAttempts
	let rightAnswers = fmap (foldl (\a (b, c) -> a + if b == c then 1 else 0) 0) interim

	putStrLn ("round time : " ++ show (diffUTCTime eTime sTime))
	putStr ("result for " ++ show attempts ++ " attempts : " ++ show goodAttempts ++ " good " ++ show badAttempts ++ " bad")
	putStrLn $ "\t" ++ (++ "%") (show (fmap fromEnum (fmap (`percent` permutations) rightAnswers)))

	putStrLn $ "User input :\t\t" ++ show result
	putStrLn $ "Correct answer :\t" ++ show (gameHigh round giveSolve)

	return $ fmap (`percent` permutations) rightAnswers
	where	giveSolve = solveRound2 level

changeConfig config = do
	runCommand "clear" >>= waitForProcess
	putStrLn "Configuration menu"
	putStrLn "Make a choice :"
	putStrLn "\t[g] : Change the game Type"
	putStrLn "\t[n] : Change the n-Back level"
	putStrLn "\t[p] : Change the amount of permutations per round"
	putStrLn "\t[d] : Change the delay of wait per permutation"
	putStrLn "\t[q] : Back to the Main Menu"

	c <- getChar
	case c of
		'g' -> do
			n <- gameTypeSetting $ gameType config
			changeConfig $ config {gameType = n}
		'n' -> do
			n <- setSetting 1 0 $ gameLevel config
			let config' = if gameRoundAmount config < n * 4
					then config {gameRoundAmount = n * 4}
					else config
			changeConfig $ config' {gameLevel = n}
		'p' -> do
			n <- setSetting (gameLevel config * 4) 0 $ gameRoundAmount config
			changeConfig $ config {gameRoundAmount = n}
		'd' -> do
			n <- setSetting 1 0 $ gameDelay config
			changeConfig $ config {gameDelay = n}
		'q' -> return config
		't' -> do
			nSetting <- setSetting 0 0 10
			nSetting `seq` runCommand "clear" >>= waitForProcess
			putStrLn $ "New value : " ++ show nSetting
			putStrLn "Press any Key to Continue"
			getChar
			changeConfig config
		_ -> putStrLn ("Incorrect command `" ++ [c] ++ "\'") >> threadDelay 1000000 >> changeConfig config
	where
		setSetting min max value = do
			runCommand "clear" >>= waitForProcess
			putStrLn "Press `u' to raise the value"
			putStrLn "Press `d' to drop the value"
			putStrLn "Press `q' or <enter> when satisfied"
			putStrLn []
			putStrLn $ "Setting's value : " ++ show value
			
			c <- getChar
			case c of
				'q' -> return value
				'\n' -> return value
				'u' -> if max == 0 || value < max then setSetting min max (value + 1) else setSetting min max value
				'd' -> if min == 0 || value > min then setSetting min max (value - 1) else setSetting min max value
				_ -> putStrLn ("Incorrect command `" ++ [c] ++ "\'") >> threadDelay 1000000 >> setSetting min max value 

		gameTypeSetting value = do
			runCommand "clear" >>= waitForProcess
			putStrLn $ "Current Game Type : " ++ show value
			putStrLn []
			putStrLn "Press `a' for Audio only"
			putStrLn "Press `p' for Position only"
			putStrLn "Press `d' for dual audio and position"
			putStrLn "Press `q' or <enter> when satisfied"

			c <- getChar
			case c of
				'q' -> return value
				'\n' -> return value
				'a' -> return (Audio undefined)
				'p' -> return (Position undefined)
				'd' -> return (Dual undefined)
				_ -> putStrLn ("Incorrect command `" ++ [c] ++ "\'") >> threadDelay 1000000 >> gameTypeSetting value 

mainMenu config = do
	home <- getHomeDirectory
	hSetEcho stdin False
	runCommand "clear" >>= waitForProcess
	putStrLn "N-back game main menu"
	putStrLn "Make a choice :"
	putStrLn "\t[c] : Change Configuration"
	putStrLn "\t[s] : Start round"
	putStrLn "\t[q] : Quit"
	putStrLn []
	
	if null (gameStats config) then return () else putStrLn "Latest score :" >> (putStrLn $ foldl (\a b -> nr a "\n" ++ show b) [] $ takeStats $ gameStats config)
	hSetBuffering stdin NoBuffering
	c <- getChar
	case c of
		'c' -> do
			config' <- changeConfig config 
			B.writeFile (home ++ dataPath) $ runPut  $ put config'
			mainMenu config'
		's' -> do
			result <- execRound (gameType config) (gameLevel config) (gameRoundAmount config) (gameDelay config)
			cTime <- result `seq` getCurrentTime
			putStrLn "Press any Key to Continue"
			purgeStdin >> getChar
			B.writeFile (home ++ dataPath) $ runPut $ put $ config {gameStats= (createStat cTime config result) : gameStats config}
			mainMenu $ config {gameStats= (createStat cTime config result) : gameStats config}
		'g' -> do
			putStrLn $ positionGrid 0 "\t\t\t\t\t\t"
			putStrLn []
			putStrLn "Press any Key to Continue"
			getChar
			mainMenu config
		'q' -> return ()
		_ -> putStrLn ("Incorrect command `" ++ [c] ++ "'") >> threadDelay 1000000 >> mainMenu config
	where
		createStat date cfg raw = Stats (gameType cfg) date (gameLevel cfg) (gameRoundAmount cfg) raw

		takeStats = reverse . take 10 . reverse

main :: IO ()
main = do
	home <- getHomeDirectory
	createDirectoryIfMissing False $ home ++ "/" ++ configDir
	exists <- doesFileExist $ home ++ dataPath
	if not exists
		then mainMenu defaultGameConfig
		else do
			gameConfig <- B.readFile $ home ++ dataPath
			if B.null gameConfig
				then mainMenu defaultGameConfig
				else mainMenu $ runGet (do a <- get; return (a :: GameConfig)) gameConfig

