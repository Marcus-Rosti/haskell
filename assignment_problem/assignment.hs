import Data.List
import Data.String.Utils

--------------------------------------------------------------------------------------------------------
-- Data Types! 
-- TODO: seperate this into its own file
data Person = 	Person 	{ 	
							-- | A person must have a name and it must be a string
							name :: String, 
							-- | The person has a Personality
							personality :: Personality 
						}
				deriving (Show)

data Personality = 	Personality { 	
									-- Person's desire for a high pary
									pay::Int, 
									-- The expected hours the Person is willing to work
									hours::Int,
									-- The Person's desire for individual impact at the Company
									impact::Int,
									-- The Person's oppourtunity to learn more at the Compay
									opportunity::Int
								}
					deriving (Show)

--implementations of Personality
entrepreneur = 	Personality {pay=4,	hours= -2,	impact=10,	opportunity=8}
moneygrubber = 	Personality {pay=10,hours= -1, 	impact=4,	opportunity=2}
academic = 		Personality {pay=2,	hours= -6, 	impact=8,	opportunity=10}
slacker = 		Personality {pay=6,	hours= -10,	impact=2,	opportunity=2}

data Company = 	Company {	
							-- How much the employee will be paid
							payC::Int, 
							-- The expected hours the employee will work
							hoursC::Int,
							-- The employee's individual impact at the Company
							impactC::Int,
							-- The employee's oppourtunity to learn more
							opportunityC::Int
						}
				deriving (Show)

--implementations of Company
bigsoftwarefirm = 	Company {payC=6,	hoursC=6, 	impactC=2, 	opportunityC=8}
investmentbank = 	Company {payC=10,	hoursC=10, 	impactC=3, 	opportunityC=4}
gradschool = 		Company {payC=1,	hoursC=4, 	impactC=3,	opportunityC=10}
hedgefund = 		Company {payC=8,	hoursC=8, 	impactC=4,	opportunityC=6}
startup = 			Company {payC=4,	hoursC=8, 	impactC=10, opportunityC=8}


data Offer = Offer Person String Company String

data Relationship 	= MortalEnemies
					| Married
					| Friends
					| Dating
					deriving (Show)


--------------------------------------------------------------------------------------------------------
-- Basic Scoring

scoreCompany :: Personality -> Company -> Int
scoreCompany (Personality pay hours impact opportunity) (Company payC hoursC impactC opportunityC) 
		= pay*payC + hours*hoursC + impact*impactC + opportunity*opportunityC

scoreOfferBase:: Person -> String -> Company -> String -> (String, Int)
scoreOfferBase (Person name personality) b company d = ( b, (scoreCompany personality company) )

scoreOffer :: Offer -> (String, Int)
scoreOffer (Offer a b c d) = scoreOfferBase a b c d

--------------------------------------------------------------------------------------------------------
makePersonality :: String -> Personality
makePersonality string 
			| string == "Academic" = academic
			| string == "Entrepreneur" = entrepreneur
			| string == "Money Grubber" = moneygrubber
			| string == "Slacker" = slacker


-- Turning arrays into data types
makePerson :: [String] -> Person
makePerson (name:personality:[]) = Person name (makePersonality personality)



--------------------------------------------------------------------------------------------------------
-- I/O bullshit as always.
-- I really hate this stuff. It's uninspiring 
removeCarriageReturn :: String -> String
removeCarriageReturn str = strip $ replace "\r" " " str

splitOnL :: String -> [String]
splitOnL str = map strip $ split "|" str

purgeInput :: String -> [[String]]
purgeInput str = map splitOnL $ map removeCarriageReturn $ lines str


--Eh there's probably a way to do this better it's fast and 
makePeopleArray str = drop 1 $ takeWhile (/=[]) $ dropWhile (==[]) str
makeOfferArray str = drop 1 $ takeWhile (/=[]) $ dropWhile (==[]) $ dropWhile (/=[]) $ dropWhile (==[]) str
makeRelArray str = drop 1 $ takeWhile (/=[]) $ dropWhile (==[]) $ dropWhile (/=[]) $ dropWhile (==[]) $ dropWhile (/=[]) $ dropWhile (==[]) str
--------------------------------------------------------------------------------------------------------

main = do 
	inputFile <- readFile "inputfile2.txt"
	let purgedAndSeperated = purgeInput inputFile
	let peopleStrArray = makePeopleArray purgedAndSeperated
	let offerStrArray = makeOfferArray purgedAndSeperated
	let relStrArray = makeRelArray purgedAndSeperated
	let peopleArray = map makePerson peopleStrArray
	print peopleArray





