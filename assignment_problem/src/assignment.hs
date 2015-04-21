import           Data.List
import           Data.Maybe
import           Data.String.Utils

--------------------------------------------------------------------------------------------------------
-- Data Types!
-- TODO: seperate this into its own file
data Person = 	Person 	{
							-- | A person must have a name and it must be a string
							name :: String,
							-- | The person has a Personality
							personality :: Personality
						}
				deriving (Show, Eq)

data Personality = 	Personality {
									-- | Person's desire for a high pary
									pay::Int,
									-- | The expected hours the Person is willing to work
									hours::Int,
									-- | The Person's desire for individual impact at the Company
									impact::Int,
									-- | The Person's oppourtunity to learn more at the Compay
									opportunity::Int
								}
					deriving (Show, Eq)

--implementations of Personality
entrepreneur :: Personality
entrepreneur = 	Personality {pay=4,	hours= -2,	impact=10,	opportunity=8}
moneygrubber :: Personality
moneygrubber = 	Personality {pay=10,hours= -1, 	impact=4,	opportunity=2}
academic :: 	Personality
academic = 		Personality {pay=2,	hours= -6, 	impact=8,	opportunity=10}
slacker :: 		Personality
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
				deriving (Show, Eq)

--implementations of Company
bigsoftwarefirm :: 	Company
bigsoftwarefirm = 	Company {payC=6,	hoursC=6, 	impactC=2, 	opportunityC=8}
investmentbank :: 	Company
investmentbank = 	Company {payC=10,	hoursC=10, 	impactC=3, 	opportunityC=4}
gradschool :: 		Company
gradschool = 		Company {payC=1,	hoursC=4, 	impactC=3,	opportunityC=10}
hedgefund :: 		Company
hedgefund = 		Company {payC=8,	hoursC=8, 	impactC=4,	opportunityC=6}
startup ::			Company
startup = 			Company {payC=4,	hoursC=8, 	impactC=10, opportunityC=8}


data Offer = Offer Person String Company String
			deriving (Show, Eq)


data Relationship 	= MortalEnemies
					| Married
					| Friends
					| Dating
					deriving (Show, Eq)

--------------------------------------------------------------------------------------------------------
-- make persons and offers
-- TODO: Relationships
makePersonality :: String -> Personality
makePersonality string
			| string == "Academic" = academic
			| string == "Entrepreneur" = entrepreneur
			| string == "Money Grubber" = moneygrubber
			| string == "Slacker" = slacker
			| otherwise = error ("Invalid Personality Type: " ++ string)

makeCompany :: String -> Company
makeCompany string
			| string == "Big Software Firm" = bigsoftwarefirm
			| string == "Investment Bank" = investmentbank
			| string == "Grad School" = gradschool
			| string == "Hedge Fund" = hedgefund
			| string == "Startup" = startup
			| otherwise = error ("Invalid Company Type: " ++ string)

-- Turning arrays into data types
makePerson :: [String] -> Person
makePerson [name,personality] = Person name (makePersonality personality)

getPerson :: String -> [Person] -> Person
getPerson str people = fromJust $ find (\(Person name personality) -> name == str) people

makeOffer :: [String] -> [Person] -> Offer
makeOffer [name, firm, company, place] people = Offer (getPerson name people) firm (makeCompany company) place

--------------------------------------------------------------------------------------------------------
-- I/O bullshit as always.
-- I really hate this stuff. It's uninspiring
removeCarriageReturn :: String -> String
removeCarriageReturn str = strip $ replace "\r" " " str

splitOnL :: String -> [String]
splitOnL str = map strip $ split "|" str

purgeInput :: String -> [[String]]
purgeInput str = map (splitOnL . removeCarriageReturn) (lines str)

dropPrevious :: [[String]] -> [[String]]
dropPrevious x = dropWhile (==[]) $ dropWhile (/=[]) x

-- Eh there's probably a way to do this better it's fast and
makePeopleArray :: [[String]] -> [[String]]
makePeopleArray str = drop 1 $ takeWhile (/=[]) $ dropWhile (==[]) str
makeOfferArray :: [[String]] -> [[String]]
makeOfferArray str = drop 1 $ takeWhile (/=[]) $ dropPrevious $ dropWhile (==[]) str
makeRelArray :: [[String]] -> [[String]]
makeRelArray str = drop 1 $ takeWhile (/=[]) $ dropPrevious $ dropPrevious $ dropWhile (==[]) str
--------------------------------------------------------------------------------------------------------
-- Basic Scoring
scoreCompany :: Personality -> Company -> Int
scoreCompany (Personality ppay phours pimpact popportunity) (Company cpay chours cimpact copportunity)
		= ppay*cpay + phours*chours + pimpact*cimpact + popportunity*copportunity

scoreOfferBase:: Person -> String -> Company -> String -> (Person, String, Int, String)
scoreOfferBase (Person name personality) b company d = (Person name personality, b, scoreCompany personality company, d)

scoreOffer :: Offer -> (Person, String, Int, String)
scoreOffer (Offer a b c d) = scoreOfferBase a b c d


--------------------------------------------------------------------------------------------------------
{-
My inclination is to do a breadth first search to essentially assign every possible job
I know this isn't efficient but I'm doing my best
-}

-- compare is backwards. Or at least in this case it is
sortOffers :: (Person, String, Int, String) -> (Person, String, Int, String) -> Ordering
sortOffers (_,_,firstVal,_) (_,_,secondVal,_) = compare secondVal firstVal

--assignJob :: Person -> [(Person, String, Int, String)] -> (Person, String)
--assignJob personToAssign [(person,comp,score,city)] = (personToAssign,comp,score,city)








--------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	inputFile <- getContents
	let purgedAndSeperated = purgeInput inputFile

	let peopleStrArray = makePeopleArray purgedAndSeperated
	let offerStrArray = makeOfferArray purgedAndSeperated
	--let relStrArray = makeRelArray purgedAndSeperated

	let peopleArray = map makePerson peopleStrArray
	let offerArray = map (`makeOffer` peopleArray) offerStrArray

	let scoredOffers = map scoreOffer offerArray

	print $ groupBy (\(person,_,_,_) (otherPerson,_,_,_) -> person==otherPerson) scoredOffers
	print $ sortBy sortOffers scoredOffers
