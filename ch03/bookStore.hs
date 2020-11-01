-- BookInfo is a type constructor
-- The Book that follows is a value constructor (or data constructor)
-- used to create a value of BookInfo type
data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 90523532 "Zolmiteton" ["Alexsty", "Hennisty", "Dylsty"]

-- This definition says that the type named BookReview
-- has a value constructor that is also named BookReview
data BookReview = BookReview BookInfo CustomerID String

-- Type synomyms (only aliases, nothing more)
type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

-- Algebraic data types (all data types defined with the keyword data are actually algebraic data types)
type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                 deriving (Show)

bookId       (Book id title authors) = id
bookTitle    (Book id title authors) = title
bookAuthors  (Book id title authors) = authors

nicerId      (Book id _ _)      = id
nicerTitle   (Book _ title _)   = title
nicerAuthors (Book _ _ authors) = authors

-- record syntax instead of writing accessor functions
data Customer = Customer {
    customerId :: CustomerID,
    customerName :: String,
    customerAddress :: Address
} deriving (Show)

-- would be identical to:
data CustomerAlt = CustomerAlt CustomerID String Address
                   deriving (Show)
customerAltId      (CustomerAlt id _ _)      = id
customerAltName    (CustomerAlt _ name _)    = name
customerAltAddress (CustomerAlt _ _ address) = address

-- still possible to use usual creation syntax
customer1 = Customer 28532 "Cristiano Ronaldo" ["Rua do Campeão", "Funchal, Madeira", "Portugal"]
customer2 = Customer {
    customerId = 82353,
    customerAddress = ["Rosaria Central", "Argentina"],
    customerName = "Lionel Messi"
}
-- in the second form, we can vary the order in which we list fields