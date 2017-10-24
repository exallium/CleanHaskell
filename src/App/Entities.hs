{-# LANGUAGE Rank2Types #-}
module App.Entities where

data CustomerId = CustomerId String | UnsavedCustomerId deriving (Show, Eq)
data Customer = Customer { customerId   :: CustomerId
                         , displayName  :: String 
                         } deriving Show

data AccountId = AccountId String | UnsavedAccountId deriving (Show, Eq)
data Account = Account { accountId      :: AccountId
                       , accountOwner   :: CustomerId
                       , accountBalance :: Int
                       } deriving Show

data TransactionId = TransactionId String | UnsavedTransactionId deriving (Show, Eq)
data Transaction = Transaction { transactionId          :: TransactionId
                               , transactionFromAccount :: AccountId
                               , transactionToAccount   :: AccountId
                               , transactionAmount      :: Int } deriving Show

newtype Error = Error String deriving Show