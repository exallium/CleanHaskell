{-# LANGUAGE RecordWildCards #-}
module App.Data where

import System.Random
import Control.Monad.IO.Class

import App.Repositories
import App.Entities

data FakeRepo = FakeRepo

fakeCustomerId :: CustomerId
fakeCustomerId = CustomerId "fake id"

randomString :: Int -> IO String
randomString n = mapM (\_ -> randomIO :: IO Char) [1..n]

instance CustomerRepository FakeRepo where
	getCustomerById _ cId = return . Right $ Customer cId "fake customer"
	createCustomer _ Customer{..} = return . Right $ Customer fakeCustomerId displayName

instance AccountRepository FakeRepo where
	getAccountById _ aId = return . Right $ Account aId fakeCustomerId 100
	createAccount _ Account{..} = do
		rid <- liftIO $ randomString 10
		return . Right $ Account (AccountId rid) fakeCustomerId 100

instance TransactionRepository FakeRepo where
	createTransaction _ Transaction{..} = do
		rid <- liftIO $ randomString 10
		return . Right $ Transaction (TransactionId rid) transactionFromAccount transactionToAccount transactionAmount