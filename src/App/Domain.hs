module App.Domain where

import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class

import App.State
import App.Repositories
import App.Entities

createCustomerWithName :: (CustomerRepository c, MonadIO m) => String -> StateT (App c a t) m (Either Error Customer)
createCustomerWithName dn = do
    repo <- getCustomerRepository
    createCustomer repo (Customer UnsavedCustomerId dn)

createAccountForLoggedInCustomer :: (CustomerRepository c, AccountRepository a, MonadIO m) => StateT (App c a t) m (Either Error Account)
createAccountForLoggedInCustomer = do
    user <- getLoggedInCustomer
    repo <- getAccountRepository
    let account = fmap mkAccount user
    either (return . Left) (createAccount repo) account

    where
    mkAccount c = Account UnsavedAccountId (customerId c) 0

performTransaction :: (CustomerRepository c, AccountRepository a, TransactionRepository t, MonadIO m) => AccountId -> AccountId -> Int -> StateT (App c a t) m (Either Error Transaction)
performTransaction from to amount = do
    user <- getLoggedInCustomer
    accountRepo <- getAccountRepository
    transactionRepo <- getTransactionRepository

    fromAccount <- getAccountById accountRepo from

    let ownsAccount = userOwnsAccount user fromAccount
    let fromHasBalance = hasBalance fromAccount amount
    let transaction = ownsAccount >> fromHasBalance >> return (Transaction UnsavedTransactionId from to amount)
    either (return . Left) (createTransaction transactionRepo) transaction

    -- ensure from and to are not the same account, and that user owns from account
    where
    
    userOwnsAccount :: Either Error Customer -> Either Error Account -> Either Error Bool
    userOwnsAccount user f = (==) . customerId <$> user <*> (accountOwner <$> f) >>= test (Error "User does not own account")

    hasBalance :: Either Error Account -> Int -> Either Error Bool
    hasBalance f balance = (>=balance) . accountBalance <$> f >>= test (Error "User has insufficient funds")

    test :: Error -> Bool -> Either Error Bool
    test _ True  = Right True
    test e False = Left e