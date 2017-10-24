module App.Repositories where

import Control.Monad.IO.Class

import App.Entities

class CustomerRepository c where
    getCustomerById :: (MonadIO m) => c -> CustomerId -> m (Either Error Customer)
    createCustomer :: (MonadIO m) => c -> Customer -> m (Either Error Customer)

class AccountRepository a where
    getAccountById :: (MonadIO m) => a -> AccountId -> m (Either Error Account)
    createAccount :: (MonadIO m) => a -> Account -> m (Either Error Account)

class TransactionRepository t where
    createTransaction :: (MonadIO m) => t -> Transaction -> m (Either Error Transaction)