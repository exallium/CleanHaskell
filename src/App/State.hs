{-# LANGUAGE Rank2Types #-}
module App.State where

import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class

import App.Repositories
import App.Entities

data AppDependencies c a t  = AppDependencies { customerRepository:: CustomerRepository c => c
                                              , accountRepository :: AccountRepository a => a
                                              , transactionRepository :: TransactionRepository t => t
                                              }

newtype AppState = AppState { loggedInCustomer :: Either Error CustomerId }
data App c a t = App { appDependencies :: AppDependencies c a t
                     , appState :: AppState 
                     }

getCustomerRepository :: (CustomerRepository c, Monad m) => StateT (App c a t) m c
getCustomerRepository = fmap (customerRepository . appDependencies) get

getAccountRepository :: (AccountRepository a, Monad m) => StateT (App c a t) m a
getAccountRepository = fmap (accountRepository . appDependencies) get

getTransactionRepository :: (TransactionRepository t, Monad m) => StateT (App c a t) m t
getTransactionRepository = fmap (transactionRepository . appDependencies) get

getLoggedInCustomer :: (CustomerRepository c, MonadIO m) => StateT (App c a t) m (Either Error Customer)
getLoggedInCustomer = do
    customer <- fmap (loggedInCustomer . appState) get
    repo <- getCustomerRepository
    either (return . Left) (getCustomerById repo) customer

setLoggedInCustomer :: (Monad m) => CustomerId -> StateT (App c a t) m (Either Error CustomerId)
setLoggedInCustomer cId = do
    app <- get
    let aSt = appState app
    let user = Right cId
    put app{appState=aSt{loggedInCustomer=user}}
    return user