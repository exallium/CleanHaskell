module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad (void)

import App.Domain
import App.State
import App.Repositories
import App.Data
import App.Entities

initialState :: App FakeRepo FakeRepo FakeRepo
initialState = App { appDependencies = AppDependencies { accountRepository = FakeRepo
                                                       , transactionRepository = FakeRepo
                                                       , customerRepository = FakeRepo
                                                       }
                   , appState = AppState { loggedInCustomer = Left . Error $ "Not Authorized" }
                   }

app :: (CustomerRepository c, AccountRepository a, TransactionRepository t) => StateT (App c a t) IO ()
app = void $ do
  setLoggedInCustomer fakeCustomerId
  a <- createAccountForLoggedInCustomer
  b <- createAccountForLoggedInCustomer
  let i = (,) <$> (accountId <$> a) <*> (accountId <$> b)
  r <- either (return . Left) (\(f, t) -> performTransaction f t 30) i
  liftIO $ print r
  return ()


main :: IO ()
main = void $ runStateT app initialState
