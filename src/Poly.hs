module Poly where

import Data.ByteString

import Polysemy
import Polysemy.KVStore

------------------------------------------------------------------------------
newtype Username = Username ByteString
newtype Password = Password ByteString
newtype PasswordHash = PasswordHash ByteString


data CryptoHash m a where
  MakeHash :: Password -> CryptoHash m PasswordHash
  ValidateHash :: Password -> PasswordHash -> CryptoHash m Bool

makeHash :: Member CryptoHash r => Password -> Sem r PasswordHash
makeHash x =
  send (MakeHash x :: CryptoHash (Sem r) PasswordHash)

validateHash :: Member CryptoHash r => Password -> PasswordHash -> Sem r Bool
validateHash password hash =
  send (ValidateHash password passwordHash :: CryptoHash (Sem r) Bool)

-- KVStore effect is already defined and provided from polysemy-zoo
-- data KVStore k v m a

------------------------------------------------------------------------------
addUser
  :: Members [CryptoHash, KVStore Username PasswordHash] r
  => Username
  -> Password
  -> Sem r ()
addUser username password = do
  hashedPassword <- makeHash password
  writeKV username hashedPassword

validatePassword
  :: Members [CryptoHash, KVStore Username PasswordHash] r
  => Username
  -> Password
  -> Sem r Bool
validatePassword username password = do
  hashInStore <- lookupKV username
  case hashInStore of
    Just h -> validateHash password h
    Nothing -> pure False
