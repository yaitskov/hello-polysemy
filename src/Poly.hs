{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-

Todo:
 - implement CryptoHash
 - choose KVStore implementation (sqlite?)
 - test
 - app and opt-parse applicative
 - logging with polysemy co-log
-}
module Poly where

import Crypto.KDF.BCrypt as BC
import Crypto.Random

import Data.ByteString

import Polysemy
import Polysemy.Internal
import Polysemy.KVStore
import Polysemy.State

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
  send (ValidateHash password hash :: CryptoHash (Sem r) Bool)

-- KVStore effect is already defined and provided from polysemy-zoo
-- data KVStore k v m a

------------------------------------------------------------------------------
-- business logic

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


------------------------------------------------------------------------------
-- effect implementions

runCryptoHashAsState
  :: (DRG gen, Member (State gen) r)
  => Sem (CryptoHash : r) a
  -> Sem r a
runCryptoHashAsState = interpret $ \case
  ValidateHash (Password password) (PasswordHash hash) ->
    pure (BC.validatePassword password hash)
  MakeHash (Password password) -> do
    drg <- get
    let (h, drg') = withDRG drg (BC.hashPassword 10 password)
    put drg'
    pure $ PasswordHash h
