{-# LANGUAGE OverloadedStrings   #-}

import Text.Printf (printf)
import Data.Text qualified as T
import Prelude qualified as Haskell
import Data.Map as Map

import Control.Monad (void)
import Ledger (Address, ScriptContext)
import Ledger.Value (Value)
import Playground.Contract

import Plutus.Contract
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import PlutusTx.Prelude hiding (Applicative (..))

-- import Data.ByteString.Lazy.UTF8 as BLU 
-- import Plutus.Contracts.Currency




{-# INLINABLE mkValidator #-}
mkValidator :: CurrencySymbol -> CurrencySymbol -> ScriptContext -> Bool
mkValidator nftDatum nftRedeemer ctx = traceIfFalse "Search for Currency" $ (nftDatum unCurrencySymbol) == (nftRedeemer unCurrencySymbol)
 

data Deposit
instance Scripts.ValidatorTypes Deposit where
    type instance DatumType Deposit = Integer
    type instance RedeemerType Deposit = ()


validateDeposit  :: Scripts.TypedValidator Deposit
validateDeposit  = Scripts.mkTypedValidator @Deposit
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @()


validator :: Validator
validator = Scripts.validatorScript validateDeposit


depositHash :: Ledger.ValidatorHash
depositHash = Scripts.validatorHash validateDeposit


depositAddress :: Ledger.Address
depositAddress = scriptAddress validator


type DepositSchema =
                 BlockchainActions
            .\/  Endpoint "depositAda_ReceiveNFT" Integer
            .\/  Endpoint "depositNFT_ReceiveADA " ()


-- mintContract = is not working on testnet
-- oneSCurrency hash tN = mapError (pack . show) (mintContract hash [(tN, 1)] :: Contract w s CurrencyError OneShotCurrency)


--depositAda_ReceiveNFT :: Integer ->  Contract () c T.Text ()
depositAda_ReceiveNFT amt = do
                              let
                                  hashOfPubKey  <-  ownPaymentPubKeyHash 
                                  tokenName      =  TokenName emptyByteString
                                  currSymbol     =  Currency.currencySymbol (oneSCurrency hashOfPubKey tokenName)
                                  nftForDeposit  =  singleton currSymbol tokenName 1 
                                   tx    =  Constraints.mustPayToTheScript currSymbol $ Ada.lovelaceValueOf amt <>
                                            Constraints.mustPayToPubKey hashOfPubKey nftForDeposit
                              ledgerTx  <- submitTxConstraints validateDeposit tx
                              void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                              
                              logInfo @Haskell.String $ printf "Amount of deposited lovelace is: %d" amt                                  


--depositNFT_ReceiveADA :: Contract () e T.Text ()                      
depositNFT_ReceiveADA = do
                         utxo1 <- utxoAt $ ownPaymentPubKeyHash
                         utxo2 <- utxoAt $ depositAddress

                         listOfValues1 = map snd $ Map.toList $ utxo1
                         searchFor1    = map  (symbols . _ciTxOutValue) listOfValues1 

                         listOfValues2 = map snd $ Map.toList $ utxo2
                         searchFor2    = map  (symbols . _ciTxOutValue) listOfValues2 
                          
                         myNFTsignature = checkCurrSymbol searchFor1 searchFor2 

                         case myNFTsignature of
                               Just nftSign  ->  let  
                                                   outRefs = fst <$> Map.toList utxos2
                                                   lookups = Constraints.unspentOutputs utxos2 <> Constraints.otherScript validator    
                                                   tx :: TxConstraints Void Void
                                                   tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ nftSign | oref <- outRefs]
                
                                                 ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                                 void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                                 logInfo @String $ "You have received your lovelaces"
                               
                               Nothing        -> traceError "You don't have the NFT for unlocking ADA"
                         
                        where 
                             checkCurrSymbol []  _ = Nothing
                             checkCurrSymbol (t:ts) (myCurr) 
                                                           | length findCS > 0 = Just $ head findCS
                                                           | otherwise         = checkCurrSymbol ts myCurr 
                               where
                                   findCS = takeWhile ( == t) myCurr



contract :: AsContractError e => Contract () DepositSchema e ()
contract = selectList [depositNFT_ReceiveADA, depositAda_ReceiveNFT]

endpoints :: AsContractError e => Contract () DepositSchema e ()
endpoints = contract

mkSchemaDefinitions ''DepositSchema

$(mkKnownCurrencies [])
