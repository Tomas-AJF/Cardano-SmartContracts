import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import           Text.Printf          (printf)
import           Plutus.Contract qualified as PC


data LockPKH = LockPKH  { lock :: PaymentPubKeyHash } 
                          deriving (Generic, FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''LockPKH

data UnlockPKH = UnlockPKH { unlock :: PaymentPubKeyHash } 
                            deriving (Generic, FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''UnlockPKH

{-# INLINABLE depositValidator  #-}
depositValidator :: LockPKH -> UnlockPKH -> ScriptContext -> Bool
depositValidator (LockPKH a) (UnlockPKH b) _ = traceIfFalse "Wrong wallet signature" $ a == b

data Deposit
instance Scripts.ValidatorTypes Deposit where
    type instance DatumType Deposit = LockPKH
    type instance RedeemerType Deposit = UnlockPKH

typedDepositValidator :: Scripts.TypedValidator Deposit
typedDepositValidator = Scripts.mkTypedValidator @Deposit
                        $$(PlutusTx.compile [|| depositValidator ||])
                        $$(PlutusTx.compile [|| wrap ||])
                        where
                             wrap = Scripts.wrapValidator @LockPKH @UnlockPKH

validator :: Validator
validator = Scripts.validatorScript typedDepositValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedDepositValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type LockOrUnlock =   Endpoint "lockMyAda" Integer  .\/ Endpoint "unlockMyAda" ()

lockMyAda :: AsContractError e => Integer -> Contract w s e ()
lockMyAda amountOfAda = do
          hashOfPubKey  <-  PC.ownPaymentPubKeyHash       
          let  lockRed = LockPKH {lock = hashOfPubKey}
               tx = mustPayToTheScript lockRed $ Ada.lovelaceValueOf amountOfAda
          ledgerTx <- submitTxConstraints typedDepositValidator tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          logInfo @String $ printf "You just locked %d lovelace in the smart-contract" amountOfAda

unlockMyAda :: forall w s e. AsContractError e => Contract w s e ()
unlockMyAda = do
          utxos <- utxosAt scrAddress
          hashOfPubKey  <-  PC.ownPaymentPubKeyHash  
          let unlockRed    = UnlockPKH {unlock = hashOfPubKey}
              orefs   = fst <$> Map.toList utxos
              lookups = Constraints.unspentOutputs utxos <> Constraints.otherScript validator
              tx :: TxConstraints Void Void
              tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData unlockRed | oref <- orefs]
          ledgerTx <- submitTxConstraintsWith @Void lookups tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          logInfo @String $ "Your signature unlocked the ADA in the smart-contract"

endpoints :: Contract () LockOrUnlock Text ()
endpoints = awaitPromise (lockMyAda' `select` unlockMyAda') >> endpoints
  where
    lockMyAda' = endpoint @"lockMyAda" lockMyAda
    unlockMyAda' = endpoint @"unlockMyAda" $ const unlockMyAda

mkSchemaDefinitions ''LockOrUnlock

mkKnownCurrencies []
