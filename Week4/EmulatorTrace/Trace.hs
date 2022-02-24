{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week4.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet

import Week4.Vesting

-- Contract w s e a
    -- the contract monad defines code that will run in the wallet i.e. the off chain code
-- EmulatorTrace a
    -- the emulator trace monad is a monad to manually do what is visually simulated in the playground
    -- it consists of a configuration for the emulator to run then providing a list of tx's you can simulate utxos
    -- you can get the default EmulatorConfig by running
        -- def :: EmulatorConfig
    -- because its a monad we can use (return) and returns a unit () and doesn't interact with the blockchain at all
        -- so running the following would demonstrate that
        -- runEmulatorTrace def $ return ()
            -- this sends the return () in as the second argument for the runEmulatorTrace function call passing in def i.e. the default EmulatorConfig for the first argument. 
        -- because of the amount of data given back this isn't very feasible
    -- you can also use a IO version called runEmulatorTraceIO by running   
        -- runEmulatorTraceIO $ return ()
        -- this uses the default Emulator config
        -- runEmulatroTraceIO' takes a traceConfig and EmulatorConfig
            -- a traceConfig takes two arguments 
                -- the first is a function that turns EmulatorEvents into Maybe Strings
                    -- so essentially something that filters out events into strings that you format into something you want to see a certain way
                -- the second is a Handle like a file handle 
    
-- this function actually runs the trace using runEmulatorTraceIO and prints to console
test :: IO ()
test = runEmulatorTraceIO myTrace

-- here we are writing our own trace using EmulatorTrace that basically does what we did last week in week3 where wallet1 gives then wallet 2 cant grab until the deadline is met and the grabber has to be wallet 2
myTrace :: EmulatorTrace ()
-- using do notation 
myTrace = do
    -- activatecontractWallet takes two arguments and we use it to build the transactions
        -- we pass in the wallet address getting it by calling knownWallet and then passing the wallet number we want
        -- and the endpoints call from the Vesting module inmported above
    -- the results are both ContractHandle's
        -- Contract
        -- ContractInstaceId
        -- ContractInstanceTag
    -- we bind these both to h1 and h2
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    -- invoke callEndpoint using Haskell's TypeApplication (@) operator on the "give" endpoint with h1 as the first argument and output of GiveParams as the second
        -- GiveParams is the constructor from Vesting that takes the validator vestingDatum parameters
    callEndpoint @"give" h1 $ GiveParams
        -- here we are getting the beneficiary i.e. wallet 2 by calling mockWalletPaymentPubKeyHash on knownWallet 2
        { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2
        -- here we set the deadline to the slot 20 based on the call slotToBeginPOSIXTime then passing 20 on the default config
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }
    -- now wait for the deadline to pass
    void $ waitUntilSlot 20
    -- setup the second call using similar Haskell syntax 
        -- we only need to attempt the grab passing in the unit and h2 info
    callEndpoint @"grab" h2 ()
        -- wait 2 slots
    s <- waitNSlots 2
        -- log out the info using logInfo and pass in the string
    Extras.logInfo $ "reached " ++ show s
