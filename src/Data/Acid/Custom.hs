{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts #-}

module Data.Acid.Custom where

import Data.Acid            ( AcidState(..), EventState(..), EventResult(..)
                            , Query(..), QueryEvent(..), Update(..), UpdateEvent(..)
                            , IsAcidic(..), makeAcidic, openLocalState
                            )
import Data.Acid.Local      ( createCheckpointAndClose
                            , openLocalStateFrom
                            )
import Data.Acid.Advanced   (query', update')
import Control.Exception.Lifted (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans (MonadIO(..))
import Data.Data (Typeable)

class HasAcidState m st where
    getAcidState :: m (AcidState st)

query :: forall event m.
         ( Functor m
         , MonadIO m
         , QueryEvent event
         , HasAcidState m (EventState event)
         ) =>
         event
      -> m (EventResult event)
query event =
    do as <- getAcidState
       query' (as :: AcidState (EventState event)) event

update :: forall event m.
          ( Functor m
          , MonadIO m
          , UpdateEvent event
          , HasAcidState m (EventState event)
          ) =>
          event
       -> m (EventResult event)
update event =
    do as <- getAcidState
       update' (as :: AcidState (EventState event)) event

withLocalState :: (MonadBaseControl IO m, MonadIO m, IsAcidic st, Typeable st) =>
                  Maybe FilePath
                 -> st
                 -> (AcidState st -> m a)
                 -> m a
withLocalState mPath initialstate =
    bracket (liftIO $ (maybe openLocalState openLocalStateFrom mPath) initialstate)
            (liftIO . createCheckpointAndClose)

