{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Network.Wreq as W
import System.Environment (getEnv)
import Web.Slack
import Web.Slack.Message
import Web.Slack.State
import Web.Slack.Types
import Web.Slack.WebAPI

genConfig :: IO SlackConfig
genConfig = SlackConfig <$> getEnv "YPHGREET_TOKEN"

-- This is copied from upstream but without annoying the MonadError constraint.
-- Instead we keep the MonadIO constraint, and simply return a disjunction.
makeSlackCall'
    :: MonadIO m
    => SlackConfig
    -> String
    -> (W.Options -> W.Options)
    -> m (Either T.Text Value)
makeSlackCall' conf method setArgs = do
    let url = "https://slack.com/api/" ++ method
    let setToken = W.param "token" .~ [T.pack (_slackApiToken conf)]
    let opts = W.defaults & setToken & setArgs
    rawResp <- liftIO $ W.getWith opts url
    let resp = rawResp ^? W.responseBody . _Value
    case resp of
      Just r ->
        case r ^? key "ok"  . _Bool of
          Just True  -> return . Right $ r
          Just False -> return . Left $ r ^. key "error" . _String
          Nothing    -> return . Left $ "Couldn't parse key 'ok' from response"
      Nothing -> return . Left $ "Unable to parse response"

openIm :: MonadIO m => SlackConfig -> Id t -> m (Either T.Text Value)
openIm conf (Id uid) = makeSlackCall' conf "im.open" (W.param "user" .~ [uid])

greeter :: SlackBot ()
greeter (TeamJoin user) = createUserIM user
greeter x = liftIO (print x) >> return ()

createUserIM :: User -> Slack s ()
createUserIM u = do
  conf <- use config
  res <- openIm conf (u ^. userId)
  case res of
    Left err -> liftIO $ putStr "Left: " >> print err
    Right v -> doGreet (v ^? key "channel" . key "id" . _String) u

doGreet :: Maybe T.Text -> User -> Slack s ()
doGreet Nothing _ = liftIO . putStrLn $ "Slack didn't give us a DM ID back."
doGreet (Just cid) u = sendMessage (Id cid) msg
  where
    fName = u ^. userProfile . profileFirstName
    msg = "Hello there, " <> (fromMaybe "newcomer" fName) <>
      "! It's a pleasure to meet you. Please check out the list of channels to \
      \find any that interest you. To do so, simply type `/open` in the input \
      \box at the bottom of the page. We'd like to invite you to introduce \
      \yourself to everyone in our #intros channel. Feel free to write a \
      \paragraph there describing your background, interests, and favorite \
      \flavor of ice cream."

main :: IO ()
main = do
  conf <- genConfig
  runBot conf greeter ()
  return ()
