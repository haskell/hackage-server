{-
  In order to test Hackage, we need to be able to send check for confirmation
  emails. In this module we provide a simple interface to do that.

  Currently we use mailinator, but the API is designed to be agnostic to the
  specific mail service used.
-}

module MailUtils (
    Email(..)
  , testEmailAddress
  , checkEmail
  , getEmail
  , emailWithSubject
  , waitForEmailWithSubject
  ) where

import Control.Concurrent (threadDelay)
import Data.Maybe
import Network.URI
import Network.HTTP hiding (user)

import qualified Text.XML.Light as XML

import HttpUtils
import Util

testEmailAddress :: String -> String
testEmailAddress user = user ++ "@mailinator.com"

data Email = Email {
    emailTitle  :: String
  , emailLink   :: URI
  , emailSender :: String
  , emailDate   :: String
  }
  deriving Show

checkEmail :: String -> IO [Email]
checkEmail user = do
    raw <- execRequest NoAuth (getRequest rssUrl)
    let rss   = XML.onlyElems (XML.parseXML raw)
        items = concatMap (XML.filterElementsName $ simpleName "item") rss
    return (map parseEmail items)
  where
    rssUrl = "http://www.mailinator.com/feed?to=" ++ user

    parseEmail :: XML.Element -> Email
    parseEmail e =
      let [title]  = XML.filterElementsName (simpleName "title")   e
          [link]   = XML.filterElementsName (simpleName "link")    e
          [sender] = XML.filterElementsName (simpleName "creator") e
          [date]   = XML.filterElementsName (simpleName "date")    e
      in Email { emailTitle  = XML.strContent title
               , emailLink   = fromJust . parseURI . XML.strContent $ link
               , emailSender = XML.strContent sender
               , emailDate   = XML.strContent date
               }

    simpleName :: String -> XML.QName -> Bool
    simpleName n = (== n) . XML.qName

emailWithSubject :: String -> String -> IO (Maybe Email)
emailWithSubject user subject = do
  emails <- checkEmail user
  return . listToMaybe . filter ((== subject) . emailTitle) $ emails

waitForEmailWithSubject :: String -> String -> IO Email
waitForEmailWithSubject user subject = f 10
  where
    f :: Int -> IO Email
    f n = do
      info $ "Waiting for confirmation email at " ++ testEmailAddress user
      mEmail <- emailWithSubject user subject
      case mEmail of
        Just email          -> return email
        Nothing | n == 0    -> die "Didn't get confirmation email"
                | otherwise -> do
          info "No confirmation email yet; will try again in 30 sec"
          threadDelay (30 * 1000000)
          f (n - 1)

getEmail :: Email -> IO String
getEmail email = execRequest NoAuth (getRequest url)
  where
    msgid = fromJust . lookup "msgid" . parseQuery . uriQuery . emailLink $ email
    url   = "http://mailinator.com/rendermail.jsp?msgid=" ++ msgid ++ "&text=true"
