-- Takes a reversed log file on the standard input and outputs web page.

module Distribution.Server.Pages.AdminLog (
    adminLogPage
  ) where

import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Pages.Template
         ( hackagePage)

import Distribution.Text
         ( display )

import qualified Text.XHtml.Strict as XHtml
import Text.XHtml
         ( Html, (<<), (!) )
import Data.Time.Clock
         ( UTCTime )
import Data.Time.Format
         ( defaultTimeLocale, formatTime )

adminLogPage :: Users -> [(UTCTime, UserId, String, UserId, String, String)] -> Html
adminLogPage users entries = hackagePage "adminstrator actions log" docBody
     where
        docBody = [XHtml.h2 << "Administrator actions",
                   XHtml.table ! [XHtml.align "center"] << (header : map makeRow entries)]
        makeRow (time, actorId, action, targetId, group, reason) = XHtml.tr << map fmtCell
             [showTime time,
              display $ Users.userIdToName users actorId,
              action,
              display $ Users.userIdToName users targetId,
              group,
              reason]
        nbsp = XHtml.primHtmlChar "nbsp"
        showTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
        header = XHtml.tr << map (XHtml.th <<) ["Time ","User ","Action ","Target ","Group ","Reason "]
        fmtCell x = XHtml.td ! [XHtml.align "left"] << [XHtml.toHtml x, nbsp, nbsp]
