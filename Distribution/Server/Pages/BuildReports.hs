-- Generate an HTML page listing all build reports for a package

module Distribution.Server.Pages.BuildReports (buildReportSummary) where

import Distribution.Server.BuildReport
import Distribution.Server.BuildReports
import Distribution.Server.Pages.Template	( hackagePage )

import Distribution.Package
         ( PackageIdentifier )
import Distribution.Text
         ( display )

import qualified Text.XHtml as XHtml
import Text.XHtml ((<<))


buildReportSummary :: PackageIdentifier
                   -> [(BuildReportId, BuildReport)] -> XHtml.Html
buildReportSummary pkgid reports = hackagePage title body
  where
    title = display pkgid ++ ": build reports"
    body  = []
