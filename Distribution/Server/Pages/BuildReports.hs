-- Generate an HTML page listing all build reports for a package

module Distribution.Server.Pages.BuildReports (buildReportSummary) where

import qualified Distribution.Server.BuildReport as BuildReport
import Distribution.Server.BuildReport (BuildReport)
import Distribution.Server.BuildReports
import Distribution.Server.Pages.Template ( hackagePage )

import Distribution.Package
         ( PackageIdentifier )
import Distribution.Text
         ( display )

import qualified Text.XHtml as XHtml
import Text.XHtml ((<<), (!), tr, th, td)


buildReportSummary :: PackageIdentifier
                   -> [(BuildReportId, BuildReport)] -> XHtml.Html
buildReportSummary pkgid reports = hackagePage title body
  where
    title = display pkgid ++ ": build reports"
    body  = [summaryTable]

    summaryTable = XHtml.table <<
                    (headerRow : dataRows)
    headerRow = tr << [ th ! [XHtml.theclass "horizontal"] <<
                          columnName
                      | columnName <- columnNames ]
    columnNames = ["Platform", "Compiler", "Build outcome"]
    dataRows =
      [ tr <<
          [ td << (display (BuildReport.arch report)
                ++ " / "
                ++ display (BuildReport.os report))
          , td << display (BuildReport.compiler report)
          , td << detailLink reportId <<
                    display (BuildReport.installOutcome report) ]
      | (reportId, report) <- reports ]
    detailLink reportId =
      XHtml.anchor ! [XHtml.href $ "/buildreports/" ++ display reportId ]
