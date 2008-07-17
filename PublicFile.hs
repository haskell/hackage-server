module PublicFile where

import Text.XHtml		( URL )

infixl 9 `slash`

-- | A local file that is also visible via a URL.
data PublicFile = PublicFile {
		localFile :: FilePath,
		webURL :: URL
	}

slash :: PublicFile -> String -> PublicFile
slash (PublicFile file url) suffix =
	PublicFile (file ++ "/" ++ suffix) (url ++ "/" ++ suffix)
