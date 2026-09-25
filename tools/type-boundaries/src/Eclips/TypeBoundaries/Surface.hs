module Eclips.TypeBoundaries.Surface
  ( SurfaceEntry (..),
    parseSurface,
    renderSurface,
    surfaceDifference,
  )
where

import Data.List (intercalate, sort, (\\))

data SurfaceEntry = SurfaceEntry
  { publicUnit :: String,
    publicModule :: String,
    exportNamespace :: String,
    exportOccurrence :: String,
    exportParent :: String,
    definingUnit :: String,
    definingModule :: String
  }
  deriving stock (Eq, Ord, Show)

renderSurface :: [SurfaceEntry] -> String
renderSurface entries =
  unlines
    ( "# ECLIPS semantic public export surface; generated candidates require review."
        : fmap renderEntry (sort entries)
    )

parseSurface :: String -> Either String [SurfaceEntry]
parseSurface contents = traverse parseLine relevantLines
  where
    relevantLines =
      [ line
      | line <- lines contents,
        not (null line),
        take 1 line /= "#"
      ]

surfaceDifference :: [SurfaceEntry] -> [SurfaceEntry] -> ([SurfaceEntry], [SurfaceEntry])
surfaceDifference expected actual =
  ( sort actual \\ sort expected,
    sort expected \\ sort actual
  )

renderEntry :: SurfaceEntry -> String
renderEntry SurfaceEntry {publicUnit, publicModule, exportNamespace, exportOccurrence, exportParent, definingUnit, definingModule} =
  intercalate
    "\t"
    [ publicUnit,
      publicModule,
      exportNamespace,
      exportOccurrence,
      exportParent,
      definingUnit,
      definingModule
    ]

parseLine :: String -> Either String SurfaceEntry
parseLine line = case splitTabs line of
  [publicUnit, publicModule, exportNamespace, exportOccurrence, exportParent, definingUnit, definingModule] ->
    Right SurfaceEntry {publicUnit, publicModule, exportNamespace, exportOccurrence, exportParent, definingUnit, definingModule}
  fields ->
    Left
      ( "expected seven tab-separated semantic-surface fields, found "
          <> show (length fields)
          <> ": "
          <> line
      )

splitTabs :: String -> [String]
splitTabs [] = [""]
splitTabs value = case break (== '\t') value of
  (field, []) -> [field]
  (field, _ : rest) -> field : splitTabs rest
