{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Run where

import qualified Data.Text as Text (unpack)
import           Brick                            hiding (fill)
import           Brick.Forms                      (editTextField, formState,
                                                   handleFormEvent, newForm,
                                                   renderForm)
import           Brick.Widgets.Border             (borderWithLabel,
                                                   hBorder, hBorderWithLabel,
                                                   vBorder)
import           Brick.Widgets.Center             (hCenter, vCenter)
import           Brick.Widgets.List               hiding (reverse)
import           Data.String.Interpolate.IsString
import           Elixir.Pretty                    hiding ((<$>))
import qualified Elixir.Version                   as EV (showVersion, version)
import qualified ElixirFM                         as EFM
import           Encode.Arabic
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           Import                           hiding (display)
import           RIO.List                         (genericIndex, intercalate, intersperse)
import Lens.Micro.Platform

run :: Options -> IO ()
run _options = do
  _ <- defaultMain haradict initialState
  return ()
  where haradict = App {..} :: App Haradict () String
        initialState = Haradict
          { haradictQuery = Nothing
          , haradictQueryForm = queryForm'
          , haradictResults = mempty
          , haradictResultIndex = 0
          , haradictException = Nothing
          , haradictViewState = Query
          }
        appStartEvent = return
        appChooseCursor = showFirstCursor
        appAttrMap _ = attrMap defAttr
          [ (listAttr, withStyle currentAttr dim)
          , (listSelectedAttr, withStyle currentAttr bold)
          , (listSelectedFocusedAttr, withStyle currentAttr $ bold + standout)
          ]
        appHandleEvent st evt = case evt of
          VtyEvent event -> case st ^. viewState of
            Query -> do
              updatedForm <- handleFormEvent evt $ st ^. queryForm
              let newState = st & queryForm .~ updatedForm
                  query' = formState updatedForm
              case event of
                EvKey KEsc []   -> halt newState
                EvKey KEnter [] -> case elixirLookup $ Text.unpack query' of
                  Left exc -> continue $ newState
                    & viewState .~ ErrorView
                    & exception ?~ exc
                  Right clips' -> continue $ newState
                    & viewState .~ Results
                    & query ?~ query'
                    & results .~ clips'
                _               -> continue newState

            Results -> do
              case event of
                EvKey KEsc [] -> continue $ st
                  & query .~ Nothing
                  & queryForm .~ queryForm'
                  & results .~ mempty
                  & resultIndex .~ 0
                  & viewState .~ Query

                EvKey KPageUp [] -> continue $ st
                  & resultIndex %~ (max 0 . flip (-) 1)
                EvKey KPageDown [] ->
                  let maxPage = length (st ^. results) - 1
                  in continue $ st
                     & resultIndex %~ (min maxPage . (+) 1)

                -- KUp/KDown/KLeft/KRight
                  -- these cycle the various declension properties shown for
                  -- the current root
                  -- each key cycles through one dimension; left does not undo right
                  -- because there are 4, maybe more, grammatical properties we need
                  -- to enable the user to navigate
                -- EvKey KUp [] -> continue st -- TODO
                -- EvKey KDown [] -> continue st -- TODO
                -- EvKey KLeft [] -> continue st -- TODO
                -- EvKey KRight [] -> continue st -- TODO
                _ -> continue st

            ErrorView -> do
              case event of
                EvKey KEsc [] -> continue $ st
                  & query .~ Nothing
                  & queryForm .~ queryForm'
                  & exception .~ Nothing
                  & viewState .~ Query
                _ -> continue st

          _ -> continue st

        appDraw st = catMaybes
          [ pure $ joinBorders $ applicationTitle $ vBox
            $ case st ^. viewState of
                Query ->
                  [ vCenter $ hCenter
                    $ hLimitPercent 90 $ borderWithLabel (str "Query")
                    $ vLimit 1 $ renderForm $ st ^. queryForm
                  , hBox $ fmap hCenter
                    [ str "Press ENTER to look up your entry."
                    , str "Press ESC to exit."
                    ]
                  , hBorder
                  , vBox copyrightNotice
                  ]
                Results ->
                  let numResults = length $ st ^. results
                      resultIdx = st ^. resultIndex + 1
                      currentResult = (st ^. results) `genericIndex` (st ^. resultIndex :: Int)

                  in [ hBorderWithLabel (str "Results")
                     , hBox $ hCenter <$>
                       [ str "Root"
                       , str "Word"
                       ]
                     , hBox $ hCenter <$>
                       [ str "Grammar"
                       , str "Morphology"
                       , str "Meaning"
                       ]
                     , hBorder

                     , vBox
                       $ intersperse hBorder
                       $ renderClip currentResult <&> drawWordInfo

                     , hBorder
                     , hBox $ hCenter <$>
                       [ str "PgUp for previous page"
                       , str ([i|Page #{resultIdx}/#{numResults}|])
                       , str "PgDn for next page"
                       ]
                     ]
                ErrorView ->
                  [ hBorderWithLabel (str "Error!")
                  , vCenter $ vBox $ hCenter
                    <$> case st ^. exception of
                      Nothing -> error "ErrorView without exception in state model."
                      Just EmptyQuery ->
                        [ str "You have made an empty query."
                        , str "This is not a supported usage."
                        ]
                      Just EmptyResults ->
                        [ str "There were no results for your query."
                        ]
                      Just NotArabicQuery ->
                        [ str "You have made a non-Arabic query."
                        , str "This is not a supported usage."
                        ]
                      Just UnknownError ->
                        [ str "An error has occurred." ]

                  , hCenter $ str "Press ESC to return."
                  ]
          ]

        drawWordInfo wordInfo = vBox
          [ hBox $ hCenter <$>
            [ str $ wordInfo ^. word
            , str $ wordInfo ^. root
            ]
            -- ^ note that RTL causes these to appear in reverse order
          , hBox $ hCenter <$>
            [ str $ wordInfo ^. tags
            -- sucks, we need the entity to make sense of the grammar
            , str $ wordInfo ^. morphs
            , str $ intercalate ", " $ wordInfo ^. meanings
            ]
          ]

        applicationTitle = borderWithLabel (str "haradict")

        queryForm' = newForm
          [ editTextField id "userEntry" Nothing ]
          mempty

        copyrightNotice =
          [ vLimit 5 $ hBox
            [ vBox $ fmap hCenter
              [ str "haradict v0.1.0"
              , str "Copyright (c) 2022 Saad Rhoulam"
              , str "haradict comes with ABSOLUTELY NO WARRANTY."
              , str "haradict is distributed under the GPLv3 license."
              ]
            , vBorder
            , vBox $ fmap hCenter
              [ str [i|ElixirFM version #{EV.showVersion EV.version}|]
              , str "Copyright (c) 2005-2017 Otakar Smrz, Viktor Bielicky"
              , str "Copyright (c) 2004 Markus Forsberg, 2002 Tim Buckwalter"
              , str "ElixirFM comes with ABSOLUTELY NO WARRANTY."
              , str "ElixirFM is distributed under the GPLv3 license."
              ]
            ]
          , hBorder
          , hCenter $ str "haradict development sponsored by Rhoulam Technologies LLC"
          , hCenter $ str "https://www.rhoulam.tech"
          ]

elixirLookup :: String -> Either HaradictException [EFM.Clips]
elixirLookup query' =
  let strippedQuery = unwords $ words query'
                      -- OPTIMIZE: use strip, not (un)words
      dutfQuery = decode Unicode strippedQuery
      isArabicQuery = any EFM.isArabic strippedQuery
  in case () of _
                  | null strippedQuery -> Left EmptyQuery
                  | isArabicQuery ->
                    let results' = EFM.lookup dutfQuery
                    in if null results'
                       then Left EmptyResults
                       else Right results'
                  | not isArabicQuery -> Left NotArabicQuery
                  | otherwise -> Left UnknownError

renderClip :: (Int, [Int]) -> [WordInfo]
renderClip clip@(_index, subindices) = do
  clips' <- if not (null subindices) || null expandedClip
            then EFM.regroup expandedClip
            else [clip]
  word' <- EFM.emanate clips'
  EFM.unwraps (toWordInfo expandedClip) word'

  where expandedClip = EFM.enumerate clip

        toWordInfo expandedClip' (EFM.Nest root' entries) = do
          (_index, entry') <- zip expandedClip' entries

          WordInfo
            { wordInfoRoot = toArabic root'
            , wordInfoWord = toArabic $ EFM.merge root' (EFM.morphs entry')
            , wordInfoTags = show $ pretty $ EFM.domain entry'
            , wordInfoMorphs = show (EFM.morphs entry')
            , wordInfoMeanings = EFM.reflex entry'
            }:buildVariants root' entry'

        buildVariants root' entry' = do
          (fieldText, morphs') <-
              [ (pretty grammaticalFlags, entityMorphs)
              | (grammaticalFlags, entityMorphs) <- EFM.display (EFM.entity entry')
              ] <>
              [ (pretty limitTag, limitMorphs')
              | (limitTag : _, limitMorphs') <- snd (EFM.limits entry')
              ]

          morph' <- morphs'

          return $ WordInfo
            { wordInfoRoot = toArabic root'
            , wordInfoWord = toArabic $ EFM.merge root' morph'
            , wordInfoTags = show fieldText -- show $ pretty $ EFM.domain entry'
            , wordInfoMorphs = show morph'
            , wordInfoMeanings = mempty
            }

        toArabic = encode Unicode . decode ArabTeX

        -- renderWord expandedClip' (Nest root' entries) = vcat
        --   [ text "\t" <> width ((text . show) index)
        --     (renderEntry root' entry')
        --   | (index, entry') <- zip expandedClip' entries
        --   ]

        -- renderEntry root' entry' width' =
        --   hcat (punctuate (line <> text "\t" <> fill width' empty)
        --         $ (text "\t" <> pretty (domain entry') <>
        --             joinText [ toArabic $ merge root' (EFM.morphs entry')
        --                      , toArabic root'
        --                      , show (EFM.morphs entry')
        --                      , intercalate ", " $ reflex entry'
        --                      , show (lookupForm root' entry')
        --                      ])
        --            :[ text "\t"
        --               <> width (pretty fieldText) (renderMorphs root' width' morphs')
        --             | (fieldText, morphs') <-
        --                 [ (pretty grammaticalFlags, entityMorphs)
        --                 | (grammaticalFlags, entityMorphs) <- display (entity entry')
        --                 ] <>
        --                 [ (pretty limitTag, limitMorphs')
        --                 | (limitTag : _, limitMorphs') <- snd (limits entry')
        --                 ]
        --             ])

        -- renderMorphs root' width1 morphs' width2 =
        --   hcat (punctuate (line
        --                    <> text "\t" <> fill width1 empty
        --                    <> text "\t" <> fill width2 empty)

        --          [ joinText [ toArabic $ merge root' morph'
        --                     , toArabic root'
        --                     , show morph'
        --                     ]
        --          | morph' <- morphs'
        --          ])
