{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Process
import qualified Data.Text                     as T
import           Data.Maybe
import           Data.List
import           System.Environment
import           Control.Monad
import           Data.Char

type LocalBranches = [T.Text]
type RemoteBranches = [T.Text]

main :: IO ()
main = listToMaybe <$> getArgs >>= app

app :: Maybe FilePath -> IO ()
app filePath = do
  diffs <- getDiffs <$> getLocalBranches filePath <*> getRemoteBranches filePath
  if null diffs
    then putStrLn "There are no local branches to remove"
    else do
      let indexed = zip [1 ..] diffs
      putStrLn "The following branches will be deleted:"
      forM_ (indexed ++ [(0, "All branches")])
        $ \(i, d) -> putStrLn $ show i ++ ": " ++ show d
      putStr "Please enter the numbers of the branches you'd like to delete: "
      getApplicableBranches indexed <$> getLine >>= deleteBranches filePath
  where
  getApplicableBranches :: [(Int, T.Text)] -> String -> LocalBranches
  getApplicableBranches indexed input = case input of
    "0" -> snd <$> indexed
    _   -> filterSelections indexed input
  filterSelections :: [(Int, T.Text)] -> String -> LocalBranches
  filterSelections indexed input =
    let selections = T.splitOn "," . T.pack $ input
        filtered =
          filter (\(i, _) -> elem (T.pack (show i)) selections) indexed
    in  snd <$> filtered


getBranches :: T.Text -> [T.Text]
getBranches = filter (not . T.isInfixOf "master") . fmap T.strip . T.lines

executeCmd :: String -> Maybe FilePath -> IO String
executeCmd cmd dir = readCreateProcess (shell cmd) { cwd = dir } ""

executeCmd' :: Maybe FilePath -> String -> IO String
executeCmd' = flip executeCmd

getLocalBranches :: Maybe FilePath -> IO [T.Text]
getLocalBranches =
  fmap (getBranches . T.pack) . executeCmd "git branch"

getRemoteBranches :: Maybe FilePath -> IO [T.Text]
getRemoteBranches dir = getBranches . T.pack <$> executeCmd "git branch -r" dir

getDiffs :: LocalBranches -> RemoteBranches -> LocalBranches
getDiffs lb rm = lb \\ (T.replace "origin/" "" <$> rm)

deleteBranches :: Maybe FilePath -> LocalBranches -> IO ()
deleteBranches dir = 
  mapM_ (executeCmd' dir . (++) "git branch -D " . T.unpack)