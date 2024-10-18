import Test.Tasty (defaultMain, TestTree, testGroup)

import Test.Tasty.Golden

import MP (expand)

import System.IO (withBinaryFile, IOMode(ReadMode, WriteMode), hPutStr, hGetContents, Handle)
import System.Directory (createDirectoryIfMissing, findExecutable, removeDirectory, listDirectory)
import Control.Monad (when)
import Control.Exception (catch, throwIO, SomeException)
import Control.DeepSeq (($!!))

-- you may wish to run this specifically with
-- cabal test macro-golden --test-options="--color always --delete-output onpass"
-- this will ensure that the output files are deleted if the tests pass
main :: IO ()
main = do
  -- this checks to see if the "diff" command is available on your system
  -- if it is, this makes for nicer test failure messages
  -- if not, oh well
  diff <- findExecutable "diff"
  withDirectory "test/out" (defaultMain (golden diff))

{-
Golden Testing
==============

This testing file is doing "golden testing". This relies on the existence of so-called
"golden tests", which are usually files with a specific contents. The idea is that
a test will perform something that writes to a file, and then the new file is compared
for exact equality with the corresponding golden test file. If they match, great! If not,
something is wrong.

It's a interesting approach, because it usually testing an entire system and can be quite
brittle. As an example, some compilers have golden tests for their outputted
assembly/bytecode: when the compiler changes so that it generates different code, it
will fail the golden tests. The interesting thing is that this means one of two things:
the behaviour of the compiler has _intentionally_ changed, in which case the golden test
is updated; or there was a bug introduced, which means the golden test remains unchanged.
When a golden test fails, then, it is up to the tester to manually check and determine
what the situation is. In fact, in this system, the golden tests are often generated
on-demand by the old compiler!

In our case, we are just using golden testing as a convenient way of testing the IO
of your system, as opposed to you having to manually run these tests and then inspect
the output yourself to see if they are right. In this sense, the "golden test generator"
is simply our model solution.
-}

golden :: Maybe FilePath -> TestTree
golden diff = testGroup "macro" [ shakespeareTest diff
                                , hardyTest diff
                                , poohTest diff
                                -- this tests the behaviour of the extension, you can uncomment
                                -- if you wish!
                                , extensionTest diff
                                ]

shakespeareTest :: Maybe FilePath -> TestTree
shakespeareTest diff =
  goldenTest diff "welcome message for Shakespeare"
                  "test/golden/welcomeShakespeare.golden"
                  "templates/welcome.txt"
                  "info/shakespeare.info"
                  "test/out/welcomeShakespeare.txt"

hardyTest :: Maybe FilePath -> TestTree
hardyTest diff =
  goldenTest diff "welcome message for Hardy"
                  "test/golden/welcomeHardy.golden"
                  "templates/welcome.txt"
                  "info/hardy.info"
                  "test/out/welcomeHardy.txt"

poohTest :: Maybe FilePath -> TestTree
poohTest diff =
  goldenTest diff "project information for Winnie the Pooh"
                  "test/golden/pooh.golden"
                  "templates/poohTem.html"
                  "info/pooh.info"
                  "test/out/pooh.html"

extensionTest :: Maybe FilePath -> TestTree
extensionTest diff =
  goldenTest diff "multiple template expansion with authors"
                  "test/golden/welcomeAuthors.golden"
                  "templates/welcome.txt"
                  "info/authors.info"
                  "test/out/welcomeAuthors.txt"

-- Internal things ('ere be Dragons)
------------------------------------

goldenTest :: Maybe FilePath -> String -> FilePath -> FilePath -> FilePath -> FilePath -> TestTree
goldenTest diff name reference template info output =
  testType diff name reference output (testOn template info output)
  where
    -- this is where we can decide whether or not we should try and diff the output
    testType Nothing = goldenVsFile
    testType (Just diff) = flip goldenVsFileDiff (\ref new -> [diff, "-u", ref, new])

-- All the file IO must be done in binary mode to avoid issues with golden testing on Windows
testOn :: FilePath -> FilePath -> FilePath -> IO ()
testOn template info output = do
  t <- readFile template
  i <- readFile info
  writeBinaryFile output (expand t i)
  where
    writeBinaryFile path stuff = withBinaryFile path WriteMode  (\file -> hPutStr file stuff)

removeDirectoryIfEmpty :: FilePath -> IO ()
removeDirectoryIfEmpty dir =
  do contents <- listDirectory dir
     when (null contents) (removeDirectory dir)

{-|
This function sets up a directory for use within the given IO action: if it is
empty after the action has completed, it is deleted. It will always be deleted
even if the action throws an exception
-}
withDirectory :: FilePath -> IO () -> IO ()
withDirectory dir action = do
  createDirectoryIfMissing True dir
  action `catch` \e -> do
    removeDirectoryIfEmpty dir
    throwIO (e :: SomeException)
