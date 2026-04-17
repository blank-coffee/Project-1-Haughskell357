-- | Central registry of every TestSpec.
--
--   To add a new test module:
--     1. Write your test functions and a list  :: [TestSpec]  in
--        a new file under  src/Tester/Tests/.
--     2. Import the list here and append it to  allTests.
--     3. Add the new module to the  exposed-modules  list in the .cabal file.
--   Nothing in the runner or menu needs to change.

module Tester.TestRegistry (allTests) where

import Tester.TestTypes
import Tester.Tests.HashTests    (hashTests)
import Tester.Tests.ScannerTests (scannerTests)
import Tester.Tests.DedupeTests  (dedupeTests)

allTests :: [TestSpec]
allTests
  =  hashTests
  ++ scannerTests
  ++ dedupeTests
