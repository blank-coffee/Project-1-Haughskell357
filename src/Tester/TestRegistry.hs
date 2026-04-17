module Tester.TestRegistry (allTests) where

import Tester.TestTypes
import Tester.Tests.HashTests      (hashTests)
import Tester.Tests.ScannerTests   (scannerTests)
import Tester.Tests.DedupeTests    (dedupeTests)
import Tester.Tests.OrganizerTests (organizerTests)

allTests :: [TestSpec]
allTests
  =  hashTests
  ++ scannerTests
  ++ dedupeTests
  ++ organizerTests