module Subtyping.Recursive.Fsubmu (fsubmuMeta) where

import Lib (AlgMeta (..), Example (..), Paper (..))

-- Fsubmu algorithm metadata
fsubmuMeta :: AlgMeta
fsubmuMeta =
  AlgMeta
    { metaId = "Fsubmu",
      metaName = "Recursive Subtyping for All",
      metaLabels = ["Subtyping", "System Fsub", "Recursive Types"],
      metaViewMode = "tree",
      metaMode = "subtyping",
      metaPaper =
        Paper
          { paperTitle = "Recursive Subtyping for All",
            paperAuthors = ["Litao Zhou", "Yaoda Zhou", "Qianyong Wan", "Bruno C. d. S. Oliveira"],
            paperYear = 2025,
            paperUrl = "https://i.cs.hku.hk/~bruno/papers/jfp25.pdf"
          },
      metaVariants = Nothing,
      metaDefaultVariant = Nothing,
      metaRules = [],
      metaRuleGroups = Nothing,
      metaVariantRules = Nothing,
      metaExamples =
        [ Example
            { exampleName = "Trivial Application",
              exampleExpression = "(\\x. x) 1",
              exampleDescription = "Trivial function application of identity function to integer literal"
            }
        ]
    }
