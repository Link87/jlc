{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Javalette.Gen.LLVM (
  generateIR  
) where

import Control.Monad.Identity
import Control.Monad.State
import Data.Text (Text)
import Javalette.Check.TypeCheck ( AnnotatedProg )

generateIR :: AnnotatedProg -> Text
generateIR _ = "\
                \ declare void @printInt(i32 %n)\n\
                \ define i32 @main() {\n\
                \   entry:  %t1 = call i32 @sum(i32 100)\n\
                \           call void @printInt(i32 %t1)\n\
                \           ret i32 0\n\ 
                \ }\n\
                \ define i32 @sum (i32 %n) {\n\
                \   entry:  %sum = alloca i32\n\
                \           store i32 0, i32* %sum\n\
                \           %i = alloca i32\n\
                \           store i32 0, i32* %i\n\
                \           br label %lab1\n\
                \   lab1:   %t1 = load i32, i32* %i\n\
                \           %t2 = add i32 %t1, 1\n\
                \          %t3 = load i32, i32* %sum\n\
                \           %t4 = add i32 %t2, %t3\n\
                \          store i32 %t2, i32* %i\n\
                \           store i32 %t4, i32* %sum\n\
                \          %t5 = icmp eq i32 %t2, %n\n\
                \           br i1 %t5, label %end,label %lab1\n\
                \   end:    ret i32 %t4\n\
                \ }\n"

type Env = ()

-- data Env = Env {
-- vars :: [Map Ident Int],
-- maxvar :: Int,
-- code :: [Instruction]
-- }

newtype Gen a = MkGen (StateT Env Identity a)
    deriving (Functor, Applicative, Monad, MonadState Env)
