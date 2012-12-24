{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Parser(parse) where

import Char

import Lang

data Token = TokenColon
           | TokenFun
           | TokenId Id
           | TokenLParen
           | TokenRParen
           | TokenDot
           | TokenComma
           | TokenProof
           deriving (Show)

-- parser produced by Happy Version 1.18.4

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

action_0 (11) = happyShift action_3
action_0 (4) = happyGoto action_2
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (17) = happyAccept
action_2 _ = happyFail

action_3 (9) = happyShift action_4
action_3 _ = happyFail

action_4 (9) = happyShift action_8
action_4 (10) = happyShift action_9
action_4 (11) = happyShift action_10
action_4 (12) = happyShift action_11
action_4 (5) = happyGoto action_5
action_4 (6) = happyGoto action_6
action_4 (8) = happyGoto action_7
action_4 _ = happyFail

action_5 (15) = happyShift action_17
action_5 (16) = happyShift action_18
action_5 _ = happyFail

action_6 (11) = happyShift action_10
action_6 (12) = happyShift action_11
action_6 (8) = happyGoto action_16
action_6 _ = happyReduce_6

action_7 _ = happyReduce_7

action_8 (11) = happyShift action_15
action_8 (7) = happyGoto action_14
action_8 _ = happyFail

action_9 (11) = happyShift action_10
action_9 (12) = happyShift action_11
action_9 (8) = happyGoto action_13
action_9 _ = happyFail

action_10 _ = happyReduce_11

action_11 (9) = happyShift action_8
action_11 (10) = happyShift action_9
action_11 (11) = happyShift action_10
action_11 (12) = happyShift action_11
action_11 (5) = happyGoto action_12
action_11 (6) = happyGoto action_6
action_11 (8) = happyGoto action_7
action_11 _ = happyFail

action_12 (13) = happyShift action_25
action_12 _ = happyFail

action_13 (9) = happyShift action_8
action_13 (10) = happyShift action_9
action_13 (11) = happyShift action_10
action_13 (12) = happyShift action_11
action_13 (5) = happyGoto action_24
action_13 (6) = happyGoto action_6
action_13 (8) = happyGoto action_7
action_13 _ = happyFail

action_14 (14) = happyShift action_22
action_14 (15) = happyShift action_23
action_14 _ = happyFail

action_15 (11) = happyShift action_10
action_15 (12) = happyShift action_11
action_15 (6) = happyGoto action_21
action_15 (8) = happyGoto action_7
action_15 _ = happyFail

action_16 _ = happyReduce_8

action_17 (11) = happyShift action_3
action_17 (4) = happyGoto action_20
action_17 _ = happyReduce_1

action_18 (9) = happyShift action_8
action_18 (10) = happyShift action_9
action_18 (11) = happyShift action_10
action_18 (12) = happyShift action_11
action_18 (5) = happyGoto action_19
action_18 (6) = happyGoto action_6
action_18 (8) = happyGoto action_7
action_18 _ = happyFail

action_19 (15) = happyShift action_28
action_19 _ = happyFail

action_20 _ = happyReduce_2

action_21 (11) = happyShift action_10
action_21 (12) = happyShift action_11
action_21 (8) = happyGoto action_16
action_21 _ = happyReduce_9

action_22 (11) = happyShift action_27
action_22 _ = happyFail

action_23 (9) = happyShift action_8
action_23 (10) = happyShift action_9
action_23 (11) = happyShift action_10
action_23 (12) = happyShift action_11
action_23 (5) = happyGoto action_26
action_23 (6) = happyGoto action_6
action_23 (8) = happyGoto action_7
action_23 _ = happyFail

action_24 _ = happyReduce_5

action_25 _ = happyReduce_12

action_26 _ = happyReduce_4

action_27 (11) = happyShift action_10
action_27 (12) = happyShift action_11
action_27 (6) = happyGoto action_30
action_27 (8) = happyGoto action_7
action_27 _ = happyFail

action_28 (11) = happyShift action_3
action_28 (4) = happyGoto action_29
action_28 _ = happyReduce_1

action_29 _ = happyReduce_3

action_30 (11) = happyShift action_10
action_30 (12) = happyShift action_11
action_30 (8) = happyGoto action_16
action_30 _ = happyReduce_10

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 ([]
	)

happyReduce_2 = happyReduce 5 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((Assume happy_var_1 happy_var_3):happy_var_5
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 7 4 happyReduction_3
happyReduction_3 ((HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((Prove happy_var_1 happy_var_3 happy_var_5):happy_var_7
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 4 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (foldr (uncurry Lam) happy_var_4 happy_var_2
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Lam "_" happy_var_2 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  6 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (App happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  7 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn7
		 ([(happy_var_1, happy_var_2)]
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 7 happyReduction_10
happyReduction_10 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	(HappyTerminal (TokenId happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (happy_var_1 ++ [(happy_var_3, happy_var_4)]
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn8
		 (Var happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 17 17 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenColon -> cont 9;
	TokenFun -> cont 10;
	TokenId happy_dollar_dollar -> cont 11;
	TokenLParen -> cont 12;
	TokenRParen -> cont 13;
	TokenComma -> cont 14;
	TokenDot -> cont 15;
	TokenProof -> cont 16;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => M a -> (a -> M b) -> M b
happyThen = (>>=)
happyReturn :: () => a -> M a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> M a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> M a
happyError' = parseError

parseTokens tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> M a
parseError msg = fail "parse error"

isBlank :: Char -> Bool 
isBlank x = x `elem` " \n\t\r"

isSymbol :: Char -> Bool 
isSymbol x = x `elem` "_+-*"

tokenize :: String -> [Token]
tokenize (x:xs)
  | isBlank x = tokenize xs
tokenize (x:xs)
  | isLower x || isSymbol x = TokenId [x] : tokenize xs
tokenize (x:xs)
  | isDigit x = TokenId y : tokenize ys
     where y  = x : takeWhile p xs
           ys = dropWhile p xs
           p  = isDigit
tokenize (x:xs)
  | isUpper x = TokenId y : tokenize ys
     where y  = x : takeWhile p xs
           ys = dropWhile p xs
           p  = isLower
tokenize ('=':xs)           = TokenProof : tokenize xs
tokenize (':':xs)           = TokenColon : tokenize xs
tokenize ('.':xs)           = TokenDot : tokenize xs
tokenize (',':xs)           = TokenComma : tokenize xs
tokenize ('(':xs)           = TokenLParen : tokenize xs
tokenize (')':xs)           = TokenRParen : tokenize xs
tokenize ('>':xs)           = TokenFun : tokenize xs
tokenize [] = []

parse :: String -> M Program
parse = parseTokens . tokenize
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
