{-# OPTIONS_GHC -w #-}
module CodeGeneration.GeneratorParser where

import CodeGeneration.GeneratorLexer
import CodeGeneration.GrammarModel
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,107) ([0,1024,0,0,256,0,0,256,0,0,0,64,0,0,0,0,2,0,0,4,0,32768,0,0,1024,0,0,0,128,0,0,0,0,0,8,0,128,0,0,0,0,0,0,0,0,4,0,0,512,0,256,0,0,128,0,0,0,0,0,0,2,0,0,0,0,32,0,0,2048,0,0,512,0,0,128,0,0,16,0,32,0,0,32,0,0,256,0,0,32768,0,0,0,0,0,512,0,0,2,0,0,0,0,0,11,0,0,0,0,32768,0,0,4096,0,0,0,0,0,0,0,0,128,0,64,0,0,0,0,0,0,8,0,0,0,0,0,0,4096,0,0,0,576,0,0,64,0,0,0,0,0,0,0,16384,0,0,0,0,0,9216,0,0,128,0,0,1024,0,0,64,0,0,32,0,32,0,0,0,0,0,2,0,0,256,0,0,0,0,0,704,0,0,0,0,0,0,0,16,0,0,0,0,0,0,1,0,0,0,32768,0,0,0,0,0,0,4,0,0,2,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseGrammar","File","StartBlock","Attributes","AttributeName","AttributeFields","AttributeFieldsC","IgnoreWhitespaces","ParseName","StartNonTermAndField","Lexer","TokenDescription","TokenDescriptionC","Grammar","GrammarC","Rule","RuleRights","RuleRightsC","Names","NamesC","Name","AttributeAction","Actions","ActionsC","'{'","'}'","'%attributetype'","'%attribute'","'%ignorewhitespaces'","'%name'","'%start'","'%token'","'%%'","':'","';'","'|'","'\"'","'eps'","TOKENNAME","WORD","ATTRIBUTEACTION","REGEX","HASKELLCODE","%eof"]
        bit_start = st Prelude.* 46
        bit_end = (st Prelude.+ 1) Prelude.* 46
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..45]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (27) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (27) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (29) = happyShift action_8
action_2 (6) = happyGoto action_6
action_2 (7) = happyGoto action_7
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (45) = happyShift action_5
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (46) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (28) = happyShift action_14
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (31) = happyShift action_13
action_6 (10) = happyGoto action_12
action_6 _ = happyReduce_9

action_7 (30) = happyShift action_11
action_7 (8) = happyGoto action_10
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (27) = happyShift action_9
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (42) = happyShift action_18
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_3

action_11 (42) = happyShift action_17
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (32) = happyShift action_16
action_12 (11) = happyGoto action_15
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_8

action_14 _ = happyReduce_2

action_15 (33) = happyShift action_23
action_15 (12) = happyGoto action_22
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (42) = happyShift action_21
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (27) = happyShift action_20
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (28) = happyShift action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_4

action_20 (42) = happyShift action_27
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_10

action_22 (34) = happyShift action_26
action_22 (13) = happyGoto action_25
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (42) = happyShift action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (42) = happyShift action_34
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (42) = happyShift action_33
action_25 (16) = happyGoto action_31
action_25 (18) = happyGoto action_32
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (41) = happyShift action_30
action_26 (14) = happyGoto action_29
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (28) = happyShift action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (30) = happyShift action_41
action_28 (9) = happyGoto action_40
action_28 _ = happyReduce_7

action_29 (35) = happyShift action_39
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (44) = happyShift action_38
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_1

action_32 (42) = happyShift action_33
action_32 (17) = happyGoto action_36
action_32 (18) = happyGoto action_37
action_32 _ = happyReduce_18

action_33 (36) = happyShift action_35
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_11

action_35 (39) = happyShift action_49
action_35 (40) = happyShift action_50
action_35 (42) = happyShift action_51
action_35 (19) = happyGoto action_46
action_35 (21) = happyGoto action_47
action_35 (23) = happyGoto action_48
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_16

action_37 (42) = happyShift action_33
action_37 (17) = happyGoto action_45
action_37 (18) = happyGoto action_37
action_37 _ = happyReduce_18

action_38 (41) = happyShift action_44
action_38 (15) = happyGoto action_43
action_38 _ = happyReduce_15

action_39 _ = happyReduce_12

action_40 _ = happyReduce_5

action_41 (42) = happyShift action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (27) = happyShift action_58
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_13

action_44 (44) = happyShift action_57
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_17

action_46 _ = happyReduce_19

action_47 (27) = happyShift action_56
action_47 (24) = happyGoto action_55
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (39) = happyShift action_49
action_48 (42) = happyShift action_51
action_48 (22) = happyGoto action_53
action_48 (23) = happyGoto action_54
action_48 _ = happyReduce_26

action_49 (41) = happyShift action_52
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_24

action_51 _ = happyReduce_27

action_52 (39) = happyShift action_66
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_23

action_54 (39) = happyShift action_49
action_54 (42) = happyShift action_51
action_54 (22) = happyGoto action_65
action_54 (23) = happyGoto action_54
action_54 _ = happyReduce_26

action_55 (38) = happyShift action_64
action_55 (20) = happyGoto action_63
action_55 _ = happyReduce_22

action_56 (43) = happyShift action_62
action_56 (25) = happyGoto action_61
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (41) = happyShift action_44
action_57 (15) = happyGoto action_60
action_57 _ = happyReduce_15

action_58 (42) = happyShift action_59
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (28) = happyShift action_71
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_14

action_61 (28) = happyShift action_70
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (37) = happyShift action_69
action_62 (26) = happyGoto action_68
action_62 _ = happyReduce_32

action_63 _ = happyReduce_20

action_64 (39) = happyShift action_49
action_64 (40) = happyShift action_50
action_64 (42) = happyShift action_51
action_64 (21) = happyGoto action_67
action_64 (23) = happyGoto action_48
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_25

action_66 _ = happyReduce_28

action_67 (27) = happyShift action_56
action_67 (24) = happyGoto action_74
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_30

action_69 (43) = happyShift action_73
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_29

action_71 (30) = happyShift action_41
action_71 (9) = happyGoto action_72
action_71 _ = happyReduce_7

action_72 _ = happyReduce_6

action_73 (37) = happyShift action_69
action_73 (26) = happyGoto action_76
action_73 _ = happyReduce_32

action_74 (38) = happyShift action_64
action_74 (20) = happyGoto action_75
action_74 _ = happyReduce_22

action_75 _ = happyReduce_21

action_76 _ = happyReduce_31

happyReduce_1 = happyReduce 7 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn16  happy_var_7) `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (GrammarFile happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	(HappyTerminal (TokenHaskellCode happy_var_2))
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Attributes happy_var_1 happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 7 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (happy_var_3
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 6 8 happyReduction_5
happyReduction_5 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((AttributeField happy_var_2 happy_var_4) : happy_var_6
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 6 9 happyReduction_6
happyReduction_6 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((AttributeField happy_var_2 happy_var_4) : happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_0  9 happyReduction_7
happyReduction_7  =  HappyAbsSyn9
		 ([]
	)

happyReduce_8 = happySpecReduce_1  10 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn10
		 (True
	)

happyReduce_9 = happySpecReduce_0  10 happyReduction_9
happyReduction_9  =  HappyAbsSyn10
		 (False
	)

happyReduce_10 = happySpecReduce_2  11 happyReduction_10
happyReduction_10 (HappyTerminal (TokenWord happy_var_2))
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  12 happyReduction_11
happyReduction_11 (HappyTerminal (TokenWord happy_var_3))
	(HappyTerminal (TokenWord happy_var_2))
	_
	 =  HappyAbsSyn12
		 (StartNonTerm happy_var_2 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  13 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  14 happyReduction_13
happyReduction_13 (HappyAbsSyn15  happy_var_3)
	(HappyTerminal (TokenRegex happy_var_2))
	(HappyTerminal (TokenNameToken happy_var_1))
	 =  HappyAbsSyn14
		 ((TokenDescription happy_var_1 happy_var_2) : happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  15 happyReduction_14
happyReduction_14 (HappyAbsSyn15  happy_var_3)
	(HappyTerminal (TokenRegex happy_var_2))
	(HappyTerminal (TokenNameToken happy_var_1))
	 =  HappyAbsSyn15
		 ((TokenDescription happy_var_1 happy_var_2) : happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  15 happyReduction_15
happyReduction_15  =  HappyAbsSyn15
		 ([]
	)

happyReduce_16 = happySpecReduce_2  16 happyReduction_16
happyReduction_16 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 : happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  17 happyReduction_17
happyReduction_17 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 : happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  17 happyReduction_18
happyReduction_18  =  HappyAbsSyn17
		 ([]
	)

happyReduce_19 = happySpecReduce_3  18 happyReduction_19
happyReduction_19 (HappyAbsSyn19  happy_var_3)
	_
	(HappyTerminal (TokenWord happy_var_1))
	 =  HappyAbsSyn18
		 (ParserRule happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  19 happyReduction_20
happyReduction_20 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn19
		 ((RuleRight happy_var_1 happy_var_2) : happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 20 happyReduction_21
happyReduction_21 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ((RuleRight happy_var_2 happy_var_3) : happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_0  20 happyReduction_22
happyReduction_22  =  HappyAbsSyn20
		 ([]
	)

happyReduce_23 = happySpecReduce_2  21 happyReduction_23
happyReduction_23 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 : happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  21 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn21
		 ([]
	)

happyReduce_25 = happySpecReduce_2  22 happyReduction_25
happyReduction_25 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 : happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  22 happyReduction_26
happyReduction_26  =  HappyAbsSyn22
		 ([]
	)

happyReduce_27 = happySpecReduce_1  23 happyReduction_27
happyReduction_27 (HappyTerminal (TokenWord happy_var_1))
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  23 happyReduction_28
happyReduction_28 _
	(HappyTerminal (TokenNameToken happy_var_2))
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  24 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  25 happyReduction_30
happyReduction_30 (HappyAbsSyn26  happy_var_2)
	(HappyTerminal (TokenAttributeAction happy_var_1))
	 =  HappyAbsSyn25
		 (happy_var_1 : happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  26 happyReduction_31
happyReduction_31 (HappyAbsSyn26  happy_var_3)
	(HappyTerminal (TokenAttributeAction happy_var_2))
	_
	 =  HappyAbsSyn26
		 (happy_var_2 : happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  26 happyReduction_32
happyReduction_32  =  HappyAbsSyn26
		 ([]
	)

happyNewToken action sts stk [] =
	action 46 46 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLCBracket -> cont 27;
	TokenRCBracket -> cont 28;
	TokenAType -> cont 29;
	TokenAttr -> cont 30;
	TokenIgnoreWhitespaces -> cont 31;
	TokenSName -> cont 32;
	TokenStart -> cont 33;
	TokenPToken -> cont 34;
	TokenDoubleP -> cont 35;
	TokenColon -> cont 36;
	TokenSemicolon -> cont 37;
	TokenStick -> cont 38;
	TokenQuotes -> cont 39;
	TokenEmpty -> cont 40;
	TokenNameToken happy_dollar_dollar -> cont 41;
	TokenWord happy_dollar_dollar -> cont 42;
	TokenAttributeAction happy_dollar_dollar -> cont 43;
	TokenRegex happy_dollar_dollar -> cont 44;
	TokenHaskellCode happy_dollar_dollar -> cont 45;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 46 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either [Token] a -> (a -> Either [Token] b) -> Either [Token] b
happyThen = (>>=)
happyReturn :: () => a -> Either [Token] a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either [Token] a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Either [Token] a
happyError' = (\(tokens, _) -> parseError tokens)
parseGrammar tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError = Left
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
