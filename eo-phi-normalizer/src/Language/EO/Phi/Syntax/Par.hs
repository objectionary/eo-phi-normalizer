{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.EO.Phi.Syntax.Par
  ( happyError
  , myLexer
  , pProgram
  , pObject
  , pAbstractObject
  , pDispatchedObject
  , pBinding
  , pListBinding
  , pBindings
  , pListBindings
  , pDispatch
  , pAttribute
  , pListAttribute
  , pDisp
  , pListDisp
  ) where

import Prelude

import qualified Language.EO.Phi.Syntax.Abs
import Language.EO.Phi.Syntax.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn16 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.Bytes))
	| HappyAbsSyn17 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.Function))
	| HappyAbsSyn18 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.LabelId))
	| HappyAbsSyn19 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.AlphaIndex))
	| HappyAbsSyn20 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.Program))
	| HappyAbsSyn21 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.Object))
	| HappyAbsSyn22 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.AbstractObject))
	| HappyAbsSyn23 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.DispatchedObject))
	| HappyAbsSyn24 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.Binding))
	| HappyAbsSyn25 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, [Language.EO.Phi.Syntax.Abs.Binding]))
	| HappyAbsSyn26 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.Bindings))
	| HappyAbsSyn27 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, [Language.EO.Phi.Syntax.Abs.Bindings]))
	| HappyAbsSyn28 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.Dispatch))
	| HappyAbsSyn29 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.Attribute))
	| HappyAbsSyn30 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, [Language.EO.Phi.Syntax.Abs.Attribute]))
	| HappyAbsSyn31 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, Language.EO.Phi.Syntax.Abs.Disp))
	| HappyAbsSyn32 ((Language.EO.Phi.Syntax.Abs.BNFC'Position, [Language.EO.Phi.Syntax.Abs.Disp]))

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,215) ([0,0,4,0,0,9344,8,0,36864,260,0,0,8194,0,0,60672,192,0,40960,6173,0,0,1,0,0,32,0,0,9216,65,0,0,33232,1,0,14848,48,0,512,0,0,16384,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,55808,385,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,3792,12,0,64,0,0,2048,0,0,0,0,0,0,64,0,0,0,0,0,0,948,3,0,0,0,0,256,0,0,0,0,0,0,16384,0,0,0,64,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,55808,385,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,948,3,0,0,0,0,0,0,0,0,33242,1,0,32,0,0,1024,0,0,0,49389,0,0,16,0,0,0,0,0,0,2048,0,0,24868,0,0,55808,385,0,8192,0,0,0,0,0,0,59392,192,0,0,6173,0,0,2,0,0,16,0,0,32768,3086,0,0,1,0,0,0,0,0,512,0,0,0,0,0,0,7424,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,24692,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram_internal","%start_pObject_internal","%start_pAbstractObject_internal","%start_pDispatchedObject_internal","%start_pBinding_internal","%start_pListBinding_internal","%start_pBindings_internal","%start_pListBindings_internal","%start_pDispatch_internal","%start_pAttribute_internal","%start_pListAttribute_internal","%start_pDisp_internal","%start_pListDisp_internal","Bytes","Function","LabelId","AlphaIndex","Program","Object","AbstractObject","DispatchedObject","Binding","ListBinding","Bindings","ListBindings","Dispatch","Attribute","ListAttribute","Disp","ListDisp","','","'.'","'{'","'}'","'\916'","'\934'","'\955'","'\957'","'\958'","'\961'","'\963'","'\966'","'\8614'","'\8709'","'\8869'","'\10509'","L_Bytes","L_Function","L_LabelId","L_AlphaIndex","%eof"]
        bit_start = st Prelude.* 53
        bit_end = (st Prelude.+ 1) Prelude.* 53
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..52]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (35) = happyShift action_57
action_0 (20) = happyGoto action_56
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (35) = happyShift action_54
action_1 (38) = happyShift action_33
action_1 (41) = happyShift action_34
action_1 (47) = happyShift action_55
action_1 (21) = happyGoto action_51
action_1 (22) = happyGoto action_52
action_1 (23) = happyGoto action_30
action_1 (28) = happyGoto action_53
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (35) = happyShift action_49
action_2 (38) = happyShift action_33
action_2 (41) = happyShift action_34
action_2 (47) = happyShift action_50
action_2 (22) = happyGoto action_47
action_2 (23) = happyGoto action_30
action_2 (28) = happyGoto action_48
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (35) = happyShift action_32
action_3 (47) = happyShift action_35
action_3 (23) = happyGoto action_46
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (37) = happyShift action_43
action_4 (39) = happyShift action_44
action_4 (40) = happyShift action_23
action_4 (42) = happyShift action_24
action_4 (43) = happyShift action_25
action_4 (44) = happyShift action_26
action_4 (51) = happyShift action_27
action_4 (52) = happyShift action_28
action_4 (18) = happyGoto action_19
action_4 (19) = happyGoto action_20
action_4 (24) = happyGoto action_45
action_4 (29) = happyGoto action_42
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (37) = happyShift action_43
action_5 (39) = happyShift action_44
action_5 (40) = happyShift action_23
action_5 (42) = happyShift action_24
action_5 (43) = happyShift action_25
action_5 (44) = happyShift action_26
action_5 (51) = happyShift action_27
action_5 (52) = happyShift action_28
action_5 (18) = happyGoto action_19
action_5 (19) = happyGoto action_20
action_5 (24) = happyGoto action_40
action_5 (25) = happyGoto action_41
action_5 (29) = happyGoto action_42
action_5 _ = happyReduce_31

action_6 (35) = happyShift action_38
action_6 (26) = happyGoto action_39
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (35) = happyShift action_38
action_7 (26) = happyGoto action_36
action_7 (27) = happyGoto action_37
action_7 _ = happyReduce_35

action_8 (35) = happyShift action_32
action_8 (38) = happyShift action_33
action_8 (41) = happyShift action_34
action_8 (47) = happyShift action_35
action_8 (23) = happyGoto action_30
action_8 (28) = happyGoto action_31
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (40) = happyShift action_23
action_9 (42) = happyShift action_24
action_9 (43) = happyShift action_25
action_9 (44) = happyShift action_26
action_9 (51) = happyShift action_27
action_9 (52) = happyShift action_28
action_9 (18) = happyGoto action_19
action_9 (19) = happyGoto action_20
action_9 (29) = happyGoto action_29
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (40) = happyShift action_23
action_10 (42) = happyShift action_24
action_10 (43) = happyShift action_25
action_10 (44) = happyShift action_26
action_10 (51) = happyShift action_27
action_10 (52) = happyShift action_28
action_10 (18) = happyGoto action_19
action_10 (19) = happyGoto action_20
action_10 (29) = happyGoto action_21
action_10 (30) = happyGoto action_22
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (35) = happyShift action_17
action_11 (31) = happyGoto action_18
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (35) = happyShift action_17
action_12 (31) = happyGoto action_15
action_12 (32) = happyGoto action_16
action_12 _ = happyReduce_49

action_13 (49) = happyShift action_14
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_13

action_15 (35) = happyShift action_17
action_15 (31) = happyGoto action_15
action_15 (32) = happyGoto action_74
action_15 _ = happyReduce_49

action_16 (53) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (37) = happyShift action_43
action_17 (39) = happyShift action_44
action_17 (40) = happyShift action_23
action_17 (42) = happyShift action_24
action_17 (43) = happyShift action_25
action_17 (44) = happyShift action_26
action_17 (51) = happyShift action_27
action_17 (52) = happyShift action_28
action_17 (18) = happyGoto action_19
action_17 (19) = happyGoto action_20
action_17 (24) = happyGoto action_40
action_17 (25) = happyGoto action_73
action_17 (29) = happyGoto action_42
action_17 _ = happyReduce_31

action_18 (53) = happyAccept
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_44

action_20 _ = happyReduce_45

action_21 (34) = happyShift action_72
action_21 _ = happyReduce_46

action_22 (53) = happyAccept
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_43

action_24 _ = happyReduce_41

action_25 _ = happyReduce_42

action_26 _ = happyReduce_40

action_27 _ = happyReduce_15

action_28 _ = happyReduce_16

action_29 (53) = happyAccept
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (35) = happyShift action_38
action_30 (26) = happyGoto action_36
action_30 (27) = happyGoto action_71
action_30 _ = happyReduce_35

action_31 (53) = happyAccept
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (37) = happyShift action_43
action_32 (39) = happyShift action_44
action_32 (40) = happyShift action_23
action_32 (42) = happyShift action_24
action_32 (43) = happyShift action_25
action_32 (44) = happyShift action_26
action_32 (51) = happyShift action_27
action_32 (52) = happyShift action_28
action_32 (18) = happyGoto action_19
action_32 (19) = happyGoto action_20
action_32 (24) = happyGoto action_40
action_32 (25) = happyGoto action_70
action_32 (29) = happyGoto action_42
action_32 _ = happyReduce_31

action_33 (34) = happyShift action_69
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (34) = happyShift action_68
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_26

action_36 (35) = happyShift action_38
action_36 (26) = happyGoto action_36
action_36 (27) = happyGoto action_67
action_36 _ = happyReduce_35

action_37 (53) = happyAccept
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (37) = happyShift action_43
action_38 (39) = happyShift action_44
action_38 (40) = happyShift action_23
action_38 (42) = happyShift action_24
action_38 (43) = happyShift action_25
action_38 (44) = happyShift action_26
action_38 (51) = happyShift action_27
action_38 (52) = happyShift action_28
action_38 (18) = happyGoto action_19
action_38 (19) = happyGoto action_20
action_38 (24) = happyGoto action_40
action_38 (25) = happyGoto action_66
action_38 (29) = happyGoto action_42
action_38 _ = happyReduce_31

action_39 (53) = happyAccept
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (33) = happyShift action_65
action_40 _ = happyReduce_32

action_41 (53) = happyAccept
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (45) = happyShift action_64
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (48) = happyShift action_63
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (48) = happyShift action_62
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (53) = happyAccept
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (53) = happyAccept
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (53) = happyAccept
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_23

action_49 (37) = happyShift action_43
action_49 (39) = happyShift action_44
action_49 (40) = happyShift action_23
action_49 (42) = happyShift action_24
action_49 (43) = happyShift action_25
action_49 (44) = happyShift action_26
action_49 (51) = happyShift action_27
action_49 (52) = happyShift action_28
action_49 (18) = happyGoto action_19
action_49 (19) = happyGoto action_20
action_49 (24) = happyGoto action_40
action_49 (25) = happyGoto action_61
action_49 (29) = happyGoto action_42
action_49 _ = happyReduce_31

action_50 (53) = happyReduce_24
action_50 _ = happyReduce_26

action_51 (53) = happyAccept
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (35) = happyShift action_60
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (35) = happyReduce_23
action_53 _ = happyReduce_20

action_54 (37) = happyShift action_43
action_54 (39) = happyShift action_44
action_54 (40) = happyShift action_23
action_54 (42) = happyShift action_24
action_54 (43) = happyShift action_25
action_54 (44) = happyShift action_26
action_54 (51) = happyShift action_27
action_54 (52) = happyShift action_28
action_54 (18) = happyGoto action_19
action_54 (19) = happyGoto action_20
action_54 (24) = happyGoto action_40
action_54 (25) = happyGoto action_59
action_54 (29) = happyGoto action_42
action_54 _ = happyReduce_31

action_55 (34) = happyReduce_26
action_55 (35) = happyReduce_26
action_55 _ = happyReduce_21

action_56 (53) = happyAccept
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (37) = happyShift action_43
action_57 (39) = happyShift action_44
action_57 (40) = happyShift action_23
action_57 (42) = happyShift action_24
action_57 (43) = happyShift action_25
action_57 (44) = happyShift action_26
action_57 (51) = happyShift action_27
action_57 (52) = happyShift action_28
action_57 (18) = happyGoto action_19
action_57 (19) = happyGoto action_20
action_57 (24) = happyGoto action_40
action_57 (25) = happyGoto action_58
action_57 (29) = happyGoto action_42
action_57 _ = happyReduce_31

action_58 (36) = happyShift action_89
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (36) = happyShift action_88
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (37) = happyShift action_43
action_60 (39) = happyShift action_44
action_60 (40) = happyShift action_23
action_60 (42) = happyShift action_24
action_60 (43) = happyShift action_25
action_60 (44) = happyShift action_26
action_60 (51) = happyShift action_27
action_60 (52) = happyShift action_28
action_60 (18) = happyGoto action_19
action_60 (19) = happyGoto action_20
action_60 (24) = happyGoto action_40
action_60 (25) = happyGoto action_87
action_60 (29) = happyGoto action_42
action_60 _ = happyReduce_31

action_61 (36) = happyShift action_86
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_30

action_63 (49) = happyShift action_14
action_63 (16) = happyGoto action_85
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (35) = happyShift action_54
action_64 (38) = happyShift action_33
action_64 (41) = happyShift action_34
action_64 (46) = happyShift action_84
action_64 (47) = happyShift action_55
action_64 (21) = happyGoto action_83
action_64 (22) = happyGoto action_52
action_64 (23) = happyGoto action_30
action_64 (28) = happyGoto action_53
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (37) = happyShift action_43
action_65 (39) = happyShift action_44
action_65 (40) = happyShift action_23
action_65 (42) = happyShift action_24
action_65 (43) = happyShift action_25
action_65 (44) = happyShift action_26
action_65 (51) = happyShift action_27
action_65 (52) = happyShift action_28
action_65 (18) = happyGoto action_19
action_65 (19) = happyGoto action_20
action_65 (24) = happyGoto action_40
action_65 (25) = happyGoto action_82
action_65 (29) = happyGoto action_42
action_65 _ = happyReduce_31

action_66 (36) = happyShift action_81
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_36

action_68 (40) = happyShift action_23
action_68 (42) = happyShift action_24
action_68 (43) = happyShift action_25
action_68 (44) = happyShift action_26
action_68 (51) = happyShift action_27
action_68 (52) = happyShift action_28
action_68 (18) = happyGoto action_19
action_68 (19) = happyGoto action_20
action_68 (29) = happyGoto action_21
action_68 (30) = happyGoto action_80
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (40) = happyShift action_23
action_69 (42) = happyShift action_24
action_69 (43) = happyShift action_25
action_69 (44) = happyShift action_26
action_69 (51) = happyShift action_27
action_69 (52) = happyShift action_28
action_69 (18) = happyGoto action_19
action_69 (19) = happyGoto action_20
action_69 (29) = happyGoto action_21
action_69 (30) = happyGoto action_79
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (36) = happyShift action_78
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (34) = happyShift action_77
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (40) = happyShift action_23
action_72 (42) = happyShift action_24
action_72 (43) = happyShift action_25
action_72 (44) = happyShift action_26
action_72 (51) = happyShift action_27
action_72 (52) = happyShift action_28
action_72 (18) = happyGoto action_19
action_72 (19) = happyGoto action_20
action_72 (29) = happyGoto action_21
action_72 (30) = happyGoto action_76
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (36) = happyShift action_75
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_50

action_75 (35) = happyShift action_38
action_75 (26) = happyGoto action_94
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_47

action_77 (40) = happyShift action_23
action_77 (42) = happyShift action_24
action_77 (43) = happyShift action_25
action_77 (44) = happyShift action_26
action_77 (51) = happyShift action_27
action_77 (52) = happyShift action_28
action_77 (18) = happyGoto action_19
action_77 (19) = happyGoto action_20
action_77 (29) = happyGoto action_21
action_77 (30) = happyGoto action_93
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_25

action_79 (35) = happyShift action_17
action_79 (31) = happyGoto action_15
action_79 (32) = happyGoto action_92
action_79 _ = happyReduce_49

action_80 (35) = happyShift action_17
action_80 (31) = happyGoto action_15
action_80 (32) = happyGoto action_91
action_80 _ = happyReduce_49

action_81 _ = happyReduce_34

action_82 _ = happyReduce_33

action_83 _ = happyReduce_27

action_84 _ = happyReduce_28

action_85 _ = happyReduce_29

action_86 (53) = happyReduce_22
action_86 _ = happyReduce_25

action_87 (36) = happyShift action_90
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (34) = happyReduce_25
action_88 (35) = happyReduce_25
action_88 _ = happyReduce_18

action_89 _ = happyReduce_17

action_90 (35) = happyShift action_38
action_90 (26) = happyGoto action_36
action_90 (27) = happyGoto action_97
action_90 _ = happyReduce_35

action_91 _ = happyReduce_39

action_92 _ = happyReduce_38

action_93 (35) = happyShift action_17
action_93 (31) = happyGoto action_15
action_93 (32) = happyGoto action_96
action_93 _ = happyReduce_49

action_94 (34) = happyShift action_95
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (40) = happyShift action_23
action_95 (42) = happyShift action_24
action_95 (43) = happyShift action_25
action_95 (44) = happyShift action_26
action_95 (51) = happyShift action_27
action_95 (52) = happyShift action_28
action_95 (18) = happyGoto action_19
action_95 (19) = happyGoto action_20
action_95 (29) = happyGoto action_21
action_95 (30) = happyGoto action_98
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_37

action_97 _ = happyReduce_19

action_98 _ = happyReduce_48

happyReduce_13 = happySpecReduce_1  16 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.Bytes (tokenText happy_var_1))
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  17 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.Function (tokenText happy_var_1))
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  18 happyReduction_15
happyReduction_15 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.LabelId (tokenText happy_var_1))
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  19 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.AlphaIndex (tokenText happy_var_1))
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  20 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn25  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.Program (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  21 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn25  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.Formation (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 5 21 happyReduction_19
happyReduction_19 ((HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((fst happy_var_1, Language.EO.Phi.Syntax.Abs.Application (fst happy_var_1) (snd happy_var_1) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1  21 happyReduction_20
happyReduction_20 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, Language.EO.Phi.Syntax.Abs.Dispatch (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  21 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.Termination (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  22 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn25  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.AbstractFormation (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  22 happyReduction_23
happyReduction_23 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn22
		 ((fst happy_var_1, Language.EO.Phi.Syntax.Abs.AbstractDispatch (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  22 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.AbstractTermination (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  23 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn25  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.DispatchedFormation (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  23 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.DispatchedTermination (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  24 happyReduction_27
happyReduction_27 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn24
		 ((fst happy_var_1, Language.EO.Phi.Syntax.Abs.AlphaBinding (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  24 happyReduction_28
happyReduction_28 _
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn24
		 ((fst happy_var_1, Language.EO.Phi.Syntax.Abs.EmptyBinding (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  24 happyReduction_29
happyReduction_29 (HappyAbsSyn16  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.DeltaBinding (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3))
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  24 happyReduction_30
happyReduction_30 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.LambdaBinding (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_0  25 happyReduction_31
happyReduction_31  =  HappyAbsSyn25
		 ((Language.EO.Phi.Syntax.Abs.BNFC'NoPosition, [])
	)

happyReduce_32 = happySpecReduce_1  25 happyReduction_32
happyReduction_32 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn25
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  25 happyReduction_33
happyReduction_33 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn25
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  26 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn25  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.Bindings (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_0  27 happyReduction_35
happyReduction_35  =  HappyAbsSyn27
		 ((Language.EO.Phi.Syntax.Abs.BNFC'NoPosition, [])
	)

happyReduce_36 = happySpecReduce_2  27 happyReduction_36
happyReduction_36 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn27
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_2))
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 5 28 happyReduction_37
happyReduction_37 ((HappyAbsSyn32  happy_var_5) `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	(HappyAbsSyn23  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 ((fst happy_var_1, Language.EO.Phi.Syntax.Abs.ObjectDispatch (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_4) (snd happy_var_5))
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 4 28 happyReduction_38
happyReduction_38 ((HappyAbsSyn32  happy_var_4) `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.HomeDispatch (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_4))
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 4 28 happyReduction_39
happyReduction_39 ((HappyAbsSyn32  happy_var_4) `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.ThisDispatch (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_4))
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_1  29 happyReduction_40
happyReduction_40 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.Phi (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  29 happyReduction_41
happyReduction_41 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.Rho (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  29 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.Sigma (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  29 happyReduction_43
happyReduction_43 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.VTX (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  29 happyReduction_44
happyReduction_44 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn29
		 ((fst happy_var_1, Language.EO.Phi.Syntax.Abs.Label (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  29 happyReduction_45
happyReduction_45 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn29
		 ((fst happy_var_1, Language.EO.Phi.Syntax.Abs.Alpha (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  30 happyReduction_46
happyReduction_46 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn30
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  30 happyReduction_47
happyReduction_47 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn30
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 6 31 happyReduction_48
happyReduction_48 ((HappyAbsSyn30  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn31
		 ((uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1), Language.EO.Phi.Syntax.Abs.Disp (uncurry Language.EO.Phi.Syntax.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_0  32 happyReduction_49
happyReduction_49  =  HappyAbsSyn32
		 ((Language.EO.Phi.Syntax.Abs.BNFC'NoPosition, [])
	)

happyReduce_50 = happySpecReduce_2  32 happyReduction_50
happyReduction_50 (HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn32
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_2))
	)
happyReduction_50 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 53 53 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 33;
	PT _ (TS _ 2) -> cont 34;
	PT _ (TS _ 3) -> cont 35;
	PT _ (TS _ 4) -> cont 36;
	PT _ (TS _ 5) -> cont 37;
	PT _ (TS _ 6) -> cont 38;
	PT _ (TS _ 7) -> cont 39;
	PT _ (TS _ 8) -> cont 40;
	PT _ (TS _ 9) -> cont 41;
	PT _ (TS _ 10) -> cont 42;
	PT _ (TS _ 11) -> cont 43;
	PT _ (TS _ 12) -> cont 44;
	PT _ (TS _ 13) -> cont 45;
	PT _ (TS _ 14) -> cont 46;
	PT _ (TS _ 15) -> cont 47;
	PT _ (TS _ 16) -> cont 48;
	PT _ (T_Bytes _) -> cont 49;
	PT _ (T_Function _) -> cont 50;
	PT _ (T_LabelId _) -> cont 51;
	PT _ (T_AlphaIndex _) -> cont 52;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 53 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

pObject_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

pAbstractObject_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

pDispatchedObject_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

pBinding_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

pListBinding_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn25 z -> happyReturn z; _other -> notHappyAtAll })

pBindings_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

pListBindings_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pDispatch_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn28 z -> happyReturn z; _other -> notHappyAtAll })

pAttribute_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn29 z -> happyReturn z; _other -> notHappyAtAll })

pListAttribute_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pDisp_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn31 z -> happyReturn z; _other -> notHappyAtAll })

pListDisp_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_12 tks) (\x -> case x of {HappyAbsSyn32 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pProgram :: [Token] -> Err Language.EO.Phi.Syntax.Abs.Program
pProgram = fmap snd . pProgram_internal

pObject :: [Token] -> Err Language.EO.Phi.Syntax.Abs.Object
pObject = fmap snd . pObject_internal

pAbstractObject :: [Token] -> Err Language.EO.Phi.Syntax.Abs.AbstractObject
pAbstractObject = fmap snd . pAbstractObject_internal

pDispatchedObject :: [Token] -> Err Language.EO.Phi.Syntax.Abs.DispatchedObject
pDispatchedObject = fmap snd . pDispatchedObject_internal

pBinding :: [Token] -> Err Language.EO.Phi.Syntax.Abs.Binding
pBinding = fmap snd . pBinding_internal

pListBinding :: [Token] -> Err [Language.EO.Phi.Syntax.Abs.Binding]
pListBinding = fmap snd . pListBinding_internal

pBindings :: [Token] -> Err Language.EO.Phi.Syntax.Abs.Bindings
pBindings = fmap snd . pBindings_internal

pListBindings :: [Token] -> Err [Language.EO.Phi.Syntax.Abs.Bindings]
pListBindings = fmap snd . pListBindings_internal

pDispatch :: [Token] -> Err Language.EO.Phi.Syntax.Abs.Dispatch
pDispatch = fmap snd . pDispatch_internal

pAttribute :: [Token] -> Err Language.EO.Phi.Syntax.Abs.Attribute
pAttribute = fmap snd . pAttribute_internal

pListAttribute :: [Token] -> Err [Language.EO.Phi.Syntax.Abs.Attribute]
pListAttribute = fmap snd . pListAttribute_internal

pDisp :: [Token] -> Err Language.EO.Phi.Syntax.Abs.Disp
pDisp = fmap snd . pDisp_internal

pListDisp :: [Token] -> Err [Language.EO.Phi.Syntax.Abs.Disp]
pListDisp = fmap snd . pListDisp_internal
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
