{-# OPTIONS_GHC -w #-}
module Parser (parseExpr, E (..)) where
import Data.Char (isDigit, isSpace, isAlpha)
import Prelude hiding (LT, GT, EQ)
import Declare
import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19
	= HappyTerminal (Token)
	| HappyErrorToken Int
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

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,433) ([0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,32768,37075,3716,596,0,0,0,0,16384,0,0,0,0,0,8,0,0,0,0,0,0,960,24256,4,0,512,0,0,512,0,0,0,0,0,0,0,0,0,0,0,13536,41252,38147,0,6768,53394,19073,0,3384,59465,9536,0,34460,29732,4768,0,17230,14866,2384,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,14336,18701,16616,37,26624,9216,64,8,1024,0,0,0,0,0,0,0,0,128,0,0,128,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,8,0,14336,18701,16616,37,2048,4096,0,0,0,1504,12128,2,0,0,0,0,0,121,35800,0,0,0,0,0,2048,30,8950,0,0,512,0,0,3384,59465,9536,0,34460,29732,4768,0,17230,14866,2384,0,8615,7433,1192,32768,37075,3716,596,49152,18537,1858,298,57344,9268,929,149,28672,37402,33232,74,14336,18701,16616,37,39936,9350,41076,18,19968,4675,20538,9,42752,2337,43037,4,256,0,0,0,1664,512,32772,0,0,0,2048,0,0,0,0,0,32768,32775,61,0,49152,49155,14,0,57344,24577,3,0,61440,0,0,0,30720,0,0,0,15360,0,0,0,7680,0,0,0,0,0,0,0,0,0,0,0,768,0,0,0,384,0,0,0,752,6064,1,3328,1024,8,1,0,0,0,0,13536,41252,38147,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,61440,45058,279,32768,37075,3716,596,32768,0,8192,0,16384,32768,0,0,0,0,2,0,0,2048,0,0,26624,8192,64,8,19968,4675,20538,9,0,240,6064,1,0,8448,0,0,0,0,4096,0,64,0,0,0,416,128,8193,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,30720,55312,139,49152,18537,1858,298,57344,9268,929,149,0,0,0,0,0,0,0,0,0,0,2,1,0,0,0,0,42752,2337,43037,4,0,0,0,0,0,0,4,0,0,2078,8950,0,0,271,4475,0,208,32832,4096,0,104,16416,2048,0,0,4,128,0,0,66,0,0,0,16,0,0,0,0,0,57344,9268,929,149,0,3840,31488,17,53248,16384,128,16,0,8192,0,0,1024,0,0,0,0,2048,0,0,0,8192,8192,0,0,0,4128,0,0,16384,0,0,0,0,0,0,16,0,0,0,49152,49219,1118,0,17230,14866,2384,0,0,128,0,32768,37075,3716,596,0,0,0,0,0,0,0,0,28672,37402,33232,74,0,0,1,0,39936,9350,41076,18,0,0,32768,0,0,1264,6064,1,3328,1024,8,1,0,316,17900,0,0,30,758,0,32,0,0,0,32768,32775,189,0,0,0,2,0,0,0,0,0,0,0,64,0,0,0,0,0,0,4096,0,57344,9268,929,149,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Program","Functions","Function","Types","Type","ids","typ","tRcds","tRcd1","Exp","Cases","Cases1","Rcds","Rcd1","App","Exps","var","id","int","Int","Bool","case","of","raise","try","with","type","'|'","'+'","'-'","'*'","'/'","'('","')'","'}'","'{'","';'","':'","','","'='","if","else","true","false","'<'","'<='","'=>'","'>'","'>='","'=='","'&&'","'!'","'||'","fun","'->'","'@'","'.'","String","string","%eof"]
        bit_start = st * 63
        bit_end = (st + 1) * 63
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..62]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (4) = happyGoto action_3
action_0 (7) = happyGoto action_2
action_0 _ = happyReduce_6

action_1 (7) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (30) = happyShift action_6
action_2 (5) = happyGoto action_4
action_2 (8) = happyGoto action_5
action_2 _ = happyReduce_3

action_3 (63) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (20) = happyShift action_11
action_4 (21) = happyShift action_12
action_4 (22) = happyShift action_13
action_4 (25) = happyShift action_14
action_4 (27) = happyShift action_15
action_4 (28) = happyShift action_16
action_4 (33) = happyShift action_17
action_4 (36) = happyShift action_18
action_4 (39) = happyShift action_19
action_4 (44) = happyShift action_20
action_4 (46) = happyShift action_21
action_4 (47) = happyShift action_22
action_4 (48) = happyShift action_23
action_4 (55) = happyShift action_24
action_4 (57) = happyShift action_25
action_4 (59) = happyShift action_26
action_4 (62) = happyShift action_27
action_4 (6) = happyGoto action_8
action_4 (13) = happyGoto action_9
action_4 (18) = happyGoto action_10
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_5

action_6 (21) = happyShift action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (43) = happyShift action_61
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_2

action_9 (32) = happyShift action_49
action_9 (33) = happyShift action_50
action_9 (34) = happyShift action_51
action_9 (35) = happyShift action_52
action_9 (48) = happyShift action_53
action_9 (49) = happyShift action_54
action_9 (51) = happyShift action_55
action_9 (52) = happyShift action_56
action_9 (53) = happyShift action_57
action_9 (54) = happyShift action_58
action_9 (56) = happyShift action_59
action_9 (60) = happyShift action_60
action_9 _ = happyReduce_1

action_10 (36) = happyShift action_48
action_10 _ = happyReduce_42

action_11 (21) = happyShift action_47
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_58

action_13 _ = happyReduce_55

action_14 (20) = happyShift action_11
action_14 (21) = happyShift action_12
action_14 (22) = happyShift action_13
action_14 (25) = happyShift action_14
action_14 (27) = happyShift action_15
action_14 (28) = happyShift action_16
action_14 (33) = happyShift action_17
action_14 (36) = happyShift action_18
action_14 (39) = happyShift action_19
action_14 (44) = happyShift action_20
action_14 (46) = happyShift action_21
action_14 (47) = happyShift action_22
action_14 (48) = happyShift action_23
action_14 (55) = happyShift action_24
action_14 (57) = happyShift action_38
action_14 (59) = happyShift action_26
action_14 (62) = happyShift action_27
action_14 (13) = happyGoto action_46
action_14 (18) = happyGoto action_10
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (20) = happyShift action_11
action_15 (21) = happyShift action_12
action_15 (22) = happyShift action_13
action_15 (25) = happyShift action_14
action_15 (27) = happyShift action_15
action_15 (28) = happyShift action_16
action_15 (33) = happyShift action_17
action_15 (36) = happyShift action_18
action_15 (39) = happyShift action_19
action_15 (44) = happyShift action_20
action_15 (46) = happyShift action_21
action_15 (47) = happyShift action_22
action_15 (48) = happyShift action_23
action_15 (55) = happyShift action_24
action_15 (57) = happyShift action_38
action_15 (59) = happyShift action_26
action_15 (62) = happyShift action_27
action_15 (13) = happyGoto action_45
action_15 (18) = happyGoto action_10
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (20) = happyShift action_11
action_16 (21) = happyShift action_12
action_16 (22) = happyShift action_13
action_16 (25) = happyShift action_14
action_16 (27) = happyShift action_15
action_16 (28) = happyShift action_16
action_16 (33) = happyShift action_17
action_16 (36) = happyShift action_18
action_16 (39) = happyShift action_19
action_16 (44) = happyShift action_20
action_16 (46) = happyShift action_21
action_16 (47) = happyShift action_22
action_16 (48) = happyShift action_23
action_16 (55) = happyShift action_24
action_16 (57) = happyShift action_38
action_16 (59) = happyShift action_26
action_16 (62) = happyShift action_27
action_16 (13) = happyGoto action_44
action_16 (18) = happyGoto action_10
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (20) = happyShift action_11
action_17 (21) = happyShift action_12
action_17 (22) = happyShift action_13
action_17 (25) = happyShift action_14
action_17 (27) = happyShift action_15
action_17 (28) = happyShift action_16
action_17 (33) = happyShift action_17
action_17 (36) = happyShift action_18
action_17 (39) = happyShift action_19
action_17 (44) = happyShift action_20
action_17 (46) = happyShift action_21
action_17 (47) = happyShift action_22
action_17 (48) = happyShift action_23
action_17 (55) = happyShift action_24
action_17 (57) = happyShift action_38
action_17 (59) = happyShift action_26
action_17 (62) = happyShift action_27
action_17 (13) = happyGoto action_43
action_17 (18) = happyGoto action_10
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (20) = happyShift action_11
action_18 (21) = happyShift action_12
action_18 (22) = happyShift action_13
action_18 (25) = happyShift action_14
action_18 (27) = happyShift action_15
action_18 (28) = happyShift action_16
action_18 (33) = happyShift action_17
action_18 (36) = happyShift action_18
action_18 (39) = happyShift action_19
action_18 (44) = happyShift action_20
action_18 (46) = happyShift action_21
action_18 (47) = happyShift action_22
action_18 (48) = happyShift action_23
action_18 (55) = happyShift action_24
action_18 (57) = happyShift action_38
action_18 (59) = happyShift action_26
action_18 (62) = happyShift action_27
action_18 (13) = happyGoto action_42
action_18 (18) = happyGoto action_10
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (16) = happyGoto action_41
action_19 _ = happyReduce_50

action_20 (36) = happyShift action_40
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_56

action_22 _ = happyReduce_57

action_23 (21) = happyShift action_39
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (20) = happyShift action_11
action_24 (21) = happyShift action_12
action_24 (22) = happyShift action_13
action_24 (25) = happyShift action_14
action_24 (27) = happyShift action_15
action_24 (28) = happyShift action_16
action_24 (33) = happyShift action_17
action_24 (36) = happyShift action_18
action_24 (39) = happyShift action_19
action_24 (44) = happyShift action_20
action_24 (46) = happyShift action_21
action_24 (47) = happyShift action_22
action_24 (48) = happyShift action_23
action_24 (55) = happyShift action_24
action_24 (57) = happyShift action_38
action_24 (59) = happyShift action_26
action_24 (62) = happyShift action_27
action_24 (13) = happyGoto action_37
action_24 (18) = happyGoto action_10
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (21) = happyShift action_30
action_25 (23) = happyShift action_31
action_25 (24) = happyShift action_32
action_25 (36) = happyShift action_33
action_25 (39) = happyShift action_34
action_25 (48) = happyShift action_35
action_25 (61) = happyShift action_36
action_25 (10) = happyGoto action_29
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (21) = happyShift action_28
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_59

action_28 (36) = happyShift action_90
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (21) = happyShift action_88
action_29 (58) = happyShift action_89
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_16

action_31 _ = happyReduce_11

action_32 _ = happyReduce_12

action_33 (21) = happyShift action_87
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (11) = happyGoto action_86
action_34 _ = happyReduce_19

action_35 (11) = happyGoto action_85
action_35 _ = happyReduce_19

action_36 _ = happyReduce_17

action_37 (60) = happyShift action_60
action_37 _ = happyReduce_37

action_38 (36) = happyShift action_33
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (43) = happyShift action_84
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (20) = happyShift action_11
action_40 (21) = happyShift action_12
action_40 (22) = happyShift action_13
action_40 (25) = happyShift action_14
action_40 (27) = happyShift action_15
action_40 (28) = happyShift action_16
action_40 (33) = happyShift action_17
action_40 (36) = happyShift action_18
action_40 (39) = happyShift action_19
action_40 (44) = happyShift action_20
action_40 (46) = happyShift action_21
action_40 (47) = happyShift action_22
action_40 (48) = happyShift action_23
action_40 (55) = happyShift action_24
action_40 (57) = happyShift action_38
action_40 (59) = happyShift action_26
action_40 (62) = happyShift action_27
action_40 (13) = happyGoto action_83
action_40 (18) = happyGoto action_10
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (21) = happyShift action_81
action_41 (38) = happyShift action_82
action_41 (17) = happyGoto action_80
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (32) = happyShift action_49
action_42 (33) = happyShift action_50
action_42 (34) = happyShift action_51
action_42 (35) = happyShift action_52
action_42 (37) = happyShift action_79
action_42 (48) = happyShift action_53
action_42 (49) = happyShift action_54
action_42 (51) = happyShift action_55
action_42 (52) = happyShift action_56
action_42 (53) = happyShift action_57
action_42 (54) = happyShift action_58
action_42 (56) = happyShift action_59
action_42 (60) = happyShift action_60
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (60) = happyShift action_60
action_43 _ = happyReduce_36

action_44 (29) = happyShift action_78
action_44 (32) = happyShift action_49
action_44 (33) = happyShift action_50
action_44 (34) = happyShift action_51
action_44 (35) = happyShift action_52
action_44 (48) = happyShift action_53
action_44 (49) = happyShift action_54
action_44 (51) = happyShift action_55
action_44 (52) = happyShift action_56
action_44 (53) = happyShift action_57
action_44 (54) = happyShift action_58
action_44 (56) = happyShift action_59
action_44 (60) = happyShift action_60
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (32) = happyShift action_49
action_45 (33) = happyShift action_50
action_45 (34) = happyShift action_51
action_45 (35) = happyShift action_52
action_45 (48) = happyShift action_53
action_45 (49) = happyShift action_54
action_45 (51) = happyShift action_55
action_45 (52) = happyShift action_56
action_45 (53) = happyShift action_57
action_45 (54) = happyShift action_58
action_45 (56) = happyShift action_59
action_45 (60) = happyShift action_60
action_45 _ = happyReduce_44

action_46 (26) = happyShift action_77
action_46 (32) = happyShift action_49
action_46 (33) = happyShift action_50
action_46 (34) = happyShift action_51
action_46 (35) = happyShift action_52
action_46 (48) = happyShift action_53
action_46 (49) = happyShift action_54
action_46 (51) = happyShift action_55
action_46 (52) = happyShift action_56
action_46 (53) = happyShift action_57
action_46 (54) = happyShift action_58
action_46 (56) = happyShift action_59
action_46 (60) = happyShift action_60
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (41) = happyShift action_76
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (20) = happyShift action_11
action_48 (21) = happyShift action_12
action_48 (22) = happyShift action_13
action_48 (25) = happyShift action_14
action_48 (27) = happyShift action_15
action_48 (28) = happyShift action_16
action_48 (33) = happyShift action_17
action_48 (36) = happyShift action_18
action_48 (39) = happyShift action_19
action_48 (44) = happyShift action_20
action_48 (46) = happyShift action_21
action_48 (47) = happyShift action_22
action_48 (48) = happyShift action_23
action_48 (55) = happyShift action_24
action_48 (57) = happyShift action_38
action_48 (59) = happyShift action_26
action_48 (62) = happyShift action_27
action_48 (13) = happyGoto action_75
action_48 (18) = happyGoto action_10
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (20) = happyShift action_11
action_49 (21) = happyShift action_12
action_49 (22) = happyShift action_13
action_49 (25) = happyShift action_14
action_49 (27) = happyShift action_15
action_49 (28) = happyShift action_16
action_49 (33) = happyShift action_17
action_49 (36) = happyShift action_18
action_49 (39) = happyShift action_19
action_49 (44) = happyShift action_20
action_49 (46) = happyShift action_21
action_49 (47) = happyShift action_22
action_49 (48) = happyShift action_23
action_49 (55) = happyShift action_24
action_49 (57) = happyShift action_38
action_49 (59) = happyShift action_26
action_49 (62) = happyShift action_27
action_49 (13) = happyGoto action_74
action_49 (18) = happyGoto action_10
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (20) = happyShift action_11
action_50 (21) = happyShift action_12
action_50 (22) = happyShift action_13
action_50 (25) = happyShift action_14
action_50 (27) = happyShift action_15
action_50 (28) = happyShift action_16
action_50 (33) = happyShift action_17
action_50 (36) = happyShift action_18
action_50 (39) = happyShift action_19
action_50 (44) = happyShift action_20
action_50 (46) = happyShift action_21
action_50 (47) = happyShift action_22
action_50 (48) = happyShift action_23
action_50 (55) = happyShift action_24
action_50 (57) = happyShift action_38
action_50 (59) = happyShift action_26
action_50 (62) = happyShift action_27
action_50 (13) = happyGoto action_73
action_50 (18) = happyGoto action_10
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (20) = happyShift action_11
action_51 (21) = happyShift action_12
action_51 (22) = happyShift action_13
action_51 (25) = happyShift action_14
action_51 (27) = happyShift action_15
action_51 (28) = happyShift action_16
action_51 (33) = happyShift action_17
action_51 (36) = happyShift action_18
action_51 (39) = happyShift action_19
action_51 (44) = happyShift action_20
action_51 (46) = happyShift action_21
action_51 (47) = happyShift action_22
action_51 (48) = happyShift action_23
action_51 (55) = happyShift action_24
action_51 (57) = happyShift action_38
action_51 (59) = happyShift action_26
action_51 (62) = happyShift action_27
action_51 (13) = happyGoto action_72
action_51 (18) = happyGoto action_10
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (20) = happyShift action_11
action_52 (21) = happyShift action_12
action_52 (22) = happyShift action_13
action_52 (25) = happyShift action_14
action_52 (27) = happyShift action_15
action_52 (28) = happyShift action_16
action_52 (33) = happyShift action_17
action_52 (36) = happyShift action_18
action_52 (39) = happyShift action_19
action_52 (44) = happyShift action_20
action_52 (46) = happyShift action_21
action_52 (47) = happyShift action_22
action_52 (48) = happyShift action_23
action_52 (55) = happyShift action_24
action_52 (57) = happyShift action_38
action_52 (59) = happyShift action_26
action_52 (62) = happyShift action_27
action_52 (13) = happyGoto action_71
action_52 (18) = happyGoto action_10
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (20) = happyShift action_11
action_53 (21) = happyShift action_12
action_53 (22) = happyShift action_13
action_53 (25) = happyShift action_14
action_53 (27) = happyShift action_15
action_53 (28) = happyShift action_16
action_53 (33) = happyShift action_17
action_53 (36) = happyShift action_18
action_53 (39) = happyShift action_19
action_53 (44) = happyShift action_20
action_53 (46) = happyShift action_21
action_53 (47) = happyShift action_22
action_53 (48) = happyShift action_23
action_53 (55) = happyShift action_24
action_53 (57) = happyShift action_38
action_53 (59) = happyShift action_26
action_53 (62) = happyShift action_27
action_53 (13) = happyGoto action_70
action_53 (18) = happyGoto action_10
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (20) = happyShift action_11
action_54 (21) = happyShift action_12
action_54 (22) = happyShift action_13
action_54 (25) = happyShift action_14
action_54 (27) = happyShift action_15
action_54 (28) = happyShift action_16
action_54 (33) = happyShift action_17
action_54 (36) = happyShift action_18
action_54 (39) = happyShift action_19
action_54 (44) = happyShift action_20
action_54 (46) = happyShift action_21
action_54 (47) = happyShift action_22
action_54 (48) = happyShift action_23
action_54 (55) = happyShift action_24
action_54 (57) = happyShift action_38
action_54 (59) = happyShift action_26
action_54 (62) = happyShift action_27
action_54 (13) = happyGoto action_69
action_54 (18) = happyGoto action_10
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (20) = happyShift action_11
action_55 (21) = happyShift action_12
action_55 (22) = happyShift action_13
action_55 (25) = happyShift action_14
action_55 (27) = happyShift action_15
action_55 (28) = happyShift action_16
action_55 (33) = happyShift action_17
action_55 (36) = happyShift action_18
action_55 (39) = happyShift action_19
action_55 (44) = happyShift action_20
action_55 (46) = happyShift action_21
action_55 (47) = happyShift action_22
action_55 (48) = happyShift action_23
action_55 (55) = happyShift action_24
action_55 (57) = happyShift action_38
action_55 (59) = happyShift action_26
action_55 (62) = happyShift action_27
action_55 (13) = happyGoto action_68
action_55 (18) = happyGoto action_10
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (20) = happyShift action_11
action_56 (21) = happyShift action_12
action_56 (22) = happyShift action_13
action_56 (25) = happyShift action_14
action_56 (27) = happyShift action_15
action_56 (28) = happyShift action_16
action_56 (33) = happyShift action_17
action_56 (36) = happyShift action_18
action_56 (39) = happyShift action_19
action_56 (44) = happyShift action_20
action_56 (46) = happyShift action_21
action_56 (47) = happyShift action_22
action_56 (48) = happyShift action_23
action_56 (55) = happyShift action_24
action_56 (57) = happyShift action_38
action_56 (59) = happyShift action_26
action_56 (62) = happyShift action_27
action_56 (13) = happyGoto action_67
action_56 (18) = happyGoto action_10
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (20) = happyShift action_11
action_57 (21) = happyShift action_12
action_57 (22) = happyShift action_13
action_57 (25) = happyShift action_14
action_57 (27) = happyShift action_15
action_57 (28) = happyShift action_16
action_57 (33) = happyShift action_17
action_57 (36) = happyShift action_18
action_57 (39) = happyShift action_19
action_57 (44) = happyShift action_20
action_57 (46) = happyShift action_21
action_57 (47) = happyShift action_22
action_57 (48) = happyShift action_23
action_57 (55) = happyShift action_24
action_57 (57) = happyShift action_38
action_57 (59) = happyShift action_26
action_57 (62) = happyShift action_27
action_57 (13) = happyGoto action_66
action_57 (18) = happyGoto action_10
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (20) = happyShift action_11
action_58 (21) = happyShift action_12
action_58 (22) = happyShift action_13
action_58 (25) = happyShift action_14
action_58 (27) = happyShift action_15
action_58 (28) = happyShift action_16
action_58 (33) = happyShift action_17
action_58 (36) = happyShift action_18
action_58 (39) = happyShift action_19
action_58 (44) = happyShift action_20
action_58 (46) = happyShift action_21
action_58 (47) = happyShift action_22
action_58 (48) = happyShift action_23
action_58 (55) = happyShift action_24
action_58 (57) = happyShift action_38
action_58 (59) = happyShift action_26
action_58 (62) = happyShift action_27
action_58 (13) = happyGoto action_65
action_58 (18) = happyGoto action_10
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (20) = happyShift action_11
action_59 (21) = happyShift action_12
action_59 (22) = happyShift action_13
action_59 (25) = happyShift action_14
action_59 (27) = happyShift action_15
action_59 (28) = happyShift action_16
action_59 (33) = happyShift action_17
action_59 (36) = happyShift action_18
action_59 (39) = happyShift action_19
action_59 (44) = happyShift action_20
action_59 (46) = happyShift action_21
action_59 (47) = happyShift action_22
action_59 (48) = happyShift action_23
action_59 (55) = happyShift action_24
action_59 (57) = happyShift action_38
action_59 (59) = happyShift action_26
action_59 (62) = happyShift action_27
action_59 (13) = happyGoto action_64
action_59 (18) = happyGoto action_10
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (21) = happyShift action_63
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (21) = happyShift action_30
action_61 (23) = happyShift action_31
action_61 (24) = happyShift action_32
action_61 (39) = happyShift action_34
action_61 (48) = happyShift action_35
action_61 (61) = happyShift action_36
action_61 (10) = happyGoto action_62
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (58) = happyShift action_89
action_62 _ = happyReduce_7

action_63 _ = happyReduce_40

action_64 (32) = happyShift action_49
action_64 (33) = happyShift action_50
action_64 (34) = happyShift action_51
action_64 (35) = happyShift action_52
action_64 (48) = happyShift action_53
action_64 (49) = happyShift action_54
action_64 (51) = happyShift action_55
action_64 (52) = happyShift action_56
action_64 (53) = happyShift action_57
action_64 (54) = happyShift action_58
action_64 (60) = happyShift action_60
action_64 _ = happyReduce_25

action_65 (32) = happyShift action_49
action_65 (33) = happyShift action_50
action_65 (34) = happyShift action_51
action_65 (35) = happyShift action_52
action_65 (48) = happyShift action_53
action_65 (49) = happyShift action_54
action_65 (51) = happyShift action_55
action_65 (52) = happyShift action_56
action_65 (53) = happyShift action_57
action_65 (60) = happyShift action_60
action_65 _ = happyReduce_26

action_66 (32) = happyShift action_49
action_66 (33) = happyShift action_50
action_66 (34) = happyShift action_51
action_66 (35) = happyShift action_52
action_66 (48) = happyShift action_53
action_66 (49) = happyShift action_54
action_66 (51) = happyShift action_55
action_66 (52) = happyShift action_56
action_66 (53) = happyFail []
action_66 (60) = happyShift action_60
action_66 _ = happyReduce_27

action_67 (32) = happyShift action_49
action_67 (33) = happyShift action_50
action_67 (34) = happyShift action_51
action_67 (35) = happyShift action_52
action_67 (48) = happyFail []
action_67 (49) = happyFail []
action_67 (51) = happyFail []
action_67 (52) = happyFail []
action_67 (60) = happyShift action_60
action_67 _ = happyReduce_31

action_68 (32) = happyShift action_49
action_68 (33) = happyShift action_50
action_68 (34) = happyShift action_51
action_68 (35) = happyShift action_52
action_68 (48) = happyFail []
action_68 (49) = happyFail []
action_68 (51) = happyFail []
action_68 (52) = happyFail []
action_68 (60) = happyShift action_60
action_68 _ = happyReduce_29

action_69 (32) = happyShift action_49
action_69 (33) = happyShift action_50
action_69 (34) = happyShift action_51
action_69 (35) = happyShift action_52
action_69 (48) = happyFail []
action_69 (49) = happyFail []
action_69 (51) = happyFail []
action_69 (52) = happyFail []
action_69 (60) = happyShift action_60
action_69 _ = happyReduce_30

action_70 (32) = happyShift action_49
action_70 (33) = happyShift action_50
action_70 (34) = happyShift action_51
action_70 (35) = happyShift action_52
action_70 (48) = happyFail []
action_70 (49) = happyFail []
action_70 (51) = happyFail []
action_70 (52) = happyFail []
action_70 (60) = happyShift action_60
action_70 _ = happyReduce_28

action_71 (60) = happyShift action_60
action_71 _ = happyReduce_35

action_72 (60) = happyShift action_60
action_72 _ = happyReduce_34

action_73 (34) = happyShift action_51
action_73 (35) = happyShift action_52
action_73 (60) = happyShift action_60
action_73 _ = happyReduce_33

action_74 (34) = happyShift action_51
action_74 (35) = happyShift action_52
action_74 (60) = happyShift action_60
action_74 _ = happyReduce_32

action_75 (32) = happyShift action_49
action_75 (33) = happyShift action_50
action_75 (34) = happyShift action_51
action_75 (35) = happyShift action_52
action_75 (37) = happyShift action_106
action_75 (48) = happyShift action_53
action_75 (49) = happyShift action_54
action_75 (51) = happyShift action_55
action_75 (52) = happyShift action_56
action_75 (53) = happyShift action_57
action_75 (54) = happyShift action_58
action_75 (56) = happyShift action_59
action_75 (60) = happyShift action_60
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (21) = happyShift action_30
action_76 (23) = happyShift action_31
action_76 (24) = happyShift action_32
action_76 (39) = happyShift action_34
action_76 (48) = happyShift action_35
action_76 (61) = happyShift action_36
action_76 (10) = happyGoto action_105
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (14) = happyGoto action_104
action_77 _ = happyReduce_47

action_78 (20) = happyShift action_11
action_78 (21) = happyShift action_12
action_78 (22) = happyShift action_13
action_78 (25) = happyShift action_14
action_78 (27) = happyShift action_15
action_78 (28) = happyShift action_16
action_78 (33) = happyShift action_17
action_78 (36) = happyShift action_18
action_78 (39) = happyShift action_19
action_78 (44) = happyShift action_20
action_78 (46) = happyShift action_21
action_78 (47) = happyShift action_22
action_78 (48) = happyShift action_23
action_78 (55) = happyShift action_24
action_78 (57) = happyShift action_38
action_78 (59) = happyShift action_26
action_78 (62) = happyShift action_27
action_78 (13) = happyGoto action_103
action_78 (18) = happyGoto action_10
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_54

action_80 _ = happyReduce_49

action_81 (43) = happyShift action_102
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_39

action_83 (32) = happyShift action_49
action_83 (33) = happyShift action_50
action_83 (34) = happyShift action_51
action_83 (35) = happyShift action_52
action_83 (37) = happyShift action_101
action_83 (48) = happyShift action_53
action_83 (49) = happyShift action_54
action_83 (51) = happyShift action_55
action_83 (52) = happyShift action_56
action_83 (53) = happyShift action_57
action_83 (54) = happyShift action_58
action_83 (56) = happyShift action_59
action_83 (60) = happyShift action_60
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (20) = happyShift action_11
action_84 (21) = happyShift action_12
action_84 (22) = happyShift action_13
action_84 (25) = happyShift action_14
action_84 (27) = happyShift action_15
action_84 (28) = happyShift action_16
action_84 (33) = happyShift action_17
action_84 (36) = happyShift action_18
action_84 (39) = happyShift action_19
action_84 (44) = happyShift action_20
action_84 (46) = happyShift action_21
action_84 (47) = happyShift action_22
action_84 (48) = happyShift action_23
action_84 (55) = happyShift action_24
action_84 (57) = happyShift action_38
action_84 (59) = happyShift action_26
action_84 (62) = happyShift action_27
action_84 (13) = happyGoto action_100
action_84 (18) = happyGoto action_10
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (21) = happyShift action_97
action_85 (51) = happyShift action_99
action_85 (12) = happyGoto action_96
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (21) = happyShift action_97
action_86 (38) = happyShift action_98
action_86 (12) = happyGoto action_96
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (41) = happyShift action_95
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (36) = happyShift action_94
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (21) = happyShift action_30
action_89 (23) = happyShift action_31
action_89 (24) = happyShift action_32
action_89 (39) = happyShift action_34
action_89 (48) = happyShift action_35
action_89 (61) = happyShift action_36
action_89 (10) = happyGoto action_93
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (20) = happyShift action_11
action_90 (21) = happyShift action_12
action_90 (22) = happyShift action_13
action_90 (25) = happyShift action_14
action_90 (27) = happyShift action_15
action_90 (28) = happyShift action_16
action_90 (33) = happyShift action_17
action_90 (36) = happyShift action_18
action_90 (39) = happyShift action_19
action_90 (44) = happyShift action_20
action_90 (46) = happyShift action_21
action_90 (47) = happyShift action_22
action_90 (48) = happyShift action_23
action_90 (55) = happyShift action_24
action_90 (57) = happyShift action_38
action_90 (59) = happyShift action_26
action_90 (62) = happyShift action_27
action_90 (13) = happyGoto action_91
action_90 (18) = happyGoto action_10
action_90 (19) = happyGoto action_92
action_90 _ = happyReduce_62

action_91 (32) = happyShift action_49
action_91 (33) = happyShift action_50
action_91 (34) = happyShift action_51
action_91 (35) = happyShift action_52
action_91 (48) = happyShift action_53
action_91 (49) = happyShift action_54
action_91 (51) = happyShift action_55
action_91 (52) = happyShift action_56
action_91 (53) = happyShift action_57
action_91 (54) = happyShift action_58
action_91 (56) = happyShift action_59
action_91 (60) = happyShift action_60
action_91 _ = happyReduce_61

action_92 (37) = happyShift action_117
action_92 (42) = happyShift action_118
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (58) = happyShift action_89
action_93 _ = happyReduce_13

action_94 (21) = happyShift action_116
action_94 (9) = happyGoto action_115
action_94 _ = happyReduce_10

action_95 (21) = happyShift action_30
action_95 (23) = happyShift action_31
action_95 (24) = happyShift action_32
action_95 (39) = happyShift action_34
action_95 (48) = happyShift action_35
action_95 (61) = happyShift action_36
action_95 (10) = happyGoto action_114
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_18

action_97 (41) = happyShift action_113
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_14

action_99 _ = happyReduce_15

action_100 (32) = happyShift action_49
action_100 (33) = happyShift action_50
action_100 (34) = happyShift action_51
action_100 (35) = happyShift action_52
action_100 (41) = happyShift action_112
action_100 (48) = happyShift action_53
action_100 (49) = happyShift action_54
action_100 (51) = happyShift action_55
action_100 (52) = happyShift action_56
action_100 (53) = happyShift action_57
action_100 (54) = happyShift action_58
action_100 (56) = happyShift action_59
action_100 (60) = happyShift action_60
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (20) = happyShift action_11
action_101 (21) = happyShift action_12
action_101 (22) = happyShift action_13
action_101 (25) = happyShift action_14
action_101 (27) = happyShift action_15
action_101 (28) = happyShift action_16
action_101 (33) = happyShift action_17
action_101 (36) = happyShift action_18
action_101 (39) = happyShift action_19
action_101 (44) = happyShift action_20
action_101 (46) = happyShift action_21
action_101 (47) = happyShift action_22
action_101 (48) = happyShift action_23
action_101 (55) = happyShift action_24
action_101 (57) = happyShift action_38
action_101 (59) = happyShift action_26
action_101 (62) = happyShift action_27
action_101 (13) = happyGoto action_111
action_101 (18) = happyGoto action_10
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (20) = happyShift action_11
action_102 (21) = happyShift action_12
action_102 (22) = happyShift action_13
action_102 (25) = happyShift action_14
action_102 (27) = happyShift action_15
action_102 (28) = happyShift action_16
action_102 (33) = happyShift action_17
action_102 (36) = happyShift action_18
action_102 (39) = happyShift action_19
action_102 (44) = happyShift action_20
action_102 (46) = happyShift action_21
action_102 (47) = happyShift action_22
action_102 (48) = happyShift action_23
action_102 (55) = happyShift action_24
action_102 (57) = happyShift action_38
action_102 (59) = happyShift action_26
action_102 (62) = happyShift action_27
action_102 (13) = happyGoto action_110
action_102 (18) = happyGoto action_10
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (32) = happyShift action_49
action_103 (33) = happyShift action_50
action_103 (34) = happyShift action_51
action_103 (35) = happyShift action_52
action_103 (48) = happyShift action_53
action_103 (49) = happyShift action_54
action_103 (51) = happyShift action_55
action_103 (52) = happyShift action_56
action_103 (53) = happyShift action_57
action_103 (54) = happyShift action_58
action_103 (56) = happyShift action_59
action_103 (60) = happyShift action_60
action_103 _ = happyReduce_45

action_104 (31) = happyShift action_109
action_104 (15) = happyGoto action_108
action_104 _ = happyReduce_43

action_105 (43) = happyShift action_107
action_105 (58) = happyShift action_89
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_53

action_107 (20) = happyShift action_11
action_107 (21) = happyShift action_12
action_107 (22) = happyShift action_13
action_107 (25) = happyShift action_14
action_107 (27) = happyShift action_15
action_107 (28) = happyShift action_16
action_107 (33) = happyShift action_17
action_107 (36) = happyShift action_18
action_107 (39) = happyShift action_19
action_107 (44) = happyShift action_20
action_107 (46) = happyShift action_21
action_107 (47) = happyShift action_22
action_107 (48) = happyShift action_23
action_107 (55) = happyShift action_24
action_107 (57) = happyShift action_38
action_107 (59) = happyShift action_26
action_107 (62) = happyShift action_27
action_107 (13) = happyGoto action_129
action_107 (18) = happyGoto action_10
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_46

action_109 (48) = happyShift action_128
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (32) = happyShift action_49
action_110 (33) = happyShift action_50
action_110 (34) = happyShift action_51
action_110 (35) = happyShift action_52
action_110 (42) = happyShift action_127
action_110 (48) = happyShift action_53
action_110 (49) = happyShift action_54
action_110 (51) = happyShift action_55
action_110 (52) = happyShift action_56
action_110 (53) = happyShift action_57
action_110 (54) = happyShift action_58
action_110 (56) = happyShift action_59
action_110 (60) = happyShift action_60
action_110 _ = happyReduce_51

action_111 (32) = happyShift action_49
action_111 (33) = happyShift action_50
action_111 (34) = happyShift action_51
action_111 (35) = happyShift action_52
action_111 (40) = happyShift action_126
action_111 (48) = happyShift action_53
action_111 (49) = happyShift action_54
action_111 (51) = happyShift action_55
action_111 (52) = happyShift action_56
action_111 (53) = happyShift action_57
action_111 (54) = happyShift action_58
action_111 (56) = happyShift action_59
action_111 (60) = happyShift action_60
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (21) = happyShift action_30
action_112 (23) = happyShift action_31
action_112 (24) = happyShift action_32
action_112 (39) = happyShift action_34
action_112 (48) = happyShift action_35
action_112 (61) = happyShift action_36
action_112 (10) = happyGoto action_125
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (21) = happyShift action_30
action_113 (23) = happyShift action_31
action_113 (24) = happyShift action_32
action_113 (39) = happyShift action_34
action_113 (48) = happyShift action_35
action_113 (61) = happyShift action_36
action_113 (10) = happyGoto action_124
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (37) = happyShift action_123
action_114 (58) = happyShift action_89
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (37) = happyShift action_121
action_115 (42) = happyShift action_122
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (41) = happyShift action_120
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_38

action_118 (20) = happyShift action_11
action_118 (21) = happyShift action_12
action_118 (22) = happyShift action_13
action_118 (25) = happyShift action_14
action_118 (27) = happyShift action_15
action_118 (28) = happyShift action_16
action_118 (33) = happyShift action_17
action_118 (36) = happyShift action_18
action_118 (39) = happyShift action_19
action_118 (44) = happyShift action_20
action_118 (46) = happyShift action_21
action_118 (47) = happyShift action_22
action_118 (48) = happyShift action_23
action_118 (55) = happyShift action_24
action_118 (57) = happyShift action_38
action_118 (59) = happyShift action_26
action_118 (62) = happyShift action_27
action_118 (13) = happyGoto action_119
action_118 (18) = happyGoto action_10
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (32) = happyShift action_49
action_119 (33) = happyShift action_50
action_119 (34) = happyShift action_51
action_119 (35) = happyShift action_52
action_119 (48) = happyShift action_53
action_119 (49) = happyShift action_54
action_119 (51) = happyShift action_55
action_119 (52) = happyShift action_56
action_119 (53) = happyShift action_57
action_119 (54) = happyShift action_58
action_119 (56) = happyShift action_59
action_119 (60) = happyShift action_60
action_119 _ = happyReduce_60

action_120 (21) = happyShift action_30
action_120 (23) = happyShift action_31
action_120 (24) = happyShift action_32
action_120 (39) = happyShift action_34
action_120 (48) = happyShift action_35
action_120 (61) = happyShift action_36
action_120 (10) = happyGoto action_138
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (39) = happyShift action_137
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (21) = happyShift action_136
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (39) = happyShift action_135
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (42) = happyShift action_134
action_124 (58) = happyShift action_89
action_124 _ = happyReduce_20

action_125 (51) = happyShift action_133
action_125 (58) = happyShift action_89
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (45) = happyShift action_132
action_126 _ = happyFail (happyExpListPerState 126)

action_127 _ = happyReduce_52

action_128 (21) = happyShift action_131
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (32) = happyShift action_49
action_129 (33) = happyShift action_50
action_129 (34) = happyShift action_51
action_129 (35) = happyShift action_52
action_129 (40) = happyShift action_130
action_129 (48) = happyShift action_53
action_129 (49) = happyShift action_54
action_129 (51) = happyShift action_55
action_129 (52) = happyShift action_56
action_129 (53) = happyShift action_57
action_129 (54) = happyShift action_58
action_129 (56) = happyShift action_59
action_129 (60) = happyShift action_60
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (20) = happyShift action_11
action_130 (21) = happyShift action_12
action_130 (22) = happyShift action_13
action_130 (25) = happyShift action_14
action_130 (27) = happyShift action_15
action_130 (28) = happyShift action_16
action_130 (33) = happyShift action_17
action_130 (36) = happyShift action_18
action_130 (39) = happyShift action_19
action_130 (44) = happyShift action_20
action_130 (46) = happyShift action_21
action_130 (47) = happyShift action_22
action_130 (48) = happyShift action_23
action_130 (55) = happyShift action_24
action_130 (57) = happyShift action_38
action_130 (59) = happyShift action_26
action_130 (62) = happyShift action_27
action_130 (13) = happyGoto action_144
action_130 (18) = happyGoto action_10
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (43) = happyShift action_143
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (20) = happyShift action_11
action_132 (21) = happyShift action_12
action_132 (22) = happyShift action_13
action_132 (25) = happyShift action_14
action_132 (27) = happyShift action_15
action_132 (28) = happyShift action_16
action_132 (33) = happyShift action_17
action_132 (36) = happyShift action_18
action_132 (39) = happyShift action_19
action_132 (44) = happyShift action_20
action_132 (46) = happyShift action_21
action_132 (47) = happyShift action_22
action_132 (48) = happyShift action_23
action_132 (55) = happyShift action_24
action_132 (57) = happyShift action_38
action_132 (59) = happyShift action_26
action_132 (62) = happyShift action_27
action_132 (13) = happyGoto action_142
action_132 (18) = happyGoto action_10
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_41

action_134 _ = happyReduce_21

action_135 (20) = happyShift action_11
action_135 (21) = happyShift action_12
action_135 (22) = happyShift action_13
action_135 (25) = happyShift action_14
action_135 (27) = happyShift action_15
action_135 (28) = happyShift action_16
action_135 (33) = happyShift action_17
action_135 (36) = happyShift action_18
action_135 (39) = happyShift action_19
action_135 (44) = happyShift action_20
action_135 (46) = happyShift action_21
action_135 (47) = happyShift action_22
action_135 (48) = happyShift action_23
action_135 (55) = happyShift action_24
action_135 (57) = happyShift action_38
action_135 (59) = happyShift action_26
action_135 (62) = happyShift action_27
action_135 (13) = happyGoto action_141
action_135 (18) = happyGoto action_10
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (41) = happyShift action_140
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (20) = happyShift action_11
action_137 (21) = happyShift action_12
action_137 (22) = happyShift action_13
action_137 (25) = happyShift action_14
action_137 (27) = happyShift action_15
action_137 (28) = happyShift action_16
action_137 (33) = happyShift action_17
action_137 (36) = happyShift action_18
action_137 (39) = happyShift action_19
action_137 (44) = happyShift action_20
action_137 (46) = happyShift action_21
action_137 (47) = happyShift action_22
action_137 (48) = happyShift action_23
action_137 (55) = happyShift action_24
action_137 (57) = happyShift action_38
action_137 (59) = happyShift action_26
action_137 (62) = happyShift action_27
action_137 (13) = happyGoto action_139
action_137 (18) = happyGoto action_10
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (58) = happyShift action_89
action_138 _ = happyReduce_9

action_139 (32) = happyShift action_49
action_139 (33) = happyShift action_50
action_139 (34) = happyShift action_51
action_139 (35) = happyShift action_52
action_139 (38) = happyShift action_148
action_139 (48) = happyShift action_53
action_139 (49) = happyShift action_54
action_139 (51) = happyShift action_55
action_139 (52) = happyShift action_56
action_139 (53) = happyShift action_57
action_139 (54) = happyShift action_58
action_139 (56) = happyShift action_59
action_139 (60) = happyShift action_60
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (21) = happyShift action_30
action_140 (23) = happyShift action_31
action_140 (24) = happyShift action_32
action_140 (39) = happyShift action_34
action_140 (48) = happyShift action_35
action_140 (61) = happyShift action_36
action_140 (10) = happyGoto action_147
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (32) = happyShift action_49
action_141 (33) = happyShift action_50
action_141 (34) = happyShift action_51
action_141 (35) = happyShift action_52
action_141 (38) = happyShift action_146
action_141 (48) = happyShift action_53
action_141 (49) = happyShift action_54
action_141 (51) = happyShift action_55
action_141 (52) = happyShift action_56
action_141 (53) = happyShift action_57
action_141 (54) = happyShift action_58
action_141 (56) = happyShift action_59
action_141 (60) = happyShift action_60
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (32) = happyShift action_49
action_142 (33) = happyShift action_50
action_142 (34) = happyShift action_51
action_142 (35) = happyShift action_52
action_142 (48) = happyShift action_53
action_142 (49) = happyShift action_54
action_142 (51) = happyShift action_55
action_142 (52) = happyShift action_56
action_142 (53) = happyShift action_57
action_142 (54) = happyShift action_58
action_142 (56) = happyShift action_59
action_142 (60) = happyShift action_60
action_142 _ = happyReduce_24

action_143 (21) = happyShift action_145
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (32) = happyShift action_49
action_144 (33) = happyShift action_50
action_144 (34) = happyShift action_51
action_144 (35) = happyShift action_52
action_144 (48) = happyShift action_53
action_144 (49) = happyShift action_54
action_144 (51) = happyShift action_55
action_144 (52) = happyShift action_56
action_144 (53) = happyShift action_57
action_144 (54) = happyShift action_58
action_144 (56) = happyShift action_59
action_144 (60) = happyShift action_60
action_144 _ = happyReduce_23

action_145 (51) = happyShift action_149
action_145 _ = happyFail (happyExpListPerState 145)

action_146 _ = happyReduce_22

action_147 (58) = happyShift action_89
action_147 _ = happyReduce_8

action_148 _ = happyReduce_4

action_149 (50) = happyShift action_150
action_149 _ = happyFail (happyExpListPerState 149)

action_150 (20) = happyShift action_11
action_150 (21) = happyShift action_12
action_150 (22) = happyShift action_13
action_150 (25) = happyShift action_14
action_150 (27) = happyShift action_15
action_150 (28) = happyShift action_16
action_150 (33) = happyShift action_17
action_150 (36) = happyShift action_18
action_150 (39) = happyShift action_19
action_150 (44) = happyShift action_20
action_150 (46) = happyShift action_21
action_150 (47) = happyShift action_22
action_150 (48) = happyShift action_23
action_150 (55) = happyShift action_24
action_150 (57) = happyShift action_38
action_150 (59) = happyShift action_26
action_150 (62) = happyShift action_27
action_150 (13) = happyGoto action_151
action_150 (18) = happyGoto action_10
action_150 _ = happyFail (happyExpListPerState 150)

action_151 (32) = happyShift action_49
action_151 (33) = happyShift action_50
action_151 (34) = happyShift action_51
action_151 (35) = happyShift action_52
action_151 (48) = happyShift action_53
action_151 (49) = happyShift action_54
action_151 (51) = happyShift action_55
action_151 (52) = happyShift action_56
action_151 (53) = happyShift action_57
action_151 (54) = happyShift action_58
action_151 (56) = happyShift action_59
action_151 (60) = happyShift action_60
action_151 _ = happyReduce_48

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (Program happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happyReduce 9 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_3)) `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_2, happy_var_3, Function happy_var_5 happy_var_8)
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  7 happyReduction_6
happyReduction_6  =  HappyAbsSyn7
		 ([]
	)

happyReduce_7 = happyReduce 4 8 happyReduction_7
happyReduction_7 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 5 9 happyReduction_8
happyReduction_8 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (happy_var_1 ++ [(happy_var_3, happy_var_5)]
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn9
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  9 happyReduction_10
happyReduction_10  =  HappyAbsSyn9
		 ([]
	)

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn10
		 (TInt
	)

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn10
		 (TBool
	)

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (TFun happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  10 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (TRcd happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (TVarnt happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn10
		 (TypDecl happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn10
		 (TString
	)

happyReduce_18 = happySpecReduce_2  11 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_0  11 happyReduction_19
happyReduction_19  =  HappyAbsSyn11
		 ([]
	)

happyReduce_20 = happySpecReduce_3  12 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn12
		 ((happy_var_1, happy_var_3)
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 12 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 9 13 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Fun (happy_var_3, happy_var_5) happy_var_8
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 8 13 happyReduction_23
happyReduction_23 ((HappyAbsSyn13  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Decl happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 8 13 happyReduction_24
happyReduction_24 ((HappyAbsSyn13  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (If happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  13 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin Or happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  13 happyReduction_26
happyReduction_26 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin And happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  13 happyReduction_27
happyReduction_27 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin EQ happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  13 happyReduction_28
happyReduction_28 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin LT happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  13 happyReduction_29
happyReduction_29 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin GT happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  13 happyReduction_30
happyReduction_30 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin LE happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  13 happyReduction_31
happyReduction_31 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin GE happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  13 happyReduction_32
happyReduction_32 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin Add happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  13 happyReduction_33
happyReduction_33 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin Sub happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  13 happyReduction_34
happyReduction_34 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin Mult happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  13 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin Div happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  13 happyReduction_36
happyReduction_36 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Unary Neg happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  13 happyReduction_37
happyReduction_37 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Unary Not happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 5 13 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Call happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_3  13 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Rcd happy_var_2
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  13 happyReduction_40
happyReduction_40 (HappyTerminal (TokenSym happy_var_3))
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (RcdProj happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happyReduce 7 13 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Varnt happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_1  13 happyReduction_42
happyReduction_42 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happyReduce 4 13 happyReduction_43
happyReduction_43 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (CaseV happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_2  13 happyReduction_44
happyReduction_44 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Raise happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happyReduce 4 13 happyReduction_45
happyReduction_45 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Try happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_2  14 happyReduction_46
happyReduction_46 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_0  14 happyReduction_47
happyReduction_47  =  HappyAbsSyn14
		 ([]
	)

happyReduce_48 = happyReduce 8 15 happyReduction_48
happyReduction_48 ((HappyAbsSyn13  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ((happy_var_3, happy_var_5, happy_var_8)
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_2  16 happyReduction_49
happyReduction_49 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_0  16 happyReduction_50
happyReduction_50  =  HappyAbsSyn16
		 ([]
	)

happyReduce_51 = happySpecReduce_3  17 happyReduction_51
happyReduction_51 (HappyAbsSyn13  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn17
		 ((happy_var_1, happy_var_3)
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happyReduce 4 17 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_53 = happyReduce 4 18 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (CallFC happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_3  18 happyReduction_54
happyReduction_54 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  18 happyReduction_55
happyReduction_55 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn18
		 (Lit (IntV happy_var_1)
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  18 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn18
		 (Lit (BoolV True)
	)

happyReduce_57 = happySpecReduce_1  18 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn18
		 (Lit (BoolV False)
	)

happyReduce_58 = happySpecReduce_1  18 happyReduction_58
happyReduction_58 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn18
		 (Var happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  18 happyReduction_59
happyReduction_59 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn18
		 (Lit (StringV happy_var_1)
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  19 happyReduction_60
happyReduction_60 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  19 happyReduction_61
happyReduction_61 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_0  19 happyReduction_62
happyReduction_62  =  HappyAbsSyn19
		 ([]
	)

happyNewToken action sts stk [] =
	action 63 63 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenVar -> cont 20;
	TokenSym happy_dollar_dollar -> cont 21;
	TokenInt happy_dollar_dollar -> cont 22;
	TokenTInt -> cont 23;
	TokenTBool -> cont 24;
	TokenCase -> cont 25;
	TokenOf -> cont 26;
	TokenRaise -> cont 27;
	TokenTry -> cont 28;
	TokenWith -> cont 29;
	TokenType -> cont 30;
	TokenBar -> cont 31;
	TokenPlus -> cont 32;
	TokenMinus -> cont 33;
	TokenTimes -> cont 34;
	TokenDiv -> cont 35;
	TokenLParen -> cont 36;
	TokenRParen -> cont 37;
	TokenRB -> cont 38;
	TokenLB -> cont 39;
	TokenSemiColon -> cont 40;
	TokenColon -> cont 41;
	TokenComma -> cont 42;
	TokenEq -> cont 43;
	TokenIf -> cont 44;
	TokenElse -> cont 45;
	TokenTrue -> cont 46;
	TokenFalse -> cont 47;
	TokenLT -> cont 48;
	TokenLE -> cont 49;
	TokenCaseArrow -> cont 50;
	TokenGT -> cont 51;
	TokenGE -> cont 52;
	TokenComp -> cont 53;
	TokenAnd -> cont 54;
	TokenNot -> cont 55;
	TokenOr -> cont 56;
	TokenFunc -> cont 57;
	TokenArrow -> cont 58;
	TokenTopLevelFun -> cont 59;
	TokenDot -> cont 60;
	TokenTString -> cont 61;
	TokenString happy_dollar_dollar -> cont 62;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 63 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => E a -> (a -> E b) -> E b
happyThen = (thenE)
happyReturn :: () => a -> E a
happyReturn = (returnE)
happyThen1 m k tks = (thenE) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> E a
happyReturn1 = \a tks -> (returnE) a
happyError' :: () => ([(Token)], [String]) -> E a
happyError' = (\(tokens, _) -> parseError tokens)
parser tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
      (Ok a) -> k a
      (Failed e) -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      (Ok a) -> Ok a
      (Failed e) -> k e


parseError :: [Token] -> E a
parseError _ = failE $ "Parse error: Unexpected syntax used in the program"

parseExpr = parser . scanTokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































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
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






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
     = case happyDrop (k - ((1) :: Int)) sts of
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





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

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
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
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
