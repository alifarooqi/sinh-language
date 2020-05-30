{-# OPTIONS_GHC -w #-}
module Parser (parseExpr) where
import Data.Char (isDigit, isSpace, isAlpha)
import Prelude hiding (LT, GT, EQ)
import Declare
import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

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
happyExpList = Happy_Data_Array.listArray (0,449) ([0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,54144,33936,21518,0,0,0,0,0,4,0,0,0,0,32,0,0,0,0,0,0,240,6064,1,0,32,0,0,8,0,0,0,0,0,0,0,0,0,0,19968,4675,20538,1,27072,16968,10759,0,3384,59465,1344,0,8615,7433,168,57344,9268,929,21,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,32768,0,0,0,14336,18701,16616,5,6656,2304,16,0,64,0,0,0,0,4,0,0,1,0,32,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,2048,0,0,17230,14866,336,32768,0,1,0,0,6016,48512,8,0,0,0,0,16384,30,8950,0,0,0,0,0,30752,55296,139,0,0,2,0,19968,4675,20538,1,27072,16968,10759,0,3384,59465,1344,0,8615,7433,168,57344,9268,929,21,39936,9350,41076,2,54144,33936,21518,0,6768,53394,2689,0,17230,14866,336,49152,18537,1858,42,14336,18701,16616,5,42752,2337,43037,0,64,0,0,0,104,16416,0,0,0,0,32,0,0,0,0,0,480,3936,0,0,60,236,0,32768,32775,13,0,61440,0,0,0,7680,0,0,0,960,0,0,0,120,0,0,0,0,0,0,0,0,0,0,12288,0,0,0,1536,0,0,0,752,6064,1,832,256,2,0,0,0,0,32768,37075,3716,84,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,61440,45058,279,57344,9268,929,21,2048,0,512,0,256,512,0,0,0,512,0,0,0,2,0,32768,6,1026,0,14336,18701,16616,5,0,240,6064,1,0,2112,0,0,0,0,256,0,1,0,0,40960,32769,256,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,7680,62980,34,39936,9350,41076,2,54144,33936,21518,0,0,0,0,0,0,0,0,0,0,32,16,0,0,0,0,42752,2337,43037,0,0,0,0,0,0,16384,0,0,30720,55328,139,0,3840,31489,17,13312,4096,32,0,1664,512,4,0,0,16,512,0,0,66,0,0,0,4,0,0,0,0,0,54144,33936,21518,0,0,15,4475,0,52,8208,0,0,0,2,0,4096,0,0,0,0,2048,0,0,0,2048,2048,0,0,0,258,0,0,256,0,0,0,0,0,1024,0,0,0,0,1084,17900,0,3384,59465,1344,0,0,128,0,57344,9268,929,21,0,0,0,0,0,0,0,0,6768,53394,2689,0,0,64,0,49152,18537,1858,42,0,0,0,2,0,1264,6064,1,832,256,2,0,49152,49171,1118,0,30720,55296,11,8192,0,0,0,0,480,12128,0,0,0,32,0,0,0,0,0,0,0,64,0,0,0,0,0,0,256,0,54144,33936,21518,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Program","Functions","Function","Types","Type","ids","typ","tRcds","tRcd1","Exp","Cases","Cases1","Rcds","Rcd1","App","Exps","var","id","int","Int","Bool","case","of","raise","try","with","type","'|'","'+'","'-'","'*'","'/'","'('","')'","'}'","'{'","';'","':'","','","'='","if","else","true","false","'<'","'<='","'=>'","'>'","'>='","'=='","'&&'","'!'","'||'","fun","'->'","'@'","'.'","%eof"]
        bit_start = st * 61
        bit_end = (st + 1) * 61
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..60]
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

action_3 (61) = happyAccept
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
action_4 (6) = happyGoto action_8
action_4 (13) = happyGoto action_9
action_4 (18) = happyGoto action_10
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_5

action_6 (21) = happyShift action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (43) = happyShift action_59
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_2

action_9 (32) = happyShift action_47
action_9 (33) = happyShift action_48
action_9 (34) = happyShift action_49
action_9 (35) = happyShift action_50
action_9 (48) = happyShift action_51
action_9 (49) = happyShift action_52
action_9 (51) = happyShift action_53
action_9 (52) = happyShift action_54
action_9 (53) = happyShift action_55
action_9 (54) = happyShift action_56
action_9 (56) = happyShift action_57
action_9 (60) = happyShift action_58
action_9 _ = happyReduce_1

action_10 (36) = happyShift action_46
action_10 _ = happyReduce_41

action_11 (21) = happyShift action_45
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_57

action_13 _ = happyReduce_54

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
action_14 (57) = happyShift action_36
action_14 (59) = happyShift action_26
action_14 (13) = happyGoto action_44
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
action_15 (57) = happyShift action_36
action_15 (59) = happyShift action_26
action_15 (13) = happyGoto action_43
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
action_16 (57) = happyShift action_36
action_16 (59) = happyShift action_26
action_16 (13) = happyGoto action_42
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
action_17 (57) = happyShift action_36
action_17 (59) = happyShift action_26
action_17 (13) = happyGoto action_41
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
action_18 (57) = happyShift action_36
action_18 (59) = happyShift action_26
action_18 (13) = happyGoto action_40
action_18 (18) = happyGoto action_10
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (16) = happyGoto action_39
action_19 _ = happyReduce_49

action_20 (36) = happyShift action_38
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_55

action_22 _ = happyReduce_56

action_23 (21) = happyShift action_37
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
action_24 (57) = happyShift action_36
action_24 (59) = happyShift action_26
action_24 (13) = happyGoto action_35
action_24 (18) = happyGoto action_10
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (21) = happyShift action_29
action_25 (23) = happyShift action_30
action_25 (24) = happyShift action_31
action_25 (36) = happyShift action_32
action_25 (39) = happyShift action_33
action_25 (48) = happyShift action_34
action_25 (10) = happyGoto action_28
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (21) = happyShift action_27
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (36) = happyShift action_88
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (21) = happyShift action_86
action_28 (58) = happyShift action_87
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_16

action_30 _ = happyReduce_11

action_31 _ = happyReduce_12

action_32 (21) = happyShift action_85
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (11) = happyGoto action_84
action_33 _ = happyReduce_18

action_34 (11) = happyGoto action_83
action_34 _ = happyReduce_18

action_35 (60) = happyShift action_58
action_35 _ = happyReduce_36

action_36 (36) = happyShift action_32
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (43) = happyShift action_82
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (20) = happyShift action_11
action_38 (21) = happyShift action_12
action_38 (22) = happyShift action_13
action_38 (25) = happyShift action_14
action_38 (27) = happyShift action_15
action_38 (28) = happyShift action_16
action_38 (33) = happyShift action_17
action_38 (36) = happyShift action_18
action_38 (39) = happyShift action_19
action_38 (44) = happyShift action_20
action_38 (46) = happyShift action_21
action_38 (47) = happyShift action_22
action_38 (48) = happyShift action_23
action_38 (55) = happyShift action_24
action_38 (57) = happyShift action_36
action_38 (59) = happyShift action_26
action_38 (13) = happyGoto action_81
action_38 (18) = happyGoto action_10
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (21) = happyShift action_79
action_39 (38) = happyShift action_80
action_39 (17) = happyGoto action_78
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (32) = happyShift action_47
action_40 (33) = happyShift action_48
action_40 (34) = happyShift action_49
action_40 (35) = happyShift action_50
action_40 (37) = happyShift action_77
action_40 (48) = happyShift action_51
action_40 (49) = happyShift action_52
action_40 (51) = happyShift action_53
action_40 (52) = happyShift action_54
action_40 (53) = happyShift action_55
action_40 (54) = happyShift action_56
action_40 (56) = happyShift action_57
action_40 (60) = happyShift action_58
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (60) = happyShift action_58
action_41 _ = happyReduce_35

action_42 (29) = happyShift action_76
action_42 (32) = happyShift action_47
action_42 (33) = happyShift action_48
action_42 (34) = happyShift action_49
action_42 (35) = happyShift action_50
action_42 (48) = happyShift action_51
action_42 (49) = happyShift action_52
action_42 (51) = happyShift action_53
action_42 (52) = happyShift action_54
action_42 (53) = happyShift action_55
action_42 (54) = happyShift action_56
action_42 (56) = happyShift action_57
action_42 (60) = happyShift action_58
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (32) = happyShift action_47
action_43 (33) = happyShift action_48
action_43 (34) = happyShift action_49
action_43 (35) = happyShift action_50
action_43 (48) = happyShift action_51
action_43 (49) = happyShift action_52
action_43 (51) = happyShift action_53
action_43 (52) = happyShift action_54
action_43 (53) = happyShift action_55
action_43 (54) = happyShift action_56
action_43 (56) = happyShift action_57
action_43 (60) = happyShift action_58
action_43 _ = happyReduce_43

action_44 (26) = happyShift action_75
action_44 (32) = happyShift action_47
action_44 (33) = happyShift action_48
action_44 (34) = happyShift action_49
action_44 (35) = happyShift action_50
action_44 (48) = happyShift action_51
action_44 (49) = happyShift action_52
action_44 (51) = happyShift action_53
action_44 (52) = happyShift action_54
action_44 (53) = happyShift action_55
action_44 (54) = happyShift action_56
action_44 (56) = happyShift action_57
action_44 (60) = happyShift action_58
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (41) = happyShift action_74
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (20) = happyShift action_11
action_46 (21) = happyShift action_12
action_46 (22) = happyShift action_13
action_46 (25) = happyShift action_14
action_46 (27) = happyShift action_15
action_46 (28) = happyShift action_16
action_46 (33) = happyShift action_17
action_46 (36) = happyShift action_18
action_46 (39) = happyShift action_19
action_46 (44) = happyShift action_20
action_46 (46) = happyShift action_21
action_46 (47) = happyShift action_22
action_46 (48) = happyShift action_23
action_46 (55) = happyShift action_24
action_46 (57) = happyShift action_36
action_46 (59) = happyShift action_26
action_46 (13) = happyGoto action_73
action_46 (18) = happyGoto action_10
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (20) = happyShift action_11
action_47 (21) = happyShift action_12
action_47 (22) = happyShift action_13
action_47 (25) = happyShift action_14
action_47 (27) = happyShift action_15
action_47 (28) = happyShift action_16
action_47 (33) = happyShift action_17
action_47 (36) = happyShift action_18
action_47 (39) = happyShift action_19
action_47 (44) = happyShift action_20
action_47 (46) = happyShift action_21
action_47 (47) = happyShift action_22
action_47 (48) = happyShift action_23
action_47 (55) = happyShift action_24
action_47 (57) = happyShift action_36
action_47 (59) = happyShift action_26
action_47 (13) = happyGoto action_72
action_47 (18) = happyGoto action_10
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
action_48 (57) = happyShift action_36
action_48 (59) = happyShift action_26
action_48 (13) = happyGoto action_71
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
action_49 (57) = happyShift action_36
action_49 (59) = happyShift action_26
action_49 (13) = happyGoto action_70
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
action_50 (57) = happyShift action_36
action_50 (59) = happyShift action_26
action_50 (13) = happyGoto action_69
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
action_51 (57) = happyShift action_36
action_51 (59) = happyShift action_26
action_51 (13) = happyGoto action_68
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
action_52 (57) = happyShift action_36
action_52 (59) = happyShift action_26
action_52 (13) = happyGoto action_67
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
action_53 (57) = happyShift action_36
action_53 (59) = happyShift action_26
action_53 (13) = happyGoto action_66
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
action_54 (57) = happyShift action_36
action_54 (59) = happyShift action_26
action_54 (13) = happyGoto action_65
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
action_55 (57) = happyShift action_36
action_55 (59) = happyShift action_26
action_55 (13) = happyGoto action_64
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
action_56 (57) = happyShift action_36
action_56 (59) = happyShift action_26
action_56 (13) = happyGoto action_63
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
action_57 (57) = happyShift action_36
action_57 (59) = happyShift action_26
action_57 (13) = happyGoto action_62
action_57 (18) = happyGoto action_10
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (21) = happyShift action_61
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (21) = happyShift action_29
action_59 (23) = happyShift action_30
action_59 (24) = happyShift action_31
action_59 (39) = happyShift action_33
action_59 (48) = happyShift action_34
action_59 (10) = happyGoto action_60
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (58) = happyShift action_87
action_60 _ = happyReduce_7

action_61 _ = happyReduce_39

action_62 (32) = happyShift action_47
action_62 (33) = happyShift action_48
action_62 (34) = happyShift action_49
action_62 (35) = happyShift action_50
action_62 (48) = happyShift action_51
action_62 (49) = happyShift action_52
action_62 (51) = happyShift action_53
action_62 (52) = happyShift action_54
action_62 (53) = happyShift action_55
action_62 (54) = happyShift action_56
action_62 (60) = happyShift action_58
action_62 _ = happyReduce_24

action_63 (32) = happyShift action_47
action_63 (33) = happyShift action_48
action_63 (34) = happyShift action_49
action_63 (35) = happyShift action_50
action_63 (48) = happyShift action_51
action_63 (49) = happyShift action_52
action_63 (51) = happyShift action_53
action_63 (52) = happyShift action_54
action_63 (53) = happyShift action_55
action_63 (60) = happyShift action_58
action_63 _ = happyReduce_25

action_64 (32) = happyShift action_47
action_64 (33) = happyShift action_48
action_64 (34) = happyShift action_49
action_64 (35) = happyShift action_50
action_64 (48) = happyShift action_51
action_64 (49) = happyShift action_52
action_64 (51) = happyShift action_53
action_64 (52) = happyShift action_54
action_64 (53) = happyFail []
action_64 (60) = happyShift action_58
action_64 _ = happyReduce_26

action_65 (32) = happyShift action_47
action_65 (33) = happyShift action_48
action_65 (34) = happyShift action_49
action_65 (35) = happyShift action_50
action_65 (48) = happyFail []
action_65 (49) = happyFail []
action_65 (51) = happyFail []
action_65 (52) = happyFail []
action_65 (60) = happyShift action_58
action_65 _ = happyReduce_30

action_66 (32) = happyShift action_47
action_66 (33) = happyShift action_48
action_66 (34) = happyShift action_49
action_66 (35) = happyShift action_50
action_66 (48) = happyFail []
action_66 (49) = happyFail []
action_66 (51) = happyFail []
action_66 (52) = happyFail []
action_66 (60) = happyShift action_58
action_66 _ = happyReduce_28

action_67 (32) = happyShift action_47
action_67 (33) = happyShift action_48
action_67 (34) = happyShift action_49
action_67 (35) = happyShift action_50
action_67 (48) = happyFail []
action_67 (49) = happyFail []
action_67 (51) = happyFail []
action_67 (52) = happyFail []
action_67 (60) = happyShift action_58
action_67 _ = happyReduce_29

action_68 (32) = happyShift action_47
action_68 (33) = happyShift action_48
action_68 (34) = happyShift action_49
action_68 (35) = happyShift action_50
action_68 (48) = happyFail []
action_68 (49) = happyFail []
action_68 (51) = happyFail []
action_68 (52) = happyFail []
action_68 (60) = happyShift action_58
action_68 _ = happyReduce_27

action_69 (60) = happyShift action_58
action_69 _ = happyReduce_34

action_70 (60) = happyShift action_58
action_70 _ = happyReduce_33

action_71 (34) = happyShift action_49
action_71 (35) = happyShift action_50
action_71 (60) = happyShift action_58
action_71 _ = happyReduce_32

action_72 (34) = happyShift action_49
action_72 (35) = happyShift action_50
action_72 (60) = happyShift action_58
action_72 _ = happyReduce_31

action_73 (32) = happyShift action_47
action_73 (33) = happyShift action_48
action_73 (34) = happyShift action_49
action_73 (35) = happyShift action_50
action_73 (37) = happyShift action_104
action_73 (48) = happyShift action_51
action_73 (49) = happyShift action_52
action_73 (51) = happyShift action_53
action_73 (52) = happyShift action_54
action_73 (53) = happyShift action_55
action_73 (54) = happyShift action_56
action_73 (56) = happyShift action_57
action_73 (60) = happyShift action_58
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (21) = happyShift action_29
action_74 (23) = happyShift action_30
action_74 (24) = happyShift action_31
action_74 (39) = happyShift action_33
action_74 (48) = happyShift action_34
action_74 (10) = happyGoto action_103
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (14) = happyGoto action_102
action_75 _ = happyReduce_46

action_76 (20) = happyShift action_11
action_76 (21) = happyShift action_12
action_76 (22) = happyShift action_13
action_76 (25) = happyShift action_14
action_76 (27) = happyShift action_15
action_76 (28) = happyShift action_16
action_76 (33) = happyShift action_17
action_76 (36) = happyShift action_18
action_76 (39) = happyShift action_19
action_76 (44) = happyShift action_20
action_76 (46) = happyShift action_21
action_76 (47) = happyShift action_22
action_76 (48) = happyShift action_23
action_76 (55) = happyShift action_24
action_76 (57) = happyShift action_36
action_76 (59) = happyShift action_26
action_76 (13) = happyGoto action_101
action_76 (18) = happyGoto action_10
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_53

action_78 _ = happyReduce_48

action_79 (43) = happyShift action_100
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_38

action_81 (32) = happyShift action_47
action_81 (33) = happyShift action_48
action_81 (34) = happyShift action_49
action_81 (35) = happyShift action_50
action_81 (37) = happyShift action_99
action_81 (48) = happyShift action_51
action_81 (49) = happyShift action_52
action_81 (51) = happyShift action_53
action_81 (52) = happyShift action_54
action_81 (53) = happyShift action_55
action_81 (54) = happyShift action_56
action_81 (56) = happyShift action_57
action_81 (60) = happyShift action_58
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (20) = happyShift action_11
action_82 (21) = happyShift action_12
action_82 (22) = happyShift action_13
action_82 (25) = happyShift action_14
action_82 (27) = happyShift action_15
action_82 (28) = happyShift action_16
action_82 (33) = happyShift action_17
action_82 (36) = happyShift action_18
action_82 (39) = happyShift action_19
action_82 (44) = happyShift action_20
action_82 (46) = happyShift action_21
action_82 (47) = happyShift action_22
action_82 (48) = happyShift action_23
action_82 (55) = happyShift action_24
action_82 (57) = happyShift action_36
action_82 (59) = happyShift action_26
action_82 (13) = happyGoto action_98
action_82 (18) = happyGoto action_10
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (21) = happyShift action_95
action_83 (51) = happyShift action_97
action_83 (12) = happyGoto action_94
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (21) = happyShift action_95
action_84 (38) = happyShift action_96
action_84 (12) = happyGoto action_94
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (41) = happyShift action_93
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (36) = happyShift action_92
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (21) = happyShift action_29
action_87 (23) = happyShift action_30
action_87 (24) = happyShift action_31
action_87 (39) = happyShift action_33
action_87 (48) = happyShift action_34
action_87 (10) = happyGoto action_91
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (20) = happyShift action_11
action_88 (21) = happyShift action_12
action_88 (22) = happyShift action_13
action_88 (25) = happyShift action_14
action_88 (27) = happyShift action_15
action_88 (28) = happyShift action_16
action_88 (33) = happyShift action_17
action_88 (36) = happyShift action_18
action_88 (39) = happyShift action_19
action_88 (44) = happyShift action_20
action_88 (46) = happyShift action_21
action_88 (47) = happyShift action_22
action_88 (48) = happyShift action_23
action_88 (55) = happyShift action_24
action_88 (57) = happyShift action_36
action_88 (59) = happyShift action_26
action_88 (13) = happyGoto action_89
action_88 (18) = happyGoto action_10
action_88 (19) = happyGoto action_90
action_88 _ = happyReduce_60

action_89 (32) = happyShift action_47
action_89 (33) = happyShift action_48
action_89 (34) = happyShift action_49
action_89 (35) = happyShift action_50
action_89 (48) = happyShift action_51
action_89 (49) = happyShift action_52
action_89 (51) = happyShift action_53
action_89 (52) = happyShift action_54
action_89 (53) = happyShift action_55
action_89 (54) = happyShift action_56
action_89 (56) = happyShift action_57
action_89 (60) = happyShift action_58
action_89 _ = happyReduce_59

action_90 (37) = happyShift action_115
action_90 (42) = happyShift action_116
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (58) = happyShift action_87
action_91 _ = happyReduce_13

action_92 (21) = happyShift action_114
action_92 (9) = happyGoto action_113
action_92 _ = happyReduce_10

action_93 (21) = happyShift action_29
action_93 (23) = happyShift action_30
action_93 (24) = happyShift action_31
action_93 (39) = happyShift action_33
action_93 (48) = happyShift action_34
action_93 (10) = happyGoto action_112
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_17

action_95 (41) = happyShift action_111
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_14

action_97 _ = happyReduce_15

action_98 (32) = happyShift action_47
action_98 (33) = happyShift action_48
action_98 (34) = happyShift action_49
action_98 (35) = happyShift action_50
action_98 (41) = happyShift action_110
action_98 (48) = happyShift action_51
action_98 (49) = happyShift action_52
action_98 (51) = happyShift action_53
action_98 (52) = happyShift action_54
action_98 (53) = happyShift action_55
action_98 (54) = happyShift action_56
action_98 (56) = happyShift action_57
action_98 (60) = happyShift action_58
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (20) = happyShift action_11
action_99 (21) = happyShift action_12
action_99 (22) = happyShift action_13
action_99 (25) = happyShift action_14
action_99 (27) = happyShift action_15
action_99 (28) = happyShift action_16
action_99 (33) = happyShift action_17
action_99 (36) = happyShift action_18
action_99 (39) = happyShift action_19
action_99 (44) = happyShift action_20
action_99 (46) = happyShift action_21
action_99 (47) = happyShift action_22
action_99 (48) = happyShift action_23
action_99 (55) = happyShift action_24
action_99 (57) = happyShift action_36
action_99 (59) = happyShift action_26
action_99 (13) = happyGoto action_109
action_99 (18) = happyGoto action_10
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (20) = happyShift action_11
action_100 (21) = happyShift action_12
action_100 (22) = happyShift action_13
action_100 (25) = happyShift action_14
action_100 (27) = happyShift action_15
action_100 (28) = happyShift action_16
action_100 (33) = happyShift action_17
action_100 (36) = happyShift action_18
action_100 (39) = happyShift action_19
action_100 (44) = happyShift action_20
action_100 (46) = happyShift action_21
action_100 (47) = happyShift action_22
action_100 (48) = happyShift action_23
action_100 (55) = happyShift action_24
action_100 (57) = happyShift action_36
action_100 (59) = happyShift action_26
action_100 (13) = happyGoto action_108
action_100 (18) = happyGoto action_10
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (32) = happyShift action_47
action_101 (33) = happyShift action_48
action_101 (34) = happyShift action_49
action_101 (35) = happyShift action_50
action_101 (48) = happyShift action_51
action_101 (49) = happyShift action_52
action_101 (51) = happyShift action_53
action_101 (52) = happyShift action_54
action_101 (53) = happyShift action_55
action_101 (54) = happyShift action_56
action_101 (56) = happyShift action_57
action_101 (60) = happyShift action_58
action_101 _ = happyReduce_44

action_102 (31) = happyShift action_107
action_102 (15) = happyGoto action_106
action_102 _ = happyReduce_42

action_103 (43) = happyShift action_105
action_103 (58) = happyShift action_87
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_52

action_105 (20) = happyShift action_11
action_105 (21) = happyShift action_12
action_105 (22) = happyShift action_13
action_105 (25) = happyShift action_14
action_105 (27) = happyShift action_15
action_105 (28) = happyShift action_16
action_105 (33) = happyShift action_17
action_105 (36) = happyShift action_18
action_105 (39) = happyShift action_19
action_105 (44) = happyShift action_20
action_105 (46) = happyShift action_21
action_105 (47) = happyShift action_22
action_105 (48) = happyShift action_23
action_105 (55) = happyShift action_24
action_105 (57) = happyShift action_36
action_105 (59) = happyShift action_26
action_105 (13) = happyGoto action_127
action_105 (18) = happyGoto action_10
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_45

action_107 (48) = happyShift action_126
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (32) = happyShift action_47
action_108 (33) = happyShift action_48
action_108 (34) = happyShift action_49
action_108 (35) = happyShift action_50
action_108 (42) = happyShift action_125
action_108 (48) = happyShift action_51
action_108 (49) = happyShift action_52
action_108 (51) = happyShift action_53
action_108 (52) = happyShift action_54
action_108 (53) = happyShift action_55
action_108 (54) = happyShift action_56
action_108 (56) = happyShift action_57
action_108 (60) = happyShift action_58
action_108 _ = happyReduce_50

action_109 (32) = happyShift action_47
action_109 (33) = happyShift action_48
action_109 (34) = happyShift action_49
action_109 (35) = happyShift action_50
action_109 (40) = happyShift action_124
action_109 (48) = happyShift action_51
action_109 (49) = happyShift action_52
action_109 (51) = happyShift action_53
action_109 (52) = happyShift action_54
action_109 (53) = happyShift action_55
action_109 (54) = happyShift action_56
action_109 (56) = happyShift action_57
action_109 (60) = happyShift action_58
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (21) = happyShift action_29
action_110 (23) = happyShift action_30
action_110 (24) = happyShift action_31
action_110 (39) = happyShift action_33
action_110 (48) = happyShift action_34
action_110 (10) = happyGoto action_123
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (21) = happyShift action_29
action_111 (23) = happyShift action_30
action_111 (24) = happyShift action_31
action_111 (39) = happyShift action_33
action_111 (48) = happyShift action_34
action_111 (10) = happyGoto action_122
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (37) = happyShift action_121
action_112 (58) = happyShift action_87
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (37) = happyShift action_119
action_113 (42) = happyShift action_120
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (41) = happyShift action_118
action_114 _ = happyFail (happyExpListPerState 114)

action_115 _ = happyReduce_37

action_116 (20) = happyShift action_11
action_116 (21) = happyShift action_12
action_116 (22) = happyShift action_13
action_116 (25) = happyShift action_14
action_116 (27) = happyShift action_15
action_116 (28) = happyShift action_16
action_116 (33) = happyShift action_17
action_116 (36) = happyShift action_18
action_116 (39) = happyShift action_19
action_116 (44) = happyShift action_20
action_116 (46) = happyShift action_21
action_116 (47) = happyShift action_22
action_116 (48) = happyShift action_23
action_116 (55) = happyShift action_24
action_116 (57) = happyShift action_36
action_116 (59) = happyShift action_26
action_116 (13) = happyGoto action_117
action_116 (18) = happyGoto action_10
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (32) = happyShift action_47
action_117 (33) = happyShift action_48
action_117 (34) = happyShift action_49
action_117 (35) = happyShift action_50
action_117 (48) = happyShift action_51
action_117 (49) = happyShift action_52
action_117 (51) = happyShift action_53
action_117 (52) = happyShift action_54
action_117 (53) = happyShift action_55
action_117 (54) = happyShift action_56
action_117 (56) = happyShift action_57
action_117 (60) = happyShift action_58
action_117 _ = happyReduce_58

action_118 (21) = happyShift action_29
action_118 (23) = happyShift action_30
action_118 (24) = happyShift action_31
action_118 (39) = happyShift action_33
action_118 (48) = happyShift action_34
action_118 (10) = happyGoto action_136
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (39) = happyShift action_135
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (21) = happyShift action_134
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (39) = happyShift action_133
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (42) = happyShift action_132
action_122 (58) = happyShift action_87
action_122 _ = happyReduce_19

action_123 (51) = happyShift action_131
action_123 (58) = happyShift action_87
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (45) = happyShift action_130
action_124 _ = happyFail (happyExpListPerState 124)

action_125 _ = happyReduce_51

action_126 (21) = happyShift action_129
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (32) = happyShift action_47
action_127 (33) = happyShift action_48
action_127 (34) = happyShift action_49
action_127 (35) = happyShift action_50
action_127 (40) = happyShift action_128
action_127 (48) = happyShift action_51
action_127 (49) = happyShift action_52
action_127 (51) = happyShift action_53
action_127 (52) = happyShift action_54
action_127 (53) = happyShift action_55
action_127 (54) = happyShift action_56
action_127 (56) = happyShift action_57
action_127 (60) = happyShift action_58
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (20) = happyShift action_11
action_128 (21) = happyShift action_12
action_128 (22) = happyShift action_13
action_128 (25) = happyShift action_14
action_128 (27) = happyShift action_15
action_128 (28) = happyShift action_16
action_128 (33) = happyShift action_17
action_128 (36) = happyShift action_18
action_128 (39) = happyShift action_19
action_128 (44) = happyShift action_20
action_128 (46) = happyShift action_21
action_128 (47) = happyShift action_22
action_128 (48) = happyShift action_23
action_128 (55) = happyShift action_24
action_128 (57) = happyShift action_36
action_128 (59) = happyShift action_26
action_128 (13) = happyGoto action_142
action_128 (18) = happyGoto action_10
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (43) = happyShift action_141
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
action_130 (57) = happyShift action_36
action_130 (59) = happyShift action_26
action_130 (13) = happyGoto action_140
action_130 (18) = happyGoto action_10
action_130 _ = happyFail (happyExpListPerState 130)

action_131 _ = happyReduce_40

action_132 _ = happyReduce_20

action_133 (20) = happyShift action_11
action_133 (21) = happyShift action_12
action_133 (22) = happyShift action_13
action_133 (25) = happyShift action_14
action_133 (27) = happyShift action_15
action_133 (28) = happyShift action_16
action_133 (33) = happyShift action_17
action_133 (36) = happyShift action_18
action_133 (39) = happyShift action_19
action_133 (44) = happyShift action_20
action_133 (46) = happyShift action_21
action_133 (47) = happyShift action_22
action_133 (48) = happyShift action_23
action_133 (55) = happyShift action_24
action_133 (57) = happyShift action_36
action_133 (59) = happyShift action_26
action_133 (13) = happyGoto action_139
action_133 (18) = happyGoto action_10
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (41) = happyShift action_138
action_134 _ = happyFail (happyExpListPerState 134)

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
action_135 (57) = happyShift action_36
action_135 (59) = happyShift action_26
action_135 (13) = happyGoto action_137
action_135 (18) = happyGoto action_10
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (58) = happyShift action_87
action_136 _ = happyReduce_9

action_137 (32) = happyShift action_47
action_137 (33) = happyShift action_48
action_137 (34) = happyShift action_49
action_137 (35) = happyShift action_50
action_137 (38) = happyShift action_146
action_137 (48) = happyShift action_51
action_137 (49) = happyShift action_52
action_137 (51) = happyShift action_53
action_137 (52) = happyShift action_54
action_137 (53) = happyShift action_55
action_137 (54) = happyShift action_56
action_137 (56) = happyShift action_57
action_137 (60) = happyShift action_58
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (21) = happyShift action_29
action_138 (23) = happyShift action_30
action_138 (24) = happyShift action_31
action_138 (39) = happyShift action_33
action_138 (48) = happyShift action_34
action_138 (10) = happyGoto action_145
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (32) = happyShift action_47
action_139 (33) = happyShift action_48
action_139 (34) = happyShift action_49
action_139 (35) = happyShift action_50
action_139 (38) = happyShift action_144
action_139 (48) = happyShift action_51
action_139 (49) = happyShift action_52
action_139 (51) = happyShift action_53
action_139 (52) = happyShift action_54
action_139 (53) = happyShift action_55
action_139 (54) = happyShift action_56
action_139 (56) = happyShift action_57
action_139 (60) = happyShift action_58
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (32) = happyShift action_47
action_140 (33) = happyShift action_48
action_140 (34) = happyShift action_49
action_140 (35) = happyShift action_50
action_140 (48) = happyShift action_51
action_140 (49) = happyShift action_52
action_140 (51) = happyShift action_53
action_140 (52) = happyShift action_54
action_140 (53) = happyShift action_55
action_140 (54) = happyShift action_56
action_140 (56) = happyShift action_57
action_140 (60) = happyShift action_58
action_140 _ = happyReduce_23

action_141 (21) = happyShift action_143
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (32) = happyShift action_47
action_142 (33) = happyShift action_48
action_142 (34) = happyShift action_49
action_142 (35) = happyShift action_50
action_142 (48) = happyShift action_51
action_142 (49) = happyShift action_52
action_142 (51) = happyShift action_53
action_142 (52) = happyShift action_54
action_142 (53) = happyShift action_55
action_142 (54) = happyShift action_56
action_142 (56) = happyShift action_57
action_142 (60) = happyShift action_58
action_142 _ = happyReduce_22

action_143 (51) = happyShift action_147
action_143 _ = happyFail (happyExpListPerState 143)

action_144 _ = happyReduce_21

action_145 (58) = happyShift action_87
action_145 _ = happyReduce_8

action_146 _ = happyReduce_4

action_147 (50) = happyShift action_148
action_147 _ = happyFail (happyExpListPerState 147)

action_148 (20) = happyShift action_11
action_148 (21) = happyShift action_12
action_148 (22) = happyShift action_13
action_148 (25) = happyShift action_14
action_148 (27) = happyShift action_15
action_148 (28) = happyShift action_16
action_148 (33) = happyShift action_17
action_148 (36) = happyShift action_18
action_148 (39) = happyShift action_19
action_148 (44) = happyShift action_20
action_148 (46) = happyShift action_21
action_148 (47) = happyShift action_22
action_148 (48) = happyShift action_23
action_148 (55) = happyShift action_24
action_148 (57) = happyShift action_36
action_148 (59) = happyShift action_26
action_148 (13) = happyGoto action_149
action_148 (18) = happyGoto action_10
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (32) = happyShift action_47
action_149 (33) = happyShift action_48
action_149 (34) = happyShift action_49
action_149 (35) = happyShift action_50
action_149 (48) = happyShift action_51
action_149 (49) = happyShift action_52
action_149 (51) = happyShift action_53
action_149 (52) = happyShift action_54
action_149 (53) = happyShift action_55
action_149 (54) = happyShift action_56
action_149 (56) = happyShift action_57
action_149 (60) = happyShift action_58
action_149 _ = happyReduce_47

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

happyReduce_17 = happySpecReduce_2  11 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  11 happyReduction_18
happyReduction_18  =  HappyAbsSyn11
		 ([]
	)

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn12
		 ((happy_var_1, happy_var_3)
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 12 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 9 13 happyReduction_21
happyReduction_21 (_ `HappyStk`
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

happyReduce_22 = happyReduce 8 13 happyReduction_22
happyReduction_22 ((HappyAbsSyn13  happy_var_8) `HappyStk`
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

happyReduce_23 = happyReduce 8 13 happyReduction_23
happyReduction_23 ((HappyAbsSyn13  happy_var_8) `HappyStk`
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

happyReduce_24 = happySpecReduce_3  13 happyReduction_24
happyReduction_24 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin Or happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  13 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin And happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  13 happyReduction_26
happyReduction_26 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin EQ happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  13 happyReduction_27
happyReduction_27 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin LT happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  13 happyReduction_28
happyReduction_28 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin GT happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  13 happyReduction_29
happyReduction_29 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin LE happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  13 happyReduction_30
happyReduction_30 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin GE happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  13 happyReduction_31
happyReduction_31 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin Add happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  13 happyReduction_32
happyReduction_32 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin Sub happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  13 happyReduction_33
happyReduction_33 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin Mult happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  13 happyReduction_34
happyReduction_34 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Bin Div happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  13 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Unary Neg happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  13 happyReduction_36
happyReduction_36 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Unary Not happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 5 13 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Call happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_3  13 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Rcd happy_var_2
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  13 happyReduction_39
happyReduction_39 (HappyTerminal (TokenSym happy_var_3))
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (RcdProj happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 7 13 happyReduction_40
happyReduction_40 (_ `HappyStk`
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

happyReduce_41 = happySpecReduce_1  13 happyReduction_41
happyReduction_41 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happyReduce 4 13 happyReduction_42
happyReduction_42 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (CaseV happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_2  13 happyReduction_43
happyReduction_43 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Raise happy_var_2
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happyReduce 4 13 happyReduction_44
happyReduction_44 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Try happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_45 = happySpecReduce_2  14 happyReduction_45
happyReduction_45 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_0  14 happyReduction_46
happyReduction_46  =  HappyAbsSyn14
		 ([]
	)

happyReduce_47 = happyReduce 8 15 happyReduction_47
happyReduction_47 ((HappyAbsSyn13  happy_var_8) `HappyStk`
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

happyReduce_48 = happySpecReduce_2  16 happyReduction_48
happyReduction_48 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_0  16 happyReduction_49
happyReduction_49  =  HappyAbsSyn16
		 ([]
	)

happyReduce_50 = happySpecReduce_3  17 happyReduction_50
happyReduction_50 (HappyAbsSyn13  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn17
		 ((happy_var_1, happy_var_3)
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happyReduce 4 17 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_52 = happyReduce 4 18 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (CallFC happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_3  18 happyReduction_53
happyReduction_53 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  18 happyReduction_54
happyReduction_54 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn18
		 (Lit (IntV happy_var_1)
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  18 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn18
		 (Lit (BoolV True)
	)

happyReduce_56 = happySpecReduce_1  18 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn18
		 (Lit (BoolV False)
	)

happyReduce_57 = happySpecReduce_1  18 happyReduction_57
happyReduction_57 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn18
		 (Var happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  19 happyReduction_58
happyReduction_58 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  19 happyReduction_59
happyReduction_59 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_0  19 happyReduction_60
happyReduction_60  =  HappyAbsSyn19
		 ([]
	)

happyNewToken action sts stk [] =
	action 61 61 notHappyAtAll (HappyState action) sts stk []

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
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 61 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr = parser . scanTokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

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

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
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

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

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

{-# LINE 333 "templates/GenericTemplate.hs" #-}
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
