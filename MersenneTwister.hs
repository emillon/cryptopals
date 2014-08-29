-- | Implementation of the MT19937 algorithm
-- for pseudo-random number generation (aka "Mersenne Twister").

{-# LANGUAGE FlexibleContexts #-}

module MersenneTwister ( createMT
                       , createMTfromState
                       , nextMT
                       , mtTests
                       , recoverSeed
                       , checkMTProps
                       , untemper
                       , mt19937cryptCTR
                       , mt19937decryptCTR
                       ) where

import Control.Monad.State
import Data.Array
import Data.Bits
import Data.List
import Data.Word
import Test.HUnit
import Test.QuickCheck hiding ((.&.))

import qualified Data.ByteString as B

import Misc
import XOR

type Seed = Word32

data MT = MT { mtArray :: Array Int Word32
             , mtIndex :: Int
             }

-- | Create a new Mersenne Twister generator with a given seed.
createMT :: Seed -> MT
createMT seed =
    MT { mtArray = array (0, 623)
                 $ map (\ (i, n) -> (fromIntegral i, n))
                 $ take 624
                 $ iterate initMT (0, seed)
       , mtIndex = 0
       }

initMT :: (Word32, Word32) -> (Word32, Word32)
initMT (i, n) =
    (i+1, 0x6c078965 * (n `xor` (n `shiftR` 30)) + i + 1)

-- | Initialize a MT with a state array. Index is set to 1.
createMTfromState :: [Word32] -> MT
createMTfromState st =
    MT { mtArray = listArray (0, 623) st
       , mtIndex = 1
       }

-- | Generate a new Word32.
nextMT :: MonadState MT m => m Word32
nextMT = do
    i <- gets mtIndex
    when (i == 0) generateNumbers
    a <- gets mtArray
    modify incrIndex
    return $ temper $ a ! i

-- | Mersenne Twister's tempering function.
temper :: Word32 -> Word32
temper =
    temper_f4 . temper_f3 . temper_f2 . temper_f1

temper_f1, temper_f2, temper_f3, temper_f4 :: Word32 -> Word32
temper_f1 y = y `xor` (y `shiftR` 11)
temper_f2 y = y `xor` ((y `shiftL` 7) .&. temperingMask2)
temper_f3 y = y `xor` ((y `shiftL` 15) .&. temperingMask3)
temper_f4 y = y `xor` (y `shiftR` 18)

temperingMask2, temperingMask3 :: Word32
temperingMask2 = 0x9d2c5680
temperingMask3 = 0xefc60000

-- | Inverse of 'temper'.
untemper :: Word32 -> Word32
untemper =
    untemper_f1 . untemper_f2 . untemper_f3 . untemper_f4

untemper_f1, untemper_f2, untemper_f3, untemper_f4 :: Word32 -> Word32
untemper_f1 y = y `xor` ((y `xor` (y `shiftR` 11)) `shiftR` 11)
untemper_f2 y = y `xor` (i .&. temperingMask2)
    where
        i = f $ f $ f $ f $ y `shiftL` 7
        f x = (y `xor` (x .&. temperingMask2)) `shiftL` 7
untemper_f3 y = y `xor` ((y `shiftL` 15) .&. temperingMask3)
untemper_f4 y = y `xor` (y `shiftR` 18)

prop_temper_f1_inv :: Word32 -> Bool
prop_temper_f1_inv w = untemper_f1 (temper_f1 w) == w

prop_temper_f2_inv :: Word32 -> Bool
prop_temper_f2_inv w = untemper_f2 (temper_f2 w) == w

prop_temper_f3_inv :: Word32 -> Bool
prop_temper_f3_inv w = untemper_f3 (temper_f3 w) == w

prop_temper_f4_inv :: Word32 -> Bool
prop_temper_f4_inv w = untemper_f4 (temper_f4 w) == w

prop_temper_inv :: Word32 -> Bool
prop_temper_inv w = untemper (temper w) == w

incrIndex :: MT -> MT
incrIndex m = m { mtIndex = (mtIndex m + 1) `mod` 624 }

generateNumbers :: MonadState MT m => m ()
generateNumbers =
    forM_ [0..623] $ \ i -> do
        a <- gets mtArray
        let y = ((a ! i) .&. 0x80000000)
              + ((a ! ((i+1) `mod` 624)) .&. 0x7fffffff)
            c = if odd y then 0x9908b0df else 0
            newVal = (a ! ((i + 397) `mod` 624)) `xor` (y `shiftR` 1) `xor` c
        setMT i newVal

setMT :: MonadState MT m => Int -> Word32 -> m ()
setMT i x =
    modify $ \ m -> m { mtArray = mtArray m // [(i, x)] }

-- | HUnit tests for Mersenne Twister.
mtTests :: Test
mtTests = "Mersenne Twister" ~: TestList
    [ "Seed" ~: specSeed ~=? getSeed 1
    , "First 200" ~: spec200 ~=? first200
    ]
        where
            specSeed =
                [ 1, 1812433254, 3713160357, 3109174145, 64984499
                , 3392658084, 446538473, 2629760756, 2453345558, 1394803949
                , 1021787430, 2063496713, 1304877364, 1713639158, 889001601
                , 1651239412, 1450863289, 745575081, 361057727, 2288771950
                , 1463387568, 2249488362, 26637982, 204036717, 1655702041
                , 1329048465, 2092351466, 1681619666, 3220660315, 1301783610
                , 626286181, 294669048, 3537128440, 3259518248, 2550101273
                , 1160881866, 308703547, 295714668, 35508674, 1599247281
                , 376272024, 3166459937, 1852735737, 3680868867, 612352556
                , 2760189833, 3816750341, 699140493, 1087846865, 394927937
                , 2063539671, 645417889, 2337669049, 3773167612, 678121169
                , 3006984620, 1163491294, 2559287860, 543155592, 3194181347
                , 2463543297, 3875146860, 475483913, 3707568076, 3881808875
                , 1264657097, 208126250, 1802809301, 367907560, 2433375693
                , 2851326449, 2380707878, 2911758972, 4243386879, 2229228726
                , 828161871, 2871116151, 990638198, 178193628, 1012573979
                , 1223581943, 3333023583, 1901888414, 3913876750, 3168662389
                , 656194888, 1553610174, 466840498, 686407570, 280737523
                , 2476489017, 1272981410, 3189431979, 3294710282, 1564477163
                , 4133221553, 823708826, 880616227, 1730254897, 335723347
                , 2123911971, 344194767, 119099153, 2915257116, 3339825470
                , 2524942970, 1191117250, 3403812186, 3988972937, 2575395295
                , 4072737183, 663832315, 808080503, 724042340, 2966189542
                , 2499643239, 3309205581, 1915303227, 72616536, 387525935
                , 2791701251, 2190905566, 3740328774, 831297460, 3750964864
                , 2190112044, 899144100, 2346558003, 3851695829, 2896963823
                , 1548614403, 3676707405, 2050891594, 4165893148, 1883017153
                , 2668787527, 50330561, 2063572142, 1853585557, 1716111087
                , 2937248370, 1650859709, 2682305722, 565243175, 3922227187
                , 3482032705, 2809081500, 2099376873, 230358556, 1065827745
                , 196966939, 3268845630, 3625508265, 1477799595, 4149453740
                , 2757835686, 3032697936, 2200108791, 3421680711, 4145382259
                , 3605253072, 1186485728, 3520482151, 3080733463, 3887314157
                , 4030447755, 1699987022, 1393253586, 1710066407, 710337383
                , 3754612557, 2741088369, 337455371, 1304761604, 3592681639
                , 3099385187, 4003676405, 317081535, 997754381, 480565460
                , 3806265432, 1068029852, 776179010, 470617537, 3653875421
                , 2273571919, 1055365147, 1317172834, 3414733003, 2835400613
                , 28845217, 631741764, 2334552212, 3565466095, 1225096926
                , 1277781438, 2416008223, 1268768054, 2750789241, 267768398
                , 2175383438, 268654341, 2550530755, 2971623408, 1666669894
                , 1934871760, 509782083, 2798468670, 2834016892, 2494149255
                , 1965005899, 2653045765, 2317194903, 1297426078, 916214929
                , 2967861004, 2236807006, 2476725285, 128488253, 4277714156
                , 3016192551, 1690883702, 1329810641, 593010415, 2341313579
                , 1754238478, 1242698701, 2152594527, 2103269013, 926178633
                , 647225267, 4243787142, 1489208161, 3188798921, 1327553793
                , 3644600811, 684513652, 2606555057, 2705329549, 2557469018
                , 1294205096, 70104222, 3020083528, 2015571237, 2768573480
                , 401698695, 2812362809, 328919870, 984940142, 1653817439
                , 471643152, 538942283, 2040555667, 1211982999, 1663497772
                , 2941793728, 3001026698, 313271977, 3644502703, 2423950047
                , 2629046069, 3450826936, 44600781, 2633869288, 4267014746
                , 4204914470, 1955987363, 2590608885, 2120168063, 1460034243
                , 258056600, 3693550087, 779446436, 902696389, 4228701387
                , 3165791227, 3478614865, 1500865135, 905884796, 3682046467
                , 2437847832, 2595888219, 4144484663, 1299603103, 648536946
                , 1762836247, 4265749196, 950840266, 2928992722, 2051369009
                , 2071186450, 1164619682, 210405235, 1296628868, 2425474719
                , 4083386904, 1978331343, 3190898799, 602128683, 2003319330
                , 1043377147, 756690484, 24776626, 1835824233, 1156421176
                , 2125448878, 1333136189, 607751135, 4255614767, 4238533009
                , 2583175632, 230472465, 3037259757, 1546348932, 2537279411
                , 110471952, 520621708, 63613561, 2843673595, 775036
                , 1899744556, 1168115970, 2685086321, 3410250658, 3151102153
                , 634647644, 3639125394, 3344624764, 1525171811, 1878800371
                , 3356530116, 3676542926, 602053165, 2686708238, 3703555082
                , 3754961372, 3970030923, 1749014201, 3391107050, 2478152000
                , 2121779806, 2636689360, 769835312, 4230539591, 1909812524
                , 417081626, 3096519324, 387659697, 3764499249, 3452925463
                , 3818277698, 3008920324, 15253694, 1479260759, 2421328720
                , 2220743357, 38831551, 1032912064, 3400956198, 2362808832
                , 3988706866, 1950464958, 3248573125, 1225815945, 1211036180
                , 346407094, 3867176764, 1257086026, 2725236231, 2843735658
                , 4147241082, 1729974832, 1256499145, 3765975901, 784776076
                , 4288277427, 3903532520, 3431522864, 2792589977, 2935989154
                , 3536596892, 3512984120, 605476293, 1774961976, 981422589
                , 822525778, 3343539932, 422954622, 1323482938, 2523465420
                , 2746609356, 1664448205, 272567300, 711582493, 3625722107
                , 3615865699, 950619756, 2864168489, 108006277, 3976313352
                , 680217319, 173747636, 291134870, 198587329, 595310009
                , 941470866, 2438488368, 1681923153, 1654783272, 3531789254
                , 4149541715, 2922706987, 684907209, 3116688362, 3288142886
                , 3953377592, 3332428007, 1400401813, 3745921798, 1701705628
                , 3744511893, 1838265811, 3314032512, 3894840150, 3810031409
                , 181324387, 983160249, 1444959400, 3836664153, 3032673327
                , 310789231, 3701565562, 1407580781, 2511575629, 3113822685
                , 1777261998, 2208898751, 106383174, 2961020500, 995776421
                , 3306087121, 2181030035, 2300064751, 1909543740, 4023156173
                , 1671619075, 2151956104, 237668401, 3204511253, 1303668692
                , 3868259787, 2737897899, 4091026033, 2877780671, 134376279
                , 398912026, 863520778, 3712468923, 3443213666, 2183809552
                , 2597379302, 349776833, 274697715, 4266593710, 4282186769
                , 3530757867, 520237914, 3369037397, 2285670338, 387086485
                , 618942879, 219892882, 2008897906, 2293749560, 2907436476
                , 3853296593, 327550390, 1558751403, 2125694704, 1822570484
                , 2409968265, 436622776, 2691124090, 1080819771, 2958107334
                , 2667158841, 2117901613, 440045635, 3861104471, 3574962701
                , 3210299248, 1368601573, 2434039520, 86704919, 3628108033
                , 1909858745, 227461000, 2530509465, 838433817, 730224848
                , 1060658180, 1318482825, 233266846, 2352800845, 2086493219
                , 3826355555, 3174377690, 1455208243, 1356597942, 663563056
                , 2501819374, 4213535259, 1585241464, 873997246, 2597898744
                , 427064229, 1587746589, 259660817, 1688808891, 4165834345
                , 1359025114, 2013923952, 2963511711, 2903220732, 356112706
                , 501549847, 1609412897, 1685128111, 2639303606, 700554261
                , 914150235, 2010650618, 2029243163, 3046509911, 715702687
                , 2206956754, 3045298216, 2922667179, 2497577415, 3001819604
                , 706666890, 2275923855, 3094184383, 2781697712, 3292952666
                , 4238614078, 278500659, 1440033346, 1552714131, 336554687
                , 2842580609, 2255044310, 2180071372, 99970159, 2078552309
                , 1172694639, 1359399314, 546452524, 349053834, 3072254369
                , 3043246719, 3314426498, 1594992663, 3582269665, 2114045278
                , 585873328, 840739494, 3475778485, 1506518790, 4008486652
                , 229989333, 3582278212, 363921215, 3592842520, 1833533669
                , 708173875, 564248927, 853943228, 2282731374, 2874158047
                , 3978663285, 2332696531, 1354524859, 58121641, 1445193461
                , 1936635021, 3374328198, 3465253060, 385589199, 1819596280
                , 912895627, 1877426726, 733280947, 2004202992, 3311780711
                , 3732053191, 309903272, 97290141, 2945419335, 3916477072
                , 1326195031, 3740938055, 3604745262, 3633308956, 3392929431
                , 1257547457, 251825182, 3318700085, 847033774, 137350663
                , 1716455973, 546850455, 4227574519, 3044214953, 2259874013
                , 2442748258, 2956971336, 2198772379, 1269686727, 2648116105
                , 1339159363, 1473334647, 2386671612, 2069268389
                ]
            spec200 =
                [  1791095845, 4282876139, 3093770124, 4005303368,     491263
                ,   550290313, 1298508491, 4290846341,  630311759, 1013994432
                ,   396591248, 1703301249,  799981516, 1666063943, 1484172013
                ,  2876537340, 1704103302, 4018109721, 2314200242, 3634877716
                ,  1800426750, 1345499493, 2942995346, 2252917204,  878115723
                ,  1904615676, 3771485674,  986026652,  117628829, 2295290254
                ,  2879636018, 3925436996, 1792310487, 1963679703, 2399554537
                ,  1849836273,  602957303, 4033523166,  850839392, 3343156310
                ,  3439171725, 3075069929, 4158651785, 3447817223, 1346146623
                ,   398576445, 2973502998, 2225448249, 3764062721, 3715233664
                ,  3842306364, 3561158865,  365262088, 3563119320,  167739021
                ,  1172740723,  729416111,  254447594, 3771593337, 2879896008
                ,   422396446, 2547196999, 1808643459, 2884732358, 4114104213
                ,  1768615473, 2289927481,  848474627, 2971589572, 1243949848
                ,  1355129329,  610401323, 2948499020, 3364310042, 3584689972
                ,  1771840848,   78547565,  146764659, 3221845289, 2680188370
                ,  4247126031, 2837408832, 3213347012, 1282027545, 1204497775
                ,  1916133090, 3389928919,  954017671,  443352346,  315096729
                ,  1923688040, 2015364118, 3902387977,  413056707, 1261063143
                ,  3879945342, 1235985687,  513207677,  558468452, 2253996187
                ,    83180453,  359158073, 2915576403, 3937889446,  908935816
                ,  3910346016, 1140514210, 1283895050, 2111290647, 2509932175
                ,   229190383, 2430573655, 2465816345, 2636844999,  630194419
                ,  4108289372, 2531048010, 1120896190, 3005439278,  992203680
                ,   439523032, 2291143831, 1778356919, 4079953217, 2982425969
                ,  2117674829, 1778886403, 2321861504,  214548472, 3287733501
                ,  2301657549,  194758406, 2850976308,  601149909, 2211431878
                ,  3403347458, 4057003596,  127995867, 2519234709, 3792995019
                ,  3880081671, 2322667597,  590449352, 1924060235,  598187340
                ,  3831694379, 3467719188, 1621712414, 1708008996, 2312516455
                ,   710190855, 2801602349, 3983619012, 1551604281, 1493642992
                ,  2452463100, 3224713426, 2739486816, 3118137613,  542518282
                ,  3793770775, 2964406140, 2678651729, 2782062471, 3225273209
                ,  1520156824, 1498506954, 3278061020, 1159331476, 1531292064
                ,  3847801996, 3233201345, 1838637662, 3785334332, 4143956457
                ,    50118808, 2849459538, 2139362163, 2670162785,  316934274
                ,   492830188, 3379930844, 4078025319,  275167074, 1932357898
                ,  1526046390, 2484164448, 4045158889, 1752934226, 1631242710
                ,  1018023110, 3276716738, 3879985479, 3313975271, 2463934640
                ,  1294333494,   12327951, 3318889349, 2650617233,  656828586
                ]


first200 :: [Word32]
first200 =
    evalState m $ createMT 1
        where
            m = mapM (\ _ -> nextMT) [1::Int ..200]

getSeed :: Word32 -> [Word32]
getSeed =
    elems . mtArray . createMT

-- | Find the seed, given the first output of the random number generator.
recoverSeed :: Word32       -- ^ Lower bound of search space.
            -> Word32       -- ^ Higher bound of search space.
            -> Word32       -- ^ Target value.
            -> Maybe Word32
recoverSeed low hi target =
    find ok [low..hi]
        where
            ok n =
                target == evalState nextMT (createMT n)

-- | QuickCheck tests for this module.
checkMTProps :: IO ()
checkMTProps = do
    quickCheck prop_temper_f1_inv
    quickCheck prop_temper_f2_inv
    quickCheck prop_temper_f3_inv
    quickCheck prop_temper_f4_inv
    quickCheck prop_temper_inv

assembleW64 :: Word32 -> Word32 -> Word64
assembleW64 lo hi =
    fromIntegral lo .|. (fromIntegral hi `shiftL` 32)

-- | The MT19937 CTR "cipher" crypt function.
mt19937cryptCTR :: Word16       -- ^ Key
                -> B.ByteString -- ^ Plaintext
                -> B.ByteString
mt19937cryptCTR key plain =
    B.concat $ zipWith xorBuffer plainBlocks keyBlocks
        where
            nblocks = 1 + (B.length plain `div` 16)
            plainBlocks = map (\ i -> nthChunk 16 i plain) [0..nblocks-1]
            keyBlocks = evalState keyBlocksM $ createMT $ fromIntegral key
            keyBlocksM = forM [0..nblocks-1] $ \ _ -> do
                a <- nextMT
                b <- nextMT
                c <- nextMT
                d <- nextMT
                let ab = w64LEtoBS $ assembleW64 a b
                    cd = w64LEtoBS $ assembleW64 c d
                return $ B.append ab cd

-- | The MT19937 CTR "cipher" decrypt function.
-- Due to how CTR works, it is the same as the 'mt19937cryptCTR'.
mt19937decryptCTR :: Word16       -- ^ Key
                  -> B.ByteString -- ^ Ciphertext
                  -> B.ByteString
mt19937decryptCTR = mt19937cryptCTR
