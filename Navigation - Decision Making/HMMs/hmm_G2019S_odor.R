library(HMM)

states <- c("Heading odor","Heading bottom","Opposite to odor","Heading top")

startProb = c(0.30190919635642, 0.21665228218504856, 0.2251730260734763, 0.25626549538505516)

HoProb <- c(0.8950655498907502, 0.04515659140568099, 0.0056081573197378, 0.05416970138383103)
HbProb <- c(0.06254598969830757, 0.8746796579635128, 0.05539062698231458, 0.007383725355865114)
OoProb <- c(0.007275390625, 0.05283203125, 0.8825439453125, 0.0573486328125)
HtProb <- c(0.06435420554733252, 0.006328163545487698, 0.04978870369178626, 0.8795289272153936)

transProb <- t(matrix(c(HoProb, HbProb, OoProb, HtProb),4))

elements <- c(225:360,0:224)

HoStateProb <- c(0.01167152, 0.01258194, 0.01165331, 0.01214494, 0.01267298,
                 0.01327385, 0.0125091 , 0.01274581, 0.01320102, 0.01219956,
                 0.01159869, 0.01290969, 0.01238165, 0.0127094 , 0.01221777,
                 0.0127094 , 0.01301894, 0.01285506, 0.01327385, 0.01239985,
                 0.01260015, 0.01223598, 0.01331027, 0.0138201 , 0.01372906,
                 0.01334669, 0.01280044, 0.01265477, 0.01305535, 0.0120721 ,
                 0.01258194, 0.01252731, 0.01327385, 0.01245448, 0.01289148,
                 0.0120721 , 0.01303714, 0.0129461 , 0.01260015, 0.01252731,
                 0.01216315, 0.01241806, 0.01121631, 0.01148944, 0.01156227,
                 0.0109614 , 0.01134377, 0.01159869, 0.01085215, 0.01039694,
                 0.01032411, 0.01003277, 0.0103059 , 0.01019665, 0.00941369,
                 0.00941369, 0.00895849, 0.00970503, 0.00999636, 0.01041515,
                 0.00919519, 0.00945011, 0.00995994, 0.00999636, 0.00990532,
                 0.00954115, 0.00894028, 0.0087764 , 0.00970503, 0.01036052,
                 0.01076111, 0.00919519, 0.00977786, 0.00968682, 0.00972323,
                 0.00928623, 0.00875819, 0.00948653, 0.00939548, 0.00866715,
                 0.00959578, 0.00914057, 0.00974144, 0.00888565, 0.0085579 ,
                 0.00897669, 0.00861253, 0.00928623, 0.00781136, 0.00879461,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        )
HbStateProb <- c(0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.00624191, 0.01197635, 0.01230621, 0.01164649, 0.01070767,
                 0.01083454, 0.01113902, 0.01050468, 0.01174799, 0.01035244,
                 0.01182411, 0.01055543, 0.01174799, 0.01037781, 0.01136738,
                 0.01091066, 0.01053006, 0.01053006, 0.01075842, 0.01093603,
                 0.01131664, 0.01078379, 0.01078379, 0.01073304, 0.01073304,
                 0.0106823 , 0.01103753, 0.0125092 , 0.0110629 , 0.01157037,
                 0.01136738, 0.01050468, 0.01136738, 0.01085991, 0.01190023,
                 0.01124052, 0.01144351, 0.01174799, 0.01134201, 0.01255995,
                 0.01182411, 0.0110629 , 0.01162112, 0.01113902, 0.01233158,
                 0.00956586, 0.01182411, 0.01065692, 0.01121514, 0.01215397,
                 0.01144351, 0.01070767, 0.01080916, 0.01040319, 0.01121514,
                 0.01070767, 0.01042856, 0.01098678, 0.01093603, 0.01091066,
                 0.01027632, 0.01073304, 0.01055543, 0.01091066, 0.01075842,
                 0.01055543, 0.01124052, 0.00918525, 0.01103753, 0.01101215,
                 0.00974347, 0.01098678, 0.01075842, 0.01121514, 0.0106823 ,
                 0.00987034, 0.01157037, 0.01088529, 0.01022557, 0.01144351,
                 0.01184948, 0.01103753, 0.01217934, 0.01172262, 0.01177336,
                 0.01159575, 0.01093603, 0.01212859, 0.01179874, 0.01098678,
                 0.00586131, 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.       )
OoStateProb <- c(0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.01144992, 0.01252411, 0.01125461, 0.01193819,
                 0.00998511, 0.01203584, 0.01110813, 0.01047338, 0.0110593 ,
                 0.01074193, 0.01101047, 0.0119626 , 0.01215791, 0.0110593 ,
                 0.01103489, 0.012695  , 0.01096165, 0.01132785, 0.01140109,
                 0.01215791, 0.01249969, 0.01154757, 0.01135226, 0.01152316,
                 0.01186494, 0.01271942, 0.01191377, 0.01071751, 0.01203584,
                 0.01218232, 0.01130343, 0.01147433, 0.0123288 , 0.01208467,
                 0.01240204, 0.01201143, 0.01147433, 0.01281707, 0.01223115,
                 0.01135226, 0.01137668, 0.01257294, 0.01201143, 0.0117917 ,
                 0.01169405, 0.01132785, 0.01125461, 0.01169405, 0.01147433,
                 0.01162081, 0.01086399, 0.01088841, 0.01086399, 0.00986304,
                 0.01103489, 0.00952125, 0.01135226, 0.01042455, 0.01157198,
                 0.0105222 , 0.01108371, 0.01049779, 0.01079075, 0.01081517,
                 0.00971656, 0.01198701, 0.01044896, 0.01008276, 0.01079075,
                 0.01040014, 0.01191377, 0.01013159, 0.01096165, 0.01020483,
                 0.0105222 , 0.00971656, 0.01000952, 0.01035131, 0.01025366,
                 0.010156  , 0.01096165, 0.00971656, 0.01008276, 0.00932594,
                 0.00937477, 0.00959449, 0.00932594, 0.01076634, 0.0092527 ,
                 0.01086399, 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        )
HtStateProb <- c(0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.        , 0.        , 0.        , 0.        ,
                 0.        , 0.00802282, 0.00958878, 0.00952442, 0.00896669,
                 0.00965313, 0.00898814, 0.00905249, 0.00982474, 0.00900959,
                 0.00855911, 0.00935281, 0.00892378, 0.00982474, 0.01006071,
                 0.0091383 , 0.00937426, 0.00954587, 0.01083296, 0.00965313,
                 0.00873072, 0.00935281, 0.01018942, 0.00982474, 0.01029667,
                 0.01003926, 0.00924555, 0.01046828, 0.01029667, 0.00870927,
                 0.01010361, 0.01027522, 0.00958878, 0.00980329, 0.01074715,
                 0.00982474, 0.01025377, 0.01031812, 0.00999635, 0.0106399 ,
                 0.01059699, 0.0098891 , 0.0098891 , 0.01031812, 0.01036103,
                 0.01124053, 0.0106399 , 0.01061844, 0.01021087, 0.00980329,
                 0.01091876, 0.01136924, 0.01066135, 0.01044683, 0.01117618,
                 0.01158376, 0.01115473, 0.01134779, 0.01239891, 0.01209859,
                 0.01196988, 0.01186263, 0.01160521, 0.01284939, 0.01261342,
                 0.01229165, 0.01239891, 0.01248472, 0.01181972, 0.01246326,
                 0.01224875, 0.01317116, 0.0129352 , 0.01308536, 0.01368599,
                 0.01276358, 0.01304245, 0.01334277, 0.01282794, 0.01299955,
                 0.01291374, 0.01304245, 0.01400777, 0.01267778, 0.01353583,
                 0.0153592 , 0.0152305 , 0.01407212, 0.01458695, 0.01432954,
                 0.01445824)

emissProb <- matrix(c(HoStateProb,HbStateProb,OoStateProb,HtStateProb), 4, byrow = T) 

hmm <- initHMM(States = states, 
               Symbols = elements,
               transProbs=transProb,
               emissionProbs = emissProb)

hmm$startProbs["Heading odor"] <- startProb[1]
hmm$startProbs["Heading bottom"] <- startProb[2]
hmm$startProbs["Opposite to odor"] <- startProb[3]
hmm$startProbs["Heading top"] <- startProb[4]

print(hmm)

simhmm <- simHMM(hmm, 1000000)
simhmm_df <- as.data.frame(simhmm)
write.table(simhmm,file = "simulacion_G2019S_odor")

table(simhmm_df$states)

pie(table(simhmm_df$states),main = "Proportion of orientations LRRK2-G2019S odor",
    col = c("lightgreen","lightblue","purple","lightyellow"),)

# Steady-state equations. [Ho Hb Oo Ht]*(I-transmat) = 0

I = diag(4)
matriz <- I- transProb
matriz <- t(matriz)
matriz <- rbind(matriz,c(1,1,1,1))
vector <- c(0,0,0,0,1)

round(qr.solve(matriz,vector)*100,digits = 2)

# Simulations

sim <- read.table("simulacion_G2019S_odor")
states_count <- table(sim$states)
percentages <- round(prop.table(states_count)*100,digits = 2)
