compute idade = v2009.
execute.


/*///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////.
/*///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////.
*2010.
Weight Off.

/*/* Comando .

***** Transições EDUCACIONAIS ADAPTADO do Censo de 2010 para PNADC 2016


*****estao estudando no EB mais ainda nao completaram (realizaram somente T1)

DO IF  ((V3002=1) & (V3003A = 2 | V3003A = 3 | V3003A = 4 | V3003A = 5)).
RECODE VD3004 (ELSE=1) INTO T1a.
END IF.

*****estao estudando no EM mas ainda nao completaram (realizaram T1 e T2 (completar EB) e T3 (entrar no EM), mas nao T4 (completar EM)

DO IF  ((V3002=1) & (V3003A = 6 | V3003A = 7)).
RECODE VD3004 (ELSE=3) INTO T3a.
END IF.

*****estao estudando no ES mas ainda nao completaram (realizaram T1, T2 (completar EB), T3 (entrar no EM), T4 (completar EM), T5 (entrar na universidade), mas nao T6 (completar universidade)

DO IF  ((V3002=1) & (V3003A = 8)).
RECODE VD3004 (ELSE=5) INTO T5a.
END IF.

*****estao estudando em PG (realizaram T1, T2 (completar EB), T3 (entrar no EM), T4 (completar EM), T5 (entrar na universidade), e T6 (completar universidade)

DO IF  ((V3002=1) & (V3003A = 9 | V3003A = 10 | V3003A = 11)).
RECODE VD3004 (ELSE=6) INTO T6a.
END IF.



***********nao estao mais na escola e realizaram somente T1

DO IF  (V3002=2) & (V3009A = 2 | V3009A = 3 | V3009A = 4 | V3009A = 5 | V3009A = 6).
RECODE VD3004 (ELSE=1) INTO T1b.
END IF.


***********nao estao mais na escola e realizaram somente T1 - Parte II - atingiram de 5a a 8a mas nao completaram EB

DO IF  (V3002=2) & (V3009A = 7 | V3009A = 8) & (V3014=2) .
RECODE VD3004 (ELSE=1) INTO T1b1.
END IF.


**********nao estao mais na escola, mas realizaram T1 e T2 (completaram EB - 8 anos de estudo)

DO IF  (V3002=2) & (V3009A = 7 | V3009A = 8) & (V3014=1).
RECODE VD3004 (ELSE=2) INTO T2b.
END IF.


**********nao estao mais na escola, mas realizaram T1, T2 e T3 (completaram EB (8 anos de estudo) e entraram no EM)

DO IF  (V3002=2) & (V3009A = 9 | V3009A = 10) & (V3014=2).
RECODE VD3004 (ELSE=3) INTO T3b.
END IF.


**********nao estao mais na escola, mas realizaram T1, T2, T3 e T4 (completaram EB e EM)

DO IF  (V3002=2) & (V3009A = 9 | V3009A = 10 | V3009A = 11) & (V3014=1).
RECODE VD3004 (ELSE=4) INTO T4b.
END IF.


**********nao estao mais na escola, mas realizaram T1, T2, T3, T4 e T5 (completaram EB, EM e entraram na universidade)

DO IF  (V3002=2) & (V3009A = 12) & (V3014=2).
RECODE VD3004 (ELSE=5) INTO T5b.
END IF.


**********nao estao mais na escola, mas realizaram T1, T2, T3, T4, T5 e T6 (completaram EB, EM e ensino superior) - parte I - Quem terminou a graduaaao

DO IF  (V3002=2) & (V3009A = 12) & (V3014=1).
RECODE VD3004 (ELSE=6) INTO T6b1.
END IF.


**********nao estao mais na escola, mas realizaram T1, T2, T3, T4, T5 e T6 (completaram EB, EM e ensino superior) - parte II - Quem atingiu PG

DO IF  (V3002=2) & (V3009A = 13 | V3009A = 14 | V3009A = 15).
RECODE VD3004 (ELSE=6) INTO T6b2.
END IF.


**********nao sabem ler e escrever e nunca frequentaram escola

DO IF  (V3001 = 2).
RECODE VD3004 (ELSE=0) INTO T0.
END IF.


********estao no segundo curso de graduaaao

DO IF  (V3007 = 1).
RECODE VD3004 (ELSE=6) INTO T6b2.
END IF.


******TRANSICAO MAIS ALTA REALIZADA

COMPUTE TrTot=MAX(T0,T1a,T1b,T1b1,T2b,T3a,T3b,T4b,T5a,T5b,T6a,T6b1,T6b2).



******CRIANDO VARIaVEIS INDICADORAS PARA AS Transições

RECODE TrTot (0=0) (1=1) (2=1) (3=1) (4=1) (5=1) (6=1) (SYSMIS=SYSMIS) INTO T1.
RECODE TrTot (0=0) (1=0) (2=1) (3=1) (4=1) (5=1) (6=1) (SYSMIS=SYSMIS) INTO T2.
RECODE TrTot (0=0) (1=0) (2=0) (3=1) (4=1) (5=1) (6=1) (SYSMIS=SYSMIS) INTO T3.
RECODE TrTot (0=0) (1=0) (2=0) (3=0) (4=1) (5=1) (6=1) (SYSMIS=SYSMIS) INTO T4.
RECODE TrTot (0=0) (1=0) (2=0) (3=0) (4=0) (5=1) (6=1) (SYSMIS=SYSMIS) INTO T5.
RECODE TrTot (0=0) (1=0) (2=0) (3=0) (4=0) (5=0) (6=1) (SYSMIS=SYSMIS) INTO T6.

EXECUTE.


VARIABLE LABELS  T1 'Educação - Transições - Realizou T1'.
VARIABLE LABELS  T2 'Educação - Transições - Realizou T2'.
VARIABLE LABELS  T3 'Educação - Transições - Realizou T3'.
VARIABLE LABELS  T4 'Educação - Transições - Realizou T4'.
VARIABLE LABELS  T5 'Educação - Transições - Realizou T5'.
VARIABLE LABELS  T6 'Educação - Transições - Realizou T6'.
VARIABLE LABELS  TrTot 'Educação Transições - Última Transição Realizada'.

VALUE LABELS TrTot
0 'Analfabeto/Nunca Freq Escola'
1 'Fundamental Incompleto'
2 'Fundamental Completo'
3 'Ensino Medio Incompleto'
4 'Ensino Medio Completo'
5 'Superior Incompleto' 
6 'Superior Completo'.


compute ind.pea = 0.
if (V4001 = 1) ind.pea = 1.
if (V4002 = 1) ind.pea = 1.
if (V4003 = 1) ind.pea = 1.
if (v4005 = 1) ind.pea = 1.
if (v4004 = 1) ind.pea = 1.
if (v4071 = 1 & v4077 = 1)  ind.pea = 1.
do if (idade < 10) .
recode ind.pea (else=sysmis) .
end if.


if (V4001 = 1) ind.ocup = 1.
if (V4002 = 1) ind.ocup = 1.
if (V4003 = 1) ind.ocup = 1.
if (v4005 = 1) ind.ocup = 1.
if (v4004 = 1) ind.ocup = 1.
if (v4071 = 1 & v4077 = 1 & (V4001 <> 1)  & (V4002 <> 1) & (V4003 <> 1) & (v4005 <> 1)  & (v4004 <> 1) ) ind.ocup = 0.
do if (idade < 10) .
recode ind.ocup (else=sysmis) .
end if.
if (ind.pea=0) PEASemana = 0.
if (ind.ocup=1) PEASemana = 1.
if (ind.ocup=0) PEASemana = 2.


recode V4012	
(7=0)
(1,2,3,4=1)
(5=2)
(6=3)
(else=SYSMIS)
into PosicaoOcupHabitual.
do if (idade < 10) .
recode PosicaoOcupHabitual (else=sysmis) .
end if.
do if (ind.ocup=1 & (
(VD4008 <> 1)&
(VD4008 <> 2)&
(VD4008 <> 3)&
(VD4008 <> 4)&
(VD4008 <> 5)&
(VD4008 <> 6)&
(VD4007 <> 1)&
(VD4007 <> 2)&
(VD4007 <> 3)&
(VD4007 <> 4) ) ).
recode PosicaoOcupHabitual (else=sysmis).
end if.

recode v4013
(0=999)
(1101=10)
(1102=10)
(1103=10)
(1104=10)
(1105=10)
(1106=10)
(1107=10)
(1108=10)
(1109=10)
(1110=10)
(1111=10)
(1112=10)
(1113=10)
(1114=10)
(1115=10)
(1116=10)
(1117=10)
(1118=10)
(1119=10)
(1201=10)
(1202=10)
(1203=10)
(1204=10)
(1205=10)
(1206=10)
(1207=10)
(1208=10)
(1209=10)
(1401=10)
(1402=10)
(1500=10)
(1999=10)
(2000=10)
(3001=10)
(3002=10)
(5000=20)
(6000=20)
(7001=20)
(7002=20)
(8001=20)
(8002=20)
(8009=20)
(8999=20)
(9000=20)
(10010=30)
(10021=30)
(10022=30)
(10030=30)
(10091=30)
(10092=30)
(10093=30)
(10099=30)
(10999=30)
(11000=30)
(12000=30)
(13001=30)
(13002=30)
(13999=30)
(14001=30)
(14002=30)
(14999=30)
(15011=30)
(15012=30)
(15020=30)
(16001=30)
(16002=30)
(17001=30)
(17002=30)
(17999=30)
(18000=30)
(19010=30)
(19020=30)
(19030=30)
(20010=30)
(20020=30)
(20090=30)
(20999=30)
(21000=30)
(22010=30)
(22020=30)
(23010=30)
(23091=30)
(23099=30)
(24001=30)
(24002=30)
(24003=30)
(24999=30)
(25001=30)
(25002=30)
(26010=30)
(26020=30)
(26030=30)
(26041=30)
(26042=30)
(26999=30)
(27010=30)
(27090=30)
(27999=30)
(28000=30)
(29001=30)
(29002=30)
(29003=30)
(30010=30)
(30020=30)
(30030=30)
(30090=30)
(30999=30)
(31000=30)
(32001=30)
(32002=30)
(32003=30)
(32009=30)
(32999=30)
(33001=30)
(33002=30)
(35010=40)
(35021=40)
(35022=40)
(36000=40)
(37000=40)
(38000=30)
(39000=30)
(41000=50)
(42000=50)
(43000=50)
(43999=50)
(45010=60)
(45020=114)
(45030=60)
(45040=60)
(48010=60)
(48020=60)
(48030=60)
(48041=60)
(48042=60)
(48050=60)
(48060=60)
(48071=60)
(48072=60)
(48073=60)
(48074=60)
(48075=60)
(48076=60)
(48077=60)
(48078=60)
(48079=60)
(48080=60)
(48090=60)
(48100=60)
(48999=60)
(49010=80)
(49030=80)
(49040=80)
(49090=80)
(49999=80)
(50000=80)
(51000=80)
(52010=80)
(52020=80)
(53001=80)
(53002=80)
(55000=70)
(56011=70)
(56012=70)
(56020=70)
(56999=70)
(58000=114)
(59000=114)
(60001=114)
(60002=114)
(61000=80)
(62000=111)
(63000=111)
(64000=90)
(65000=90)
(66001=90)
(66002=90)
(68000=111)
(69000=111)
(70000=111)
(71000=111)
(72000=111)
(73010=111)
(73020=111)
(74000=111)
(75000=113)
(77010=114)
(77020=111)
(78000=111)
(79000=114)
(80000=111)
(81011=111)
(81012=111)
(81020=111)
(82001=111)
(82002=111)
(82003=111)
(82009=111)
(84011=100)
(84012=100)
(84013=100)
(84014=100)
(84015=100)
(84016=100)
(84017=100)
(84020=100)
(84999=100)
(85011=112)
(85012=112)
(85013=112)
(85014=112)
(85021=112)
(85029=112)
(85999=112)
(86001=113)
(86002=113)
(86003=113)
(86004=113)
(86009=113)
(86999=113)
(87000=113)
(88000=113)
(90000=114)
(91000=114)
(92000=114)
(93011=114)
(93012=114)
(93020=114)
(94010=114)
(94020=114)
(94091=114)
(94099=114)
(95010=114)
(95030=114)
(96010=114)
(96020=114)
(96030=114)
(96090=114)
(97000=120)
(99000=114)
(99999=999)
into ISIC.
execute.


value labels ISIC
0 'NIU (not in universe)' 
10 'Agriculture, fishing, and forestry' 
20 'Mining' 
30 'Manufacturing' 
40 'Electricity, gas and water' 
50 'Construction' 
60 'Wholesale and retail trade' 
70 'Hotels and restaurants' 
80 'Transportation and communications' 
90 'Financial services and insurance' 
100 'Public administration and defense' 
111 'Real estate and business services' 
112 'Education' 
113 'Health and social work' 
114 'Other services' 
120 'Private household services' 
999 'Unknown' .

VALUE LABELS ind.pea
0 'PNEA'
1 'PEA '.

VALUE LABELS ind.ocup
0 'Descoupados'
1 'Ocupados'.

VALUE LABELS PEASemana
0 'PNEA'
1 'PEA Ocupados'
2 'PEA nao-ocupados'.

VALUE LABELS PosicaoOcupHabitual
0 'Nao Remunerado'
1 'Empregado'
2 'Empregador'	
3 'Conta propria'.


COMPUTE PosicaoOcup = PosicaoOcupHabitual.

recode V4010 
(0=sysmis)
(5168=sysmis)
(110=110)
(210=110)
(299=110)
(411=110)
(412=110)
(511=110)
(512=110)
(599=110)
(999=110)
(5411=110)
(1111=1110)
(1113=1110)
(1112=1120)
(1114=1143)
(1120=1210)
(1311=1221)
(1312=1221)
(1321=1222)
(1322=1222)
(1323=1223)
(1420=1224)
(1411=1225)
(1412=1225)
(1324=1226)
(1330=1226)
(1219=1229)
(1346=1227)
(1213=1229)
(1341=1229)
(1342=1229)
(1343=1229)
(1344=1229)
(1345=1229)
(1349=1229)
(1431=1229)
(1439=1229)
(1211=1231)
(1212=1232)
(1221=1233)
(1222=1234)
(1223=1237)
(2111=2111)
(2112=2112)
(2113=2113)
(2114=2114)
(2120=2121)
(2511=2131)
(2512=2131)
(2513=2131)
(2514=2131)
(2519=2131)
(2521=2131)
(2522=2131)
(2523=2131)
(2529=2131)
(2161=2141)
(2162=2141)
(2164=2141)
(2142=2142)
(2151=2143)
(2152=2144)
(2153=2144)
(2144=2145)
(2145=2146)
(2146=2147)
(2165=2148)
(2141=2149)
(2143=2149)
(2149=2149)
(2131=2211)
(2133=2211)
(2132=2213)
(2211=2221)
(2212=2221)
(2261=2222)
(2250=2223)
(2262=2224)
(2263=2229)
(2266=2229)
(2269=2229)
(2221=2230)
(2222=2230)
(2310=2310)
(2330=2320)
(2341=2331)
(2342=2332)
(2351=2351)
(2320=2359)
(2352=2359)
(2353=2359)
(2354=2359)
(2355=2359)
(2356=2359)
(2359=2359)
(3423=2359)
(5165=2359)
(2411=2411)
(2412=2411)
(2423=2412)
(2424=2412)
(2413=2419)
(2421=2419)
(2422=2419)
(2431=2419)
(2432=2419)
(2611=2421)
(2619=2421)
(2612=2422)
(2621=2431)
(2622=2432)
(2631=2441)
(2632=2442)
(2633=2443)
(2643=2444)
(2634=2445)
(2635=2446)
(2641=2451)
(2642=2451)
(2651=2452)
(2652=2453)
(2653=2454)
(2654=2455)
(2655=2455)
(2659=2455)
(2636=2460)
(3111=3111)
(3139=3111)
(3112=3112)
(3113=3113)
(3114=3114)
(3522=3114)
(3115=3115)
(3116=3116)
(3117=3117)
(3118=3118)
(3119=3119)
(3511=3121)
(3512=3121)
(3513=3121)
(3514=3121)
(3431=3131)
(3521=3131)
(3211=3133)
(3151=3141)
(3152=3142)
(3153=3143)
(3154=3144)
(3155=3145)
(3257=3152)
(7543=3152)
(3141=3211)
(3212=3211)
(3142=3212)
(3143=3212)
(2240=3221)
(3256=3221)
(3252=3222)
(2265=3223)
(2267=3224)
(3254=3224)
(3214=3225)
(3251=3225)
(2264=3226)
(3255=3226)
(3240=3227)
(3213=3228)
(3259=3229)
(3221=3231)
(3222=3232)
(2230=3241)
(3230=3241)
(3413=3242)
(3311=3411)
(3312=3411)
(3321=3412)
(3334=3413)
(4221=3414)
(2433=3415)
(2434=3415)
(3322=3415)
(3339=3415)
(3323=3416)
(3315=3417)
(3324=3421)
(3331=3422)
(3351=3422)
(3333=3423)
(3341=3431)
(3342=3431)
(3343=3431)
(3344=3431)
(3411=3432)
(3313=3433)
(3314=3434)
(3332=3439)
(3433=3439)
(3435=3439)
(3352=3442)
(3353=3443)
(3354=3444)
(3359=3444)
(3355=3450)
(3412=3460)
(2163=3471)
(2166=3471)
(3432=3471)
(2656=3472)
(3421=3475)
(3422=3475)
(4131=4111)
(4132=4113)
(4120=4115)
(4211=4121)
(4311=4121)
(4313=4121)
(4227=4122)
(4312=4122)
(4321=4131)
(4322=4132)
(4323=4133)
(4411=4141)
(4415=4141)
(4412=4142)
(4413=4143)
(4414=4144)
(4110=4190)
(4416=4190)
(4419=4190)
(5230=4211)
(4212=4212)
(4213=4214)
(4214=4215)
(4224=4222)
(4225=4222)
(4226=4222)
(4229=4222)
(4222=4223)
(4223=4223)
(5111=5111)
(5112=5112)
(8331=5112)
(5113=5113)
(5151=5121)
(5152=5121)
(3434=5122)
(5120=5122)
(9411=5122)
(9412=5122)
(5131=5123)
(5132=5123)
(5311=5131)
(5312=5131)
(3258=5132)
(5321=5132)
(5329=5132)
(5322=5133)
(3253=5139)
(5164=5139)
(5141=5141)
(5142=5141)
(5162=5142)
(5163=5143)
(5169=5149)
(5161=5151)
(5412=5162)
(5413=5163)
(5414=9152)
(5419=5169)
(5241=5210)
(5221=5220)
(5222=5220)
(5223=5220)
(5242=5220)
(5245=5220)
(5246=5220)
(5249=5220)
(5211=5230)
(6111=6111)
(6114=6111)
(6112=6113)
(9214=9211)
(6121=6121)
(6122=6122)
(6123=6123)
(6129=6129)
(6130=6141)
(6210=6141)
(9215=9212)
(6221=6151)
(6225=6152)
(6224=6154)
(9216=9212)
(3121=7111)
(8111=7111)
(7542=7112)
(7113=7113)
(7111=7122)
(7112=7122)
(9312=7122)
(9313=7122)
(7114=7123)
(7115=7124)
(3123=7129)
(7119=7129)
(7121=7131)
(7122=7132)
(7123=7133)
(7124=7134)
(7125=7135)
(7126=7136)
(7411=7137)
(7131=7141)
(7132=7142)
(7133=7143)
(7544=7143)
(7211=7211)
(7212=7212)
(7213=7213)
(7214=7214)
(7215=7215)
(7541=7216)
(7221=7221)
(7222=7222)
(7223=7223)
(7224=7224)
(7231=7231)
(7234=7231)
(7232=7232)
(7127=7233)
(7233=7233)
(7412=7241)
(7421=7242)
(7422=7244)
(7413=7245)
(7311=7311)
(7312=7312)
(7313=7313)
(7314=7321)
(7315=7322)
(7316=7324)
(7317=7331)
(7319=7331)
(7549=7331)
(7318=7332)
(7321=7341)
(8132=7344)
(7323=7345)
(7511=7411)
(7512=7412)
(7513=7413)
(7514=7414)
(7515=7415)
(7516=7416)
(7521=7421)
(7522=7422)
(7523=7423)
(8152=8262)
(7531=7433)
(7532=7435)
(7533=7436)
(7534=7437)
(7535=7441)
(7536=7442)
(8112=8112)
(8113=8113)
(3135=8121)
(8121=8121)
(8181=8131)
(8172=8141)
(8171=8142)
(8131=8151)
(3133=8152)
(3134=8155)
(3131=8161)
(8182=8162)
(3132=8163)
(3122=8171)
(8114=8212)
(8122=8223)
(8141=8231)
(8142=8232)
(7322=8251)
(8143=8253)
(8151=8261)
(8153=8263)
(8154=8264)
(8157=8264)
(8155=8265)
(8156=8266)
(8159=8269)
(8160=8271)
(8211=8281)
(8212=8282)
(8189=8290)
(8219=8290)
(8311=8311)
(8312=8312)
(8321=8321)
(8322=8322)
(8332=8324)
(8341=8331)
(8342=8332)
(8343=8333)
(8344=8334)
(8350=8340)
(5212=9111)
(9520=9112)
(5243=9113)
(5244=9113)
(9510=9120)
(9111=9131)
(9112=9132)
(9129=9132)
(9121=9133)
(9122=9142)
(9123=9142)
(9621=9151)
(5153=9141)
(9623=9153)
(9611=9161)
(9612=9161)
(9613=9162)
(9622=9162)
(9624=9162)
(9211=9211)
(9212=9211)
(9213=9211)
(9311=9311)
(9329=9321)
(8183=9322)
(9321=9322)
(9331=9331)
(9332=9332)
(9333=9333)
(9334=9333)
(9629=sysmis)
(9999=sysmis)
(9998=sysmis)
(9988=sysmis)
into @isko.

if (@isko=6122 & PosicaoOcup = 2) @isko = 1311.
if (@isko =6121 & PosicaoOcup = 2) @isko = 1311.
if(@isko=1237 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1234 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1233 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1232 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1231 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1229 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1227 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1226 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1225 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1224 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1223 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1222 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1221 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1210 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if(@isko=1143 & isic = 100 & PosicaoOcup <> 3) @isko = 1120.
if (@isko = 2131 & trtot < 5 ) @isko = 3121.
if ((@isko=9211 | @isko = 6113) & (isic = 114 | isic = 120)) @isko = 9141.
if (@isko=6129 & PosicaoOcup = 1) @isko = 9211.
if (@isko=6123 & PosicaoOcup = 1) @isko = 9211.
if (@isko=6113 & PosicaoOcup = 1) @isko = 9211.
if (@isko=6111 & PosicaoOcup = 1) @isko = 9211.
if (@isko =6154 & PosicaoOcup = 1) @isko = 9213.
if (@isko =6153 & PosicaoOcup = 1) @isko = 9213.
if (@isko =6152 & PosicaoOcup = 1) @isko = 9213.
if (@isko =6151 & PosicaoOcup = 1) @isko = 9213.
if (@isko =6142 & PosicaoOcup = 1) @isko = 9212.
if (@isko =6141 & PosicaoOcup = 1) @isko = 9212.
if (@isko =6122 & PosicaoOcup = 1) @isko = 9211.
if (@isko =6121 & PosicaoOcup = 1) @isko = 9211.
if (@isko=6129 & PosicaoOcup = 2) @isko = 1311.
if (@isko=6123 & PosicaoOcup = 2) @isko = 1311.
if (@isko=6113 & PosicaoOcup = 2) @isko = 1311.
if (@isko=6111 & PosicaoOcup = 2) @isko = 1311. 
execute.

recode	@isko	(	1110	=	1	)	into	@egp11.
recode	@isko	(	1120	=	1	)	into	@egp11.
recode	@isko	(	1130	=	1	)	into	@egp11.
recode	@isko	(	1210	=	1	)	into	@egp11.
recode	@isko	(	1222	=	1	)	into	@egp11.
recode	@isko	(	1223	=	1	)	into	@egp11.
recode	@isko	(	1224	=	1	)	into	@egp11.
recode	@isko	(	1225	=	1	)	into	@egp11.
recode	@isko	(	1226	=	1	)	into	@egp11.
recode	@isko	(	1227	=	1	)	into	@egp11.
recode	@isko	(	1228	=	1	)	into	@egp11.
recode	@isko	(	1229	=	1	)	into	@egp11.
recode	@isko	(	1231	=	1	)	into	@egp11.
recode	@isko	(	1232	=	1	)	into	@egp11.
recode	@isko	(	1233	=	1	)	into	@egp11.
recode	@isko	(	1234	=	1	)	into	@egp11.
recode	@isko	(	1235	=	1	)	into	@egp11.
recode	@isko	(	1236	=	1	)	into	@egp11.
recode	@isko	(	1237	=	1	)	into	@egp11.
recode	@isko	(	1239	=	1	)	into	@egp11.
recode	@isko	(	2111	=	1	)	into	@egp11.
recode	@isko	(	2112	=	1	)	into	@egp11.
recode	@isko	(	2113	=	1	)	into	@egp11.
recode	@isko	(	2114	=	1	)	into	@egp11.
recode	@isko	(	2121	=	1	)	into	@egp11.
recode	@isko	(	2122	=	1	)	into	@egp11.
recode	@isko	(	2131	=	1	)	into	@egp11.
recode	@isko	(	2141	=	1	)	into	@egp11.
recode	@isko	(	2142	=	1	)	into	@egp11.
recode	@isko	(	2143	=	1	)	into	@egp11.
recode	@isko	(	2144	=	1	)	into	@egp11.
recode	@isko	(	2145	=	1	)	into	@egp11.
recode	@isko	(	2146	=	1	)	into	@egp11.
recode	@isko	(	2147	=	1	)	into	@egp11.
recode	@isko	(	2148	=	1	)	into	@egp11.
recode	@isko	(	2149	=	1	)	into	@egp11.
recode	@isko	(	2211	=	1	)	into	@egp11.
recode	@isko	(	2212	=	1	)	into	@egp11.
recode	@isko	(	2213	=	1	)	into	@egp11.
recode	@isko	(	2221	=	1	)	into	@egp11.
recode	@isko	(	2222	=	1	)	into	@egp11.
recode	@isko	(	2223	=	1	)	into	@egp11.
recode	@isko	(	2224	=	1	)	into	@egp11.
recode	@isko	(	2229	=	1	)	into	@egp11.
recode	@isko	(	2310	=	1	)	into	@egp11.
recode	@isko	(	2351	=	1	)	into	@egp11.
recode	@isko	(	2352	=	2	)	into	@egp11.
recode	@isko	(	2411	=	1	)	into	@egp11.
recode	@isko	(	2421	=	1	)	into	@egp11.
recode	@isko	(	2422	=	1	)	into	@egp11.
recode	@isko	(	2429	=	1	)	into	@egp11.
recode	@isko	(	2441	=	1	)	into	@egp11.
recode	@isko	(	2442	=	1	)	into	@egp11.
recode	@isko	(	2443	=	1	)	into	@egp11.
recode	@isko	(	2445	=	1	)	into	@egp11.
recode	@isko	(	3143	=	1	)	into	@egp11.
recode	@isko	(	3144	=	2	)	into	@egp11.
recode	@isko	(	2452	=	2	)	into	@egp11.
recode	@isko	(	5151	=	2	)	into	@egp11.
recode	@isko	(	2453	=	2	)	into	@egp11.
recode	@isko	(	2460	=	2	)	into	@egp11.
recode	@isko	(	3232	=	2	)	into	@egp11.
recode	@isko	(	1143	=	2	)	into	@egp11.
recode	@isko	(	3472	=	2	)	into	@egp11.
recode	@isko	(	3471	=	2	)	into	@egp11.
recode	@isko	(	3417	=	2	)	into	@egp11.
recode	@isko	(	3413	=	2	)	into	@egp11.
recode	@isko	(	3415	=	2	)	into	@egp11.
recode	@isko	(	3225	=	2	)	into	@egp11.
recode	@isko	(	2431	=	2	)	into	@egp11.
recode	@isko	(	3231	=	2	)	into	@egp11.
recode	@isko	(	2455	=	2	)	into	@egp11.
recode	@isko	(	3224	=	2	)	into	@egp11.
recode	@isko	(	3416	=	2	)	into	@egp11.
recode	@isko	(	2359	=	3	)	into	@egp11.
recode	@isko	(	3475	=	2	)	into	@egp11.
recode	@isko	(	3431	=	2	)	into	@egp11.
recode	@isko	(	3411	=	2	)	into	@egp11.
recode	@isko	(	3122	=	2	)	into	@egp11.
recode	@isko	(	2419	=	2	)	into	@egp11.
recode	@isko	(	3450	=	2	)	into	@egp11.
recode	@isko	(	3434	=	2	)	into	@egp11.
recode	@isko	(	2332	=	3	)	into	@egp11.
recode	@isko	(	2446	=	2	)	into	@egp11.
recode	@isko	(	3449	=	2	)	into	@egp11.
recode	@isko	(	2331	=	3	)	into	@egp11.
recode	@isko	(	3432	=	2	)	into	@egp11.
recode	@isko	(	2444	=	2	)	into	@egp11.
recode	@isko	(	3422	=	2	)	into	@egp11.
recode	@isko	(	3228	=	2	)	into	@egp11.
recode	@isko	(	2451	=	2	)	into	@egp11.
recode	@isko	(	3423	=	2	)	into	@egp11.
recode	@isko	(	3227	=	2	)	into	@egp11.
recode	@isko	(	3226	=	2	)	into	@egp11.
recode	@isko	(	2230	=	2	)	into	@egp11.
recode	@isko	(	2320	=	2	)	into	@egp11.
recode	@isko	(	1141	=	2	)	into	@egp11.
recode	@isko	(	1142	=	2	)	into	@egp11.
recode	@isko	(	1312	=	2	)	into	@egp11.
recode	@isko	(	1313	=	2	)	into	@egp11.
recode	@isko	(	1314	=	2	)	into	@egp11.
recode	@isko	(	1315	=	2	)	into	@egp11.
recode	@isko	(	1316	=	2	)	into	@egp11.
recode	@isko	(	1317	=	2	)	into	@egp11.
recode	@isko	(	1318	=	2	)	into	@egp11.
recode	@isko	(	1319	=	2	)	into	@egp11.
recode	@isko	(	2132	=	2	)	into	@egp11.
recode	@isko	(	2139	=	2	)	into	@egp11.
recode	@isko	(	2340	=	2	)	into	@egp11.
recode	@isko	(	2412	=	2	)	into	@egp11.
recode	@isko	(	2454	=	2	)	into	@egp11.
recode	@isko	(	3121	=	2	)	into	@egp11.
recode	@isko	(	3221	=	2	)	into	@egp11.
recode	@isko	(	3222	=	2	)	into	@egp11.
recode	@isko	(	3223	=	2	)	into	@egp11.
recode	@isko	(	3229	=	2	)	into	@egp11.
recode	@isko	(	3241	=	2	)	into	@egp11.
recode	@isko	(	3242	=	2	)	into	@egp11.
recode	@isko	(	3310	=	2	)	into	@egp11.
recode	@isko	(	3320	=	2	)	into	@egp11.
recode	@isko	(	3412	=	2	)	into	@egp11.
recode	@isko	(	3414	=	2	)	into	@egp11.
recode	@isko	(	3419	=	2	)	into	@egp11.
recode	@isko	(	3421	=	2	)	into	@egp11.
recode	@isko	(	3429	=	2	)	into	@egp11.
recode	@isko	(	3441	=	2	)	into	@egp11.
recode	@isko	(	3442	=	2	)	into	@egp11.
recode	@isko	(	3443	=	2	)	into	@egp11.
recode	@isko	(	3444	=	2	)	into	@egp11.
recode	@isko	(	3473	=	2	)	into	@egp11.
recode	@isko	(	3474	=	2	)	into	@egp11.
recode	@isko	(	5152	=	2	)	into	@egp11.
recode	@isko	(	2432	=	3	)	into	@egp11.
recode	@isko	(	4213	=	4	)	into	@egp11.
recode	@isko	(	5112	=	3	)	into	@egp11.
recode	@isko	(	4132	=	3	)	into	@egp11.
recode	@isko	(	4131	=	3	)	into	@egp11.
recode	@isko	(	5113	=	3	)	into	@egp11.
recode	@isko	(	4215	=	3	)	into	@egp11.
recode	@isko	(	3340	=	3	)	into	@egp11.
recode	@isko	(	4133	=	3	)	into	@egp11.
recode	@isko	(	5210	=	3	)	into	@egp11.
recode	@isko	(	5111	=	3	)	into	@egp11.
recode	@isko	(	4141	=	3	)	into	@egp11.
recode	@isko	(	4111	=	3	)	into	@egp11.
recode	@isko	(	5162	=	3	)	into	@egp11.
recode	@isko	(	4115	=	3	)	into	@egp11.
recode	@isko	(	4121	=	3	)	into	@egp11.
recode	@isko	(	3439	=	3	)	into	@egp11.
recode	@isko	(	3433	=	3	)	into	@egp11.
recode	@isko	(	4122	=	3	)	into	@egp11.
recode	@isko	(	3330	=	3	)	into	@egp11.
recode	@isko	(	3460	=	3	)	into	@egp11.
recode	@isko	(	3480	=	3	)	into	@egp11.
recode	@isko	(	4112	=	3	)	into	@egp11.
recode	@isko	(	4113	=	3	)	into	@egp11.
recode	@isko	(	4114	=	3	)	into	@egp11.
recode	@isko	(	4143	=	3	)	into	@egp11.
recode	@isko	(	4144	=	3	)	into	@egp11.
recode	@isko	(	4190	=	3	)	into	@egp11.
recode	@isko	(	4212	=	3	)	into	@egp11.
recode	@isko	(	4214	=	3	)	into	@egp11.
recode	@isko	(	4221	=	3	)	into	@egp11.
recode	@isko	(	5161	=	3	)	into	@egp11.
recode	@isko	(	5149	=	4	)	into	@egp11.
recode	@isko	(	9152	=	4	)	into	@egp11.
recode	@isko	(	5123	=	4	)	into	@egp11.
recode	@isko	(	9113	=	4	)	into	@egp11.
recode	@isko	(	9151	=	4	)	into	@egp11.
recode	@isko	(	5220	=	4	)	into	@egp11.
recode	@isko	(	4211	=	4	)	into	@egp11.
recode	@isko	(	5139	=	4	)	into	@egp11.
recode	@isko	(	4142	=	4	)	into	@egp11.
recode	@isko	(	4222	=	4	)	into	@egp11.
recode	@isko	(	4223	=	4	)	into	@egp11.
recode	@isko	(	5132	=	4	)	into	@egp11.
recode	@isko	(	5142	=	4	)	into	@egp11.
recode	@isko	(	7111	=	10	)	into	@egp11.
recode	@isko	(	5143	=	8	)	into	@egp11.
recode	@isko	(	3142	=	8	)	into	@egp11.
recode	@isko	(	7216	=	8	)	into	@egp11.
recode	@isko	(	7242	=	8	)	into	@egp11.
recode	@isko	(	7313	=	8	)	into	@egp11.
recode	@isko	(	7243	=	8	)	into	@egp11.
recode	@isko	(	7312	=	8	)	into	@egp11.
recode	@isko	(	7233	=	8	)	into	@egp11.
recode	@isko	(	7311	=	8	)	into	@egp11.
recode	@isko	(	3115	=	8	)	into	@egp11.
recode	@isko	(	3113	=	8	)	into	@egp11.
recode	@isko	(	3131	=	8	)	into	@egp11.
recode	@isko	(	7245	=	8	)	into	@egp11.
recode	@isko	(	3117	=	8	)	into	@egp11.
recode	@isko	(	7241	=	8	)	into	@egp11.
recode	@isko	(	7222	=	8	)	into	@egp11.
recode	@isko	(	7344	=	8	)	into	@egp11.
recode	@isko	(	3112	=	8	)	into	@egp11.
recode	@isko	(	7343	=	8	)	into	@egp11.
recode	@isko	(	3114	=	8	)	into	@egp11.
recode	@isko	(	3211	=	8	)	into	@egp11.
recode	@isko	(	3132	=	8	)	into	@egp11.
recode	@isko	(	3152	=	8	)	into	@egp11.
recode	@isko	(	3116	=	8	)	into	@egp11.
recode	@isko	(	3111	=	8	)	into	@egp11.
recode	@isko	(	8151	=	8	)	into	@egp11.
recode	@isko	(	3118	=	8	)	into	@egp11.
recode	@isko	(	7232	=	8	)	into	@egp11.
recode	@isko	(	3212	=	8	)	into	@egp11.
recode	@isko	(	8311	=	8	)	into	@egp11.
recode	@isko	(	3133	=	8	)	into	@egp11.
recode	@isko	(	3119	=	8	)	into	@egp11.
recode	@isko	(	8159	=	8	)	into	@egp11.
recode	@isko	(	3123	=	8	)	into	@egp11.
recode	@isko	(	3139	=	8	)	into	@egp11.
recode	@isko	(	3141	=	8	)	into	@egp11.
recode	@isko	(	3145	=	8	)	into	@egp11.
recode	@isko	(	3151	=	8	)	into	@egp11.
recode	@isko	(	3213	=	8	)	into	@egp11.
recode	@isko	(	1221	=	9	)	into	@egp11.
recode	@isko	(	1311	=	9	)	into	@egp11.
recode	@isko	(	8273	=	10	)	into	@egp11.
recode	@isko	(	7424	=	9	)	into	@egp11.
recode	@isko	(	7132	=	9	)	into	@egp11.
recode	@isko	(	7321	=	9	)	into	@egp11.
recode	@isko	(	8276	=	9	)	into	@egp11.
recode	@isko	(	8112	=	9	)	into	@egp11.
recode	@isko	(	7124	=	9	)	into	@egp11.
recode	@isko	(	7122	=	9	)	into	@egp11.
recode	@isko	(	8141	=	9	)	into	@egp11.
recode	@isko	(	8340	=	9	)	into	@egp11.

* mestre de obras: .
recode	@isko	(	7129	=	8	)	into	@egp11. 

recode	@isko	(	7123	=	9	)	into	@egp11.
recode	@isko	(	8277	=	9	)	into	@egp11.
recode	@isko	(	8332	=	9	)	into	@egp11.
recode	@isko	(	7416	=	9	)	into	@egp11.
recode	@isko	(	7432	=	9	)	into	@egp11.
recode	@isko	(	8212	=	9	)	into	@egp11.
recode	@isko	(	8278	=	9	)	into	@egp11.
recode	@isko	(	8111	=	9	)	into	@egp11.
recode	@isko	(	8271	=	9	)	into	@egp11.
recode	@isko	(	7136	=	9	)	into	@egp11.
recode	@isko	(	7113	=	9	)	into	@egp11.
recode	@isko	(	5122	=	9	)	into	@egp11.
recode	@isko	(	7141	=	9	)	into	@egp11.
recode	@isko	(	7423	=	9	)	into	@egp11.
recode	@isko	(	7214	=	9	)	into	@egp11.
recode	@isko	(	8324	=	9	)	into	@egp11.
recode	@isko	(	7441	=	9	)	into	@egp11.
recode	@isko	(	7133	=	9	)	into	@egp11.
recode	@isko	(	7411	=	9	)	into	@egp11.
recode	@isko	(	7436	=	9	)	into	@egp11.
recode	@isko	(	7422	=	9	)	into	@egp11.
recode	@isko	(	8142	=	9	)	into	@egp11.
recode	@isko	(	7431	=	9	)	into	@egp11.
recode	@isko	(	8162	=	9	)	into	@egp11.
recode	@isko	(	7412	=	9	)	into	@egp11.
recode	@isko	(	8139	=	9	)	into	@egp11.
recode	@isko	(	7435	=	9	)	into	@egp11.
recode	@isko	(	7442	=	9	)	into	@egp11.
recode	@isko	(	8262	=	9	)	into	@egp11.
recode	@isko	(	8153	=	9	)	into	@egp11.
recode	@isko	(	7231	=	9	)	into	@egp11.
recode	@isko	(	8272	=	9	)	into	@egp11.
recode	@isko	(	7213	=	9	)	into	@egp11.
recode	@isko	(	8121	=	9	)	into	@egp11.
recode	@isko	(	7134	=	9	)	into	@egp11.
recode	@isko	(	8231	=	9	)	into	@egp11.
recode	@isko	(	8333	=	9	)	into	@egp11.
recode	@isko	(	8222	=	9	)	into	@egp11.
recode	@isko	(	7135	=	9	)	into	@egp11.
recode	@isko	(	8266	=	9	)	into	@egp11.
recode	@isko	(	8223	=	9	)	into	@egp11.
recode	@isko	(	8124	=	9	)	into	@egp11.
recode	@isko	(	8322	=	9	)	into	@egp11.
recode	@isko	(	7212	=	9	)	into	@egp11.
recode	@isko	(	7421	=	9	)	into	@egp11.
recode	@isko	(	7224	=	9	)	into	@egp11.
recode	@isko	(	7137	=	9	)	into	@egp11.
recode	@isko	(	8261	=	9	)	into	@egp11.
recode	@isko	(	8122	=	9	)	into	@egp11.
recode	@isko	(	8163	=	9	)	into	@egp11.
recode	@isko	(	7211	=	9	)	into	@egp11.
recode	@isko	(	7221	=	9	)	into	@egp11.
recode	@isko	(	7345	=	9	)	into	@egp11.
recode	@isko	(	8269	=	9	)	into	@egp11.
recode	@isko	(	8152	=	9	)	into	@egp11.
recode	@isko	(	7215	=	9	)	into	@egp11.
recode	@isko	(	5141	=	9	)	into	@egp11.
recode	@isko	(	8232	=	9	)	into	@egp11.
recode	@isko	(	7322	=	9	)	into	@egp11.
recode	@isko	(	8281	=	9	)	into	@egp11.
recode	@isko	(	8264	=	9	)	into	@egp11.
recode	@isko	(	7324	=	9	)	into	@egp11.
recode	@isko	(	7131	=	9	)	into	@egp11.
recode	@isko	(	8143	=	9	)	into	@egp11.
recode	@isko	(	7341	=	9	)	into	@egp11.
recode	@isko	(	8312	=	9	)	into	@egp11.
recode	@isko	(	8113	=	9	)	into	@egp11.
recode	@isko	(	8161	=	9	)	into	@egp11.
recode	@isko	(	8253	=	9	)	into	@egp11.
recode	@isko	(	5163	=	9	)	into	@egp11.
recode	@isko	(	8251	=	9	)	into	@egp11.
recode	@isko	(	7332	=	9	)	into	@egp11.
recode	@isko	(	8240	=	9	)	into	@egp11.
recode	@isko	(	7223	=	9	)	into	@egp11.
recode	@isko	(	7433	=	9	)	into	@egp11.
recode	@isko	(	8252	=	9	)	into	@egp11.
recode	@isko	(	8283	=	9	)	into	@egp11.
recode	@isko	(	5169	=	9	)	into	@egp11.
recode	@isko	(	7415	=	9	)	into	@egp11.
recode	@isko	(	7244	=	9	)	into	@egp11.
recode	@isko	(	8221	=	9	)	into	@egp11.
recode	@isko	(	8290	=	9	)	into	@egp11.
recode	@isko	(	8155	=	8	)	into	@egp11.
recode	@isko	(	8172	=	9	)	into	@egp11.
recode	@isko	(	8154	=	8	)	into	@egp11.
recode	@isko	(	7112	=	9	)	into	@egp11.
recode	@isko	(	7121	=	9	)	into	@egp11.
recode	@isko	(	7142	=	9	)	into	@egp11.
recode	@isko	(	7143	=	9	)	into	@egp11.
recode	@isko	(	7323	=	9	)	into	@egp11.
recode	@isko	(	7331	=	9	)	into	@egp11.
recode	@isko	(	7342	=	9	)	into	@egp11.
recode	@isko	(	7346	=	9	)	into	@egp11.
recode	@isko	(	7413	=	9	)	into	@egp11.
recode	@isko	(	7414	=	9	)	into	@egp11.
recode	@isko	(	7434	=	9	)	into	@egp11.
recode	@isko	(	7437	=	9	)	into	@egp11.
recode	@isko	(	8123	=	9	)	into	@egp11.
recode	@isko	(	8131	=	9	)	into	@egp11.
recode	@isko	(	8171	=	9	)	into	@egp11.
recode	@isko	(	8211	=	9	)	into	@egp11.
recode	@isko	(	8224	=	9	)	into	@egp11.
recode	@isko	(	8229	=	9	)	into	@egp11.
recode	@isko	(	8263	=	9	)	into	@egp11.
recode	@isko	(	8265	=	9	)	into	@egp11.
recode	@isko	(	8274	=	9	)	into	@egp11.
recode	@isko	(	8275	=	9	)	into	@egp11.
recode	@isko	(	8279	=	9	)	into	@egp11.
recode	@isko	(	8282	=	9	)	into	@egp11.
recode	@isko	(	8284	=	9	)	into	@egp11.
recode	@isko	(	8285	=	9	)	into	@egp11.
recode	@isko	(	8286	=	9	)	into	@egp11.
recode	@isko	(	8321	=	9	)	into	@egp11.
recode	@isko	(	8323	=	9	)	into	@egp11.
recode	@isko	(	8334	=	9	)	into	@egp11.
recode	@isko	(	9153	=	9	)	into	@egp11.
recode	@isko	(	9313	=	9	)	into	@egp11.
recode	@isko	(	9120	=	10	)	into	@egp11.
recode	@isko	(	9162	=	10	)	into	@egp11.
recode	@isko	(	9311	=	10	)	into	@egp11.
recode	@isko	(	9312	=	10	)	into	@egp11.
recode	@isko	(	9321	=	10	)	into	@egp11.
recode	@isko	(	5121	=	10	)	into	@egp11.
recode	@isko	(	5131	=	10	)	into	@egp11.
recode	@isko	(	5133	=	10	)	into	@egp11.
recode	@isko	(	5230	=	10	)	into	@egp11.
recode	@isko	(	9111	=	10	)	into	@egp11.
recode	@isko	(	9112	=	10	)	into	@egp11.
recode	@isko	(	9131	=	10	)	into	@egp11.
recode	@isko	(	9132	=	10	)	into	@egp11.
recode	@isko	(	9133	=	10	)	into	@egp11.
recode	@isko	(	9141	=	10	)	into	@egp11.
recode	@isko	(	9142	=	10	)	into	@egp11.
recode	@isko	(	9161	=	10	)	into	@egp11.
recode	@isko	(	9322	=	10	)	into	@egp11.
recode	@isko	(	9331	=	10	)	into	@egp11.
recode	@isko	(	9332	=	10	)	into	@egp11.
recode	@isko	(	9333	=	10	)	into	@egp11.
recode	@isko	(	6111	=	11	)	into	@egp11.
recode	@isko	(	6112	=	11	)	into	@egp11.
recode	@isko	(	6113	=	11	)	into	@egp11.
recode	@isko	(	6114	=	11	)	into	@egp11.
recode	@isko	(	6121	=	11	)	into	@egp11.
recode	@isko	(	6122	=	11	)	into	@egp11.
recode	@isko	(	6123	=	11	)	into	@egp11.
recode	@isko	(	6124	=	11	)	into	@egp11.
recode	@isko	(	6129	=	11	)	into	@egp11.
recode	@isko	(	6130	=	11	)	into	@egp11.
recode	@isko	(	6141	=	11	)	into	@egp11.
recode	@isko	(	6142	=	11	)	into	@egp11.
recode	@isko	(	6151	=	11	)	into	@egp11.
recode	@isko	(	6152	=	11	)	into	@egp11.
recode	@isko	(	6153	=	11	)	into	@egp11.
recode	@isko	(	6154	=	11	)	into	@egp11.
recode	@isko	(	6210	=	11	)	into	@egp11.
recode	@isko	(	8331	=	11	)	into	@egp11.
recode	@isko	(	9211	=	11	)	into	@egp11.
recode	@isko	(	9212	=	11	)	into	@egp11.
recode	@isko	(	9213	=	11	)	into	@egp11.
recode	@isko	(	1000	=	1	)	into	@egp11.
recode	@isko	(	1100	=	1	)	into	@egp11.
recode	@isko	(	1200	=	1	)	into	@egp11.
recode	@isko	(	1220	=	1	)	into	@egp11.
recode	@isko	(	1230	=	1	)	into	@egp11.
recode	@isko	(	1250	=	1	)	into	@egp11.
recode	@isko	(	1251	=	1	)	into	@egp11.
recode	@isko	(	2000	=	1	)	into	@egp11.
recode	@isko	(	2100	=	1	)	into	@egp11.
recode	@isko	(	2110	=	1	)	into	@egp11.
recode	@isko	(	2120	=	1	)	into	@egp11.
recode	@isko	(	2130	=	1	)	into	@egp11.
recode	@isko	(	2140	=	1	)	into	@egp11.
recode	@isko	(	2200	=	1	)	into	@egp11.
recode	@isko	(	2210	=	1	)	into	@egp11.
recode	@isko	(	2220	=	1	)	into	@egp11.
recode	@isko	(	2350	=	1	)	into	@egp11.
recode	@isko	(	2400	=	1	)	into	@egp11.
recode	@isko	(	2420	=	1	)	into	@egp11.
recode	@isko	(	2440	=	1	)	into	@egp11.
recode	@isko	(	1140	=	2	)	into	@egp11.
recode	@isko	(	1240	=	2	)	into	@egp11.
recode	@isko	(	1252	=	2	)	into	@egp11.
recode	@isko	(	1300	=	2	)	into	@egp11.
recode	@isko	(	1310	=	2	)	into	@egp11.
recode	@isko	(	2300	=	2	)	into	@egp11.
recode	@isko	(	2321	=	2	)	into	@egp11.
recode	@isko	(	2322	=	2	)	into	@egp11.
recode	@isko	(	2323	=	2	)	into	@egp11.
recode	@isko	(	2330	=	2	)	into	@egp11.
recode	@isko	(	2410	=	2	)	into	@egp11.
recode	@isko	(	2430	=	2	)	into	@egp11.
recode	@isko	(	2450	=	2	)	into	@egp11.
recode	@isko	(	3000	=	2	)	into	@egp11.
recode	@isko	(	3100	=	2	)	into	@egp11.
recode	@isko	(	3110	=	2	)	into	@egp11.
recode	@isko	(	3120	=	2	)	into	@egp11.
recode	@isko	(	3130	=	2	)	into	@egp11.
recode	@isko	(	3140	=	2	)	into	@egp11.
recode	@isko	(	3150	=	2	)	into	@egp11.
recode	@isko	(	3200	=	2	)	into	@egp11.
recode	@isko	(	3210	=	2	)	into	@egp11.
recode	@isko	(	3220	=	2	)	into	@egp11.
recode	@isko	(	3240	=	2	)	into	@egp11.
recode	@isko	(	3400	=	2	)	into	@egp11.
recode	@isko	(	3410	=	2	)	into	@egp11.
recode	@isko	(	3420	=	2	)	into	@egp11.
recode	@isko	(	3440	=	2	)	into	@egp11.
recode	@isko	(	3451	=	2	)	into	@egp11.
recode	@isko	(	3470	=	2	)	into	@egp11.
recode	@isko	(	5150	=	2	)	into	@egp11.
recode	@isko	(	3230	=	3	)	into	@egp11.
recode	@isko	(	3300	=	3	)	into	@egp11.
recode	@isko	(	3430	=	3	)	into	@egp11.
recode	@isko	(	4000	=	3	)	into	@egp11.
recode	@isko	(	4100	=	3	)	into	@egp11.
recode	@isko	(	4110	=	3	)	into	@egp11.
recode	@isko	(	4120	=	3	)	into	@egp11.
recode	@isko	(	4130	=	3	)	into	@egp11.
recode	@isko	(	4140	=	3	)	into	@egp11.
recode	@isko	(	4200	=	3	)	into	@egp11.
recode	@isko	(	4210	=	3	)	into	@egp11.
recode	@isko	(	4220	=	3	)	into	@egp11.
recode	@isko	(	5000	=	3	)	into	@egp11.
recode	@isko	(	5100	=	3	)	into	@egp11.
recode	@isko	(	5110	=	3	)	into	@egp11.
recode	@isko	(	5120	=	3	)	into	@egp11.
recode	@isko	(	5130	=	3	)	into	@egp11.
recode	@isko	(	5200	=	3	)	into	@egp11.
recode	@isko	(	9100	=	3	)	into	@egp11.
recode	@isko	(	9110	=	3	)	into	@egp11.
recode	@isko	(	3452	=	7	)	into	@egp11.
recode	@isko	(	7510	=	7	)	into	@egp11.
recode	@isko	(	5140	=	8	)	into	@egp11.
recode	@isko	(	5164	=	8	)	into	@egp11.
recode	@isko	(	7000	=	8	)	into	@egp11.
recode	@isko	(	7100	=	8	)	into	@egp11.
recode	@isko	(	7110	=	8	)	into	@egp11.
recode	@isko	(	7120	=	8	)	into	@egp11.
recode	@isko	(	7130	=	8	)	into	@egp11.
recode	@isko	(	7140	=	8	)	into	@egp11.
recode	@isko	(	7200	=	8	)	into	@egp11.
recode	@isko	(	7210	=	8	)	into	@egp11.
recode	@isko	(	7220	=	8	)	into	@egp11.
recode	@isko	(	7230	=	8	)	into	@egp11.
recode	@isko	(	7240	=	8	)	into	@egp11.
recode	@isko	(	7300	=	8	)	into	@egp11.
recode	@isko	(	7310	=	8	)	into	@egp11.
recode	@isko	(	7340	=	8	)	into	@egp11.
recode	@isko	(	7400	=	8	)	into	@egp11.
recode	@isko	(	7410	=	8	)	into	@egp11.
recode	@isko	(	7420	=	8	)	into	@egp11.
recode	@isko	(	7430	=	8	)	into	@egp11.
recode	@isko	(	7440	=	8	)	into	@egp11.
recode	@isko	(	7500	=	8	)	into	@egp11.
recode	@isko	(	7520	=	8	)	into	@egp11.
recode	@isko	(	8110	=	8	)	into	@egp11.
recode	@isko	(	8120	=	8	)	into	@egp11.
recode	@isko	(	8150	=	8	)	into	@egp11.
recode	@isko	(	8160	=	8	)	into	@egp11.
recode	@isko	(	8170	=	8	)	into	@egp11.
recode	@isko	(	8210	=	8	)	into	@egp11.
recode	@isko	(	5160	=	9	)	into	@egp11.
recode	@isko	(	7234	=	9	)	into	@egp11.
recode	@isko	(	7320	=	9	)	into	@egp11.
recode	@isko	(	7330	=	9	)	into	@egp11.
recode	@isko	(	7530	=	9	)	into	@egp11.
recode	@isko	(	8000	=	9	)	into	@egp11.
recode	@isko	(	8100	=	9	)	into	@egp11.
recode	@isko	(	8130	=	9	)	into	@egp11.
recode	@isko	(	8140	=	9	)	into	@egp11.
recode	@isko	(	8200	=	9	)	into	@egp11.
recode	@isko	(	8220	=	9	)	into	@egp11.
recode	@isko	(	8230	=	9	)	into	@egp11.
recode	@isko	(	8250	=	9	)	into	@egp11.
recode	@isko	(	8260	=	9	)	into	@egp11.
recode	@isko	(	8270	=	9	)	into	@egp11.
recode	@isko	(	8280	=	9	)	into	@egp11.
recode	@isko	(	8300	=	9	)	into	@egp11.
recode	@isko	(	8310	=	9	)	into	@egp11.
recode	@isko	(	8320	=	9	)	into	@egp11.
recode	@isko	(	8330	=	9	)	into	@egp11.
recode	@isko	(	8400	=	9	)	into	@egp11.
recode	@isko	(	9000	=	9	)	into	@egp11.
recode	@isko	(	9130	=	9	)	into	@egp11.
recode	@isko	(	9140	=	9	)	into	@egp11.
recode	@isko	(	9150	=	9	)	into	@egp11.
recode	@isko	(	9160	=	9	)	into	@egp11.
recode	@isko	(	9300	=	9	)	into	@egp11.
recode	@isko	(	9310	=	9	)	into	@egp11.
recode	@isko	(	9320	=	9	)	into	@egp11.
recode	@isko	(	9330	=	9	)	into	@egp11.
recode	@isko	(	6000	=	10	)	into	@egp11.
recode	@isko	(	6100	=	10	)	into	@egp11.
recode	@isko	(	6110	=	10	)	into	@egp11.
recode	@isko	(	6120	=	10	)	into	@egp11.
recode	@isko	(	6134	=	10	)	into	@egp11.
recode	@isko	(	6140	=	10	)	into	@egp11.
recode	@isko	(	6150	=	10	)	into	@egp11.
recode	@isko	(	9200	=	10	)	into	@egp11.
recode	@isko	(	9210	=	10	)	into	@egp11.
recode	@isko	(	6131	=	11	)	into	@egp11.
recode	@isko	(	6132	=	11	)	into	@egp11.
recode	@isko	(	6133	=	11	)	into	@egp11.
recode	@isko	(	6200	=	11	)	into	@egp11.
 
* Promove os empregadores (não rurais).
if (isic <> 10 &  isic <> 100  & PosicaoOcup = 2) @egp11 = 5.

* Promove os empregadores rurais.
if (@egp11 = 1 & PosicaoOcup = 2 & isic=10) @egp11 = 6.
if (@egp11 = 2 & PosicaoOcup = 2 & isic=10) @egp11 = 6.
if (@egp11 = 3 & PosicaoOcup = 2 & isic=10) @egp11 = 6.
if (@egp11 = 4 & PosicaoOcup = 2 & isic=10) @egp11 = 6.
if (@egp11 = 8 & PosicaoOcup = 2 & isic=10) @egp11 = 6.
if (@egp11 = 9 & PosicaoOcup = 2 & isic=10) @egp11 = 6.
if (@egp11 = 10 & PosicaoOcup = 2 & isic=10) @egp11 = 6.
if (@egp11 = 11 & PosicaoOcup = 2) @egp11 = 6.

* Promove os conta-própria rurais.
if (@egp11 = 11 & PosicaoOcup = 0) @egp11 = 7.
if (@egp11 = 11 & PosicaoOcup = 3) @egp11 = 7.
if (@egp11 = 11 & VD5005=0) @egp11 = 7.

* para o censo de 2010 (tentativa de categorizar casos de ocupação missing) .
if ((v4010 = 0 | v4010=9629) & isic = 10 & trtot > 4 & PosicaoOcup = 1) @egp11 = 8.
if ((v4010 = 0 | v4010=9629) & isic = 10 & trtot <= 4 & PosicaoOcup = 1) @egp11 = 11.
if ((v4010 = 0 | v4010=9629) & isic = 10 & PosicaoOcup = 2) @egp11 = 6.
if ((v4010 = 0 | v4010=9629) & isic = 10 & (PosicaoOcup = 0 |PosicaoOcup = 3)) @egp11 = 7.
if ((v4010 = 0 | v4010=9629) & isic <> 10 &  PosicaoOcup = 2 ) @egp11 = 5.

execute.

value labels @egp11
  1 'I. Higher professionals'
  2 'II. Lower professionals'
  3 'IIIa. Routine non-manuals, higher degree'
  4 'IIIb. Routine non-manuals, lower degree'
  5 'IVa2. Proprietors and employers'
  6 'IVc1 - Rural employers'
  7 'IVc2. Self-employed farmers and subsistence agriculture workers'
  8 'V.  Technicians and Supervisors of manual workers'
  9 'VI. Skilled workers'
 10 'VIIa Semi-, and unskilled workers'
 11 'VIIb Agricultural workers'.



/*/* Comando (fim).
