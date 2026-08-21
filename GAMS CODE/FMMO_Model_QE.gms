$title GAMS CODE FOR "THE GEOGRAPHIC FOOTPRINT OF U.S. DAIRY POLICY," QUANTITATIVE ECONOMICS
*THIS FILE RUNS SEVERAL COUNTERFACTUAL MODELS DEFINED IN SEPARATE SECTIONS:
*(0) PARAMETER CALCULATIONS OR MODEL EQUATIONS ARE USED TO DERIVE KEY MODEL ELASTICITIES AS REPORTED IN SECTION 5 AND SUPPLEMENTAL APPENDIX D
*(1) REMOVAL OF FMMO PRICE DISTORTIONS (MODEL FMMO)
*(2) IDEAL COMPONENT WEDGES THAT MAXIMIZE DOMESTIC CONSUMER SURPLUS WHILE PRESERVING REGIONAL LAND RENTS (MODEL BETTERWEDGESREG)
*(3) REGIONAL MILK QUOTAS THAT MAXIMIZE DOMESTIC CONSUMER SURPLUS WHILE PRESERVING REGIONAL LAND + QUOTA RENTS (MODEL QUOTAREG)
*(4) INCREASE OF FARM MILK TRADE COSTS UNTIL FARM MILK TRADE VANISHES (MODEL MILKTRADECOST)
*(5) COMPONENT WEDGES RESHUFFLING TO INCREASE DOMESTIC LAND RENT TRANSFERS WITHOUT HURTING DOMESTIC CONSUMERS (MODEL BETTERSUPPORT)
*(6) COMPARISON BETWEEN OPTIMAL WEDGES AND QUOTA WHEN EPSILON0 IS MADE EQUAL TO 100 (VERY ELASTIC FOREIGN DEMAND)
*(7) REGIONAL MILK QUOTAS THAT MAXIMIZE DOMESTIC CONSUMER SURPLUS WHILE PRESERVING REGIONAL LAND + QUOTA RENTS BUT KEEPING THE FULL ARBITRAGE ON FARM MILK TRADE (MODEL QUOTAREGALT)
*(8) SIMULTANEOUS CHANGES IN COMPONENT PRICES TO DERIVE AGGREGATE COMPONENT DEMAND ELASTICITIES FOR BEVERAGE AND OTHER PRODUCTS (MODEL COMPDEMAND) SUPPORTING CLAIMS MADE IN APPENDIX G

*THE CODE ALSO EXPORTS KEY METRICS THAT ARE USED TO SUBSTANTIATE STANDALONE CLAIMS MADE IN THE MANUSCRIPT. A STANDALONE CLAIM IS ONE BASED
*ON MODEL RESULTS THAT ARE NOT ACTUALLY REPORTED IN MANUSCRIPT OR APPENDIX TABLES.


*THIS CODE WAS TESTED USING GAMS STUDIO 1.25.5 ON A 14-INCH 2021 APPLE MACBOOKPRO WITH APPLE M1 CHIP and 32GB OF MEMORY, RUNNING MACOS SONOMA 14.7

** SET OPTIONS

option nlp = conopt3 ;
option decimals = 8;

** DEFINE SETS, SCALARS, AND PARAMETERS

sets        I           all regions         /Northeast, Appalachian, Florida, Southeast, Upper-Midwest, Central, Mideast, California, Pacific-Northwest, Southwest, Arizona, Unregulated, ROW/
            DOMI(I)     domestic regions    /Northeast, Appalachian, Florida, Southeast, Upper-Midwest, Central, Mideast, California, Pacific-Northwest, Southwest, Arizona, Unregulated/
        
            N           products            /Beverage, Softs, Cheese, Butter-Powder/
            SUBN(N)                         /Softs, Cheese, Butter-Powder/
        
            K           components          /Fat, Protein, Other-Solids/
        
            L           crops               /Grains, Oilseeds, Hay, Silage, Other-Crops /
            SUBL(L)                         /Grains, Oilseeds, Hay, Silage/
            SUBSUBL(SUBL)                   /Grains, Oilseeds, Hay/
            ;
        
alias       (I, J)
            (DOMI, DOMJ, DOMII)
            (L, M)
            (SUBL, SUBM)
            (SUBSUBL, SUBSUBM)
            (N, O)
            (K, S)
            ;
            
scalars     EPSILON         "absolute value of dairy product demand elasticity, 0 < e < 1"          /0.2/
            EPSILON0        "absolute value of foreign dairy product demand elasticity"             /1.3/  
            KAPPA           "elasticity of substitution between dairy products, k > e"              /0.5/            
            KAPPA0          "foreign elasticity of substitution between dairy products"             /1.8/ 
            RHO             "elasticity of substitution across feed crops, r > 0"                   /0.2/    
            THETA           "heterogeneity parameter, > 1"                                          /1.05/
            ;
            
parameters        
SIGMA(N) "elasticity of substitution between dairy product n origin regions, > 0"
    /   beverage        2.5
        softs           4
        cheese          4
        butter-powder   7 /
        
SIGMA0(SUBN) "foreign elasticity of substitution between dairy product n origins, >0"
    /   softs           8
        cheese          8
        butter-powder   10 /
        
        
VARSIGMA(N) "elasticity of substitution between components used in product n, > 0"
    /   beverage        0.3
        softs           0.8
        cheese          0.2
        butter-powder   0.2 /        
;
            

** IMPORT CSV DATA

**dairy products
parameter   VALPRODSHIP(I, J, N)    "value of shipments of product N from region I to region J"
/
$ondelim
$include "CSV DATA FILES/CFS_Dairy_for_GAMS_Gravity.csv"
$offdelim
/ ;

**milk components
parameter   VALCOMPUSE(DOMI, N, K)  "value of component K used in product N in region I"
/
$ondelim
$include "CSV DATA FILES/Comp_Value_for_GAMS.csv"
$offdelim
/;

parameter   QTYCOMPUSE(DOMI, N, K)  "quantity of component K used in product N in region I"
/
$ondelim
$include "CSV DATA FILES/Comp_Quantity_for_GAMS.csv"
$offdelim
/;

**farm milk
table       QTYMILKSHIP(DOMI, DOMJ) "quantity of milk shipped from region I to region J"
$ondelim
$include "CSV DATA FILES/Milk_Shipments.csv"
$offdelim
;

**crops
parameter VALCROPPROD(DOMI, L)  "value of crop production"
/
$ondelim
$include "CSV DATA FILES/Crop_Prod_Values.csv"
$offdelim
/ ;

parameter VALCROPUSE(DOMI, SUBSUBL)  "value of crop use in dairy (other than silage)"
/
$ondelim
$include "CSV DATA FILES/Dairy_Crop_Cons_Value.csv"
$offdelim
/
;

parameter   CROPACRES(DOMI, L)      "acres of crop L harvested in region I"
/
$ondelim
$include "CSV DATA FILES/Crop_Areas.csv"
$offdelim 
/ ;

parameter   CROPLANDRENT(DOMI)        "Rent of cropland in region I in $/acre"
/
$ondelim
$include "CSV DATA FILES/Cropland_Rent.csv"
$offdelim
/ ;

** SUM UP SOME DATA

parameters  VALPRODUCTS(I, N)           "value of dairy product N produced in region I (or exported to US regions for ROW)"
            QTYMILKUSE(DOMJ)            "quantity of milk used (received) in region DOMJ"
            VALMILKUSE(DOMJ)            "value of milk used in region DOMJ"
            MILKPRICE(DOMI)             "price of milk in region DOMI"
            VALMILKPROD(DOMI)           "value of milk produced in region DOMI"
            QTYMILKPROD(DOMI)           "quantity of milk produced in region DOMI (used only to compute aggregate milk production and price effects)"
            PC                          "total value of dairy consumption (including foreign)" 
            ;
            
VALPRODUCTS(I, N) = sum(J, VALPRODSHIP(I, J, N)) ;
QTYMILKUSE(DOMJ) = sum(DOMI, QTYMILKSHIP(DOMI, DOMJ)) ;
VALMILKUSE(DOMJ) = sum((N, K), VALCOMPUSE(DOMJ, N, K)) ;
MILKPRICE(DOMI) = VALMILKUSE(DOMI) / QTYMILKUSE(DOMI) ;
VALMILKPROD(DOMI) = sum(DOMJ, QTYMILKSHIP(DOMI, DOMJ) * MILKPRICE(DOMJ)) ;
QTYMILKPROD(DOMI) = VALMILKPROD(DOMI)  / MILKPRICE(DOMI) ;
PC = sum((I, N), VALPRODUCTS(I,N)) ;


** CALCULATE PARAMETERS DEFINED BY THE DATA

parameters  A0                          "share of domestic production in total dairy product revenue"
            A1(DOMI)                    "share of region DOMI in value of domestic dairy production"
            A2(I,N)                     "share of product N in value of dairy production in region DOMI (or in US exports for ROW)"
            A3(I,J,N)                   "share of region I production of N shipped to J"
            MU2(DOMI, DOMJ)             "share of milk used in region DOMJ shipped from region DOMI"
            THETA1(DOMI)                "overall cost share of milk in dairy products made in region DOMI"
            THETA2(DOMI, K)             "share of component K in total milk value in region DOMI"
            THETA3(DOMI, N, K)          "share of product N in value of component K in region DOMI"
            CHI(DOMI, N, K)             "share of product N in volume of component K in region DOMI"
            RHO1(DOMI)                  "share of region DOMI in value of domestic crop production"
            RHO2(DOMI, L)               "share of crop L in region i value of crop production"
            XI1(I)                      "share of total feed crop value used in region I"
            XI2(DOMI, SUBL)             "share of crop SUBL in value of region DOMI's feed crop use"
            PHI0                        "feed crop share of the milk dollar"
            VARPHI1(DOMI)               "land share of the crop dollar in region DOMI"
            PI(DOMI,L)                  "share of crop L in region DOMI's total cropland"
            ;

A0 = sum((DOMJ, N), VALPRODUCTS(DOMJ, N)) / sum((I, N), VALPRODUCTS(I, N))  ;
            
A1(DOMI) = sum(N, VALPRODUCTS(DOMI, N)) / sum((DOMJ, N), VALPRODUCTS(DOMJ, N)) ;

A2(I, N) = VALPRODUCTS(I, N) / sum(O, VALPRODUCTS(I, O)) ;

A3(I, J, N)$(VALPRODUCTS(I, N) > 0) = VALPRODSHIP(I, J, N) / VALPRODUCTS(I, N) ;

MU2(DOMI, DOMJ) = QTYMILKSHIP(DOMI, DOMJ) / QTYMILKUSE(DOMJ) ;

THETA1(DOMI) = VALMILKUSE(DOMI) / sum(N, VALPRODUCTS(DOMI, N)) ;

THETA2(DOMI, K) = sum(N, VALCOMPUSE(DOMI, N, K)) / sum((N, S), VALCOMPUSE(DOMI, N, S)) ;

THETA3(DOMI, N, K) = VALCOMPUSE(DOMI, N, K) / sum(O, VALCOMPUSE(DOMI, O, K)) ;

CHI(DOMI, N, K) = QTYCOMPUSE(DOMI, N, K) / sum(O, QTYCOMPUSE(DOMI, O, K)) ;

RHO1(DOMI) = sum(L, VALCROPPROD(DOMI, L)) / sum((DOMJ, L), VALCROPPROD(DOMJ, L)) ;

RHO2(DOMI, L) = VALCROPPROD(DOMI, L) / sum(M, VALCROPPROD(DOMI, M)) ;

XI1(DOMI) = (VALCROPPROD(DOMI,"silage") + sum(SUBSUBL, VALCROPUSE(DOMI, SUBSUBL))) / sum((DOMJ, SUBL), VALCROPPROD(DOMJ, SUBL)) ;

XI2(DOMI, SUBSUBL) = VALCROPUSE(DOMI, SUBSUBL) / (VALCROPPROD(DOMI,"silage") + sum(SUBSUBM, VALCROPUSE(DOMI, SUBSUBM))) ;
XI2(DOMI, "silage") = VALCROPPROD(DOMI,"silage") / (VALCROPPROD(DOMI,"silage") + sum(SUBSUBL, VALCROPUSE(DOMI, SUBSUBL))) ;

PHI0 = sum(DOMI, VALCROPPROD(DOMI,"silage") + sum(SUBSUBL, VALCROPUSE(DOMI, SUBSUBL))) / sum(DOMI, VALMILKUSE(DOMI)) ;

VARPHI1(DOMI) = CROPLANDRENT(DOMI)*sum(L, CROPACRES(DOMI,L)) / sum(L, VALCROPPROD(DOMI, L)) ;

PI(DOMI, L) = CROPACRES(DOMI, L) / sum(M, CROPACRES(DOMI, M)) ;


** CALCULATE PARAMETERS DEDUCED FROM OTHER PARAMETERS

parameters  B1(J)                   "expenditure share of region J in total dairy consumption"
            B2(J, N)                "expenditure share of dairy product N in region J"
            B3(I, J, N)             "budget share of product N from region I in region J"
            PSI2(DOMI, N)           "cost share of milk components for product N in region DOMI"
            C3(DOMI, N, K)          "cost share of component K for product N in region DOMI"
            PSI                     "milk share of domestic dairy product dollar"
            MU1(DOMI)               "share of region DOMI in value of domestic milk production"
            A(DOMI,DOMJ)            "share of region DOMI milk value shipped to region DOMJ"
            RHO0                    "share of non-feed crop in value of total crop production"
            PHI1(DOMI)              "feed crop share of the milk dollar in region DOMI"
            VARPHI2(DOMI,L)         "land share of the crop dollar for crop L in region DOMI"
            DELTA(DOMI, N, K)       "component price wedge relative to butter-powder"
            ;
            
B1(J) = A0 * sum(DOMI, A1(DOMI) * sum(N, A2(DOMI, N) * A3(DOMI, J, N))) + (1 - A0) * sum(N, A2("ROW", N) * A3("ROW", J, N)) ;

B2(J, N)$(B1(J) > 0) = [A0 * sum(DOMI, A1(DOMI) * A2(DOMI, N) * A3(DOMI, J, N)) + (1 - A0) * A2("ROW", N) * A3("ROW", J, N)] / B1(J) ;

B3(DOMI, J, N)$(B2(J, N) > 0) = A0 * A1(DOMI) * A2(DOMI, N) * A3(DOMI, J, N) / (B1(J) * B2(J, N)) ;
B3("ROW", J, N)$(B2(J, N) > 0) = (1 - A0) * A2("ROW", N) * A3("ROW", J, N) / (B1(J) * B2(J, N)) ;

PSI2(DOMI, N) = THETA1(DOMI) / A2(DOMI, N) * sum(K, THETA2(DOMI, K) * THETA3(DOMI, N, K)) ;

C3(DOMI, N, K) = THETA1(DOMI) * THETA2(DOMI, K) * THETA3(DOMI, N, K) / (A2(DOMI, N) * PSI2(DOMI, N)) ;

PSI = sum(DOMI, A1(DOMI) * THETA1(DOMI)) ;

MU1(DOMI) = sum(DOMJ, A1(DOMJ) * THETA1(DOMJ) * MU2(DOMI, DOMJ)) / PSI ;

A(DOMI, DOMJ) = A1(DOMJ) * THETA1(DOMJ) * MU2(DOMI, DOMJ) / (PSI * MU1(DOMI)) ;

RHO0 = sum(DOMI, RHO2(DOMI, "Other-Crops") * RHO1(DOMI)) ;

XI1("ROW") = 1 -sum(DOMI, XI1(DOMI)) ;

PHI1(DOMI) = XI1(DOMI) * PHI0 / (MU1(DOMI) * (1 - XI1("ROW"))) ;

VARPHI2(DOMI, L)$(RHO2(DOMI, L) > 0) = PI(DOMI, L) * VARPHI1(DOMI) / RHO2(DOMI, L) ;

DELTA(DOMI, N, K) = THETA3(DOMI, N, K) / THETA3(DOMI, "Butter-Powder", K) * CHI(DOMI, "Butter-Powder", K) / CHI(DOMI, N, K) ;


****************************************************************************
***************************COUNTERFACTUAL MODELS****************************
****************************************************************************

**EXOGENOUS CHANGES FOR COUNTERFACTUAL MODELS

parameters  DELTAHAT(DOMI, N, K)      "hat value of policy distortion parameter (for MODEL FMMO)" 
            TAU(DOMI, DOMJ)           "relative change in farm milk trade cost (for MODEL MILKTRADECOST)"
            MPRICEEX(DOMI)            "exogenous change in farm milk price (for MODEL MILKSUPELAS)"            
            VINDEXEX(DOMI,N)          "exogenous change in price index for components used in product N in region DOMI (for MODEL COMPDEMAND)"
            ; 

**VARIABLES

variables   C(I, DOMJ, N)             "relative change in region DOMJ demand for product N from origin I"
            C0(DOMI, N)               "relative change in foreign demand for product N from origin DOMI"
            PINDEX1(DOMJ)             "relative change in dairy product price index in region DOMJ"
            PINDEX2(DOMJ, N)          "relative change in price of product N in region DOMJ"
            PINDEX10                  "relative change in foreign price index for dairy products"
            PINDEX20(SUBN)            "relative change in foreign price of product SUBN"
            Q(DOMI, N)                "relative change in production of product N in region DOMI"
            Z(DOMI, N, K)             "relative change in derived demand for component K used in product N in region DOMI"
            VINDEX(DOMI, N)           "relative change in price index for components used in product N in region DOMI"
            V(DOMI, K)                "relative change in price of component K in region DOMI"
            P(I, N)                   "relative change in producer price of product N in region I"
            MQUANT(DOMI)              "relative change in milk production in region DOMI"
            MSHIP(DOMI, DOMJ)         "relative change in quantity of milk used in region DOMJ that comes from region DOMI"
            MPRICE(DOMI)              "relative change in milk price in region DOMI"
            MMPRICE(DOMI)             "relative change in processor milk price in region DOMI (for MODELS QUOTAREG AND QUOTAREGALT)"      
            WINDEX1(DOMI)             "relative change in feed crop price index in region DOMI"
            R(DOMI, L)                "relative change in land rent for crop L in region DOMI"
            W(DOMI, L)                "relative change in producer price of crop L in region DOMI"
            Y(DOMI, L)                "relative change in production of crop L in region DOMI"
            OBJ                       "constant objective variable for optimization algorithm"
            DELTAHATOPT(DOMI, N, K)   "wedges as choice variables"
            DOMCONSSURP               "domestic consumer surplus (for MODELS BETTERWEDGESREG, QUOTAREG, AND QUOTAREGALT)"  
            TRANSFER                  "aggregate land rent transfer (for MODEL BETTERSUPPORT)"      
            ;                    

**EQUATIONS

equations   EQ1(I, DOMJ, N)         "domestic consumer demand in region DOMJ for dairy products from region I"
            EQ2(DOMI, SUBN)         "foreign consumer demand for US regional dairy products"
            EQ3(DOMJ)               "definition of region DOMJ dairy product price index"
            EQ4(DOMJ, N)            "definition of region DOMJ price index for product N"
            EQ5                     "definition of foreign dairy product price index"
            EQ6(SUBN)               "definition of foreign price index for product N"
            EQ7(DOMI, N)            "dairy product market clearing"            
            EQ8(DOMI, N, K)         "derived demand for component K used in product N in region DOMI"
            EQ8OPT(DOMI, N, K)      "EQ8 with endogenous deltahat (for for MODELS BETTERWEDGESREG AND BETTERSUPPORT)"
            EQ9(DOMI, N)            "definition of component price index for product N in region DOMI"
            EQ9OPT(DOMI, N)         "EQ9 with endogenous deltahat (for MODELS BETTERWEDGESREG AND BETTERSUPPORT)"               
            EQ10(DOMI, N)           "dairy product price relationship"
            EQ10EX(DOMI, N)         "EQ10 with exogenous VINDEX (for MODEL COMPDEMAND)" 
            EQ11(DOMI)              "milk market clearing"
            EQ11TRC(DOMI)           "EQ11 for MODEL MILKTRADECOST"             
            EQ12(DOMI, K)           "component market clearing"
            EQ13(DOMI)              "milk value and component value relationship"
            EQ13OPT(DOMI)           "EQ13 with endogenous deltahat (for for MODELS BETTERWEDGESREG and BETTERSUPPORT)" 
            EQ13QU(DOMI)            "milk processing value and component value relationship for MODELS QUOTAREG and QUOTAREGALT"   
            EQ14A(DOMJ, DOMI)       "milk price co-movements between trading regions"
            EQ14B(DOMJ, DOMI)       "milk price co-movements between trading regions"
            EQ14AQU(DOMJ, DOMI)     "EQ14A for MODELS QUOTAREG and QUOTAREGALT"
            EQ14BQU(DOMJ, DOMI)     "EQ14B for MODEL QUOTAREGALT"            
            EQ14ATRC(DOMJ, DOMI)    "EQ14A for MODEL MILKTRADECOST"
            EQ14BTRC(DOMJ, DOMI)    "EQ14B for MODEL MILKTRADECOST"            
            EQ15(DOMI)              "milk price and crop price relationship"
            EQ15MILKSUPPLY(DOMI)    "EQ15 for MODEL MILKSUPELAS"
            EQ16(DOMI)              "demand for silage"
            EQ17(DOMI)              "definition of feed crop price index"
            EQ18(DOMI, L)           "crop price and land rent relationship"
            EQ19(DOMI, SUBSUBL)     "feed crop price change equal to one for SUBSUBL crops"
            EQ20(DOMI, L)           "crop supply"
            EQ21                    "objective function"           
            EQ22                    "domestic consumer surplus (objective for MODELS BETTERWEDGESREG and QUOTAREG/QUOTAREGALT)"
            EQ23(DOMI)              "regional land rents preserved"             
            EQ24(DOMI)              "sum of regional land rents and regional quota value unchanged (for MODEL QUOTAREG/QUOTAREGALT)" 
            EQ25                    "aggregate land rent transfer (objective for MODEL BETTERSUPPORT)"     
            EQ26                    "domestic consumer surplus preserved"
            ;

EQ1(I, DOMJ, N)..       C(I, DOMJ, N)       =E= PINDEX1(DOMJ) ** (KAPPA - EPSILON) * PINDEX2(DOMJ, N) ** (SIGMA(N) - KAPPA) * P(I, N) ** (-SIGMA(N)) ;
EQ2(DOMI, SUBN)..       C0(DOMI, SUBN)      =E= PINDEX10 ** (KAPPA0 - EPSILON0) * PINDEX20(SUBN) ** (SIGMA0(SUBN) - KAPPA0) * P(DOMI,SUBN) ** (-SIGMA0(SUBN)) ;
EQ3(DOMJ)..             PINDEX1(DOMJ)       =E= sum(N, B2(DOMJ, N) * (PINDEX2(DOMJ, N)) ** (1 - KAPPA)) ** (1 / (1 - KAPPA)) ;
EQ4(DOMJ, N)..          PINDEX2(DOMJ, N)    =E= sum(I, B3(I, DOMJ, N) * (P(I, N)) ** (1 - SIGMA(N))) ** (1 / (1 - SIGMA(N))) ;
EQ5..                   PINDEX10            =E= sum(SUBN, B2("ROW", SUBN) * (PINDEX20(SUBN) ** (1 - KAPPA0))) ** (1 / (1 - KAPPA0)) ;
EQ6(SUBN)..             PINDEX20(SUBN)      =E= sum(DOMI, B3(DOMI, "ROW", SUBN) * (P(DOMI, SUBN)) ** (1 - SIGMA0(SUBN))) ** (1 / (1 - SIGMA0(SUBN))) ;
EQ7(DOMI, N)..          Q(DOMI, N)          =E= sum(DOMJ, A3(DOMI, DOMJ, N) * C(DOMI, DOMJ, N)) + A3(DOMI, "ROW", N) * C0(DOMI, N) ;
EQ8(DOMI, N, K)..       Z(DOMI, N, K)       =E= Q(DOMI, N) * VINDEX(DOMI, N) ** VARSIGMA(N) * (DELTAHAT(DOMI, N, K) * V(DOMI, K)) ** (- VARSIGMA(N)) ;
EQ9(DOMI, N)..          VINDEX(DOMI, N)     =E= sum(K, C3(DOMI, N, K) * (DELTAHAT(DOMI, N, K) * V(DOMI, K)) ** (1 - VARSIGMA(N))) ** (1 / (1 - VARSIGMA(N))) ;

EQ10(DOMI, N)..         P(DOMI, N)          =E= PSI2(DOMI, N) * VINDEX(DOMI, N) + 1 - PSI2(DOMI, N) ;

EQ10EX(DOMI, N)..       P(DOMI, N)          =E= PSI2(DOMI, N) * VINDEXEX(DOMI, N) + 1 - PSI2(DOMI, N) ;

EQ11(DOMI)..            MQUANT(DOMI)        =E= sum(DOMJ, A(DOMI, DOMJ) * MSHIP(DOMI, DOMJ)) ;
EQ11TRC(DOMI)..         MQUANT(DOMI)        =E= sum(DOMJ, A(DOMI, DOMJ) * TAU(DOMI, DOMJ) * MSHIP(DOMI, DOMJ)) ;

EQ12(DOMI, K)..         sum(N, CHI(DOMI, N, K) * Z(DOMI, N, K)) =E= sum(DOMJ, MU2(DOMJ, DOMI) * MSHIP(DOMJ, DOMI)) ;

EQ13(DOMI)..            MPRICE(DOMI) * sum(DOMJ, MU2(DOMJ, DOMI) * MSHIP(DOMJ, DOMI))  =E=  sum(K, THETA2(DOMI, K) * V(DOMI, K) * sum(N, THETA3(DOMI, N, K) * DELTAHAT(DOMI, N, K) * Z(DOMI, N, K))) ;
EQ14A(DOMJ, DOMI)$(MU2(DOMJ, DOMI)>0)..      0 =E= MSHIP(DOMJ, DOMI) * (MPRICE(DOMI) - MPRICE(DOMJ)) ;
EQ14B(DOMJ, DOMI)$(MU2(DOMJ, DOMI)>0)..      MPRICE(DOMI)  =L= MPRICE(DOMJ) ;

EQ14ATRC(DOMJ, DOMI)$(MU2(DOMJ, DOMI)>0)..   0 =E= MSHIP(DOMJ, DOMI) * (MPRICE(DOMI) - TAU(DOMJ, DOMI) * MPRICE(DOMJ)) ;
EQ14BTRC(DOMJ, DOMI)$(MU2(DOMJ, DOMI)>0)..   MPRICE(DOMI)  =L= TAU(DOMJ, DOMI) * MPRICE(DOMJ) ;

EQ13QU(DOMI)..          MMPRICE(DOMI) * sum(DOMJ, MU2(DOMJ, DOMI) * MSHIP(DOMJ, DOMI))  =E= sum(K, THETA2(DOMI, K) * V(DOMI, K) * sum(N, THETA3(DOMI, N, K) * DELTAHAT(DOMI, N, K) * Z(DOMI, N, K))) ;
EQ14AQU(DOMJ, DOMI)$(MU2(DOMJ, DOMI)>0)..   0 =E= MSHIP(DOMJ, DOMI) * (MMPRICE(DOMI) - MMPRICE(DOMJ)) ;
EQ14BQU(DOMJ, DOMI)$(MU2(DOMJ, DOMI)>0)..   MMPRICE(DOMI)  =L= MMPRICE(DOMJ) ;

EQ8OPT(DOMI, N, K)..    Z(DOMI, N, K)       =E= Q(DOMI, N) * VINDEX(DOMI, N) ** VARSIGMA(N) * (DELTAHATOPT(DOMI, N, K) * V(DOMI, K)) ** (- VARSIGMA(N)) ;
EQ9OPT(DOMI, N)..       VINDEX(DOMI, N)     =E= sum(K, C3(DOMI, N, K) * (DELTAHATOPT(DOMI, N, K) * V(DOMI, K)) ** (1 - VARSIGMA(N))) ** (1 / (1 - VARSIGMA(N))) ;                    
EQ13OPT(DOMI)..         MPRICE(DOMI) * sum(DOMJ, MU2(DOMJ, DOMI) * MSHIP(DOMJ, DOMI))  =E= sum(K, THETA2(DOMI, K) * V(DOMI, K) * sum(N, THETA3(DOMI, N, K) * DELTAHATOPT(DOMI, N, K) * Z(DOMI, N, K))) ;

EQ15(DOMI)..            MPRICE(DOMI)        =E= PHI1(DOMI) * WINDEX1(DOMI) + 1 - PHI1(DOMI) ;

EQ15MILKSUPPLY(DOMI)..  MPRICEEX(DOMI)      =E= PHI1(DOMI) * WINDEX1(DOMI) + 1 - PHI1(DOMI) ;

EQ16(DOMI)..            Y(DOMI, "SILAGE")   =E= MQUANT(DOMI) * WINDEX1(DOMI) ** RHO * W(DOMI, "SILAGE") ** (- RHO) ;

EQ17(DOMI)..            WINDEX1(DOMI)       =E= (XI2(DOMI, "SILAGE") * W(DOMI, "SILAGE") ** (1 - RHO) + 1 - XI2(DOMI, "SILAGE")) ** (1 / (1 - RHO)) ;

EQ18(DOMI, L)..         W(DOMI, L)          =E= VARPHI2(DOMI, L) * R(DOMI, L) + 1 - VARPHI2(DOMI, L) ;

EQ19(DOMI, SUBSUBL)..   W(DOMI, SUBSUBL)    =E= 1 ;
            
EQ20(DOMI, L)..         Y(DOMI, L)          =E= R(DOMI, L) ** (THETA - 1) / sum(M, PI(DOMI, M) * R(DOMI, M) ** THETA) ** ((THETA - 1) / THETA) ;

EQ21..                  OBJ                 =E=  1 ;

EQ22..                  DOMCONSSURP         =E= [sum(DOMI, B1(DOMI) * (1 - PINDEX1(DOMI) ** (1 - EPSILON)) ) / (1 - EPSILON) ] * 1E6  ;    

EQ23(DOMI)..            0                   =L=  A0 * PSI * PHI0 / (1 - XI1("ROW")) * RHO1(DOMI) / (1 - RHO0) * sum(L, VARPHI2(DOMI, L) * RHO2(DOMI, L) * (R(DOMI, L) * Y(DOMI, L) - 1))   ;

EQ24(DOMI)..            0                   =L=  A0 * PSI * PHI0 / (1 - XI1("ROW")) * RHO1(DOMI) / (1 - RHO0) * sum(L, VARPHI2(DOMI, L) * RHO2(DOMI, L) * (R(DOMI, L) * Y(DOMI, L) - 1)) +
                                                 A0 * PSI * PHI0 / (1 - XI1("ROW")) * XI1(DOMI) / PHI1(DOMI) * (MMPRICE(DOMI) - MPRICE(DOMI)) * MQUANT(DOMI) ;

EQ25..                 TRANSFER             =L= sum(DOMI,  RHO1(DOMI) / sum(DOMJ, RHO1(DOMJ) * VARPHI1(DOMJ)) * sum(L, VARPHI2(DOMI, L) * RHO2(DOMI, L) * (R(DOMI, L) * Y(DOMI, L) - 1))) ; 
EQ26..                 0                    =L= sum(DOMI, B1(DOMI) * (1 - PINDEX1(DOMI) ** (1 - EPSILON)) )    ;    

* Non-negativity constraints
V.LO(DOMI, K) = 0.001 ;
P.LO(I, N) = 0 ;
MSHIP.LO(DOMI, DOMJ) = 0 ;
MPRICE.LO(DOMI) = 0 ;
R.LO(DOMI, L) = 0 ;
W.LO(DOMI, SUBL) = 0 ;

PINDEX2.LO(DOMI, N) = 0 ;
PINDEX1.LO(DOMI) = 0 ;
PINDEX20.LO(SUBN) = 0 ;
PINDEX10.LO = 0 ;
WINDEX1.LO(DOMI) = 0 ;
VINDEX.LO(DOMI, N) = 0 ;

DELTAHATOPT.LO(DOMI, N, K) = 0.0001 ;
MMPRICE.LO(DOMI) = 0 ;

* Fixed Values To Close the Model
P.FX("ROW", N) = 1 ;
W.FX(DOMI,"Other-Crops") = 1 ;
MSHIP.FX(DOMI, DOMJ)$(MU2(DOMI, DOMJ) = 0) = 1 ;
C0.FX(DOMI, "Beverage") = 1 ;
DELTAHATOPT.FX(DOMI, "Butter-Powder", K) = 1 ;


*parameters that are used to generate tables after model runs
parameters MUTILDE(DOMI)                 "share of regional milk use in total milk use (in value)"  
           DELTAMILKVAL(DOMI)            "change in regional milk production value"
           DELTAMILKVALTOT               "change in total milk production value"
           DELTAMILKPRICE(DOMI)          "change in regional milk price"
           DELTAMILKPRICETOT             "change in aggregate milk price (uses baseline milk quantity data)" 
           DELTAMILKPROD(DOMI)           "change in regional quantity of milk produced"
           DELTAMILKPRODTOT              "change in total quantity of milk produced (uses baseline milk quantity data)"
           DELTAMILKUSE(DOMI)            "change in regional quantity of milk used"
           DELTAMILKUSETOT               "change in total quantity of milk used (uses baseline milk quantity data)"
           MILKPRODSHARESHIP(DOMI)       "regional share of milk production shipped out in baseline"
           MILKPRODSHARESHIPPRIME(DOMI)  "regional share of milk production shipped out in counterfactual"
           DELTAMILKPRODSHARESHIP(DOMI)  "change in regional share of milk production shipped out"
           MILKPRODSHARESHIPTOT          "total share of milk produced that is shipped out in baseline"
           MILKPRODSHARESHIPTOTPRIME     "total share of milk produced that is shipped out in counterfactual"
           DELTAMILKPRODSHARESHIPTOT     "change in total share of milk production shipped out"  
           MILKUSESHARESHIP(DOMI)        "regional share of milk use shipped in in baseline"
           MILKUSESHARESHIPPRIME(DOMI)   "regional share of milk use shipped in in counterfactual"
           DELTAMILKUSESHARESHIP(DOMI)   "change in regional share of milk use shipped in"
           ;
           
MUTILDE(DOMI) = sum(DOMJ, MU1(DOMJ) * A(DOMJ, DOMI)) ;
MILKPRODSHARESHIP(DOMI) = 1 - A(DOMI, DOMI) - 1 / MU1(DOMI) * sum(DOMJ$(ORD(DOMJ) NE ORD(DOMI)), MU2(DOMJ, DOMI) * MUTILDE(DOMI)) ;
MILKPRODSHARESHIPTOT = sum(DOMI$(MILKPRODSHARESHIP(DOMI) > 0), MU1(DOMI) * MILKPRODSHARESHIP(DOMI)) ;
MILKUSESHARESHIP(DOMI) = 1 - MU2(DOMI, DOMI) - 1 / MUTILDE(DOMI) * sum(DOMJ$(ORD(DOMJ) NE ORD(DOMI)), MU1(DOMI) * A(DOMI, DOMJ))   ;

                 
parameters    DAIRYQUANT(DOMI, N)    "regional change in dairy product quantity produced"
              DAIRYVAL(DOMI)         "regional change in total dairy product value"
              DAIRYVALTOT            "aggregate change in total dairy product value" 
              DAIRYEXP(DOMI)         "regional change in dairy export value"
              DAIRYEXPSHARE(DOMI)    "regional share of total US dairy exports"
              DAIRYEXPTOT            "total change in dairy export value"
              ;

parameters       CS(DOMI)        "regional change in consumer surplus relative to regional consumption value"
                 CSDOM           "domestic change in consumer surplus relative to domestic consumption value"
                 CSFOR           "foreign change in consumer surplus relative to foreign consumption value"  
                 CSTOT           "change in total consumer surplus relative to total consumption value"
                 PS(DOMI)        "relative change in regional land rents"
                 PSDOM           "relative change in domestic land rents"
                 PSTOT           "change in domestic land rents relative to total consumption value"
                 WELF(DOMI)      "change in regional welfare relative to total consumption value"
                 WELFDOM         "change in domestic welfare relative to total consumption value"
                 WELFFOR         "change in foreign welfare relative to total consumption value"
                 WELFTOT         "change in total welfare relative to total consumption value"
                 PC              "total value of dairy consumption"
                 CSVAL(DOMI)     "change in regional consumer surplus in USD"     
                 CSDOMVAL        "change in domestic consumer surplus in USD"
                 CSFORVAL        "change in foreign consumer surplus in USD"
                 CSTOTVAL        "change in total consumer surplus in USD"
                 PSVAL(DOMI)     "change in regional land rents in USD"
                 PSDOMVAL        "chnage in domestic land rents in USD"  
                 WELFVAL(DOMI)   "change in regional welfare in USD"
                 WELFDOMVAL      "change in domestic welfare in USD"
                 WELFTOTVAL      "change in total welfare in USD"
                 ;
                 

*************************************************************
********(0) DERIVATION OF SELECTED MODEL ELASTICITIES********
*************************************************************
**THIS PART OF THE CODE GENERATES THE VALUES REPORTED
*IN TABLE 1 OF SECTION 5 AND TABLES D.1-D.4 OF THE SUPPLEMENTAL APPENDIX
*THIS PART OF THE CODE ALSO SUBSTANTIATES THE CLAIM THAT THE SHARE OF FARM MILK IN TOTAL DAIRY PRODUCT VALUE IS 18% (SECTION 1)

$if not set run_section1 $goto skip_section1

execute_unload "../MANUSCRIPT CLAIMS/Section 1/output_ex.gdx", PSI ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 1/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 1/milkshareofdairydollar.csv" symb=PSI' ;
$label skip_section1

***************************************************
*(0.1) REGIONAL MILK SUPPLY ELASTICITIES (TABLE 1)

$if not set run_table1 $goto skip_table1

MPRICEEX(DOMI) = 1.0001 ;

MODEL MILKSUPELAS /EQ15MILKSUPPLY,EQ16,EQ17,EQ18,EQ19,EQ20,EQ21/ ;

* Starting Values
R.L(DOMI, L) = 1 ;
W.L(DOMI, L) = 1 ;
WINDEX1.L(DOMI) = 1 ;
MSHIP.L(DOMI, DOMJ) = 1 ;
MQUANT.L(DOMI) = 1 ;
MPRICE.L(DOMI) = 1 ;
Y.L(DOMI, L) = 1 ;

SOLVE MILKSUPELAS USING NLP MAXIMIZING OBJ ;

parameters  MILKSUPPLY(DOMI)               "regional milk supply elasticity"
            MILKSUPPLYTOT                  "US milk supply elasticity"
            ;

MILKSUPPLY(DOMI) = ( MQUANT.L(DOMI) - 1 ) / ( MPRICEEX(DOMI) - 1 ) ;
MILKSUPPLYTOT = sum(DOMI, MILKSUPPLY(DOMI) * MU1(DOMI)) ;

display MILKSUPPLY, MILKSUPPLYTOT ;

execute_unload "../MANUSCRIPT TABLES/Table 1/output_ex.gdx", MILKSUPPLY, MILKSUPPLYTOT ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 1/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 1/milksupply.csv" symb=MILKSUPPLY' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 1/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 1/milksupplytot.csv" symb=MILKSUPPLYTOT' ;

parameter LANDSHARESILREV(DOMI)    "regional land share of silage revenue"
          SILFEEDSHARE(DOMI)       "regional share of silage in dairy feed crop expenditure" 
          ;

LANDSHARESILREV(DOMI) = VARPHI2(DOMI,"Silage") ;
SILFEEDSHARE(DOMI) = XI2(DOMI,"Silage") ;      

execute_unload "../MANUSCRIPT CLAIMS/Section 5.7/output_ex.gdx", MILKSUPPLY, LANDSHARESILREV, PHI1, SILFEEDSHARE ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 5.7/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 5.7/milksupply.csv" symb=MILKSUPPLY' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 5.7/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 5.7/landsharesilrev.csv" symb=LANDSHARESILREV' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 5.7/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 5.7/feedsharemilkrev.csv" symb=PHI1' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 5.7/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 5.7/silfeedshare.csv" symb=SILFEEDSHARE' ;

$label skip_table1

*************************************************************
*(0.2) REGIONAL DAIRY PRODUCT DEMAND ELASTICITIES (TABLE D.1)

$if not set run_tablesd1_d3 $goto skip_tablesd1_d3

parameters ELASDEMDOM(DOMI, N)    "regional demand elasticity for dairy products"
           ELASDEMFOR(SUBN)       "ROW demand elasticity for dairy products"
           ;

ELASDEMDOM(DOMI, N) = (KAPPA - EPSILON) * B2(DOMI, N) - KAPPA ;
ELASDEMFOR(SUBN) = (KAPPA0 - EPSILON0) * B2("ROW", SUBN) - KAPPA0 ;

display ELASDEMDOM, ELASDEMFOR ;

execute_unload "../MANUSCRIPT TABLES/Table D.1/output_ex.gdx", ELASDEMDOM, ELASDEMFOR ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table D.1/output_ex" format=csv output="../MANUSCRIPT TABLES/Table D.1/elasdemdom.csv" symb=ELASDEMDOM' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table D.1/output_ex" format=csv output="../MANUSCRIPT TABLES/Table D.1/elasdemfor.csv" symb=ELASDEMFOR' ;


*********************************************************
*(0.3) REGIONAL FEED CROP DEMAND ELASTICITIES (TABLE D.2)


parameter ELASFEED(DOMI, SUBL)   "regional feed crop demand elasticity"
          ;
ELASFEED(DOMI, SUBL) = RHO * XI2(DOMI, SUBL) - RHO ;

display ELASFEED ;

execute_unload "../MANUSCRIPT TABLES/Table D.2/output_ex.gdx", ELASFEED ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table D.2/output_ex" format=csv output="../MANUSCRIPT TABLES/Table D.2/elasfeed.csv" symb=ELASFEED' ;


****************************************************
*(0.4) REGIONAL CROP OUTPUT ELASTICITIES (TABLE D.3)


parameter ELASCROP(DOMI, L)     "regional crop output supply elasticity"
          ;
ELASCROP(DOMI, L)$(VARPHI2(DOMI, L) > 0) = (THETA-1) * (1 - PI(DOMI, L)) / VARPHI2(DOMI, L) ;

display ELASCROP ;

execute_unload "../MANUSCRIPT TABLES/Table D.3/output_ex.gdx", ELASCROP ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table D.3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table D.3/elascrop.csv" symb=ELASCROP' ;

parameter OILSEEDAREASHARE(DOMI)    "regional cropland share of oilseeds"
          ELASCROPAREA(DOMI, L)     "regional crop acreage elasticity"
          ;
          
OILSEEDAREASHARE(DOMI)  = PI(DOMI, "Oilseeds") ;
ELASCROPAREA(DOMI, L)$(VARPHI2(DOMI, L) > 0) = THETA * (1 - PI(DOMI, L)) / VARPHI2(DOMI, L) ;

execute_unload "../MANUSCRIPT CLAIMS/Section 5.6/output_ex.gdx", OILSEEDAREASHARE, ELASCROPAREA ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 5.6/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 5.6/oilseedareashare.csv" symb=OILSEEDAREASHARE' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 5.6/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 5.6/elascroparea.csv" symb=ELASCROPAREA' ;

$label skip_tablesd1_d3

**************************************************************
*(0.5) REGIONAL MILK COMPONENT DEMAND ELASTICITIES (TABLE D.4)

MODEL COMPDEMAND /EQ1,EQ2,EQ3,EQ4,EQ5,EQ6,EQ7,EQ10EX,EQ21/ ;

$if not set run_tabled4 $goto skip_tabled4

parameters ELASCOMPDEMAND(DOMI, N)     "regional component demand elasticity"
           ;          

LOOP ((DOMII, O),
  VINDEXEX(DOMI , N) = 1 ;
  VINDEXEX(DOMII, O) = 1.0001 ;
* Starting Values
  P.L(DOMI, N) = 1 ;
  PINDEX2.L(DOMI, N) = 1 ;
  PINDEX1.L(DOMI) = 1 ;
  PINDEX20.L(SUBN) = 1 ;
  PINDEX10.L = 1 ;
  C.L(I, DOMJ, N) = 1 ;
  C0.L(DOMI, SUBN) = 1 ;
  Q.L(DOMI, N) = 1 ;
  
  SOLVE COMPDEMAND USING NLP MAXIMIZING OBJ ;
  ELASCOMPDEMAND(DOMII, O) = ( Q.L(DOMII, O)- 1 ) / ( VINDEXEX(DOMII, O) - 1 ) ;
)
;

display  ELASCOMPDEMAND ;

execute_unload "../MANUSCRIPT TABLES/Table D.4/output_ex.gdx", ELASCOMPDEMAND ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table D.4/output_ex" format=csv output="../MANUSCRIPT TABLES/Table D.4/elascompdemand.csv" symb=ELASCOMPDEMAND' ;

parameter  COMPBUTPOWCOSTSHARE(DOMI)    "regional share of milk components in butter-powder products"
           ;
           
COMPBUTPOWCOSTSHARE(DOMI) =  PSI2(DOMI, "Butter-powder")  ;       

display COMPBUTPOWCOSTSHARE ;

execute_unload "../MANUSCRIPT CLAIMS/Section 5.8/output_ex.gdx", COMPBUTPOWCOSTSHARE ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 5.8/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 5.8/compbutpowcostshare.csv" symb=COMPBUTPOWCOSTSHARE' ;

$label skip_tabled4

*************************************************************
****(1) COUNTERFACTUAL REMOVAL OF FMMO COMPONENT WEDGES******
*************************************************************
**THIS PART OF THE CODE PROVIDES THE VALUES REPORTED IN TABLES 2-6 AND THOSE REPORTED IN
*TABLE E.1 (SENSITIVITY ANALYSIS)
*IT ALSO PROVIDES SUPPORT FOR SOME OF THE NUMBERS REPORTED IN SECTION 1 THAT ARE DIRECTLY
*TAKEN FROM THESE TABLES, NOTABLY REGARDING WELFARE EFFECTS OF FMMO REMOVAL

***********************************************
*(1.1) EXTRACT WEDGE PARAMETERS (FOR TABLE 2)

$if not set run_table2 $goto skip_table2

display DELTA ;

execute_unload "../MANUSCRIPT TABLES/Table 2/output_ex.gdx", DELTA ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 2/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 2/delta.csv" symb=DELTA' ;

$label skip_table2

********************************************************************
**THEN RUN MAIN COUNTERFACTUAL WITH BASELINE SHAPE PARAMETER VALUES
********************************************************************

MODEL FMMO /EQ1, EQ2, EQ3, EQ4, EQ5, EQ6, EQ7, EQ8, EQ9, EQ10, EQ11, EQ12, EQ13, EQ14A, EQ14B, EQ15, EQ16, EQ17, EQ18, EQ19, EQ20, EQ21/ ;

$if not set run_maincounterfactual $goto skip_maincounterfactual

DELTAHAT(DOMI, N, K) = 1 / DELTA(DOMI, N, K) ;

* Starting Values
P.L(DOMI, N) = 1 ;
PINDEX2.L(DOMI, N) = 1 ;
PINDEX1.L(DOMI) = 1 ;
PINDEX20.L(SUBN) = 1 ;
PINDEX10.L = 1 ;
C.L(I, DOMJ, N) = 1 ;
C0.L(DOMI, SUBN) = 1 ;
Q.L(DOMI, N) = 1 ;
V.L(DOMI, K) = 1 ;
VINDEX.L(DOMI, N) = 1 ;
Z.L(DOMI, N, K) = 1 ;
R.L(DOMI, L) = 1 ;
W.L(DOMI, L) = 1 ;
WINDEX1.L(DOMI) = 1 ;
MSHIP.L(DOMI, DOMJ) = 1 ;
MQUANT.L(DOMI) = 1 ;
MPRICE.L(DOMI) = 1 ;
Y.L(DOMI, L) = 1 ;

SOLVE FMMO USING NLP MAXIMIZING OBJ ;

********************************************
*(1.2) GENERATE TABLE 3 (FARM MILK MARKET)
*NOTE THAT AGGREGATE VALUE SHARES OF MILK PRODUCED THAT IS SHIPPED OUT ARE EQUAL TO
*AGGREGATE VALUE SHARES OF MILK USED THAT IS SHIPPED IN AND ARE NOT RE_COMPUTED HERE


DELTAMILKVAL(DOMI) = MPRICE.L(DOMI) * MQUANT.L(DOMI) - 1 ;
DELTAMILKVALTOT    = sum(DOMI, DELTAMILKVAL(DOMI) * MU1(DOMI)) ;

DELTAMILKPRICE(DOMI) = MPRICE.L(DOMI) - 1 ;
DELTAMILKPRICETOT = sum(DOMI, MU1(DOMI) * MPRICE.L(DOMI) * MQUANT.L(DOMI)) / sum(DOMI, QTYMILKPROD(DOMI) / sum(DOMJ, QTYMILKPROD(DOMJ)) * MQUANT.L(DOMI)) - 1 ;

DELTAMILKPROD(DOMI) = MQUANT.L(DOMI) - 1 ;
DELTAMILKPRODTOT = sum(DOMI, QTYMILKPROD(DOMI) / sum(DOMJ, QTYMILKPROD(DOMJ)) * DELTAMILKPROD(DOMI)) ; 

DELTAMILKUSE(DOMI) = sum(DOMJ, MU2(DOMJ, DOMI) * MSHIP.L(DOMJ, DOMI)) -1 ;
DELTAMILKUSETOT = sum(DOMI, QTYMILKUSE(DOMI) / sum(DOMJ, QTYMILKUSE(DOMJ)) * DELTAMILKUSE(DOMI)) ;

MILKPRODSHARESHIPPRIME(DOMI) = sum(DOMJ$(ORD(DOMJ) NE ORD(DOMI)), A(DOMI, DOMJ) * MSHIP.L(DOMI, DOMJ) / MQUANT.L(DOMI)) - sum(DOMJ$(ORD(DOMJ) NE ORD(DOMI)), MU2(DOMJ, DOMI) * MUTILDE(DOMI) / MU1(DOMI) * MSHIP.L(DOMJ, DOMI) / MQUANT.L(DOMI))   ;
DELTAMILKPRODSHARESHIP(DOMI)$(MILKPRODSHARESHIP(DOMI) > 0) = MILKPRODSHARESHIPPRIME(DOMI) - MILKPRODSHARESHIP(DOMI) ;

MILKPRODSHARESHIPTOTPRIME = sum(DOMI$(MILKPRODSHARESHIPPRIME(DOMI) > 0), MU1(DOMI) * MPRICE.L(DOMI) * MQUANT.L(DOMI) * MILKPRODSHARESHIPPRIME(DOMI)) / sum(DOMI, MU1(DOMI) * MPRICE.L(DOMI) * MQUANT.L(DOMI)) ;
DELTAMILKPRODSHARESHIPTOT = MILKPRODSHARESHIPTOTPRIME - MILKPRODSHARESHIPTOT ;

MILKUSESHARESHIPPRIME(DOMI) = (sum(DOMJ$(ORD(DOMJ) NE ORD(DOMI)), MU2(DOMJ, DOMI) * MSHIP.L(DOMJ, DOMI)) - sum(DOMJ$(ORD(DOMJ) NE ORD(DOMI)), A(DOMI, DOMJ) * MU1(DOMI) / MUTILDE(DOMI) * MSHIP.L(DOMI, DOMJ))) / sum(DOMJ, MU2(DOMJ, DOMI) * MSHIP.L(DOMJ, DOMI))   ;
DELTAMILKUSESHARESHIP(DOMI)$(MILKUSESHARESHIP(DOMI) > 0) = MILKUSESHARESHIPPRIME(DOMI) - MILKUSESHARESHIP(DOMI) ;


display DELTAMILKVALTOT, DELTAMILKPRICETOT, DELTAMILKPRODTOT, DELTAMILKUSETOT, MILKPRODSHARESHIPTOT,
        DELTAMILKPRODSHARESHIPTOT ;

execute_unload "../MANUSCRIPT TABLES/Table 3/output_ex.gdx", DELTAMILKVAL, MU1, DELTAMILKVALTOT, DELTAMILKPRICE, DELTAMILKPRICETOT,
                                DELTAMILKPROD, DELTAMILKPRODTOT, DELTAMILKUSE, DELTAMILKUSETOT,
                                MILKPRODSHARESHIP, MILKPRODSHARESHIPTOT, DELTAMILKPRODSHARESHIP, DELTAMILKPRODSHARESHIPTOT,
                                MILKUSESHARESHIP, DELTAMILKUSESHARESHIP;

execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/deltamilkval.csv" symb=DELTAMILKVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/milkvalshare.csv" symb=MU1' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/deltamilkvaltot.csv" symb=DELTAMILKVALTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/deltamilkprice.csv" symb=DELTAMILKPRICE' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/deltamilkpricetot.csv" symb=DELTAMILKPRICETOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/deltamilkprod.csv" symb=DELTAMILKPROD' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/deltamilkprodtot.csv" symb=DELTAMILKPRODTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/deltamilkuse.csv" symb=DELTAMILKUSE' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/deltamilkusetot.csv" symb=DELTAMILKUSETOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/milkprodshareship.csv" symb=MILKPRODSHARESHIP' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/milkprodshareshiptot.csv" symb=MILKPRODSHARESHIPTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/deltamilkprodshareship.csv" symb=DELTAMILKPRODSHARESHIP' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/deltamilkprodshareshiptot.csv" symb=DELTAMILKPRODSHARESHIPTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/milkuseshareship.csv" symb=MILKUSESHARESHIP' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 3/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 3/deltamilkuseshareship.csv" symb=DELTAMILKUSESHARESHIP' ;

**ADDTIONAL CLAIMS MADE IN SECTION 6.1 
parameter     MILKSHIP(DOMI, DOMJ)             "change in milk shipments"
              DELTAFEEDCROPUSE(DOMI, SUBL)     "change in the use of feed crop SUBL in region DOMI"
              ;

MILKSHIP(DOMI, DOMJ) = MSHIP.L(DOMI, DOMJ) - 1 ;
DELTAFEEDCROPUSE(DOMI, SUBL) = MQUANT.L(DOMI) * WINDEX1.L(DOMI)**RHO * W.L(DOMI, SUBL)**(-RHO) -1 ;
display A, MILKSHIP, DELTAFEEDCROPUSE ;

execute_unload "../MANUSCRIPT CLAIMS/Section 6.1/output1_ex.gdx", MILKSHIP, DELTAFEEDCROPUSE ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.1/output1_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.1/milkship.csv" symb=MILKSHIP' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.1/output1_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.1/deltafeedcropuse.csv" symb=DELTAFEEDCROPUSE' ;


**************************************
*(1.3) GENERATE TABLE 4 (COMPONENTS)

parameters     COMPPRICE(DOMI,K)         "regional change in component price in butter-powder"
               COMPCOSTINDEX(DOMI, N)    "regional change in component price index for a dairy product"
               ;
               
COMPPRICE(DOMI,K) = V.L(DOMI, K) - 1 ;
COMPCOSTINDEX(DOMI, N) = VINDEX.L(DOMI, N) - 1 ;

display COMPPRICE, COMPCOSTINDEX ;

execute_unload "../MANUSCRIPT TABLES/Table 4/output_ex.gdx", COMPPRICE, COMPCOSTINDEX ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 4/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 4/compprice.csv" symb=COMPPRICE' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 4/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 4/compcostindex.csv" symb=COMPCOSTINDEX' ;


*********************************************
*(1.4) GENERATE TABLE 5 (DAIRY PRODUCTS)

DAIRYQUANT(DOMI, N) = Q.L(DOMI, N) -1 ;
DAIRYVAL(DOMI) = sum(N, A2(DOMI, N) * P.L(DOMI, N) * Q.L(DOMI, N)) - 1 ;
DAIRYVALTOT = sum(DOMI, A1(DOMI) * DAIRYVAL(DOMI)) ;
DAIRYEXP(DOMI) = sum(N, A2(DOMI, N) * A3(DOMI, "ROW", N) * P.L(DOMI, N) * C0.L(DOMI, N)) / sum(N, A2(DOMI, N) * A3(DOMI, "ROW", N)) -1 ;
DAIRYEXPSHARE(DOMI) = A1(DOMI) * sum(N, A2(DOMI, N) * A3(DOMI, "ROW", N)) / sum(DOMJ, A1(DOMJ) * sum(N, A2(DOMJ, N) * A3(DOMJ, "ROW", N))) ;
DAIRYEXPTOT = PINDEX10.L ** (1 - EPSILON0) - 1 ;

display DAIRYVALTOT, DAIRYEXPTOT ;

execute_unload "../MANUSCRIPT TABLES/Table 5/output_ex.gdx", DAIRYQUANT, DAIRYVAL, DAIRYVALTOT, A1, DAIRYEXP, DAIRYEXPTOT, DAIRYEXPSHARE ;

execute 'gdxdump "../MANUSCRIPT TABLES/Table 5/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 5/dairyquant.csv" symb=DAIRYQUANT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 5/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 5/dairyval.csv" symb=DAIRYVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 5/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 5/dairyvaltot.csv" symb=DAIRYVALTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 5/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 5/dairyvalshare.csv" symb=A1' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 5/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 5/dairyexp.csv" symb=DAIRYEXP' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 5/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 5/dairyexptot.csv" symb=DAIRYEXPTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 5/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 5/dairyexpshare.csv" symb=DAIRYEXPSHARE' ;

**ADDITIONAL CLAIMS MADE IN SECTION 6.3 REGARDING SHIPMENTS OF BEVERAGE PRODUCTS FROM THE SOUTHEAST
parameter DELTABEVSHIP(DOMI)     "change in shipment of beverages from the Southeast to region DOMI"
          ;
DELTABEVSHIP(DOMI) = C.L("Southeast",DOMI,"Beverage") - 1 ;
display DELTABEVSHIP ;

execute_unload "../MANUSCRIPT CLAIMS/Section 6.3/output_ex.gdx", DELTABEVSHIP ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.3/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.3/deltabevship.csv" symb=DELTABEVSHIP' ;


*********************************************
*(1.5) GENERATE TABLE 6 (WELFARE EFFECTS)

CS(DOMI)   =  (1 - PINDEX1.L(DOMI) ** (1 - EPSILON)) / (1 - EPSILON) ;
CSDOM      =  sum(DOMI, B1(DOMI) * CS(DOMI)) / sum(DOMI, B1(DOMI));
CSFOR     =  (1 - PINDEX10.L ** (1 - EPSILON0)) / (1 - EPSILON0) ;
CSTOT      = sum(DOMI, B1(DOMI) * CS(DOMI)) + B1("ROW") * CSFOR  ;  
PS(DOMI)   = sum(L, VARPHI2(DOMI, L) * RHO2(DOMI, L) / VARPHI1(DOMI) * (R.L(DOMI, L) * Y.L(DOMI, L) - 1)) ;
PSDOM      = sum(DOMI, PS(DOMI) * VARPHI1(DOMI) * RHO1(DOMI)) / sum(DOMJ, VARPHI1(DOMJ) * RHO1(DOMJ)) ;
PSTOT      = (A0 * PSI * PHI0 ) / ((1 - RHO0) * (1 - XI1("ROW"))) * sum(DOMI,  RHO1(DOMI) * VARPHI1(DOMI) * PS(DOMI))  ;
WELF(DOMI) = B1(DOMI) * CS(DOMI) + (A0 * PSI * PHI0 * RHO1(DOMI)) / ((1 - RHO0) * (1 - XI1("ROW"))) * VARPHI1(DOMI) * PS(DOMI) ;
WELFDOM    = sum(DOMI, WELF(DOMI)) ;
WELFFOR    = B1("ROW") * CSFOR ;
WELFTOT    = WELFDOM + WELFFOR ;

CSVAL(DOMI)    = CS(DOMI) * B1(DOMI) * PC ;
CSDOMVAL       = sum(DOMI, CSVAL(DOMI)) ;
CSFORVAL       = CSFOR * B1("ROW") * PC ;
CSTOTVAL       = CSDOMVAL + CSFORVAL ;
PSVAL(DOMI)    = (A0 * PSI * PHI0 ) / ((1 - RHO0) * (1 - XI1("ROW"))) * RHO1(DOMI) * VARPHI1(DOMI) * PS(DOMI) * PC ;
PSDOMVAL       = sum(DOMI, PSVAL(DOMI)) ;
WELFVAL(DOMI)  = CSVAL(DOMI) + PSVAL(DOMI)   ;
WELFDOMVAL     = sum(DOMI, WELFVAL(DOMI)) ;   
WELFTOTVAL     = WELFDOMVAL + CSFORVAL   ;

execute_unload "../MANUSCRIPT TABLES/Table 6/output_ex.gdx", CS, CSDOM, CSFOR, CSTOT, PS, PSDOM, PSTOT, WELF, WELFDOM, WELFFOR, WELFTOT,
                                CSVAL, PSVAL, WELFVAL, CSDOMVAL, CSFORVAL, CSTOTVAL, PSDOMVAL, WELFDOMVAL, WELFTOTVAL ;

execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/cs.csv" symb=CS' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/csdom.csv" symb=CSDOM' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/csfor.csv" symb=CSFOR' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/cstot.csv" symb=CSTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/ps.csv" symb=PS' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/psdom.csv" symb=PSDOM' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/pstot.csv" symb=PSTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/welf.csv" symb=WELF' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/welfdom.csv" symb=WELFDOM' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/welffor.csv" symb=WELFFOR' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/welftot.csv" symb=WELFTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/csval.csv" symb=CSVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/psval.csv" symb=PSVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/welfval.csv" symb=WELFVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/csdomval.csv" symb=CSDOMVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/csforval.csv" symb=CSFORVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/cstotval.csv" symb=CSTOTVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/psdomval.csv" symb=PSDOMVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/welfdomval.csv" symb=WELFDOMVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 6/output_ex" format=csv output="../MANUSCRIPT TABLES/Table 6/welftotval.csv" symb=WELFTOTVAL' ;

**ADDITIONAL CLAIMS MADE IN SECTION 6.4 
display P.L, PINDEX2.L, PINDEX20.L ;

parameter PRODUCERPRICES(DOMI, N)   "regional producer prices of dairy products"
          PRINDEX(DOMI, N)          "regional consumer prices for dairy products"
          PRINDEXFOR(SUBN)          "foreign consumer prices for dairy products"
          CALIFBUTTPOWEXPSHARE      "expenditure share on butter-powder products in California"
          CALIFBEVLOCEXPSHARE       "expenditure share on local beverages in California"
          CALIFBEVPRINDEX           "change in the beverage price index in California"
          
          SILAGEPR(DOMI)            "change in the regional silage price"
          SILAGEAREASHARE(DOMI)     "baseline cropland share of silage"
          ;
          
PRODUCERPRICES(DOMI, N) = P.L(DOMI, N) - 1 ;
PRINDEX(DOMI, N) = PINDEX2.L(DOMI, N) - 1 ;
PRINDEXFOR(SUBN)  = PINDEX20.L(SUBN) - 1 ;
CALIFBUTTPOWEXPSHARE = B2("California","Butter-Powder") ;
CALIFBEVLOCEXPSHARE = B3("California","California","Beverage") ;
CALIFBEVPRINDEX = PINDEX2.L("California","Beverage") -1 ;
SILAGEPR(DOMI)  = W.L(DOMI,"Silage") - 1 ;
SILAGEAREASHARE(DOMI) = PI(DOMI, "Silage") ;

display PRODUCERPRICES, CALIFBUTTPOWEXPSHARE, CALIFBEVLOCEXPSHARE, CALIFBEVPRINDEX ;

execute_unload "../MANUSCRIPT CLAIMS/Section 6.4/output_ex.gdx", PRODUCERPRICES, PRINDEX, PRINDEXFOR, CALIFBUTTPOWEXPSHARE, CALIFBEVLOCEXPSHARE, CALIFBEVPRINDEX,
                                                                              SILAGEPR, SILAGEAREASHARE ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.4/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.4/producerprices.csv" symb=PRODUCERPRICES' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.4/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.4/prindex.csv" symb=PRINDEX' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.4/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.4/prindexfor.csv" symb=PRINDEXFOR' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.4/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.4/califbuttpowexpshare.csv" symb=CALIFBUTTPOWEXPSHARE' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.4/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.4/califbevlocexpshare.csv" symb=CALIFBEVLOCEXPSHARE' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.4/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.4/califbevprindex.csv" symb=CALIFBEVPRINDEX' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.4/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.4/silagepr.csv" symb=SILAGEPR' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.4/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.4/silageareashare.csv" symb=SILAGEAREASHARE' ;

$label skip_maincounterfactual


************************************************
*(1.6) SENSITIVITY ANALYSIS (TABLE E.1)

****THIS CODE RUNS THE 7 SCENARIOS IN TABLE E.1, AND ADDITIONAL SCENARIOS TO SUPPORT THE CLAIM IN SECTION 6.5 THAT
*VARSIGMA^CHEESE IS THE MOST CRITICAL SUBSTITUTION ELASTICITY AMONG DAIRY PRODUCTS

$if not set run_sensitivity $goto skip_sensitivity

file cmd /''/;

set SC scenarios /s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11/ ;

parameters
SEPSILON0(SC) "absolute value of foreign dairy product demand elasticity"
/ s1    1.3
  s2    2.0
  s3    1.3
  s4    1.3  
  s5    1.3    
  s6    1.3
  s7    1.3
  s8    1.3  
  s9    1.3    
  s10   1.3
  s11   1.3/
  
SKAPPA0(SC) "foreign elasticity of substitution between dairy products"
/ s1    1.8 
  s2    3.0 
  s3    1.8 
  s4    1.8 
  s5    1.8
  s6    1.8
  s7    1.8
  s8    1.8 
  s9    1.8
  s10   1.8
  s11   1.8/           
            
SRHO(SC) "elasticity of substitution across feed crops, r > 0" 
/ s1    0.2
  s2    0.2
  s3    0.2 
  s4    0.2
  s5    0.5
  s6    0.2
  s7    0.2
  s8    0.2
  s9    0.2
  s10   0.2 
  s11   0.2/
            
STHETA(SC) "heterogeneity parameter, > 1"
/ s1    1.05
  s2    1.05 
  s3    1.05 
  s4    1.05
  s5    1.05  
  s6    1.01  
  s7    1.10
  s8    1.05
  s9    1.05 
  s10   1.05 
  s11   1.05/
;
            
table
SVARSIGMA(N,SC) "elasticity of substitution between components used in product n, > 0"
                      s1      s2     s3      s4      s5     s6     s7     s8     s9    s10    s11
    beverage          0.3    0.3   0.15     0.6     0.3    0.3    0.3    0.6    0.3    0.3    0.3 
    softs             0.8    0.8    0.4     1.6     0.8    0.8    0.8    0.8    1.6    0.8    0.8  
    cheese            0.2    0.2    0.1     0.4     0.2    0.2    0.2    0.2    0.2    0.4    0.2
    butter-powder     0.2    0.2    0.1     0.4     0.2    0.2    0.2    0.2    0.2    0.2    0.4
;

DELTAHAT(DOMI, N, K) = 1 / DELTA(DOMI, N, K) ;

* Starting Values
P.L(DOMI, N) = 1 ;
PINDEX2.L(DOMI, N) = 1 ;
PINDEX1.L(DOMI) = 1 ;
PINDEX20.L(SUBN) = 1 ;
PINDEX10.L = 1 ;
C.L(I, DOMJ, N) = 1 ;
C0.L(DOMI, SUBN) = 1 ;
Q.L(DOMI, N) = 1 ;
V.L(DOMI, K) = 1 ;
VINDEX.L(DOMI, N) = 1 ;
Z.L(DOMI, N, K) = 1 ;
R.L(DOMI, L) = 1 ;
W.L(DOMI, L) = 1 ;
WINDEX1.L(DOMI) = 1 ;
MSHIP.L(DOMI, DOMJ) = 1 ;
MQUANT.L(DOMI) = 1 ;
MPRICE.L(DOMI) = 1 ;
Y.L(DOMI, L) = 1 ;

LOOP(SC,

EPSILON0 = SEPSILON0(SC) ;
KAPPA0 = SKAPPA0(SC) ;
VARSIGMA(N) = SVARSIGMA(N,SC) ;
RHO = SRHO(SC) ;
THETA = STHETA(SC) ;

SOLVE FMMO USING NLP MAXIMIZING OBJ ;

DELTAMILKVAL(DOMI) = MPRICE.L(DOMI) * MQUANT.L(DOMI) - 1 ;
DELTAMILKVALTOT    = sum(DOMI, DELTAMILKVAL(DOMI) * MU1(DOMI)) ;

MILKPRODSHARESHIPPRIME(DOMI) = sum(DOMJ$(ORD(DOMJ) NE ORD(DOMI)), A(DOMI, DOMJ) * MSHIP.L(DOMI, DOMJ) / MQUANT.L(DOMI)) - sum(DOMJ$(ORD(DOMJ) NE ORD(DOMI)), MU2(DOMJ, DOMI) * MUTILDE(DOMI) / MU1(DOMI) * MSHIP.L(DOMJ, DOMI) / MQUANT.L(DOMI))   ;
MILKPRODSHARESHIPTOTPRIME = sum(DOMI$(MILKPRODSHARESHIPPRIME(DOMI) > 0), MU1(DOMI) * MPRICE.L(DOMI) * MQUANT.L(DOMI) * MILKPRODSHARESHIPPRIME(DOMI)) / sum(DOMI, MU1(DOMI) * MPRICE.L(DOMI) * MQUANT.L(DOMI)) ;
DELTAMILKPRODSHARESHIPTOT = MILKPRODSHARESHIPTOTPRIME - MILKPRODSHARESHIPTOT ;

DAIRYVAL(DOMI) = sum(N, A2(DOMI, N) * P.L(DOMI, N) * Q.L(DOMI, N)) - 1 ;
DAIRYVALTOT = sum(DOMI, A1(DOMI) * DAIRYVAL(DOMI)) ;
DAIRYEXPTOT = PINDEX10.L ** (1 - EPSILON0) - 1 ;

CS(DOMI)   =  (1 - PINDEX1.L(DOMI) ** (1 - EPSILON)) / (1 - EPSILON) ;
CSDOM      =  sum(DOMI, B1(DOMI) * CS(DOMI)) / sum(DOMI, B1(DOMI));
PS(DOMI)   = sum(L, VARPHI2(DOMI, L) * RHO2(DOMI, L) / VARPHI1(DOMI) * (R.L(DOMI, L) * Y.L(DOMI, L) - 1)) ;
PSDOM      = sum(DOMI, PS(DOMI) * VARPHI1(DOMI) * RHO1(DOMI)) / sum(DOMJ, VARPHI1(DOMJ) * RHO1(DOMJ)) ;
WELF(DOMI) = B1(DOMI) * CS(DOMI) + (A0 * PSI * PHI0 * RHO1(DOMI)) / ((1 - RHO0) * (1 - XI1("ROW"))) * VARPHI1(DOMI) * PS(DOMI) ;
WELFDOM    = sum(DOMI, WELF(DOMI)) ;
CSFOR     =  (1 - PINDEX10.L ** (1 - EPSILON0)) / (1 - EPSILON0) ;
WELFFOR    = B1("ROW") * CSFOR ;
        
execute_unload "../MANUSCRIPT TABLES/Table E.1/output_ex.gdx", DELTAMILKVALTOT, DELTAMILKPRODSHARESHIPTOT,DAIRYVALTOT,
                                DAIRYEXPTOT, CSDOM, PSDOM, WELFDOM, CSFOR, WELFFOR ;
 
put_utility cmd 'exec' /
        'gdxdump "../MANUSCRIPT TABLES/Table E.1/output_ex.gdx" format=csv output="../MANUSCRIPT TABLES/Table E.1/deltamilkvaltot_' SC.tl:0 '.csv" symb=DELTAMILKVALTOT';

put_utility cmd 'exec' /
        'gdxdump "../MANUSCRIPT TABLES/Table E.1/output_ex.gdx" format=csv output="../MANUSCRIPT TABLES/Table E.1/deltamilkprodshareshiptot_' SC.tl:0 '.csv" symb=DELTAMILKPRODSHARESHIPTOT';

put_utility cmd 'exec' /
        'gdxdump "../MANUSCRIPT TABLES/Table E.1/output_ex.gdx" format=csv output="../MANUSCRIPT TABLES/Table E.1/dairyvaltot_' SC.tl:0 '.csv" symb=DAIRYVALTOT';

put_utility cmd 'exec' /
        'gdxdump "../MANUSCRIPT TABLES/Table E.1/output_ex.gdx" format=csv output="../MANUSCRIPT TABLES/Table E.1/dairyexptot_' SC.tl:0 '.csv" symb=DAIRYEXPTOT';

put_utility cmd 'exec' /
        'gdxdump "../MANUSCRIPT TABLES/Table E.1/output_ex.gdx" format=csv output="../MANUSCRIPT TABLES/Table E.1/csdom_' SC.tl:0 '.csv" symb=CSDOM';
        
put_utility cmd 'exec' /
        'gdxdump "../MANUSCRIPT TABLES/Table E.1/output_ex.gdx" format=csv output="../MANUSCRIPT TABLES/Table E.1/psdom_' SC.tl:0 '.csv" symb=PSDOM';

put_utility cmd 'exec' /
        'gdxdump "../MANUSCRIPT TABLES/Table E.1/output_ex.gdx" format=csv output="../MANUSCRIPT TABLES/Table E.1/welfdom_' SC.tl:0 '.csv" symb=WELFDOM';

put_utility cmd 'exec' /
        'gdxdump "../MANUSCRIPT TABLES/Table E.1/output_ex.gdx" format=csv output="../MANUSCRIPT TABLES/Table E.1/csfor_' SC.tl:0 '.csv" symb=CSFOR';
        
put_utility cmd 'exec' /
        'gdxdump "../MANUSCRIPT TABLES/Table E.1/output_ex.gdx" format=csv output="../MANUSCRIPT TABLES/Table E.1/welffor_' SC.tl:0 '.csv" symb=WELFFOR';

)
;

parameter THETATILDE(DOMI, N)    "share of product N in total component value in region DOMI"
          ;

THETATILDE(DOMI, N) = sum(K, THETA2(DOMI, K) * THETA3(DOMI, N, K)) ;

display THETATILDE;

execute_unload "../MANUSCRIPT CLAIMS/Section 6.5/output_ex.gdx",  THETATILDE ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.5/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.5/dairyprodcompvalshare.csv" symb=THETATILDE' ;

$label skip_sensitivity


******************************************************************
***(2) COUNTERFACTUAL OPTIMAL WEDGLES (FIRST COLUMN OF TABLE 7)***
******************************************************************

MODEL BETTERWEDGESREG /EQ1, EQ2, EQ3, EQ4, EQ5, EQ6, EQ7, EQ8OPT, EQ9OPT, EQ10, EQ11, EQ12, EQ13OPT, EQ14A, EQ14B, EQ15, EQ16, EQ17, EQ18, EQ19, EQ20, EQ22, EQ23 / ;
*THIS MODEL IMPROVES ON THE BASELINE BY MAXIMIZING DOMESTIC CONS SURPLUS SUBJ TO NO HARM TO REGIONAL LAND RENT

$if not set run_wedgecounterfactual $goto skip_wedgecounterfactual

* Starting Values
P.L(DOMI, N) = 1 ;
PINDEX2.L(DOMI, N) = 1 ;
PINDEX1.L(DOMI) = 1 ;
PINDEX20.L(SUBN) = 1 ;
PINDEX10.L = 1 ;
C.L(I, DOMJ, N) = 1 ;
C0.L(DOMI, SUBN) = 1 ;
Q.L(DOMI, N) = 1 ;
V.L(DOMI, K) = 1 ;
VINDEX.L(DOMI, N) = 1 ;
Z.L(DOMI, N, K) = 1 ;
R.L(DOMI, L) = 1 ;
W.L(DOMI, L) = 1 ;
WINDEX1.L(DOMI) = 1 ;
MSHIP.L(DOMI, DOMJ) = 1 ;
MQUANT.L(DOMI) = 1 ;
MPRICE.L(DOMI) = 1 ;
Y.L(DOMI, L) = 1 ;

DELTAHATOPT.L(DOMI, N, K) = 1 / DELTA(DOMI, N, K) ;

SOLVE BETTERWEDGESREG USING NLP MAXIMIZING DOMCONSSURP ;

DELTAMILKVAL(DOMI) = MPRICE.L(DOMI) * MQUANT.L(DOMI) - 1 ;
DELTAMILKVALTOT    = sum(DOMI, DELTAMILKVAL(DOMI) * MU1(DOMI)) ;
DAIRYVAL(DOMI) = sum(N, A2(DOMI, N) * P.L(DOMI, N) * Q.L(DOMI, N)) - 1 ;
DAIRYVALTOT = sum(DOMI, A1(DOMI) * DAIRYVAL(DOMI)) ;
DAIRYEXPTOT = PINDEX10.L ** (1 - EPSILON0) - 1 ;

CS(DOMI)   =  (1 - PINDEX1.L(DOMI) ** (1 - EPSILON)) / (1 - EPSILON) ;
CSDOM      =  sum(DOMI, B1(DOMI) * CS(DOMI)) / sum(DOMI, B1(DOMI));
CSFOR     =  (1 - PINDEX10.L ** (1 - EPSILON0)) / (1 - EPSILON0) ;
CSTOT      = sum(DOMI, B1(DOMI) * CS(DOMI)) + B1("ROW") * CSFOR  ;  

CSVAL(DOMI)    = CS(DOMI) * B1(DOMI) * PC ;
CSDOMVAL       = sum(DOMI, CSVAL(DOMI)) ;
CSFORVAL       = CSFOR * B1("ROW") * PC ;
CSTOTVAL       = CSDOMVAL + CSFORVAL ;

display DELTAMILKVALTOT, DAIRYVALTOT, DAIRYEXPTOT, CSDOM, CSFOR, CSTOT, CSDOMVAL, CSFORVAL, CSTOTVAL ; 

execute_unload "../MANUSCRIPT TABLES/Table 7/output1_ex.gdx", DELTAMILKVALTOT, DAIRYVALTOT, DAIRYEXPTOT
                                CSDOM, CSFOR, CSTOT, CSDOMVAL, CSFORVAL, CSTOTVAL  ;

execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output1_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/deltamilkvaltotwedge.csv" symb=DELTAMILKVALTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output1_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/dairyvaltotwedge.csv" symb=DAIRYVALTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output1_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/dairyexptotwedge.csv" symb=DAIRYEXPTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output1_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/csdomwedge.csv" symb=CSDOM' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output1_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/csforwedge.csv" symb=CSFOR' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output1_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/cstotwedge.csv" symb=CSTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output1_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/csdomvalwedge.csv" symb=CSDOMVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output1_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/csforvalwedge.csv" symb=CSFORVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output1_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/cstotvalwedge.csv" symb=CSTOTVAL' ;

*THE FOLLOWING VALUES SUBSTANTIATE CLAIMS REGARDING THE SHARE OF THE EQUIVALENT QUOTA RENT IN TOTAL MILK VALUE
*MADE IN SECTION 1 AND IN SECTION 6.6, AS WELL AS CLAIMS REGARDING PRICE CHANGES MADE IN SECTION 6.6
  
parameter    PRICECHANGEWED(DOMI,N)    "change in regional price index for dairy product N"  
             PRICECHANGEminWED(N)      "minimum change in regional price index for dairy product N across regions"
             PRICECHANGEmaxWED(N)      "maximum change in regional price index for dairy product N across regions"
             FORPRINDEXCHANGEWED        "change in foreign price index for dairy" 
             ;
            
PRICECHANGEWED(DOMI,N) = PINDEX2.L(DOMI,N) -1 ;
PRICECHANGEminWED(N)= smin(DOMI, PRICECHANGEWED(DOMI,N)) ;
PRICECHANGEmaxWED(N)= smax(DOMI, PRICECHANGEWED(DOMI,N)) ;
FORPRINDEXCHANGEWED = PINDEX10.L - 1 ;

display  PRICECHANGEminWED, PRICECHANGEmaxWED, FORPRINDEXCHANGEWED ;

execute_unload "../MANUSCRIPT CLAIMS/Section 6.6/output1_ex.gdx", PRICECHANGEminWED, PRICECHANGEmaxWED, FORPRINDEXCHANGEWED ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.6/output1_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.6/pricechangeminwed.csv" symb=PRICECHANGEminWED' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.6/output1_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.6/pricechangemaxwed.csv" symb=PRICECHANGEmaxWED' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.6/output1_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.6/forprindexchangewed.csv" symb=FORPRINDEXCHANGEWED' ;

$label skip_wedgecounterfactual


************************************************************************
***(3) COUNTERFACTUAL REGIONAL MILK QUOTAS (SECOND COLUMN OF TABLE 7)***
************************************************************************

MODEL QUOTAREG /EQ1, EQ2, EQ3, EQ4, EQ5, EQ6, EQ7, EQ8, EQ9, EQ10, EQ11, EQ12, EQ13QU, EQ14AQU, EQ15, EQ16, EQ17, EQ18, EQ19, EQ20, EQ22, EQ24 / ;
*THIS MODEL USES A DAIRY QUOTA TO SUSTAIN THE SUM OF LAND RENTS AND DAIRY QUOTA RENTS AT REGIONAL LEVEL

$if not set run_quotacounterfactual $goto skip_quotacounterfactual

* Starting Values
P.L(DOMI, N) = 1 ;
PINDEX2.L(DOMI, N) = 1 ;
PINDEX1.L(DOMI) = 1 ;
PINDEX20.L(SUBN) = 1 ;
PINDEX10.L = 1 ;
C.L(I, DOMJ, N) = 1 ;
C0.L(DOMI, SUBN) = 1 ;
Q.L(DOMI, N) = 1 ;
V.L(DOMI, K) = 1 ;
VINDEX.L(DOMI, N) = 1 ;
Z.L(DOMI, N, K) = 1 ;
R.L(DOMI, L) = 1 ;
W.L(DOMI, L) = 1 ;
WINDEX1.L(DOMI) = 1 ;
MSHIP.L(DOMI, DOMJ) = 1 ;
MQUANT.L(DOMI) = 1 ;
MPRICE.L(DOMI) = 1 ;
Y.L(DOMI, L) = 1 ;

MMPRICE.L(DOMI) = 1 ;

DELTAHAT(DOMI, N, K) = 1 / DELTA(DOMI, N, K) ;

SOLVE QUOTAREG USING NLP MAXIMIZING  DOMCONSSURP ;

display MMPRICE.L ;

parameters    DELTAMILKVALQUOTA(DOMI)
              DELTAMILKVALTOTQUOTA
              ;           

DELTAMILKVALQUOTA(DOMI) = MMPRICE.L(DOMI) * sum(DOMJ, MU2(DOMJ, DOMI) * MSHIP.L(DOMJ,DOMI)) - 1 ;
DELTAMILKVALTOTQUOTA  = sum(DOMI, MUTILDE(DOMI) * DELTAMILKVALQUOTA(DOMI) ) ;

DAIRYVAL(DOMI) = sum(N, A2(DOMI, N) * P.L(DOMI, N) * Q.L(DOMI, N)) - 1 ;
DAIRYVALTOT = sum(DOMI, A1(DOMI) * DAIRYVAL(DOMI)) ;
DAIRYEXPTOT = PINDEX10.L ** (1 - EPSILON0) - 1 ;

CS(DOMI)   =  (1 - PINDEX1.L(DOMI) ** (1 - EPSILON)) / (1 - EPSILON) ;
CSDOM      =  sum(DOMI, B1(DOMI) * CS(DOMI)) / sum(DOMI, B1(DOMI));
CSFOR     =  (1 - PINDEX10.L ** (1 - EPSILON0)) / (1 - EPSILON0) ;
CSTOT      = sum(DOMI, B1(DOMI) * CS(DOMI)) + B1("ROW") * CSFOR  ;  

CSVAL(DOMI)    = CS(DOMI) * B1(DOMI) * PC ;
CSDOMVAL       = sum(DOMI, CSVAL(DOMI)) ;
CSFORVAL       = CSFOR * B1("ROW") * PC ;
CSTOTVAL       = CSDOMVAL + CSFORVAL ;

execute_unload "../MANUSCRIPT TABLES/Table 7/output2_ex.gdx", DELTAMILKVALTOTQUOTA, DAIRYVALTOT, DAIRYEXPTOT
                                CSDOM, CSFOR, CSTOT, CSDOMVAL, CSFORVAL, CSTOTVAL  ;
                                
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output2_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/deltamilkvaltotquota.csv" symb=DELTAMILKVALTOTQUOTA' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output2_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/dairyvaltotquota.csv" symb=DAIRYVALTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output2_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/dairyexptotquota.csv" symb=DAIRYEXPTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output2_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/csdomquota.csv" symb=CSDOM' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output2_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/csforquota.csv" symb=CSFOR' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output2_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/cstotquota.csv" symb=CSTOT' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output2_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/csdomvalquota.csv" symb=CSDOMVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output2_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/csforvalquota.csv" symb=CSFORVAL' ;
execute 'gdxdump "../MANUSCRIPT TABLES/Table 7/output2_ex" format=csv output="../MANUSCRIPT TABLES/Table 7/cstotvalquota.csv" symb=CSTOTVAL' ;

display DELTAMILKVALTOTQUOTA, DAIRYVALTOT, DAIRYEXPTOT, CSDOM, CSFOR, CSTOT, CSDOMVAL, CSFORVAL, CSTOTVAL ; 

*THE FOLLOWING VALUES SUBSTANTIATE CLAIMS REGARDING THE SHARE OF THE EQUIVALENT QUOTA RENT IN TOTAL MILK VALUE
*MADE IN SECTION 1 AND IN SECTION 6.6, AS WELL AS CLAIMS REGARDING PRICE CHANGES MADE IN SECTION 6.6

parameter   QUOTARENTSHARE              "share of aggregate milk value represented by the milk quota"   
            PRICECHANGEQU(DOMI,N)       "change in regional price index for dairy product N"  
            PRICECHANGEminQU(N)         "minimum change in regional price index for dairy product N across regions"
            PRICECHANGEmaxQU(N)         "maximum change in regional price index for dairy product N across regions"
            FORPRINDEXCHANGEQU           "change in foreign price index for dairy"
            ;           
            
QUOTARENTSHARE = SUM(DOMI, MU1(DOMI) * (MMPRICE.L(DOMI) - MPRICE.L(DOMI)) * MQUANT.L(DOMI)) / SUM(DOMI, MU1(DOMI) * (MMPRICE.L(DOMI)) * MQUANT.L(DOMI)) ;         
PRICECHANGEQU(DOMI,N) = PINDEX2.L(DOMI,N) -1 ;
PRICECHANGEminQU(N)= smin(DOMI, PRICECHANGEQU(DOMI,N)) ;
PRICECHANGEmaxQU(N)= smax(DOMI, PRICECHANGEQU(DOMI,N)) ;
FORPRINDEXCHANGEQU = PINDEX10.L - 1 ;

display  QUOTARENTSHARE, PRICECHANGEminQU, PRICECHANGEmaxQU, FORPRINDEXCHANGEQU ;

SOLVE FMMO USING NLP MAXIMIZING OBJ ;

parameter   BEVPRICECOMPEQ(DOMI)        "change in consumer beverage price under FMMO removal"
            BEVPRICEDIFF(DOMI)          "difference in consumer beverage price between milk quota and FMMO removal counterfactuals"
            ;

BEVPRICECOMPEQ(DOMI) = PINDEX2.L(DOMI, "Beverage") - 1 ;
BEVPRICEDIFF(DOMI) = PRICECHANGEQU(DOMI, "Beverage") - BEVPRICECOMPEQ(DOMI) ;

display PRICECHANGEQU, BEVPRICEDIFF ;

execute_unload "../MANUSCRIPT CLAIMS/Section 6.6/output2_ex.gdx", QUOTARENTSHARE, PRICECHANGEminQU, PRICECHANGEmaxQU, FORPRINDEXCHANGEQU, BEVPRICEDIFF ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.6/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.6/quotarentshare.csv" symb=QUOTARENTSHARE' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.6/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.6/pricechangeminqu.csv" symb=PRICECHANGEminQU' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.6/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.6/pricechangemaxqu.csv" symb=PRICECHANGEmaxQU' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.6/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.6/forprindexchangequ.csv" symb=FORPRINDEXCHANGEQU' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.6/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.6/bevpricediff.csv" symb=BEVPRICEDIFF' ;

$label skip_quotacounterfactual

*************************************************************
******(4) COUNTERFACTUAL INCREASE IN MILK TRADE COSTS********
*************************************************************

*THIS PROGRAM SUBSTANTIATES CLAIMS REGARDING COUNTERFACTUAL INCREASES IN MILK TRADE COSTS
*MADE IN SECTION 1 AND IN SECTION 6.1

MODEL MILKTRADECOST /EQ1, EQ2, EQ3, EQ4, EQ5, EQ6, EQ7, EQ8, EQ9, EQ10, EQ11TRC, EQ12, EQ13, EQ14ATRC, EQ14BTRC, EQ15, EQ16, EQ17, EQ18, EQ19, EQ20, EQ21/ ;
*THIS MODEL INCREASES MILK TRADE COSTS UNIFORMLY UNTIL ALL TRADE IN FARM MILK DISAPPEARS
*THE FOLLOWING ITERATIONS OF THE MODEL DEMONSTRATE THAT A SMALL MILK TRADE FLOW EXISTS AT TAU(DOMI, DOMJ) = 1.61 YET DISAPPEARS AT TAU(DOMI, DOMJ) = 1.62 
*TRADE IS ASSUMED TO DISAPPEAR IF THE PARAMETER DELTAMILKPRODSHARESHIPTOT IS EQUAL TO THE NEGATIVE OF MILKPRODSHARESHIPTOT (ITS BASELINE VALUE)

$if not set run_milktradecost $goto skip_milktradecost

*SET DELTAHAT TO 1 TO MAINTAIN BASELINE COMPONENT PRICE DISTORTIONS WHILE COMPUTING COUNTERFACTUAL INCREASE IN MILK TRADE COSTS
DELTAHAT(DOMI, N, K) = 1 ;

scalar TAU1 /1.61/ ;

TAU(DOMI, DOMJ) = TAU1 ;
TAU(DOMI, DOMI) = 1 ;
TAU(DOMI, DOMJ)$(MU2(DOMI, DOMJ) = 0) = 1 ;

* Starting Values
P.L(DOMI, N) = 1 ;
PINDEX2.L(DOMI, N) = 1 ;
PINDEX1.L(DOMI) = 1 ;
PINDEX20.L(SUBN) = 1 ;
PINDEX10.L = 1 ;
C.L(I, DOMJ, N) = 1 ;
C0.L(DOMI, SUBN) = 1 ;
Q.L(DOMI, N) = 1 ;
V.L(DOMI, K) = 1 ;
VINDEX.L(DOMI, N) = 1 ;
Z.L(DOMI, N, K) = 1 ;
R.L(DOMI, L) = 1 ;
W.L(DOMI, L) = 1 ;
WINDEX1.L(DOMI) = 1 ;
MSHIP.L(DOMI, DOMJ) = 1 ;
MQUANT.L(DOMI) = 1 ;
MPRICE.L(DOMI) = 1 ;
Y.L(DOMI, L) = 1 ;

SOLVE MILKTRADECOST USING NLP MAXIMIZING OBJ ;

MILKPRODSHARESHIPPRIME(DOMI) = sum(DOMJ$(ORD(DOMJ) NE ORD(DOMI)), A(DOMI, DOMJ) * TAU(DOMI, DOMJ) * MSHIP.L(DOMI, DOMJ) / MQUANT.L(DOMI)) - sum(DOMJ$(ORD(DOMJ) NE ORD(DOMI)), MU2(DOMJ, DOMI) * MUTILDE(DOMI) / MU1(DOMI) * MSHIP.L(DOMJ, DOMI) / MQUANT.L(DOMI))   ;
MILKPRODSHARESHIPTOTPRIME = sum(DOMI$(MILKPRODSHARESHIPPRIME(DOMI) > 0), MU1(DOMI) * MPRICE.L(DOMI) * MQUANT.L(DOMI) * MILKPRODSHARESHIPPRIME(DOMI)) / sum(DOMI, MU1(DOMI) * MPRICE.L(DOMI) * MQUANT.L(DOMI)) ;
DELTAMILKPRODSHARESHIPTOT = MILKPRODSHARESHIPTOTPRIME - MILKPRODSHARESHIPTOT ;

display MILKPRODSHARESHIPTOT, DELTAMILKPRODSHARESHIPTOT ;

execute_unload "../MANUSCRIPT CLAIMS/Section 6.1/output2_ex.gdx", TAU1, MILKPRODSHARESHIPTOT, DELTAMILKPRODSHARESHIPTOT ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.1/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.1/TAU1.csv" symb=TAU1' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.1/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.1/milkprodshareshiptot.csv" symb=MILKPRODSHARESHIPTOT' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.1/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.1/deltamilkprodshareshiptot1.csv" symb=DELTAMILKPRODSHARESHIPTOT' ;

scalar TAU2 /1.62/ ;

TAU(DOMI, DOMJ) =  TAU2 ;
TAU(DOMI, DOMI) = 1 ;
TAU(DOMI, DOMJ)$(MU2(DOMI, DOMJ) = 0) = 1 ;

SOLVE MILKTRADECOST USING NLP MAXIMIZING OBJ ;

MILKPRODSHARESHIPPRIME(DOMI) = sum(DOMJ$(ORD(DOMJ) NE ORD(DOMI)), A(DOMI, DOMJ) * TAU(DOMI, DOMJ) * MSHIP.L(DOMI, DOMJ) / MQUANT.L(DOMI)) - sum(DOMJ$(ORD(DOMJ) NE ORD(DOMI)), MU2(DOMJ, DOMI) * MUTILDE(DOMI) / MU1(DOMI) * MSHIP.L(DOMJ, DOMI) / MQUANT.L(DOMI))   ;
MILKPRODSHARESHIPTOTPRIME = sum(DOMI$(MILKPRODSHARESHIPPRIME(DOMI) > 0), MU1(DOMI) * MPRICE.L(DOMI) * MQUANT.L(DOMI) * MILKPRODSHARESHIPPRIME(DOMI)) / sum(DOMI, MU1(DOMI) * MPRICE.L(DOMI) * MQUANT.L(DOMI)) ;
DELTAMILKPRODSHARESHIPTOT = MILKPRODSHARESHIPTOTPRIME - MILKPRODSHARESHIPTOT ;

display MILKPRODSHARESHIPTOT, DELTAMILKPRODSHARESHIPTOT ;

execute_unload "../MANUSCRIPT CLAIMS/Section 6.1/output3_ex.gdx", TAU2, DELTAMILKPRODSHARESHIPTOT ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.1/output3_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.1/TAU2.csv" symb=TAU2' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.1/output3_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.1/deltamilkprodshareshiptot2.csv" symb=DELTAMILKPRODSHARESHIPTOT' ;

$label skip_milktradecost


********************************************************************
***(5) MAXIMUM TRANSFER THAT PRESERVES DOMESTIC CONSUMER SURPLUS****
********************************************************************

*THIS PROGRAM SUBSTANTIATES THE CLAIM MADE IN SECTION 6.6 REGARDING THE POSSIBILITY TO
*INCREASE THE LAND RENT TRANSFER WHITHOUT HURTING DOMESTIC CONSUMERS VIA CHANGES IN COMPONENT WEDGES

MODEL BETTERSUPPORT /EQ1, EQ2, EQ3, EQ4, EQ5, EQ6, EQ7, EQ8OPT, EQ9OPT, EQ10, EQ11, EQ12, EQ13OPT, EQ14A, EQ14B, EQ15, EQ16, EQ17, EQ18, EQ19, EQ20, EQ25, EQ23, EQ26 / ;
*THIS PROGRAM RESHUFFLES COMPONENT PRICE WEDGES TO MAXIMIZE AGGREGATE LAND RENTS SUBJECT TO NO HARM TO CONSUMERS OR TO ANY REGIONAL
*LAND RENT RELATIVE TO BASELINE

$if not set run_landrentcounterfactual $goto skip_landrentcounterfactual

* Starting Values
P.L(DOMI, N) = 1 ;
PINDEX2.L(DOMI, N) = 1 ;
PINDEX1.L(DOMI) = 1 ;
PINDEX20.L(SUBN) = 1 ;
PINDEX10.L = 1 ;
C.L(I, DOMJ, N) = 1 ;
C0.L(DOMI, SUBN) = 1 ;
Q.L(DOMI, N) = 1 ;
V.L(DOMI, K) = 1 ;
VINDEX.L(DOMI, N) = 1 ;
Z.L(DOMI, N, K) = 1 ;
R.L(DOMI, L) = 1 ;
W.L(DOMI, L) = 1 ;
WINDEX1.L(DOMI) = 1 ;
MSHIP.L(DOMI, DOMJ) = 1 ;
MQUANT.L(DOMI) = 1 ;
MPRICE.L(DOMI) = 1 ;
Y.L(DOMI, L) = 1 ;

DELTAHATOPT.L(DOMI, N, K) = 1 / DELTA(DOMI, N, K)  ;

SOLVE BETTERSUPPORT USING NLP MAXIMIZING TRANSFER ;

parameter LANDRENTMAX ;

LANDRENTMAX = TRANSFER.L ;
display LANDRENTMAX ;

execute_unload "../MANUSCRIPT CLAIMS/Section 6.6/output3_ex.gdx", LANDRENTMAX ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.6/output3_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.6/landrentmax.csv" symb=LANDRENTMAX' ;

$label skip_landrentcounterfactual

****************************************************************************************
*************(6) ALTERNATIVE POLICY COMPARISON WITH EPSILON0=100************************
****************************************************************************************

*THIS PART OF THE CODE SUBSTANTIATES THE CONTENTS OF FOOTNOTE 25

$if not set run_epsilonzero100 $goto skip_epsilonzero100

EPSILON0 = 100 ;
* Starting Values
P.L(DOMI, N) = 1 ;
PINDEX2.L(DOMI, N) = 1 ;
PINDEX1.L(DOMI) = 1 ;
PINDEX20.L(SUBN) = 1 ;
PINDEX10.L = 1 ;
C.L(I, DOMJ, N) = 1 ;
C0.L(DOMI, SUBN) = 1 ;
Q.L(DOMI, N) = 1 ;
V.L(DOMI, K) = 1 ;
VINDEX.L(DOMI, N) = 1 ;
Z.L(DOMI, N, K) = 1 ;
R.L(DOMI, L) = 1 ;
W.L(DOMI, L) = 1 ;
WINDEX1.L(DOMI) = 1 ;
MSHIP.L(DOMI, DOMJ) = 1 ;
MQUANT.L(DOMI) = 1 ;
MPRICE.L(DOMI) = 1 ;
Y.L(DOMI, L) = 1 ;

DELTAHATOPT.L(DOMI, N, K) = 1 / DELTA(DOMI, N, K) ;

SOLVE BETTERWEDGESREG USING NLP MAXIMIZING DOMCONSSURP ;

CS(DOMI)   =  (1 - PINDEX1.L(DOMI) ** (1 - EPSILON)) / (1 - EPSILON) ;
CSFOR     =  (1 - PINDEX10.L ** (1 - EPSILON0)) / (1 - EPSILON0) ;
CSTOT      = sum(DOMI, B1(DOMI) * CS(DOMI)) + B1("ROW") * CSFOR  ;

display CSTOT ;

execute_unload "../MANUSCRIPT CLAIMS/Section 6.6/output4_ex.gdx", CSTOT ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.6/output4_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.6/cstot100wed.csv" symb=CSTOT' ;

* Starting Values
P.L(DOMI, N) = 1 ;
PINDEX2.L(DOMI, N) = 1 ;
PINDEX1.L(DOMI) = 1 ;
PINDEX20.L(SUBN) = 1 ;
PINDEX10.L = 1 ;
C.L(I, DOMJ, N) = 1 ;
C0.L(DOMI, SUBN) = 1 ;
Q.L(DOMI, N) = 1 ;
V.L(DOMI, K) = 1 ;
VINDEX.L(DOMI, N) = 1 ;
Z.L(DOMI, N, K) = 1 ;
R.L(DOMI, L) = 1 ;
W.L(DOMI, L) = 1 ;
WINDEX1.L(DOMI) = 1 ;
MSHIP.L(DOMI, DOMJ) = 1 ;
MQUANT.L(DOMI) = 1 ;
MPRICE.L(DOMI) = 1 ;
Y.L(DOMI, L) = 1 ;

MMPRICE.L(DOMI) = 1 ;

DELTAHAT(DOMI, N, K) = 1 / DELTA(DOMI, N, K) ;

SOLVE QUOTAREG USING NLP MAXIMIZING  DOMCONSSURP ;

CS(DOMI)   =  (1 - PINDEX1.L(DOMI) ** (1 - EPSILON)) / (1 - EPSILON) ;
CSFOR     =  (1 - PINDEX10.L ** (1 - EPSILON0)) / (1 - EPSILON0) ;
CSTOT      = sum(DOMI, B1(DOMI) * CS(DOMI)) + B1("ROW") * CSFOR  ;

display CSTOT ;

execute_unload "../MANUSCRIPT CLAIMS/Section 6.6/output5_ex.gdx", CSTOT ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Section 6.6/output5_ex" format=csv output="../MANUSCRIPT CLAIMS/Section 6.6/cstot100qu.csv" symb=CSTOT' ;

$label skip_epsilonzero100


****************************************************************************************
*************(7) REGIONAL MILK QUOTAS WITH FULL ARBITRAGE CONDITION*********************
****************************************************************************************
*THIS PART OF THE CODE SUBSTANTIATES THE CLAIM MADE IN APPENDIX F.3 THAT IF ONE KEEPS
*ALL THE CONDITIONS IN EQUATION (19), THEN THE REGIONAL LAND RENT CONSTRAINT BECOMES NON-BINDING
*IN SOME MILK EXPORTING REGIONS, REDUCING THE VALUE OF DOMESTIC CONSUMER SURPLUS THAT CAN BE ACHIEVED.

MODEL QUOTAREGALT /EQ1, EQ2, EQ3, EQ4, EQ5, EQ6, EQ7, EQ8, EQ9, EQ10, EQ11, EQ12, EQ13QU, EQ14AQU, EQ14BQU, EQ15, EQ16, EQ17, EQ18, EQ19, EQ20, EQ22, EQ24 / ;

$if not set run_quotacounterfactual2 $goto skip_quotacounterfactual2

* Starting Values
P.L(DOMI, N) = 1 ;
PINDEX2.L(DOMI, N) = 1 ;
PINDEX1.L(DOMI) = 1 ;
PINDEX20.L(SUBN) = 1 ;
PINDEX10.L = 1 ;
C.L(I, DOMJ, N) = 1 ;
C0.L(DOMI, SUBN) = 1 ;
Q.L(DOMI, N) = 1 ;
V.L(DOMI, K) = 1 ;
VINDEX.L(DOMI, N) = 1 ;
Z.L(DOMI, N, K) = 1 ;
R.L(DOMI, L) = 1 ;
W.L(DOMI, L) = 1 ;
WINDEX1.L(DOMI) = 1 ;
MSHIP.L(DOMI, DOMJ) = 1 ;
MQUANT.L(DOMI) = 1 ;
MPRICE.L(DOMI) = 1 ;
Y.L(DOMI, L) = 1 ;

MMPRICE.L(DOMI) = 1 ;

DELTAHAT(DOMI, N, K) = 1 / DELTA(DOMI, N, K) ;

SOLVE QUOTAREGALT USING NLP MAXIMIZING  DOMCONSSURP ;

parameters MILKPROCPRICE(DOMI)           "regional milk processing price"
           SHADOWVALRENT(DOMI)           "shadow value of regional rent constraint"
           PRODRENT(DOMI)                "regional land and quota rents relative to total consumption value"
           PRODRENTTOT                   "aggregate land and quota rents relative to total consumption value" 
           ;
           
MILKPROCPRICE(DOMI) = MMPRICE.L(DOMI) - 1 ;
SHADOWVALRENT(DOMI) = EQ24.M(DOMI) ;
SHADOWVALRENT(DOMI)$(SHADOWVALRENT(DOMI) EQ 0) = EPS ;

CS(DOMI)   =  (1 - PINDEX1.L(DOMI) ** (1 - EPSILON)) / (1 - EPSILON) ;
CSDOM      =  sum(DOMI, B1(DOMI) * CS(DOMI)) / sum(DOMI, B1(DOMI));
CSFOR     =  (1 - PINDEX10.L ** (1 - EPSILON0)) / (1 - EPSILON0) ;
CSTOT      = sum(DOMI, B1(DOMI) * CS(DOMI)) + B1("ROW") * CSFOR  ;

PRODRENT(DOMI) = A0 * PSI * PHI0 / (1 - XI1("ROW")) * RHO1(DOMI) / (1 - RHO0) * sum(L, VARPHI2(DOMI, L) * RHO2(DOMI, L) * (R.L(DOMI, L) * Y.L(DOMI, L) - 1)) +
                 A0 * PSI * PHI0 / (1 - XI1("ROW")) * XI1(DOMI) / PHI1(DOMI) * (MMPRICE.L(DOMI) - MPRICE.L(DOMI)) * MQUANT.L(DOMI) ;                
PRODRENTTOT = sum(DOMI, PRODRENT(DOMI)) ;

display MILKPROCPRICE, SHADOWVALRENT, CSDOM, CSFOR, CSTOT, PRODRENTTOT ;

execute_unload "../MANUSCRIPT CLAIMS/Appendix F.3/output1_ex.gdx", MILKPROCPRICE, SHADOWVALRENT, CSDOM, CSFOR, CSTOT, PRODRENTTOT ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix F.3/output1_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix F.3/milkprocpricearb.csv" symb=MILKPROCPRICE' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix F.3/output1_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix F.3/shadowvalrentarb.csv" symb=SHADOWVALRENT' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix F.3/output1_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix F.3/csdomarb.csv" symb=CSDOM' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix F.3/output1_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix F.3/csforarb.csv" symb=CSFOR' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix F.3/output1_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix F.3/cstotarb.csv" symb=CSTOT' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix F.3/output1_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix F.3/prodrenttotarb.csv" symb=PRODRENTTOT' ;


* Starting Values
P.L(DOMI, N) = 1 ;
PINDEX2.L(DOMI, N) = 1 ;
PINDEX1.L(DOMI) = 1 ;
PINDEX20.L(SUBN) = 1 ;
PINDEX10.L = 1 ;
C.L(I, DOMJ, N) = 1 ;
C0.L(DOMI, SUBN) = 1 ;
Q.L(DOMI, N) = 1 ;
V.L(DOMI, K) = 1 ;
VINDEX.L(DOMI, N) = 1 ;
Z.L(DOMI, N, K) = 1 ;
R.L(DOMI, L) = 1 ;
W.L(DOMI, L) = 1 ;
WINDEX1.L(DOMI) = 1 ;
MSHIP.L(DOMI, DOMJ) = 1 ;
MQUANT.L(DOMI) = 1 ;
MPRICE.L(DOMI) = 1 ;
Y.L(DOMI, L) = 1 ;

MMPRICE.L(DOMI) = 1 ;

DELTAHAT(DOMI, N, K) = 1 / DELTA(DOMI, N, K) ;

SOLVE QUOTAREG USING NLP MAXIMIZING  DOMCONSSURP ;

MILKPROCPRICE(DOMI) = MMPRICE.L(DOMI) - 1 ;
SHADOWVALRENT(DOMI) = EQ24.M(DOMI) ;
SHADOWVALRENT(DOMI)$(SHADOWVALRENT(DOMI) EQ 0) = EPS ;

CS(DOMI)   =  (1 - PINDEX1.L(DOMI) ** (1 - EPSILON)) / (1 - EPSILON) ;
CSDOM      =  sum(DOMI, B1(DOMI) * CS(DOMI)) / sum(DOMI, B1(DOMI));
CSFOR     =  (1 - PINDEX10.L ** (1 - EPSILON0)) / (1 - EPSILON0) ;
CSTOT      = sum(DOMI, B1(DOMI) * CS(DOMI)) + B1("ROW") * CSFOR  ;

PRODRENT(DOMI) = A0 * PSI * PHI0 / (1 - XI1("ROW")) * RHO1(DOMI) / (1 - RHO0) * sum(L, VARPHI2(DOMI, L) * RHO2(DOMI, L) * (R.L(DOMI, L) * Y.L(DOMI, L) - 1)) +
                 A0 * PSI * PHI0 / (1 - XI1("ROW")) * XI1(DOMI) / PHI1(DOMI) * (MMPRICE.L(DOMI) - MPRICE.L(DOMI)) * MQUANT.L(DOMI) ;                
PRODRENTTOT = sum(DOMI, PRODRENT(DOMI)) ;

display MILKPROCPRICE, SHADOWVALRENT, CSDOM, CSFOR, CSTOT, PRODRENTTOT ;

execute_unload "../MANUSCRIPT CLAIMS/Appendix F.3/output2_ex.gdx", MILKPROCPRICE, SHADOWVALRENT, CSDOM, CSFOR, CSTOT, PRODRENTTOT ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix F.3/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix F.3/milkprocprice.csv" symb=MILKPROCPRICE' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix F.3/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix F.3/shadowvalrent.csv" symb=SHADOWVALRENT' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix F.3/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix F.3/csdom.csv" symb=CSDOM' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix F.3/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix F.3/csfor.csv" symb=CSFOR' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix F.3/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix F.3/cstot.csv" symb=CSTOT' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix F.3/output2_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix F.3/prodrenttot.csv" symb=PRODRENTTOT' ;

$label skip_quotacounterfactual2 


****************************************************************************************
***(8) AGGREGATE COMPONENT DEMAND ELASTICITIES FOR USE IN BEVERAGE AND OTHER PRODUCTS***
****************************************************************************************
*THIS PART OF THE CODE SUBSTANTIATES THE VALUES OF EPSILON1 (0.1) AND EPSILON2 (0.3)
*USED IN APPENDIX G
*IT ALSO SUBSTANTIATES THE CLAIM THAT THE VALUE SHARE OF BEVERAGE PRODUCTS IN THE TOTAL VALUE OF DAIRY PRODUCTS IS 19.4% (APPENDIX G.1)
*IT ALSO RECOMPUTES THE DERIVED DEMAND ELASTICITY FOR MANUFACTURED PRODUCTS WHEN SETTING EPSILON_0 EQUAL TO 100
*AND CONFIRMS THAT THE SHARE OF FOREIGN IN TOTAL CONSUMPTION OF MANUFACTURED PRODUCTS OF US ORIGIN IS 3.6% (APPENDIX G.2)
*NOTE: TO AGGREGATE COMPONENT DEMAND ACROSS REGIONS GIVEN DIFFERENTIATED DAIRY PRODUCTS BY ORIGIN, THE RELATIVE CHANGE IN THE AGGREGATE VALUE (NOT QUANTITY) OF COMPONENTS
*IS COMPUTED, AND WE SUBTRACT 1 TO THE ELASTICITY OF AGGREGATE VALUE W.R.T. TO THE COMMON PRICE INCREASE TO TURN THE VALUE
*ELASTICITY BACK INTO A QUANTITY ELASTICITY

$if not set run_aggregatecompdemand $goto skip_aggregatecompdemand

parameters ELASCOMPUSEBEV(DOMI)      "elasticity of component demand for use in beverage products"
           ELASCOMPUSEMAN(DOMI)      "elasticity of component demand for use in other dairy products"
           ELASCOMPUSEBEVTOT         "aggregate elasticity of component demand for use in beverage products"
           ELASCOMPUSEMANTOT         "aggregate elasticity of component demand for use in other dairy products"
           ELASCOMPUSEMANTOT100      "aggregate elasticity of component demand for use in other dairy products when EPSILON0=100"
           SHAREBEV                  "share of component value used in beverage products"
           SHAREMANFOR               "share of foreign in value of consumption of non-beverage dairy products"
           ;          

VINDEXEX(DOMI , SUBN) = 1 ;
VINDEXEX(DOMI, "beverage") = 1.0001 ;

* Starting Values
  P.L(DOMI, N) = 1 ;
  PINDEX2.L(DOMI, N) = 1 ;
  PINDEX1.L(DOMI) = 1 ;
  PINDEX20.L(SUBN) = 1 ;
  PINDEX10.L = 1 ;
  C.L(I, DOMJ, N) = 1 ;
  C0.L(DOMI, SUBN) = 1 ;
  Q.L(DOMI, N) = 1 ;


SOLVE COMPDEMAND USING NLP MAXIMIZING OBJ ;
ELASCOMPUSEBEV(DOMI) = ( Q.L(DOMI, "beverage")- 1 ) / ( 1.0001 - 1 ) ;
ELASCOMPUSEBEVTOT = (sum(DOMI, A1(DOMI)*A2(DOMI,"beverage")*PSI2(DOMI,"beverage")/sum(DOMJ, A1(DOMJ)*A2(DOMJ,"beverage")*PSI2(DOMJ,"beverage")) * VINDEXEX(DOMI, "beverage") * Q.L(DOMI, "beverage")) -1 )/0.0001 -1 ;

VINDEXEX(DOMI, "beverage") = 1 ;
VINDEXEX(DOMI , SUBN) = 1.0001 ;

* Starting Values
  P.L(DOMI, N) = 1 ;
  PINDEX2.L(DOMI, N) = 1 ;
  PINDEX1.L(DOMI) = 1 ;
  PINDEX20.L(SUBN) = 1 ;
  PINDEX10.L = 1 ;
  C.L(I, DOMJ, N) = 1 ;
  C0.L(DOMI, SUBN) = 1 ;
  Q.L(DOMI, N) = 1 ;

SOLVE COMPDEMAND USING NLP MAXIMIZING OBJ ;

ELASCOMPUSEMAN(DOMI) = ( sum(SUBN, Q.L(DOMI, SUBN) * B2(DOMI, SUBN) /(1 - B2(DOMI, "beverage"))) - 1 ) / ( 1.0001 - 1 ) ;
ELASCOMPUSEMANTOT = (sum((DOMI, SUBN), A1(DOMI)*A2(DOMI,SUBN)*PSI2(DOMI,SUBN)* VINDEXEX(DOMI, SUBN) * Q.L(DOMI,SUBN)) / sum((DOMJ, SUBN), A1(DOMJ)*A2(DOMJ, SUBN)*PSI2(DOMJ, SUBN))  -1 )/0.0001 -1 ;

SHAREBEV = sum(DOMI, A1(DOMI)*A2(DOMI,"beverage")*PSI2(DOMI,"beverage")) / sum((DOMI, N), A1(DOMI)*A2(DOMI,N)*PSI2(DOMI,N) ) ;

SHAREMANFOR = sum((SUBN, DOMJ), B1("ROW") * B2("ROW", SUBN) * B3(DOMJ, "ROW", SUBN)) / sum((DOMI, SUBN, DOMJ), B1(DOMI) * B2(DOMI, SUBN) * B3(DOMJ, DOMI, SUBN)) ;

EPSILON0 = 100 ;
* Starting Values
  P.L(DOMI, N) = 1 ;
  PINDEX2.L(DOMI, N) = 1 ;
  PINDEX1.L(DOMI) = 1 ;
  PINDEX20.L(SUBN) = 1 ;
  PINDEX10.L = 1 ;
  C.L(I, DOMJ, N) = 1 ;
  C0.L(DOMI, SUBN) = 1 ;
  Q.L(DOMI, N) = 1 ;

SOLVE COMPDEMAND USING NLP MAXIMIZING OBJ ;

ELASCOMPUSEMANTOT100 = (sum((DOMI, SUBN), A1(DOMI)*A2(DOMI,SUBN)*PSI2(DOMI,SUBN)* VINDEXEX(DOMI, SUBN) * Q.L(DOMI,SUBN)) / sum((DOMJ, SUBN), A1(DOMJ)*A2(DOMJ, SUBN)*PSI2(DOMJ, SUBN))  -1 )/0.0001 -1 ;


display ELASCOMPUSEBEVTOT, ELASCOMPUSEMANTOT, SHAREBEV, SHAREMANFOR, ELASCOMPUSEMANTOT100, PSI ;

execute_unload "../MANUSCRIPT CLAIMS/Appendix G/output_ex.gdx", ELASCOMPUSEBEVTOT, ELASCOMPUSEMANTOT, SHAREBEV, SHAREMANFOR, ELASCOMPUSEMANTOT100 ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix G/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix G/sharebev.csv" symb=SHAREBEV' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix G/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix G/sharemanfor.csv" symb=SHAREMANFOR' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix G/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix G/elascompusebevtot.csv" symb=ELASCOMPUSEBEVTOT' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix G/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix G/elascompusemantot.csv" symb=ELASCOMPUSEMANTOT' ;
execute 'gdxdump "../MANUSCRIPT CLAIMS/Appendix G/output_ex" format=csv output="../MANUSCRIPT CLAIMS/Appendix G/elascompusemantot100.csv" symb=ELASCOMPUSEMANTOT100' ;

$label skip_aggregatecompdemand
