Henson Q6
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ---------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(arules)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

    ## 
    ## Attaching package: 'arules'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
library(arulesViz)
```

    ## Loading required package: grid

    ## Registered S3 method overwritten by 'seriation':
    ##   method         from 
    ##   reorder.hclust gclus

``` r
groceries_long = read.csv("../data/groceries_long_form.csv", header = TRUE)
names(groceries_long) = c('Transaction', 'Item')
groceries_long$Transaction = factor(groceries_long$Transaction)
```

``` r
str(groceries_long)
```

    ## 'data.frame':    43367 obs. of  2 variables:
    ##  $ Transaction: Factor w/ 9835 levels "1","2","3","4",..: 1 1 1 1 2 2 2 3 4 4 ...
    ##  $ Item       : Factor w/ 169 levels "abrasive cleaner",..: 30 133 89 119 158 168 34 167 110 168 ...

``` r
lists = split(x=groceries_long$Item, f=groceries_long$Transaction)

## Remove duplicates ("de-dupe")
lists = lapply(lists, unique)

## Cast this variable as a special arules "transactions" class.
trans = as(lists, "transactions")
```

``` r
grocery_rules = apriori(trans, parameter=list(support=.005, confidence=.1, maxlen=5))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.1    0.1    1 none FALSE            TRUE       5   0.005      1
    ##  maxlen target   ext
    ##       5  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 49 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [120 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 done [0.00s].
    ## writing ... [1582 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
levels(as.factor(groceries_long$Item))
```

    ##   [1] "abrasive cleaner"          "artif. sweetener"         
    ##   [3] "baby cosmetics"            "baby food"                
    ##   [5] "bags"                      "baking powder"            
    ##   [7] "bathroom cleaner"          "beef"                     
    ##   [9] "berries"                   "beverages"                
    ##  [11] "bottled beer"              "bottled water"            
    ##  [13] "brandy"                    "brown bread"              
    ##  [15] "butter"                    "butter milk"              
    ##  [17] "cake bar"                  "candles"                  
    ##  [19] "candy"                     "canned beer"              
    ##  [21] "canned fish"               "canned fruit"             
    ##  [23] "canned vegetables"         "cat food"                 
    ##  [25] "cereals"                   "chewing gum"              
    ##  [27] "chicken"                   "chocolate"                
    ##  [29] "chocolate marshmallow"     "citrus fruit"             
    ##  [31] "cleaner"                   "cling film/bags"          
    ##  [33] "cocoa drinks"              "coffee"                   
    ##  [35] "condensed milk"            "cooking chocolate"        
    ##  [37] "cookware"                  "cream"                    
    ##  [39] "cream cheese "             "curd"                     
    ##  [41] "curd cheese"               "decalcifier"              
    ##  [43] "dental care"               "dessert"                  
    ##  [45] "detergent"                 "dish cleaner"             
    ##  [47] "dishes"                    "dog food"                 
    ##  [49] "domestic eggs"             "female sanitary products" 
    ##  [51] "finished products"         "fish"                     
    ##  [53] "flour"                     "flower (seeds)"           
    ##  [55] "flower soil/fertilizer"    "frankfurter"              
    ##  [57] "frozen chicken"            "frozen dessert"           
    ##  [59] "frozen fish"               "frozen fruits"            
    ##  [61] "frozen meals"              "frozen potato products"   
    ##  [63] "frozen vegetables"         "fruit/vegetable juice"    
    ##  [65] "grapes"                    "hair spray"               
    ##  [67] "ham"                       "hamburger meat"           
    ##  [69] "hard cheese"               "herbs"                    
    ##  [71] "honey"                     "house keeping products"   
    ##  [73] "hygiene articles"          "ice cream"                
    ##  [75] "instant coffee"            "Instant food products"    
    ##  [77] "jam"                       "ketchup"                  
    ##  [79] "kitchen towels"            "kitchen utensil"          
    ##  [81] "light bulbs"               "liqueur"                  
    ##  [83] "liquor"                    "liquor (appetizer)"       
    ##  [85] "liver loaf"                "long life bakery product" 
    ##  [87] "make up remover"           "male cosmetics"           
    ##  [89] "margarine"                 "mayonnaise"               
    ##  [91] "meat"                      "meat spreads"             
    ##  [93] "misc. beverages"           "mustard"                  
    ##  [95] "napkins"                   "newspapers"               
    ##  [97] "nut snack"                 "nuts/prunes"              
    ##  [99] "oil"                       "onions"                   
    ## [101] "organic products"          "organic sausage"          
    ## [103] "other vegetables"          "packaged fruit/vegetables"
    ## [105] "pasta"                     "pastry"                   
    ## [107] "pet care"                  "photo/film"               
    ## [109] "pickled vegetables"        "pip fruit"                
    ## [111] "popcorn"                   "pork"                     
    ## [113] "pot plants"                "potato products"          
    ## [115] "preservation products"     "processed cheese"         
    ## [117] "prosecco"                  "pudding powder"           
    ## [119] "ready soups"               "red/blush wine"           
    ## [121] "rice"                      "roll products "           
    ## [123] "rolls/buns"                "root vegetables"          
    ## [125] "rubbing alcohol"           "rum"                      
    ## [127] "salad dressing"            "salt"                     
    ## [129] "salty snack"               "sauces"                   
    ## [131] "sausage"                   "seasonal products"        
    ## [133] "semi-finished bread"       "shopping bags"            
    ## [135] "skin care"                 "sliced cheese"            
    ## [137] "snack products"            "soap"                     
    ## [139] "soda"                      "soft cheese"              
    ## [141] "softener"                  "sound storage medium"     
    ## [143] "soups"                     "sparkling wine"           
    ## [145] "specialty bar"             "specialty cheese"         
    ## [147] "specialty chocolate"       "specialty fat"            
    ## [149] "specialty vegetables"      "spices"                   
    ## [151] "spread cheese"             "sugar"                    
    ## [153] "sweet spreads"             "syrup"                    
    ## [155] "tea"                       "tidbits"                  
    ## [157] "toilet cleaner"            "tropical fruit"           
    ## [159] "turkey"                    "UHT-milk"                 
    ## [161] "vinegar"                   "waffles"                  
    ## [163] "whipped/sour cream"        "whisky"                   
    ## [165] "white bread"               "white wine"               
    ## [167] "whole milk"                "yogurt"                   
    ## [169] "zwieback"

``` r
inspect(grocery_rules)
```

    ##        lhs                           rhs                            support confidence      lift count
    ## [1]    {}                         => {bottled water}            0.110523640  0.1105236 1.0000000  1087
    ## [2]    {}                         => {tropical fruit}           0.104931368  0.1049314 1.0000000  1032
    ## [3]    {}                         => {root vegetables}          0.108998475  0.1089985 1.0000000  1072
    ## [4]    {}                         => {soda}                     0.174377224  0.1743772 1.0000000  1715
    ## [5]    {}                         => {yogurt}                   0.139501779  0.1395018 1.0000000  1372
    ## [6]    {}                         => {rolls/buns}               0.183934926  0.1839349 1.0000000  1809
    ## [7]    {}                         => {other vegetables}         0.193492628  0.1934926 1.0000000  1903
    ## [8]    {}                         => {whole milk}               0.255516014  0.2555160 1.0000000  2513
    ## [9]    {cake bar}                 => {whole milk}               0.005592272  0.4230769 1.6557746    55
    ## [10]   {dishes}                   => {other vegetables}         0.005998983  0.3410405 1.7625502    59
    ## [11]   {dishes}                   => {whole milk}               0.005287239  0.3005780 1.1763569    52
    ## [12]   {mustard}                  => {whole milk}               0.005185562  0.4322034 1.6914924    51
    ## [13]   {pot plants}               => {whole milk}               0.006914082  0.4000000 1.5654596    68
    ## [14]   {chewing gum}              => {soda}                     0.005388917  0.2560386 1.4683033    53
    ## [15]   {chewing gum}              => {whole milk}               0.005083884  0.2415459 0.9453259    50
    ## [16]   {canned fish}              => {other vegetables}         0.005083884  0.3378378 1.7459985    50
    ## [17]   {pasta}                    => {whole milk}               0.006100661  0.4054054 1.5866145    60
    ## [18]   {herbs}                    => {root vegetables}          0.007015760  0.4312500 3.9564774    69
    ## [19]   {herbs}                    => {other vegetables}         0.007727504  0.4750000 2.4548739    76
    ## [20]   {herbs}                    => {whole milk}               0.007727504  0.4750000 1.8589833    76
    ## [21]   {processed cheese}         => {soda}                     0.005287239  0.3190184 1.8294729    52
    ## [22]   {processed cheese}         => {other vegetables}         0.005490595  0.3312883 1.7121497    54
    ## [23]   {processed cheese}         => {whole milk}               0.007015760  0.4233129 1.6566981    69
    ## [24]   {semi-finished bread}      => {other vegetables}         0.005185562  0.2931034 1.5148042    51
    ## [25]   {semi-finished bread}      => {whole milk}               0.007117438  0.4022989 1.5744565    70
    ## [26]   {beverages}                => {yogurt}                   0.005490595  0.2109375 1.5120775    54
    ## [27]   {beverages}                => {rolls/buns}               0.005388917  0.2070312 1.1255679    53
    ## [28]   {beverages}                => {other vegetables}         0.005185562  0.1992188 1.0295935    51
    ## [29]   {beverages}                => {whole milk}               0.006812405  0.2617188 1.0242753    67
    ## [30]   {ice cream}                => {soda}                     0.006100661  0.2439024 1.3987058    60
    ## [31]   {ice cream}                => {other vegetables}         0.005083884  0.2032520 1.0504381    50
    ## [32]   {ice cream}                => {whole milk}               0.005897306  0.2357724 0.9227303    58
    ## [33]   {detergent}                => {other vegetables}         0.006405694  0.3333333 1.7227185    63
    ## [34]   {detergent}                => {whole milk}               0.008947636  0.4656085 1.8222281    88
    ## [35]   {pickled vegetables}       => {other vegetables}         0.006405694  0.3579545 1.8499648    63
    ## [36]   {pickled vegetables}       => {whole milk}               0.007117438  0.3977273 1.5565650    70
    ## [37]   {baking powder}            => {other vegetables}         0.007320793  0.4137931 2.1385471    72
    ## [38]   {baking powder}            => {whole milk}               0.009252669  0.5229885 2.0467935    91
    ## [39]   {flour}                    => {other vegetables}         0.006304016  0.3625731 1.8738342    62
    ## [40]   {flour}                    => {whole milk}               0.008439248  0.4853801 1.8996074    83
    ## [41]   {soft cheese}              => {yogurt}                   0.005998983  0.3511905 2.5174623    59
    ## [42]   {soft cheese}              => {rolls/buns}               0.005388917  0.3154762 1.7151511    53
    ## [43]   {soft cheese}              => {other vegetables}         0.007117438  0.4166667 2.1533981    70
    ## [44]   {soft cheese}              => {whole milk}               0.007524148  0.4404762 1.7238692    74
    ## [45]   {specialty bar}            => {soda}                     0.007219115  0.2639405 1.5136181    71
    ## [46]   {specialty bar}            => {rolls/buns}               0.005592272  0.2044610 1.1115940    55
    ## [47]   {specialty bar}            => {other vegetables}         0.005592272  0.2044610 1.0566861    55
    ## [48]   {specialty bar}            => {whole milk}               0.006507372  0.2379182 0.9311284    64
    ## [49]   {misc. beverages}          => {bottled water}            0.005287239  0.1863799 1.6863354    52
    ## [50]   {misc. beverages}          => {soda}                     0.007320793  0.2580645 1.4799210    72
    ## [51]   {misc. beverages}          => {other vegetables}         0.005592272  0.1971326 1.0188120    55
    ## [52]   {misc. beverages}          => {whole milk}               0.007015760  0.2473118 0.9678917    69
    ## [53]   {grapes}                   => {tropical fruit}           0.006100661  0.2727273 2.5991015    60
    ## [54]   {grapes}                   => {other vegetables}         0.009049314  0.4045455 2.0907538    89
    ## [55]   {grapes}                   => {whole milk}               0.007320793  0.3272727 1.2808306    72
    ## [56]   {cat food}                 => {yogurt}                   0.006202339  0.2663755 1.9094778    61
    ## [57]   {cat food}                 => {other vegetables}         0.006507372  0.2794760 1.4443753    64
    ## [58]   {cat food}                 => {whole milk}               0.008845958  0.3799127 1.4868448    87
    ## [59]   {specialty chocolate}      => {soda}                     0.006304016  0.2073579 1.1891338    62
    ## [60]   {specialty chocolate}      => {rolls/buns}               0.005592272  0.1839465 1.0000629    55
    ## [61]   {specialty chocolate}      => {other vegetables}         0.006100661  0.2006689 1.0370881    60
    ## [62]   {specialty chocolate}      => {whole milk}               0.008032537  0.2642140 1.0340410    79
    ## [63]   {meat}                     => {sausage}                  0.005287239  0.2047244 2.1790742    52
    ## [64]   {meat}                     => {root vegetables}          0.005083884  0.1968504 1.8059922    50
    ## [65]   {meat}                     => {soda}                     0.005490595  0.2125984 1.2191869    54
    ## [66]   {meat}                     => {yogurt}                   0.005287239  0.2047244 1.4675398    52
    ## [67]   {meat}                     => {rolls/buns}               0.006914082  0.2677165 1.4554959    68
    ## [68]   {meat}                     => {other vegetables}         0.009964413  0.3858268 1.9940128    98
    ## [69]   {meat}                     => {whole milk}               0.009964413  0.3858268 1.5099906    98
    ## [70]   {frozen meals}             => {tropical fruit}           0.005490595  0.1935484 1.8445236    54
    ## [71]   {frozen meals}             => {soda}                     0.006202339  0.2186380 1.2538220    61
    ## [72]   {frozen meals}             => {yogurt}                   0.006202339  0.2186380 1.5672774    61
    ## [73]   {frozen meals}             => {other vegetables}         0.007524148  0.2652330 1.3707653    74
    ## [74]   {frozen meals}             => {whole milk}               0.009862735  0.3476703 1.3606593    97
    ## [75]   {hard cheese}              => {sausage}                  0.005185562  0.2116183 2.2524519    51
    ## [76]   {hard cheese}              => {root vegetables}          0.005592272  0.2282158 2.0937519    55
    ## [77]   {hard cheese}              => {yogurt}                   0.006405694  0.2614108 1.8738886    63
    ## [78]   {hard cheese}              => {rolls/buns}               0.005897306  0.2406639 1.3084187    58
    ## [79]   {hard cheese}              => {other vegetables}         0.009456024  0.3858921 1.9943505    93
    ## [80]   {hard cheese}              => {whole milk}               0.010066090  0.4107884 1.6076815    99
    ## [81]   {butter milk}              => {pip fruit}                0.005083884  0.1818182 2.4034702    50
    ## [82]   {butter milk}              => {tropical fruit}           0.005490595  0.1963636 1.8713531    54
    ## [83]   {butter milk}              => {root vegetables}          0.005083884  0.1818182 1.6680801    50
    ## [84]   {butter milk}              => {yogurt}                   0.008540925  0.3054545 2.1896104    84
    ## [85]   {butter milk}              => {rolls/buns}               0.007625826  0.2727273 1.4827378    75
    ## [86]   {butter milk}              => {other vegetables}         0.010371124  0.3709091 1.9169159   102
    ## [87]   {butter milk}              => {whole milk}               0.011591256  0.4145455 1.6223854   114
    ## [88]   {candy}                    => {tropical fruit}           0.005388917  0.1802721 1.7180002    53
    ## [89]   {candy}                    => {soda}                     0.008642603  0.2891156 1.6579897    85
    ## [90]   {candy}                    => {yogurt}                   0.005490595  0.1836735 1.3166389    54
    ## [91]   {candy}                    => {rolls/buns}               0.007117438  0.2380952 1.2944537    70
    ## [92]   {candy}                    => {other vegetables}         0.006914082  0.2312925 1.1953557    68
    ## [93]   {candy}                    => {whole milk}               0.008235892  0.2755102 1.0782502    81
    ## [94]   {ham}                      => {white bread}              0.005083884  0.1953125 4.6398513    50
    ## [95]   {white bread}              => {ham}                      0.005083884  0.1207729 4.6398513    50
    ## [96]   {ham}                      => {tropical fruit}           0.005388917  0.2070312 1.9730158    53
    ## [97]   {ham}                      => {yogurt}                   0.006710727  0.2578125 1.8480947    66
    ## [98]   {ham}                      => {rolls/buns}               0.006914082  0.2656250 1.4441249    68
    ## [99]   {ham}                      => {other vegetables}         0.009150991  0.3515625 1.8169297    90
    ## [100]  {ham}                      => {whole milk}               0.011489578  0.4414062 1.7275091   113
    ## [101]  {sliced cheese}            => {sausage}                  0.007015760  0.2863071 3.0474349    69
    ## [102]  {sliced cheese}            => {tropical fruit}           0.005287239  0.2157676 2.0562739    52
    ## [103]  {sliced cheese}            => {root vegetables}          0.005592272  0.2282158 2.0937519    55
    ## [104]  {sliced cheese}            => {soda}                     0.005083884  0.2074689 1.1897705    50
    ## [105]  {sliced cheese}            => {yogurt}                   0.008032537  0.3278008 2.3497968    79
    ## [106]  {sliced cheese}            => {rolls/buns}               0.007625826  0.3112033 1.6919208    75
    ## [107]  {sliced cheese}            => {other vegetables}         0.009049314  0.3692946 1.9085720    89
    ## [108]  {sliced cheese}            => {whole milk}               0.010777834  0.4398340 1.7213560   106
    ## [109]  {UHT-milk}                 => {bottled water}            0.007320793  0.2188450 1.9800740    72
    ## [110]  {UHT-milk}                 => {soda}                     0.007625826  0.2279635 1.3073010    75
    ## [111]  {UHT-milk}                 => {yogurt}                   0.007422471  0.2218845 1.5905496    73
    ## [112]  {UHT-milk}                 => {rolls/buns}               0.006405694  0.1914894 1.0410712    63
    ## [113]  {UHT-milk}                 => {other vegetables}         0.008134215  0.2431611 1.2566944    80
    ## [114]  {oil}                      => {root vegetables}          0.007015760  0.2500000 2.2936101    69
    ## [115]  {oil}                      => {yogurt}                   0.005287239  0.1884058 1.3505620    52
    ## [116]  {oil}                      => {rolls/buns}               0.005083884  0.1811594 0.9849104    50
    ## [117]  {oil}                      => {other vegetables}         0.009964413  0.3550725 1.8350697    98
    ## [118]  {oil}                      => {whole milk}               0.011286223  0.4021739 1.5739675   111
    ## [119]  {onions}                   => {whipped/sour cream}       0.005083884  0.1639344 2.2869434    50
    ## [120]  {onions}                   => {citrus fruit}             0.005592272  0.1803279 2.1787771    55
    ## [121]  {onions}                   => {bottled water}            0.005897306  0.1901639 1.7205725    58
    ## [122]  {onions}                   => {tropical fruit}           0.005693950  0.1836066 1.7497776    56
    ## [123]  {onions}                   => {root vegetables}          0.009456024  0.3049180 2.7974523    93
    ## [124]  {onions}                   => {soda}                     0.005287239  0.1704918 0.9777183    52
    ## [125]  {onions}                   => {yogurt}                   0.007219115  0.2327869 1.6687019    71
    ## [126]  {onions}                   => {rolls/buns}               0.006812405  0.2196721 1.1942927    67
    ## [127]  {onions}                   => {other vegetables}         0.014234875  0.4590164 2.3722681   140
    ## [128]  {onions}                   => {whole milk}               0.012099644  0.3901639 1.5269647   119
    ## [129]  {berries}                  => {whipped/sour cream}       0.009049314  0.2721713 3.7968855    89
    ## [130]  {whipped/sour cream}       => {berries}                  0.009049314  0.1262411 3.7968855    89
    ## [131]  {berries}                  => {citrus fruit}             0.005388917  0.1620795 1.9582948    53
    ## [132]  {berries}                  => {tropical fruit}           0.006710727  0.2018349 1.9234941    66
    ## [133]  {berries}                  => {root vegetables}          0.006609049  0.1987768 1.8236655    65
    ## [134]  {berries}                  => {soda}                     0.007320793  0.2201835 1.2626849    72
    ## [135]  {berries}                  => {yogurt}                   0.010574479  0.3180428 2.2798477   104
    ## [136]  {berries}                  => {rolls/buns}               0.006609049  0.1987768 1.0806907    65
    ## [137]  {berries}                  => {other vegetables}         0.010269446  0.3088685 1.5962805   101
    ## [138]  {berries}                  => {whole milk}               0.011794611  0.3547401 1.3883281   116
    ## [139]  {hamburger meat}           => {sausage}                  0.005185562  0.1559633 1.6600639    51
    ## [140]  {hamburger meat}           => {root vegetables}          0.006202339  0.1865443 1.7114399    61
    ## [141]  {hamburger meat}           => {soda}                     0.005795628  0.1743119 0.9996255    57
    ## [142]  {hamburger meat}           => {yogurt}                   0.006507372  0.1957187 1.4029832    64
    ## [143]  {hamburger meat}           => {rolls/buns}               0.008642603  0.2599388 1.4132109    85
    ## [144]  {hamburger meat}           => {other vegetables}         0.013828165  0.4159021 2.1494470   136
    ## [145]  {hamburger meat}           => {whole milk}               0.014743264  0.4434251 1.7354101   145
    ## [146]  {hygiene articles}         => {napkins}                  0.006100661  0.1851852 3.5364977    60
    ## [147]  {napkins}                  => {hygiene articles}         0.006100661  0.1165049 3.5364977    60
    ## [148]  {hygiene articles}         => {citrus fruit}             0.005287239  0.1604938 1.9391361    52
    ## [149]  {hygiene articles}         => {shopping bags}            0.005185562  0.1574074 1.5976283    51
    ## [150]  {hygiene articles}         => {bottled water}            0.005693950  0.1728395 1.5638239    56
    ## [151]  {hygiene articles}         => {tropical fruit}           0.006710727  0.2037037 1.9413042    66
    ## [152]  {hygiene articles}         => {root vegetables}          0.005388917  0.1635802 1.5007572    53
    ## [153]  {hygiene articles}         => {soda}                     0.007015760  0.2129630 1.2212774    69
    ## [154]  {hygiene articles}         => {yogurt}                   0.007320793  0.2222222 1.5929705    72
    ## [155]  {hygiene articles}         => {rolls/buns}               0.005897306  0.1790123 0.9732374    58
    ## [156]  {hygiene articles}         => {other vegetables}         0.009557702  0.2901235 1.4994032    94
    ## [157]  {hygiene articles}         => {whole milk}               0.012811388  0.3888889 1.5219746   126
    ## [158]  {salty snack}              => {fruit/vegetable juice}    0.005998983  0.1586022 2.1938849    59
    ## [159]  {salty snack}              => {whipped/sour cream}       0.005185562  0.1370968 1.9125486    51
    ## [160]  {salty snack}              => {pastry}                   0.005185562  0.1370968 1.5409677    51
    ## [161]  {salty snack}              => {shopping bags}            0.005998983  0.1586022 1.6097545    59
    ## [162]  {salty snack}              => {sausage}                  0.005287239  0.1397849 1.4878625    52
    ## [163]  {salty snack}              => {tropical fruit}           0.005592272  0.1478495 1.4090111    55
    ## [164]  {salty snack}              => {soda}                     0.009354347  0.2473118 1.4182576    92
    ## [165]  {salty snack}              => {yogurt}                   0.006202339  0.1639785 1.1754581    61
    ## [166]  {salty snack}              => {other vegetables}         0.010777834  0.2849462 1.4726465   106
    ## [167]  {salty snack}              => {whole milk}               0.011184545  0.2956989 1.1572618   110
    ## [168]  {sugar}                    => {margarine}                0.005490595  0.1621622 2.7688626    54
    ## [169]  {sugar}                    => {pastry}                   0.005185562  0.1531532 1.7214414    51
    ## [170]  {sugar}                    => {root vegetables}          0.006405694  0.1891892 1.7357049    63
    ## [171]  {sugar}                    => {soda}                     0.007320793  0.2162162 1.2399338    72
    ## [172]  {sugar}                    => {yogurt}                   0.006914082  0.2042042 1.4638107    68
    ## [173]  {sugar}                    => {rolls/buns}               0.007015760  0.2072072 1.1265245    69
    ## [174]  {sugar}                    => {other vegetables}         0.010777834  0.3183183 1.6451186   106
    ## [175]  {sugar}                    => {whole milk}               0.015048297  0.4444444 1.7393996   148
    ## [176]  {waffles}                  => {chocolate}                0.005795628  0.1507937 3.0390483    57
    ## [177]  {chocolate}                => {waffles}                  0.005795628  0.1168033 3.0390483    57
    ## [178]  {waffles}                  => {whipped/sour cream}       0.005083884  0.1322751 1.8452850    50
    ## [179]  {waffles}                  => {pastry}                   0.007015760  0.1825397 2.0517460    69
    ## [180]  {waffles}                  => {shopping bags}            0.005490595  0.1428571 1.4499484    54
    ## [181]  {waffles}                  => {tropical fruit}           0.006100661  0.1587302 1.5127046    60
    ## [182]  {waffles}                  => {root vegetables}          0.006609049  0.1719577 1.5776154    65
    ## [183]  {waffles}                  => {soda}                     0.009557702  0.2486772 1.4260879    94
    ## [184]  {waffles}                  => {yogurt}                   0.007524148  0.1957672 1.4033312    74
    ## [185]  {waffles}                  => {rolls/buns}               0.009150991  0.2380952 1.2944537    90
    ## [186]  {waffles}                  => {other vegetables}         0.010066090  0.2619048 1.3535645    99
    ## [187]  {waffles}                  => {whole milk}               0.012709710  0.3306878 1.2941961   125
    ## [188]  {long life bakery product} => {chocolate}                0.005287239  0.1413043 2.8478038    52
    ## [189]  {chocolate}                => {long life bakery product} 0.005287239  0.1065574 2.8478038    52
    ## [190]  {long life bakery product} => {fruit/vegetable juice}    0.006202339  0.1657609 2.2929088    61
    ## [191]  {long life bakery product} => {whipped/sour cream}       0.005795628  0.1548913 2.1607886    57
    ## [192]  {long life bakery product} => {pastry}                   0.005897306  0.1576087 1.7715217    58
    ## [193]  {long life bakery product} => {shopping bags}            0.005388917  0.1440217 1.4617686    53
    ## [194]  {long life bakery product} => {sausage}                  0.005388917  0.1440217 1.5329587    53
    ## [195]  {long life bakery product} => {tropical fruit}           0.006304016  0.1684783 1.6056044    62
    ## [196]  {long life bakery product} => {root vegetables}          0.005287239  0.1413043 1.2963883    52
    ## [197]  {long life bakery product} => {soda}                     0.007625826  0.2038043 1.1687555    75
    ## [198]  {long life bakery product} => {yogurt}                   0.008744281  0.2336957 1.6752163    86
    ## [199]  {long life bakery product} => {rolls/buns}               0.007930859  0.2119565 1.1523452    78
    ## [200]  {long life bakery product} => {other vegetables}         0.010676157  0.2853261 1.4746096   105
    ## [201]  {long life bakery product} => {whole milk}               0.013523132  0.3614130 1.4144438   133
    ## [202]  {dessert}                  => {curd}                     0.005185562  0.1397260 2.6225295    51
    ## [203]  {dessert}                  => {fruit/vegetable juice}    0.005998983  0.1616438 2.2359594    59
    ## [204]  {dessert}                  => {pastry}                   0.005388917  0.1452055 1.6321096    53
    ## [205]  {dessert}                  => {shopping bags}            0.006202339  0.1671233 1.6962410    61
    ## [206]  {dessert}                  => {sausage}                  0.005897306  0.1589041 1.6913657    58
    ## [207]  {dessert}                  => {bottled water}            0.005185562  0.1397260 1.2642185    51
    ## [208]  {dessert}                  => {tropical fruit}           0.006304016  0.1698630 1.6188011    62
    ## [209]  {dessert}                  => {root vegetables}          0.005795628  0.1561644 1.4327208    57
    ## [210]  {dessert}                  => {soda}                     0.009862735  0.2657534 1.5240145    97
    ## [211]  {dessert}                  => {yogurt}                   0.009862735  0.2657534 1.9050182    97
    ## [212]  {dessert}                  => {rolls/buns}               0.006812405  0.1835616 0.9979706    67
    ## [213]  {dessert}                  => {other vegetables}         0.011591256  0.3123288 1.6141636   114
    ## [214]  {dessert}                  => {whole milk}               0.013726487  0.3698630 1.4475140   135
    ## [215]  {canned beer}              => {shopping bags}            0.011387900  0.1465969 1.4879052   112
    ## [216]  {shopping bags}            => {canned beer}              0.011387900  0.1155831 1.4879052   112
    ## [217]  {canned beer}              => {bottled water}            0.008032537  0.1034031 0.9355749    79
    ## [218]  {canned beer}              => {soda}                     0.013828165  0.1780105 1.0208356   136
    ## [219]  {canned beer}              => {rolls/buns}               0.011286223  0.1452880 0.7898878   111
    ## [220]  {canned beer}              => {other vegetables}         0.009049314  0.1164921 0.6020495    89
    ## [221]  {canned beer}              => {whole milk}               0.008845958  0.1138743 0.4456642    87
    ## [222]  {cream cheese }            => {curd}                     0.005083884  0.1282051 2.4062928    50
    ## [223]  {cream cheese }            => {domestic eggs}            0.005083884  0.1282051 2.0206690    50
    ## [224]  {cream cheese }            => {fruit/vegetable juice}    0.005693950  0.1435897 1.9862238    56
    ## [225]  {cream cheese }            => {whipped/sour cream}       0.006405694  0.1615385 2.2535188    63
    ## [226]  {cream cheese }            => {pip fruit}                0.006100661  0.1538462 2.0337055    60
    ## [227]  {cream cheese }            => {citrus fruit}             0.005693950  0.1435897 1.7348957    56
    ## [228]  {cream cheese }            => {shopping bags}            0.005592272  0.1410256 1.4313593    55
    ## [229]  {cream cheese }            => {sausage}                  0.005592272  0.1410256 1.5010684    55
    ## [230]  {cream cheese }            => {bottled water}            0.005897306  0.1487179 1.3455759    58
    ## [231]  {cream cheese }            => {tropical fruit}           0.007219115  0.1820513 1.7349558    71
    ## [232]  {cream cheese }            => {root vegetables}          0.007524148  0.1897436 1.7407912    74
    ## [233]  {cream cheese }            => {soda}                     0.006812405  0.1717949 0.9851910    67
    ## [234]  {cream cheese }            => {yogurt}                   0.012404677  0.3128205 2.2424123   122
    ## [235]  {cream cheese }            => {rolls/buns}               0.009964413  0.2512821 1.3661465    98
    ## [236]  {cream cheese }            => {other vegetables}         0.013726487  0.3461538 1.7889769   135
    ## [237]  {cream cheese }            => {whole milk}               0.016471784  0.4153846 1.6256696   162
    ## [238]  {chicken}                  => {frozen vegetables}        0.006710727  0.1563981 3.2519564    66
    ## [239]  {frozen vegetables}        => {chicken}                  0.006710727  0.1395349 3.2519564    66
    ## [240]  {chicken}                  => {pork}                     0.005795628  0.1350711 2.3428998    57
    ## [241]  {pork}                     => {chicken}                  0.005795628  0.1005291 2.3428998    57
    ## [242]  {chicken}                  => {butter}                   0.005795628  0.1350711 2.4374755    57
    ## [243]  {butter}                   => {chicken}                  0.005795628  0.1045872 2.4374755    57
    ## [244]  {chicken}                  => {newspapers}               0.005185562  0.1208531 1.5141274    51
    ## [245]  {chicken}                  => {domestic eggs}            0.006202339  0.1445498 2.2782803    61
    ## [246]  {chicken}                  => {whipped/sour cream}       0.007219115  0.1682464 2.3470976    71
    ## [247]  {whipped/sour cream}       => {chicken}                  0.007219115  0.1007092 2.3470976    71
    ## [248]  {chicken}                  => {citrus fruit}             0.006914082  0.1611374 1.9469124    68
    ## [249]  {chicken}                  => {sausage}                  0.005287239  0.1232227 1.3115755    52
    ## [250]  {chicken}                  => {bottled water}            0.005287239  0.1232227 1.1148995    52
    ## [251]  {chicken}                  => {tropical fruit}           0.006405694  0.1492891 1.4227309    63
    ## [252]  {chicken}                  => {root vegetables}          0.010879512  0.2535545 2.3262206   107
    ## [253]  {chicken}                  => {soda}                     0.008337570  0.1943128 1.1143244    82
    ## [254]  {chicken}                  => {yogurt}                   0.008337570  0.1943128 1.3929055    82
    ## [255]  {chicken}                  => {rolls/buns}               0.009659380  0.2251185 1.2239029    95
    ## [256]  {chicken}                  => {other vegetables}         0.017895272  0.4170616 2.1554393   176
    ## [257]  {chicken}                  => {whole milk}               0.017590239  0.4099526 1.6044106   173
    ## [258]  {white bread}              => {frankfurter}              0.005185562  0.1231884 2.0888931    51
    ## [259]  {white bread}              => {domestic eggs}            0.005795628  0.1376812 2.1700228    57
    ## [260]  {white bread}              => {fruit/vegetable juice}    0.007422471  0.1763285 2.4390869    73
    ## [261]  {fruit/vegetable juice}    => {white bread}              0.007422471  0.1026723 2.4390869    73
    ## [262]  {white bread}              => {whipped/sour cream}       0.005490595  0.1304348 1.8196115    54
    ## [263]  {white bread}              => {pip fruit}                0.006609049  0.1570048 2.0754604    65
    ## [264]  {white bread}              => {pastry}                   0.005592272  0.1328502 1.4932367    55
    ## [265]  {white bread}              => {shopping bags}            0.007422471  0.1763285 1.7896706    73
    ## [266]  {white bread}              => {sausage}                  0.007219115  0.1714976 1.8254099    71
    ## [267]  {white bread}              => {tropical fruit}           0.008744281  0.2077295 1.9796699    86
    ## [268]  {white bread}              => {root vegetables}          0.007930859  0.1884058 1.7285177    78
    ## [269]  {white bread}              => {soda}                     0.010269446  0.2439614 1.3990437   101
    ## [270]  {white bread}              => {yogurt}                   0.009049314  0.2149758 1.5410258    89
    ## [271]  {white bread}              => {rolls/buns}               0.006507372  0.1545894 0.8404569    64
    ## [272]  {white bread}              => {other vegetables}         0.013726487  0.3260870 1.6852681   135
    ## [273]  {white bread}              => {whole milk}               0.017081851  0.4057971 1.5881474   168
    ## [274]  {chocolate}                => {butter}                   0.006202339  0.1250000 2.2557339    61
    ## [275]  {butter}                   => {chocolate}                0.006202339  0.1119266 2.2557339    61
    ## [276]  {chocolate}                => {newspapers}               0.005490595  0.1106557 1.3863684    54
    ## [277]  {chocolate}                => {fruit/vegetable juice}    0.006812405  0.1372951 1.8991521    67
    ## [278]  {chocolate}                => {pip fruit}                0.006100661  0.1229508 1.6252975    60
    ## [279]  {chocolate}                => {pastry}                   0.008032537  0.1618852 1.8195902    79
    ## [280]  {chocolate}                => {citrus fruit}             0.006405694  0.1290984 1.5598064    63
    ## [281]  {chocolate}                => {shopping bags}            0.008134215  0.1639344 1.6638752    80
    ## [282]  {chocolate}                => {sausage}                  0.006609049  0.1331967 1.4177378    65
    ## [283]  {chocolate}                => {bottled water}            0.005795628  0.1168033 1.0568172    57
    ## [284]  {chocolate}                => {tropical fruit}           0.008134215  0.1639344 1.5623014    80
    ## [285]  {chocolate}                => {root vegetables}          0.006405694  0.1290984 1.1844052    63
    ## [286]  {chocolate}                => {soda}                     0.013523132  0.2725410 1.5629391   133
    ## [287]  {chocolate}                => {yogurt}                   0.009252669  0.1864754 1.3367242    91
    ## [288]  {chocolate}                => {rolls/buns}               0.011794611  0.2377049 1.2923316   116
    ## [289]  {chocolate}                => {other vegetables}         0.012709710  0.2561475 1.3238103   125
    ## [290]  {chocolate}                => {whole milk}               0.016675140  0.3360656 1.3152427   164
    ## [291]  {coffee}                   => {fruit/vegetable juice}    0.005998983  0.1033275 1.4292910    59
    ## [292]  {coffee}                   => {whipped/sour cream}       0.006100661  0.1050788 1.4658866    60
    ## [293]  {coffee}                   => {pip fruit}                0.006914082  0.1190893 1.5742519    68
    ## [294]  {coffee}                   => {pastry}                   0.006914082  0.1190893 1.3385639    68
    ## [295]  {coffee}                   => {citrus fruit}             0.006405694  0.1103327 1.3330744    63
    ## [296]  {coffee}                   => {shopping bags}            0.009354347  0.1611208 1.6353183    92
    ## [297]  {coffee}                   => {sausage}                  0.006914082  0.1190893 1.2675795    68
    ## [298]  {coffee}                   => {bottled water}            0.007320793  0.1260946 1.1408833    72
    ## [299]  {coffee}                   => {tropical fruit}           0.007117438  0.1225919 1.1683060    70
    ## [300]  {coffee}                   => {root vegetables}          0.007320793  0.1260946 1.1568471    72
    ## [301]  {coffee}                   => {soda}                     0.009964413  0.1716287 0.9842382    98
    ## [302]  {coffee}                   => {yogurt}                   0.009761057  0.1681261 1.2051896    96
    ## [303]  {coffee}                   => {rolls/buns}               0.010981190  0.1891419 1.0283085   108
    ## [304]  {coffee}                   => {other vegetables}         0.013421454  0.2311734 1.1947400   132
    ## [305]  {coffee}                   => {whole milk}               0.018708693  0.3222417 1.2611408   184
    ## [306]  {frozen vegetables}        => {pork}                     0.006405694  0.1331924 2.3103124    63
    ## [307]  {pork}                     => {frozen vegetables}        0.006405694  0.1111111 2.3103124    63
    ## [308]  {frozen vegetables}        => {frankfurter}              0.005083884  0.1057082 1.7924838    50
    ## [309]  {frozen vegetables}        => {margarine}                0.005083884  0.1057082 1.8049316    50
    ## [310]  {frozen vegetables}        => {butter}                   0.005795628  0.1205074 2.1746611    57
    ## [311]  {butter}                   => {frozen vegetables}        0.005795628  0.1045872 2.1746611    57
    ## [312]  {frozen vegetables}        => {domestic eggs}            0.005185562  0.1078224 1.6994125    51
    ## [313]  {frozen vegetables}        => {fruit/vegetable juice}    0.007829181  0.1627907 2.2518235    77
    ## [314]  {fruit/vegetable juice}    => {frozen vegetables}        0.007829181  0.1082982 2.2518235    77
    ## [315]  {frozen vegetables}        => {whipped/sour cream}       0.007930859  0.1649049 2.3004813    78
    ## [316]  {whipped/sour cream}       => {frozen vegetables}        0.007930859  0.1106383 2.3004813    78
    ## [317]  {frozen vegetables}        => {pip fruit}                0.007320793  0.1522199 2.0122076    72
    ## [318]  {frozen vegetables}        => {citrus fruit}             0.006609049  0.1374207 1.6603597    65
    ## [319]  {frozen vegetables}        => {sausage}                  0.005998983  0.1247357 1.3276795    59
    ## [320]  {frozen vegetables}        => {bottled water}            0.006202339  0.1289641 1.1668459    61
    ## [321]  {frozen vegetables}        => {tropical fruit}           0.008744281  0.1818182 1.7327343    86
    ## [322]  {frozen vegetables}        => {root vegetables}          0.011591256  0.2410148 2.2111759   114
    ## [323]  {root vegetables}          => {frozen vegetables}        0.011591256  0.1063433 2.2111759   114
    ## [324]  {frozen vegetables}        => {soda}                     0.008642603  0.1797040 1.0305475    85
    ## [325]  {frozen vegetables}        => {yogurt}                   0.012404677  0.2579281 1.8489235   122
    ## [326]  {frozen vegetables}        => {rolls/buns}               0.010167768  0.2114165 1.1494092   100
    ## [327]  {frozen vegetables}        => {other vegetables}         0.017793594  0.3699789 1.9121083   175
    ## [328]  {frozen vegetables}        => {whole milk}               0.020437214  0.4249471 1.6630940   201
    ## [329]  {beef}                     => {pork}                     0.007625826  0.1453488 2.5211743    75
    ## [330]  {pork}                     => {beef}                     0.007625826  0.1322751 2.5211743    75
    ## [331]  {beef}                     => {margarine}                0.006202339  0.1182171 2.0185152    61
    ## [332]  {margarine}                => {beef}                     0.006202339  0.1059028 2.0185152    61
    ## [333]  {beef}                     => {butter}                   0.005795628  0.1104651 1.9934393    57
    ## [334]  {butter}                   => {beef}                     0.005795628  0.1045872 1.9934393    57
    ## [335]  {beef}                     => {newspapers}               0.006405694  0.1220930 1.5296623    63
    ## [336]  {beef}                     => {domestic eggs}            0.005998983  0.1143411 1.8021548    59
    ## [337]  {beef}                     => {whipped/sour cream}       0.006710727  0.1279070 1.7843477    66
    ## [338]  {beef}                     => {pastry}                   0.006304016  0.1201550 1.3505426    62
    ## [339]  {beef}                     => {citrus fruit}             0.008439248  0.1608527 1.9434723    83
    ## [340]  {citrus fruit}             => {beef}                     0.008439248  0.1019656 1.9434723    83
    ## [341]  {beef}                     => {sausage}                  0.005592272  0.1065891 1.1345284    55
    ## [342]  {beef}                     => {bottled water}            0.006202339  0.1182171 1.0696088    61
    ## [343]  {beef}                     => {tropical fruit}           0.007625826  0.1453488 1.3851801    75
    ## [344]  {beef}                     => {root vegetables}          0.017386884  0.3313953 3.0403668   171
    ## [345]  {root vegetables}          => {beef}                     0.017386884  0.1595149 3.0403668   171
    ## [346]  {beef}                     => {soda}                     0.008134215  0.1550388 0.8890998    80
    ## [347]  {beef}                     => {yogurt}                   0.011692933  0.2228682 1.5976012   115
    ## [348]  {beef}                     => {rolls/buns}               0.013624809  0.2596899 1.4118576   134
    ## [349]  {beef}                     => {other vegetables}         0.019725470  0.3759690 1.9430662   194
    ## [350]  {other vegetables}         => {beef}                     0.019725470  0.1019443 1.9430662   194
    ## [351]  {beef}                     => {whole milk}               0.021250635  0.4050388 1.5851795   209
    ## [352]  {curd}                     => {margarine}                0.006304016  0.1183206 2.0202833    62
    ## [353]  {margarine}                => {curd}                     0.006304016  0.1076389 2.0202833    62
    ## [354]  {curd}                     => {butter}                   0.006812405  0.1278626 2.3073920    67
    ## [355]  {butter}                   => {curd}                     0.006812405  0.1229358 2.3073920    67
    ## [356]  {curd}                     => {newspapers}               0.005693950  0.1068702 1.3389410    56
    ## [357]  {curd}                     => {domestic eggs}            0.006507372  0.1221374 1.9250343    64
    ## [358]  {domestic eggs}            => {curd}                     0.006507372  0.1025641 1.9250343    64
    ## [359]  {curd}                     => {whipped/sour cream}       0.010472801  0.1965649 2.7421499   103
    ## [360]  {whipped/sour cream}       => {curd}                     0.010472801  0.1460993 2.7421499   103
    ## [361]  {curd}                     => {pip fruit}                0.007829181  0.1469466 1.9424993    77
    ## [362]  {pip fruit}                => {curd}                     0.007829181  0.1034946 1.9424993    77
    ## [363]  {curd}                     => {pastry}                   0.007524148  0.1412214 1.5873282    74
    ## [364]  {curd}                     => {citrus fruit}             0.007117438  0.1335878 1.6140490    70
    ## [365]  {curd}                     => {shopping bags}            0.005388917  0.1011450 1.0265856    53
    ## [366]  {curd}                     => {sausage}                  0.007625826  0.1431298 1.5234646    75
    ## [367]  {curd}                     => {bottled water}            0.006100661  0.1145038 1.0360120    60
    ## [368]  {curd}                     => {tropical fruit}           0.010269446  0.1927481 1.8368968   101
    ## [369]  {curd}                     => {root vegetables}          0.010879512  0.2041985 1.8734067   107
    ## [370]  {curd}                     => {soda}                     0.008134215  0.1526718 0.8755258    80
    ## [371]  {curd}                     => {yogurt}                   0.017285206  0.3244275 2.3256154   170
    ## [372]  {yogurt}                   => {curd}                     0.017285206  0.1239067 2.3256154   170
    ## [373]  {curd}                     => {rolls/buns}               0.010066090  0.1889313 1.0271638    99
    ## [374]  {curd}                     => {other vegetables}         0.017183528  0.3225191 1.6668288   169
    ## [375]  {curd}                     => {whole milk}               0.026131164  0.4904580 1.9194805   257
    ## [376]  {whole milk}               => {curd}                     0.026131164  0.1022682 1.9194805   257
    ## [377]  {napkins}                  => {newspapers}               0.006202339  0.1184466 1.4839775    61
    ## [378]  {napkins}                  => {domestic eggs}            0.005998983  0.1145631 1.8056541    59
    ## [379]  {napkins}                  => {fruit/vegetable juice}    0.006914082  0.1320388 1.8264444    68
    ## [380]  {napkins}                  => {whipped/sour cream}       0.007219115  0.1378641 1.9232528    71
    ## [381]  {whipped/sour cream}       => {napkins}                  0.007219115  0.1007092 1.9232528    71
    ## [382]  {napkins}                  => {pip fruit}                0.006710727  0.1281553 1.6940965    66
    ## [383]  {napkins}                  => {pastry}                   0.007015760  0.1339806 1.5059417    69
    ## [384]  {napkins}                  => {citrus fruit}             0.007625826  0.1456311 1.7595596    75
    ## [385]  {napkins}                  => {shopping bags}            0.007219115  0.1378641 1.3992706    71
    ## [386]  {napkins}                  => {sausage}                  0.006710727  0.1281553 1.3640777    66
    ## [387]  {napkins}                  => {bottled water}            0.008642603  0.1650485 1.4933325    85
    ## [388]  {napkins}                  => {tropical fruit}           0.010066090  0.1922330 1.8319880    99
    ## [389]  {napkins}                  => {root vegetables}          0.009964413  0.1902913 1.7458158    98
    ## [390]  {napkins}                  => {soda}                     0.011997966  0.2291262 1.3139687   118
    ## [391]  {napkins}                  => {yogurt}                   0.012302999  0.2349515 1.6842183   121
    ## [392]  {napkins}                  => {rolls/buns}               0.011692933  0.2233010 1.2140216   115
    ## [393]  {napkins}                  => {other vegetables}         0.014438231  0.2757282 1.4250060   142
    ## [394]  {napkins}                  => {whole milk}               0.019725470  0.3766990 1.4742678   194
    ## [395]  {pork}                     => {frankfurter}              0.005897306  0.1022928 1.7345679    58
    ## [396]  {frankfurter}              => {pork}                     0.005897306  0.1000000 1.7345679    58
    ## [397]  {pork}                     => {margarine}                0.006405694  0.1111111 1.8971836    63
    ## [398]  {margarine}                => {pork}                     0.006405694  0.1093750 1.8971836    63
    ## [399]  {pork}                     => {newspapers}               0.006609049  0.1146384 1.4362664    65
    ## [400]  {pork}                     => {whipped/sour cream}       0.008235892  0.1428571 1.9929078    81
    ## [401]  {whipped/sour cream}       => {pork}                     0.008235892  0.1148936 1.9929078    81
    ## [402]  {pork}                     => {pip fruit}                0.006100661  0.1058201 1.3988451    60
    ## [403]  {pork}                     => {pastry}                   0.006304016  0.1093474 1.2290653    62
    ## [404]  {pork}                     => {citrus fruit}             0.006507372  0.1128748 1.3637880    64
    ## [405]  {pork}                     => {shopping bags}            0.006405694  0.1111111 1.1277376    63
    ## [406]  {pork}                     => {sausage}                  0.006507372  0.1128748 1.2014323    64
    ## [407]  {pork}                     => {bottled water}            0.007422471  0.1287478 1.1648892    73
    ## [408]  {pork}                     => {tropical fruit}           0.008540925  0.1481481 1.4118576    84
    ## [409]  {pork}                     => {root vegetables}          0.013624809  0.2363316 2.1682099   134
    ## [410]  {root vegetables}          => {pork}                     0.013624809  0.1250000 2.1682099   134
    ## [411]  {pork}                     => {soda}                     0.011896289  0.2063492 1.1833495   117
    ## [412]  {pork}                     => {yogurt}                   0.009557702  0.1657848 1.1884066    94
    ## [413]  {pork}                     => {rolls/buns}               0.011286223  0.1957672 1.0643286   111
    ## [414]  {pork}                     => {other vegetables}         0.021657346  0.3756614 1.9414764   213
    ## [415]  {other vegetables}         => {pork}                     0.021657346  0.1119285 1.9414764   213
    ## [416]  {pork}                     => {whole milk}               0.022165735  0.3844797 1.5047187   218
    ## [417]  {frankfurter}              => {brown bread}              0.007117438  0.1206897 1.8604745    70
    ## [418]  {brown bread}              => {frankfurter}              0.007117438  0.1097179 1.8604745    70
    ## [419]  {frankfurter}              => {margarine}                0.006405694  0.1086207 1.8546606    63
    ## [420]  {margarine}                => {frankfurter}              0.006405694  0.1093750 1.8546606    63
    ## [421]  {frankfurter}              => {domestic eggs}            0.007015760  0.1189655 1.8750414    69
    ## [422]  {domestic eggs}            => {frankfurter}              0.007015760  0.1105769 1.8750414    69
    ## [423]  {frankfurter}              => {whipped/sour cream}       0.006202339  0.1051724 1.4671925    61
    ## [424]  {frankfurter}              => {pip fruit}                0.007219115  0.1224138 1.6181985    71
    ## [425]  {frankfurter}              => {pastry}                   0.008337570  0.1413793 1.5891034    82
    ## [426]  {frankfurter}              => {citrus fruit}             0.006507372  0.1103448 1.3332204    64
    ## [427]  {frankfurter}              => {shopping bags}            0.008235892  0.1396552 1.4174496    81
    ## [428]  {frankfurter}              => {sausage}                  0.010066090  0.1706897 1.8168103    99
    ## [429]  {sausage}                  => {frankfurter}              0.010066090  0.1071429 1.8168103    99
    ## [430]  {frankfurter}              => {bottled water}            0.007320793  0.1241379 1.1231799    72
    ## [431]  {frankfurter}              => {tropical fruit}           0.009456024  0.1603448 1.5280924    93
    ## [432]  {frankfurter}              => {root vegetables}          0.010167768  0.1724138 1.5818001   100
    ## [433]  {frankfurter}              => {soda}                     0.011286223  0.1913793 1.0975018   111
    ## [434]  {frankfurter}              => {yogurt}                   0.011184545  0.1896552 1.3595179   110
    ## [435]  {frankfurter}              => {rolls/buns}               0.019217082  0.3258621 1.7716161   189
    ## [436]  {rolls/buns}               => {frankfurter}              0.019217082  0.1044776 1.7716161   189
    ## [437]  {frankfurter}              => {other vegetables}         0.016471784  0.2793103 1.4435193   162
    ## [438]  {frankfurter}              => {whole milk}               0.020538892  0.3482759 1.3630295   202
    ## [439]  {margarine}                => {bottled beer}             0.006100661  0.1041667 1.2935343    60
    ## [440]  {butter}                   => {bottled beer}             0.005795628  0.1045872 1.2987559    57
    ## [441]  {bottled beer}             => {bottled water}            0.015760041  0.1957071 1.7707259   155
    ## [442]  {bottled water}            => {bottled beer}             0.015760041  0.1425943 1.7707259   155
    ## [443]  {bottled beer}             => {tropical fruit}           0.008235892  0.1022727 0.9746631    81
    ## [444]  {bottled beer}             => {root vegetables}          0.009659380  0.1199495 1.1004695    95
    ## [445]  {bottled beer}             => {soda}                     0.016980173  0.2108586 1.2092094   167
    ## [446]  {bottled beer}             => {yogurt}                   0.009252669  0.1148990 0.8236382    91
    ## [447]  {bottled beer}             => {rolls/buns}               0.013624809  0.1691919 0.9198466   134
    ## [448]  {bottled beer}             => {other vegetables}         0.016166751  0.2007576 1.0375464   159
    ## [449]  {bottled beer}             => {whole milk}               0.020437214  0.2537879 0.9932367   201
    ## [450]  {brown bread}              => {margarine}                0.006507372  0.1003135 1.7128178    64
    ## [451]  {margarine}                => {brown bread}              0.006507372  0.1111111 1.7128178    64
    ## [452]  {butter}                   => {brown bread}              0.005795628  0.1045872 1.6122487    57
    ## [453]  {brown bread}              => {newspapers}               0.007625826  0.1175549 1.4728051    75
    ## [454]  {brown bread}              => {domestic eggs}            0.006812405  0.1050157 1.6551749    67
    ## [455]  {domestic eggs}            => {brown bread}              0.006812405  0.1073718 1.6551749    67
    ## [456]  {brown bread}              => {fruit/vegetable juice}    0.008337570  0.1285266 1.7778615    82
    ## [457]  {fruit/vegetable juice}    => {brown bread}              0.008337570  0.1153305 1.7778615    82
    ## [458]  {brown bread}              => {pip fruit}                0.007625826  0.1175549 1.5539678    75
    ## [459]  {pip fruit}                => {brown bread}              0.007625826  0.1008065 1.5539678    75
    ## [460]  {brown bread}              => {pastry}                   0.009659380  0.1489028 1.6736677    95
    ## [461]  {pastry}                   => {brown bread}              0.009659380  0.1085714 1.6736677    95
    ## [462]  {brown bread}              => {citrus fruit}             0.008337570  0.1285266 1.5528987    82
    ## [463]  {citrus fruit}             => {brown bread}              0.008337570  0.1007371 1.5528987    82
    ## [464]  {brown bread}              => {shopping bags}            0.009252669  0.1426332 1.4476758    91
    ## [465]  {brown bread}              => {sausage}                  0.010676157  0.1645768 1.7517455   105
    ## [466]  {sausage}                  => {brown bread}              0.010676157  0.1136364 1.7517455   105
    ## [467]  {brown bread}              => {bottled water}            0.008235892  0.1269592 1.1487067    81
    ## [468]  {brown bread}              => {tropical fruit}           0.010676157  0.1645768 1.5684233   105
    ## [469]  {tropical fruit}           => {brown bread}              0.010676157  0.1017442 1.5684233   105
    ## [470]  {brown bread}              => {root vegetables}          0.010167768  0.1567398 1.4380000   100
    ## [471]  {brown bread}              => {soda}                     0.012608033  0.1943574 1.1145800   124
    ## [472]  {brown bread}              => {yogurt}                   0.014539908  0.2241379 1.6067030   143
    ## [473]  {yogurt}                   => {brown bread}              0.014539908  0.1042274 1.6067030   143
    ## [474]  {brown bread}              => {rolls/buns}               0.012608033  0.1943574 1.0566637   124
    ## [475]  {brown bread}              => {other vegetables}         0.018708693  0.2884013 1.4905025   184
    ## [476]  {brown bread}              => {whole milk}               0.025216065  0.3887147 1.5212930   248
    ## [477]  {margarine}                => {butter}                   0.006710727  0.1145833 2.0677561    66
    ## [478]  {butter}                   => {margarine}                0.006710727  0.1211009 2.0677561    66
    ## [479]  {margarine}                => {newspapers}               0.007117438  0.1215278 1.5225805    70
    ## [480]  {margarine}                => {domestic eggs}            0.008337570  0.1423611 2.2437845    82
    ## [481]  {domestic eggs}            => {margarine}                0.008337570  0.1314103 2.2437845    82
    ## [482]  {margarine}                => {fruit/vegetable juice}    0.006202339  0.1059028 1.4649140    61
    ## [483]  {margarine}                => {whipped/sour cream}       0.006812405  0.1163194 1.6226975    67
    ## [484]  {margarine}                => {pip fruit}                0.008540925  0.1458333 1.9277834    84
    ## [485]  {pip fruit}                => {margarine}                0.008540925  0.1129032 1.9277834    84
    ## [486]  {margarine}                => {pastry}                   0.006812405  0.1163194 1.3074306    67
    ## [487]  {margarine}                => {citrus fruit}             0.007930859  0.1354167 1.6361461    78
    ## [488]  {margarine}                => {sausage}                  0.007117438  0.1215278 1.2935343    70
    ## [489]  {margarine}                => {bottled water}            0.010269446  0.1753472 1.5865133   101
    ## [490]  {margarine}                => {tropical fruit}           0.009354347  0.1597222 1.5221590    92
    ## [491]  {margarine}                => {root vegetables}          0.011082867  0.1892361 1.7361354   109
    ## [492]  {root vegetables}          => {margarine}                0.011082867  0.1016791 1.7361354   109
    ## [493]  {margarine}                => {soda}                     0.010167768  0.1736111 0.9956066   100
    ## [494]  {margarine}                => {yogurt}                   0.014234875  0.2430556 1.7423115   140
    ## [495]  {yogurt}                   => {margarine}                0.014234875  0.1020408 1.7423115   140
    ## [496]  {margarine}                => {rolls/buns}               0.014743264  0.2517361 1.3686151   145
    ## [497]  {margarine}                => {other vegetables}         0.019725470  0.3368056 1.7406635   194
    ## [498]  {other vegetables}         => {margarine}                0.019725470  0.1019443 1.7406635   194
    ## [499]  {margarine}                => {whole milk}               0.024199288  0.4131944 1.6170980   238
    ## [500]  {butter}                   => {newspapers}               0.005795628  0.1045872 1.3103372    57
    ## [501]  {butter}                   => {domestic eggs}            0.009659380  0.1743119 2.7473683    95
    ## [502]  {domestic eggs}            => {butter}                   0.009659380  0.1522436 2.7473683    95
    ## [503]  {butter}                   => {fruit/vegetable juice}    0.008032537  0.1449541 2.0050968    79
    ## [504]  {fruit/vegetable juice}    => {butter}                   0.008032537  0.1111111 2.0050968    79
    ## [505]  {butter}                   => {whipped/sour cream}       0.010167768  0.1834862 2.5596981   100
    ## [506]  {whipped/sour cream}       => {butter}                   0.010167768  0.1418440 2.5596981   100
    ## [507]  {butter}                   => {pip fruit}                0.007320793  0.1321101 1.7463747    72
    ## [508]  {butter}                   => {pastry}                   0.007625826  0.1376147 1.5467890    75
    ## [509]  {butter}                   => {citrus fruit}             0.009150991  0.1651376 1.9952438    90
    ## [510]  {citrus fruit}             => {butter}                   0.009150991  0.1105651 1.9952438    90
    ## [511]  {butter}                   => {sausage}                  0.008642603  0.1559633 1.6600639    85
    ## [512]  {butter}                   => {bottled water}            0.008947636  0.1614679 1.4609353    88
    ## [513]  {butter}                   => {tropical fruit}           0.009964413  0.1798165 1.7136583    98
    ## [514]  {butter}                   => {root vegetables}          0.012913066  0.2330275 2.1378971   127
    ## [515]  {root vegetables}          => {butter}                   0.012913066  0.1184701 2.1378971   127
    ## [516]  {butter}                   => {soda}                     0.008845958  0.1596330 0.9154465    87
    ## [517]  {butter}                   => {yogurt}                   0.014641586  0.2642202 1.8940273   144
    ## [518]  {yogurt}                   => {butter}                   0.014641586  0.1049563 1.8940273   144
    ## [519]  {butter}                   => {rolls/buns}               0.013421454  0.2422018 1.3167800   132
    ## [520]  {butter}                   => {other vegetables}         0.020030503  0.3614679 1.8681223   197
    ## [521]  {other vegetables}         => {butter}                   0.020030503  0.1035208 1.8681223   197
    ## [522]  {butter}                   => {whole milk}               0.027554652  0.4972477 1.9460530   271
    ## [523]  {whole milk}               => {butter}                   0.027554652  0.1078392 1.9460530   271
    ## [524]  {domestic eggs}            => {newspapers}               0.006914082  0.1089744 1.3653030    68
    ## [525]  {newspapers}               => {fruit/vegetable juice}    0.008235892  0.1031847 1.4273160    81
    ## [526]  {fruit/vegetable juice}    => {newspapers}               0.008235892  0.1139241 1.4273160    81
    ## [527]  {whipped/sour cream}       => {newspapers}               0.007219115  0.1007092 1.2617518    71
    ## [528]  {newspapers}               => {pastry}                   0.008439248  0.1057325 1.1884331    83
    ## [529]  {newspapers}               => {citrus fruit}             0.008337570  0.1044586 1.2621011    82
    ## [530]  {citrus fruit}             => {newspapers}               0.008337570  0.1007371 1.2621011    82
    ## [531]  {newspapers}               => {sausage}                  0.008032537  0.1006369 1.0711735    79
    ## [532]  {newspapers}               => {bottled water}            0.011286223  0.1414013 1.2793758   111
    ## [533]  {bottled water}            => {newspapers}               0.011286223  0.1021159 1.2793758   111
    ## [534]  {newspapers}               => {tropical fruit}           0.011794611  0.1477707 1.4082605   116
    ## [535]  {tropical fruit}           => {newspapers}               0.011794611  0.1124031 1.4082605   116
    ## [536]  {newspapers}               => {root vegetables}          0.011489578  0.1439490 1.3206519   113
    ## [537]  {root vegetables}          => {newspapers}               0.011489578  0.1054104 1.3206519   113
    ## [538]  {newspapers}               => {soda}                     0.014641586  0.1834395 1.0519693   144
    ## [539]  {newspapers}               => {yogurt}                   0.015353330  0.1923567 1.3788834   151
    ## [540]  {yogurt}                   => {newspapers}               0.015353330  0.1100583 1.3788834   151
    ## [541]  {newspapers}               => {rolls/buns}               0.019725470  0.2471338 1.3435934   194
    ## [542]  {rolls/buns}               => {newspapers}               0.019725470  0.1072416 1.3435934   194
    ## [543]  {newspapers}               => {other vegetables}         0.019318760  0.2420382 1.2508912   190
    ## [544]  {newspapers}               => {whole milk}               0.027351296  0.3426752 1.3411103   269
    ## [545]  {whole milk}               => {newspapers}               0.027351296  0.1070434 1.3411103   269
    ## [546]  {domestic eggs}            => {fruit/vegetable juice}    0.008032537  0.1266026 1.7512464    79
    ## [547]  {fruit/vegetable juice}    => {domestic eggs}            0.008032537  0.1111111 1.7512464    79
    ## [548]  {domestic eggs}            => {whipped/sour cream}       0.009964413  0.1570513 2.1909211    98
    ## [549]  {whipped/sour cream}       => {domestic eggs}            0.009964413  0.1390071 2.1909211    98
    ## [550]  {domestic eggs}            => {pip fruit}                0.008642603  0.1362179 1.8006768    85
    ## [551]  {pip fruit}                => {domestic eggs}            0.008642603  0.1142473 1.8006768    85
    ## [552]  {domestic eggs}            => {pastry}                   0.009049314  0.1426282 1.6031410    89
    ## [553]  {pastry}                   => {domestic eggs}            0.009049314  0.1017143 1.6031410    89
    ## [554]  {domestic eggs}            => {citrus fruit}             0.010371124  0.1634615 1.9749929   102
    ## [555]  {citrus fruit}             => {domestic eggs}            0.010371124  0.1253071 1.9749929   102
    ## [556]  {domestic eggs}            => {shopping bags}            0.009049314  0.1426282 1.4476248    89
    ## [557]  {domestic eggs}            => {sausage}                  0.009557702  0.1506410 1.6034139    94
    ## [558]  {sausage}                  => {domestic eggs}            0.009557702  0.1017316 1.6034139    94
    ## [559]  {domestic eggs}            => {bottled water}            0.009150991  0.1442308 1.3049766    90
    ## [560]  {domestic eggs}            => {tropical fruit}           0.011387900  0.1794872 1.7105198   112
    ## [561]  {tropical fruit}           => {domestic eggs}            0.011387900  0.1085271 1.7105198   112
    ## [562]  {domestic eggs}            => {root vegetables}          0.014336553  0.2259615 2.0730706   141
    ## [563]  {root vegetables}          => {domestic eggs}            0.014336553  0.1315299 2.0730706   141
    ## [564]  {domestic eggs}            => {soda}                     0.012404677  0.1955128 1.1212062   122
    ## [565]  {domestic eggs}            => {yogurt}                   0.014336553  0.2259615 1.6197753   141
    ## [566]  {yogurt}                   => {domestic eggs}            0.014336553  0.1027697 1.6197753   141
    ## [567]  {domestic eggs}            => {rolls/buns}               0.015658363  0.2467949 1.3417510   154
    ## [568]  {domestic eggs}            => {other vegetables}         0.022267412  0.3509615 1.8138238   219
    ## [569]  {other vegetables}         => {domestic eggs}            0.022267412  0.1150815 1.8138238   219
    ## [570]  {domestic eggs}            => {whole milk}               0.029994916  0.4727564 1.8502027   295
    ## [571]  {whole milk}               => {domestic eggs}            0.029994916  0.1173896 1.8502027   295
    ## [572]  {fruit/vegetable juice}    => {whipped/sour cream}       0.009049314  0.1251758 1.7462469    89
    ## [573]  {whipped/sour cream}       => {fruit/vegetable juice}    0.009049314  0.1262411 1.7462469    89
    ## [574]  {fruit/vegetable juice}    => {pip fruit}                0.009557702  0.1322082 1.7476710    94
    ## [575]  {pip fruit}                => {fruit/vegetable juice}    0.009557702  0.1263441 1.7476710    94
    ## [576]  {fruit/vegetable juice}    => {pastry}                   0.008540925  0.1181435 1.3279325    84
    ## [577]  {fruit/vegetable juice}    => {citrus fruit}             0.010371124  0.1434599 1.7333271   102
    ## [578]  {citrus fruit}             => {fruit/vegetable juice}    0.010371124  0.1253071 1.7333271   102
    ## [579]  {fruit/vegetable juice}    => {shopping bags}            0.010676157  0.1476793 1.4988918   105
    ## [580]  {shopping bags}            => {fruit/vegetable juice}    0.010676157  0.1083591 1.4988918   105
    ## [581]  {fruit/vegetable juice}    => {sausage}                  0.010066090  0.1392405 1.4820675    99
    ## [582]  {sausage}                  => {fruit/vegetable juice}    0.010066090  0.1071429 1.4820675    99
    ## [583]  {fruit/vegetable juice}    => {bottled water}            0.014234875  0.1969058 1.7815715   140
    ## [584]  {bottled water}            => {fruit/vegetable juice}    0.014234875  0.1287948 1.7815715   140
    ## [585]  {fruit/vegetable juice}    => {tropical fruit}           0.013726487  0.1898734 1.8095010   135
    ## [586]  {tropical fruit}           => {fruit/vegetable juice}    0.013726487  0.1308140 1.8095010   135
    ## [587]  {fruit/vegetable juice}    => {root vegetables}          0.011997966  0.1659634 1.5226216   118
    ## [588]  {root vegetables}          => {fruit/vegetable juice}    0.011997966  0.1100746 1.5226216   118
    ## [589]  {fruit/vegetable juice}    => {soda}                     0.018403660  0.2545710 1.4598869   181
    ## [590]  {soda}                     => {fruit/vegetable juice}    0.018403660  0.1055394 1.4598869   181
    ## [591]  {fruit/vegetable juice}    => {yogurt}                   0.018708693  0.2587904 1.8551049   184
    ## [592]  {yogurt}                   => {fruit/vegetable juice}    0.018708693  0.1341108 1.8551049   184
    ## [593]  {fruit/vegetable juice}    => {rolls/buns}               0.014539908  0.2011252 1.0934583   143
    ## [594]  {fruit/vegetable juice}    => {other vegetables}         0.021047280  0.2911392 1.5046529   207
    ## [595]  {other vegetables}         => {fruit/vegetable juice}    0.021047280  0.1087756 1.5046529   207
    ## [596]  {fruit/vegetable juice}    => {whole milk}               0.026639553  0.3684951 1.4421604   262
    ## [597]  {whole milk}               => {fruit/vegetable juice}    0.026639553  0.1042579 1.4421604   262
    ## [598]  {whipped/sour cream}       => {pip fruit}                0.009252669  0.1290780 1.7062934    91
    ## [599]  {pip fruit}                => {whipped/sour cream}       0.009252669  0.1223118 1.7062934    91
    ## [600]  {whipped/sour cream}       => {pastry}                   0.007524148  0.1049645 1.1798014    74
    ## [601]  {whipped/sour cream}       => {citrus fruit}             0.010879512  0.1517730 1.8337690   107
    ## [602]  {citrus fruit}             => {whipped/sour cream}       0.010879512  0.1314496 1.8337690   107
    ## [603]  {whipped/sour cream}       => {shopping bags}            0.007930859  0.1106383 1.1229388    78
    ## [604]  {whipped/sour cream}       => {sausage}                  0.009049314  0.1262411 1.3437030    89
    ## [605]  {whipped/sour cream}       => {bottled water}            0.008744281  0.1219858 1.1037079    86
    ## [606]  {whipped/sour cream}       => {tropical fruit}           0.013828165  0.1929078 1.8384188   136
    ## [607]  {tropical fruit}           => {whipped/sour cream}       0.013828165  0.1317829 1.8384188   136
    ## [608]  {whipped/sour cream}       => {root vegetables}          0.017081851  0.2382979 2.1862496   168
    ## [609]  {root vegetables}          => {whipped/sour cream}       0.017081851  0.1567164 2.1862496   168
    ## [610]  {whipped/sour cream}       => {soda}                     0.011591256  0.1617021 0.9273122   114
    ## [611]  {whipped/sour cream}       => {yogurt}                   0.020742247  0.2893617 2.0742510   204
    ## [612]  {yogurt}                   => {whipped/sour cream}       0.020742247  0.1486880 2.0742510   204
    ## [613]  {whipped/sour cream}       => {rolls/buns}               0.014641586  0.2042553 1.1104760   144
    ## [614]  {whipped/sour cream}       => {other vegetables}         0.028876462  0.4028369 2.0819237   284
    ## [615]  {other vegetables}         => {whipped/sour cream}       0.028876462  0.1492380 2.0819237   284
    ## [616]  {whipped/sour cream}       => {whole milk}               0.032231825  0.4496454 1.7597542   317
    ## [617]  {whole milk}               => {whipped/sour cream}       0.032231825  0.1261441 1.7597542   317
    ## [618]  {pip fruit}                => {pastry}                   0.010676157  0.1411290 1.5862903   105
    ## [619]  {pastry}                   => {pip fruit}                0.010676157  0.1200000 1.5862903   105
    ## [620]  {pip fruit}                => {citrus fruit}             0.013828165  0.1827957 2.2085942   136
    ## [621]  {citrus fruit}             => {pip fruit}                0.013828165  0.1670762 2.2085942   136
    ## [622]  {pip fruit}                => {shopping bags}            0.009354347  0.1236559 1.2550629    92
    ## [623]  {pip fruit}                => {sausage}                  0.010777834  0.1424731 1.5164752   106
    ## [624]  {sausage}                  => {pip fruit}                0.010777834  0.1147186 1.5164752   106
    ## [625]  {pip fruit}                => {bottled water}            0.010574479  0.1397849 1.2647516   104
    ## [626]  {pip fruit}                => {tropical fruit}           0.020437214  0.2701613 2.5746476   201
    ## [627]  {tropical fruit}           => {pip fruit}                0.020437214  0.1947674 2.5746476   201
    ## [628]  {pip fruit}                => {root vegetables}          0.015556685  0.2056452 1.8866793   153
    ## [629]  {root vegetables}          => {pip fruit}                0.015556685  0.1427239 1.8866793   153
    ## [630]  {pip fruit}                => {soda}                     0.013319776  0.1760753 1.0097378   131
    ## [631]  {pip fruit}                => {yogurt}                   0.017996950  0.2379032 1.7053777   177
    ## [632]  {yogurt}                   => {pip fruit}                0.017996950  0.1290087 1.7053777   177
    ## [633]  {pip fruit}                => {rolls/buns}               0.013929842  0.1841398 1.0011138   137
    ## [634]  {pip fruit}                => {other vegetables}         0.026131164  0.3454301 1.7852365   257
    ## [635]  {other vegetables}         => {pip fruit}                0.026131164  0.1350499 1.7852365   257
    ## [636]  {pip fruit}                => {whole milk}               0.030096594  0.3978495 1.5570432   296
    ## [637]  {whole milk}               => {pip fruit}                0.030096594  0.1177875 1.5570432   296
    ## [638]  {pastry}                   => {citrus fruit}             0.009761057  0.1097143 1.3256020    96
    ## [639]  {citrus fruit}             => {pastry}                   0.009761057  0.1179361 1.3256020    96
    ## [640]  {pastry}                   => {shopping bags}            0.011896289  0.1337143 1.3571517   117
    ## [641]  {shopping bags}            => {pastry}                   0.011896289  0.1207430 1.3571517   117
    ## [642]  {pastry}                   => {sausage}                  0.012506355  0.1405714 1.4962338   123
    ## [643]  {sausage}                  => {pastry}                   0.012506355  0.1331169 1.4962338   123
    ## [644]  {pastry}                   => {bottled water}            0.008947636  0.1005714 0.9099540    88
    ## [645]  {pastry}                   => {tropical fruit}           0.013218099  0.1485714 1.4158915   130
    ## [646]  {tropical fruit}           => {pastry}                   0.013218099  0.1259690 1.4158915   130
    ## [647]  {pastry}                   => {root vegetables}          0.010981190  0.1234286 1.1323881   108
    ## [648]  {root vegetables}          => {pastry}                   0.010981190  0.1007463 1.1323881   108
    ## [649]  {pastry}                   => {soda}                     0.021047280  0.2365714 1.3566647   207
    ## [650]  {soda}                     => {pastry}                   0.021047280  0.1206997 1.3566647   207
    ## [651]  {pastry}                   => {yogurt}                   0.017691917  0.1988571 1.4254810   174
    ## [652]  {yogurt}                   => {pastry}                   0.017691917  0.1268222 1.4254810   174
    ## [653]  {pastry}                   => {rolls/buns}               0.020945602  0.2354286 1.2799558   206
    ## [654]  {rolls/buns}               => {pastry}                   0.020945602  0.1138751 1.2799558   206
    ## [655]  {pastry}                   => {other vegetables}         0.022572445  0.2537143 1.3112349   222
    ## [656]  {other vegetables}         => {pastry}                   0.022572445  0.1166579 1.3112349   222
    ## [657]  {pastry}                   => {whole milk}               0.033248602  0.3737143 1.4625865   327
    ## [658]  {whole milk}               => {pastry}                   0.033248602  0.1301234 1.4625865   327
    ## [659]  {citrus fruit}             => {shopping bags}            0.009761057  0.1179361 1.1970090    96
    ## [660]  {citrus fruit}             => {sausage}                  0.011286223  0.1363636 1.4514463   111
    ## [661]  {sausage}                  => {citrus fruit}             0.011286223  0.1201299 1.4514463   111
    ## [662]  {citrus fruit}             => {bottled water}            0.013523132  0.1633907 1.4783323   133
    ## [663]  {bottled water}            => {citrus fruit}             0.013523132  0.1223551 1.4783323   133
    ## [664]  {citrus fruit}             => {tropical fruit}           0.019928826  0.2407862 2.2947022   196
    ## [665]  {tropical fruit}           => {citrus fruit}             0.019928826  0.1899225 2.2947022   196
    ## [666]  {citrus fruit}             => {root vegetables}          0.017691917  0.2137592 1.9611211   174
    ## [667]  {root vegetables}          => {citrus fruit}             0.017691917  0.1623134 1.9611211   174
    ## [668]  {citrus fruit}             => {soda}                     0.012811388  0.1547912 0.8876799   126
    ## [669]  {citrus fruit}             => {yogurt}                   0.021657346  0.2616708 1.8757521   213
    ## [670]  {yogurt}                   => {citrus fruit}             0.021657346  0.1552478 1.8757521   213
    ## [671]  {citrus fruit}             => {rolls/buns}               0.016776817  0.2027027 1.1020349   165
    ## [672]  {citrus fruit}             => {other vegetables}         0.028876462  0.3488943 1.8031403   284
    ## [673]  {other vegetables}         => {citrus fruit}             0.028876462  0.1492380 1.8031403   284
    ## [674]  {citrus fruit}             => {whole milk}               0.030503305  0.3685504 1.4423768   300
    ## [675]  {whole milk}               => {citrus fruit}             0.030503305  0.1193792 1.4423768   300
    ## [676]  {shopping bags}            => {sausage}                  0.015658363  0.1589267 1.6916065   154
    ## [677]  {sausage}                  => {shopping bags}            0.015658363  0.1666667 1.6916065   154
    ## [678]  {shopping bags}            => {bottled water}            0.010981190  0.1114551 1.0084278   108
    ## [679]  {shopping bags}            => {tropical fruit}           0.013523132  0.1372549 1.3080445   133
    ## [680]  {tropical fruit}           => {shopping bags}            0.013523132  0.1288760 1.3080445   133
    ## [681]  {shopping bags}            => {root vegetables}          0.012811388  0.1300310 1.1929613   126
    ## [682]  {root vegetables}          => {shopping bags}            0.012811388  0.1175373 1.1929613   126
    ## [683]  {shopping bags}            => {soda}                     0.024605999  0.2497420 1.4321939   242
    ## [684]  {soda}                     => {shopping bags}            0.024605999  0.1411079 1.4321939   242
    ## [685]  {shopping bags}            => {yogurt}                   0.015251652  0.1547988 1.1096544   150
    ## [686]  {yogurt}                   => {shopping bags}            0.015251652  0.1093294 1.1096544   150
    ## [687]  {shopping bags}            => {rolls/buns}               0.019522115  0.1981424 1.0772419   192
    ## [688]  {rolls/buns}               => {shopping bags}            0.019522115  0.1061360 1.0772419   192
    ## [689]  {shopping bags}            => {other vegetables}         0.023182511  0.2352941 1.2160366   228
    ## [690]  {other vegetables}         => {shopping bags}            0.023182511  0.1198108 1.2160366   228
    ## [691]  {shopping bags}            => {whole milk}               0.024504321  0.2487100 0.9733637   241
    ## [692]  {sausage}                  => {bottled water}            0.011997966  0.1277056 1.1554598   118
    ## [693]  {bottled water}            => {sausage}                  0.011997966  0.1085557 1.1554598   118
    ## [694]  {sausage}                  => {tropical fruit}           0.013929842  0.1482684 1.4130036   137
    ## [695]  {tropical fruit}           => {sausage}                  0.013929842  0.1327519 1.4130036   137
    ## [696]  {sausage}                  => {root vegetables}          0.014946619  0.1590909 1.4595700   147
    ## [697]  {root vegetables}          => {sausage}                  0.014946619  0.1371269 1.4595700   147
    ## [698]  {sausage}                  => {soda}                     0.024300966  0.2586580 1.4833245   239
    ## [699]  {soda}                     => {sausage}                  0.024300966  0.1393586 1.4833245   239
    ## [700]  {sausage}                  => {yogurt}                   0.019623793  0.2088745 1.4972889   193
    ## [701]  {yogurt}                   => {sausage}                  0.019623793  0.1406706 1.4972889   193
    ## [702]  {sausage}                  => {rolls/buns}               0.030604982  0.3257576 1.7710480   301
    ## [703]  {rolls/buns}               => {sausage}                  0.030604982  0.1663903 1.7710480   301
    ## [704]  {sausage}                  => {other vegetables}         0.026944586  0.2867965 1.4822091   265
    ## [705]  {other vegetables}         => {sausage}                  0.026944586  0.1392538 1.4822091   265
    ## [706]  {sausage}                  => {whole milk}               0.029893238  0.3181818 1.2452520   294
    ## [707]  {whole milk}               => {sausage}                  0.029893238  0.1169916 1.2452520   294
    ## [708]  {bottled water}            => {tropical fruit}           0.018505338  0.1674333 1.5956459   182
    ## [709]  {tropical fruit}           => {bottled water}            0.018505338  0.1763566 1.5956459   182
    ## [710]  {bottled water}            => {root vegetables}          0.015658363  0.1416743 1.2997827   154
    ## [711]  {root vegetables}          => {bottled water}            0.015658363  0.1436567 1.2997827   154
    ## [712]  {bottled water}            => {soda}                     0.028978139  0.2621895 1.5035766   285
    ## [713]  {soda}                     => {bottled water}            0.028978139  0.1661808 1.5035766   285
    ## [714]  {bottled water}            => {yogurt}                   0.022979156  0.2079117 1.4903873   226
    ## [715]  {yogurt}                   => {bottled water}            0.022979156  0.1647230 1.4903873   226
    ## [716]  {bottled water}            => {rolls/buns}               0.024199288  0.2189512 1.1903734   238
    ## [717]  {rolls/buns}               => {bottled water}            0.024199288  0.1315644 1.1903734   238
    ## [718]  {bottled water}            => {other vegetables}         0.024809354  0.2244710 1.1601012   244
    ## [719]  {other vegetables}         => {bottled water}            0.024809354  0.1282186 1.1601012   244
    ## [720]  {bottled water}            => {whole milk}               0.034367056  0.3109476 1.2169396   338
    ## [721]  {whole milk}               => {bottled water}            0.034367056  0.1345006 1.2169396   338
    ## [722]  {tropical fruit}           => {root vegetables}          0.021047280  0.2005814 1.8402220   207
    ## [723]  {root vegetables}          => {tropical fruit}           0.021047280  0.1930970 1.8402220   207
    ## [724]  {tropical fruit}           => {soda}                     0.020843925  0.1986434 1.1391592   205
    ## [725]  {soda}                     => {tropical fruit}           0.020843925  0.1195335 1.1391592   205
    ## [726]  {tropical fruit}           => {yogurt}                   0.029283172  0.2790698 2.0004746   288
    ## [727]  {yogurt}                   => {tropical fruit}           0.029283172  0.2099125 2.0004746   288
    ## [728]  {tropical fruit}           => {rolls/buns}               0.024605999  0.2344961 1.2748863   242
    ## [729]  {rolls/buns}               => {tropical fruit}           0.024605999  0.1337756 1.2748863   242
    ## [730]  {tropical fruit}           => {other vegetables}         0.035892222  0.3420543 1.7677896   353
    ## [731]  {other vegetables}         => {tropical fruit}           0.035892222  0.1854966 1.7677896   353
    ## [732]  {tropical fruit}           => {whole milk}               0.042297916  0.4031008 1.5775950   416
    ## [733]  {whole milk}               => {tropical fruit}           0.042297916  0.1655392 1.5775950   416
    ## [734]  {root vegetables}          => {soda}                     0.018607016  0.1707090 0.9789636   183
    ## [735]  {soda}                     => {root vegetables}          0.018607016  0.1067055 0.9789636   183
    ## [736]  {root vegetables}          => {yogurt}                   0.025826131  0.2369403 1.6984751   254
    ## [737]  {yogurt}                   => {root vegetables}          0.025826131  0.1851312 1.6984751   254
    ## [738]  {root vegetables}          => {rolls/buns}               0.024300966  0.2229478 1.2121013   239
    ## [739]  {rolls/buns}               => {root vegetables}          0.024300966  0.1321172 1.2121013   239
    ## [740]  {root vegetables}          => {other vegetables}         0.047381800  0.4347015 2.2466049   466
    ## [741]  {other vegetables}         => {root vegetables}          0.047381800  0.2448765 2.2466049   466
    ## [742]  {root vegetables}          => {whole milk}               0.048906965  0.4486940 1.7560310   481
    ## [743]  {whole milk}               => {root vegetables}          0.048906965  0.1914047 1.7560310   481
    ## [744]  {soda}                     => {yogurt}                   0.027351296  0.1568513 1.1243678   269
    ## [745]  {yogurt}                   => {soda}                     0.027351296  0.1960641 1.1243678   269
    ## [746]  {soda}                     => {rolls/buns}               0.038332486  0.2198251 1.1951242   377
    ## [747]  {rolls/buns}               => {soda}                     0.038332486  0.2084024 1.1951242   377
    ## [748]  {soda}                     => {other vegetables}         0.032740214  0.1877551 0.9703476   322
    ## [749]  {other vegetables}         => {soda}                     0.032740214  0.1692065 0.9703476   322
    ## [750]  {soda}                     => {whole milk}               0.040061007  0.2297376 0.8991124   394
    ## [751]  {whole milk}               => {soda}                     0.040061007  0.1567847 0.8991124   394
    ## [752]  {yogurt}                   => {rolls/buns}               0.034367056  0.2463557 1.3393633   338
    ## [753]  {rolls/buns}               => {yogurt}                   0.034367056  0.1868436 1.3393633   338
    ## [754]  {yogurt}                   => {other vegetables}         0.043416370  0.3112245 1.6084566   427
    ## [755]  {other vegetables}         => {yogurt}                   0.043416370  0.2243826 1.6084566   427
    ## [756]  {yogurt}                   => {whole milk}               0.056024403  0.4016035 1.5717351   551
    ## [757]  {whole milk}               => {yogurt}                   0.056024403  0.2192598 1.5717351   551
    ## [758]  {rolls/buns}               => {other vegetables}         0.042602949  0.2316197 1.1970465   419
    ## [759]  {other vegetables}         => {rolls/buns}               0.042602949  0.2201787 1.1970465   419
    ## [760]  {rolls/buns}               => {whole milk}               0.056634469  0.3079049 1.2050318   557
    ## [761]  {whole milk}               => {rolls/buns}               0.056634469  0.2216474 1.2050318   557
    ## [762]  {other vegetables}         => {whole milk}               0.074834774  0.3867578 1.5136341   736
    ## [763]  {whole milk}               => {other vegetables}         0.074834774  0.2928770 1.5136341   736
    ## [764]  {oil,                                                                                          
    ##         other vegetables}         => {whole milk}               0.005083884  0.5102041 1.9967597    50
    ## [765]  {oil,                                                                                          
    ##         whole milk}               => {other vegetables}         0.005083884  0.4504505 2.3279980    50
    ## [766]  {onions,                                                                                       
    ##         root vegetables}          => {other vegetables}         0.005693950  0.6021505 3.1120076    56
    ## [767]  {onions,                                                                                       
    ##         other vegetables}         => {root vegetables}          0.005693950  0.4000000 3.6697761    56
    ## [768]  {other vegetables,                                                                             
    ##         root vegetables}          => {onions}                   0.005693950  0.1201717 3.8750440    56
    ## [769]  {onions,                                                                                       
    ##         other vegetables}         => {whole milk}               0.006609049  0.4642857 1.8170513    65
    ## [770]  {onions,                                                                                       
    ##         whole milk}               => {other vegetables}         0.006609049  0.5462185 2.8229421    65
    ## [771]  {hamburger meat,                                                                               
    ##         other vegetables}         => {whole milk}               0.006304016  0.4558824 1.7841635    62
    ## [772]  {hamburger meat,                                                                               
    ##         whole milk}               => {other vegetables}         0.006304016  0.4275862 2.2098320    62
    ## [773]  {hygiene articles,                                                                             
    ##         other vegetables}         => {whole milk}               0.005185562  0.5425532 2.1233628    51
    ## [774]  {hygiene articles,                                                                             
    ##         whole milk}               => {other vegetables}         0.005185562  0.4047619 2.0918725    51
    ## [775]  {other vegetables,                                                                             
    ##         sugar}                    => {whole milk}               0.006304016  0.5849057 2.2891155    62
    ## [776]  {sugar,                                                                                        
    ##         whole milk}               => {other vegetables}         0.006304016  0.4189189 2.1650381    62
    ## [777]  {long life bakery product,                                                                     
    ##         other vegetables}         => {whole milk}               0.005693950  0.5333333 2.0872795    56
    ## [778]  {long life bakery product,                                                                     
    ##         whole milk}               => {other vegetables}         0.005693950  0.4210526 2.1760655    56
    ## [779]  {cream cheese ,                                                                                
    ##         yogurt}                   => {other vegetables}         0.005287239  0.4262295 2.2028204    52
    ## [780]  {cream cheese ,                                                                                
    ##         other vegetables}         => {yogurt}                   0.005287239  0.3851852 2.7611489    52
    ## [781]  {other vegetables,                                                                             
    ##         yogurt}                   => {cream cheese }            0.005287239  0.1217799 3.0710383    52
    ## [782]  {cream cheese ,                                                                                
    ##         yogurt}                   => {whole milk}               0.006609049  0.5327869 2.0851409    65
    ## [783]  {cream cheese ,                                                                                
    ##         whole milk}               => {yogurt}                   0.006609049  0.4012346 2.8761968    65
    ## [784]  {whole milk,                                                                                   
    ##         yogurt}                   => {cream cheese }            0.006609049  0.1179673 2.9748941    65
    ## [785]  {cream cheese ,                                                                                
    ##         other vegetables}         => {whole milk}               0.006710727  0.4888889 1.9133395    66
    ## [786]  {cream cheese ,                                                                                
    ##         whole milk}               => {other vegetables}         0.006710727  0.4074074 2.1055449    66
    ## [787]  {chicken,                                                                                      
    ##         root vegetables}          => {other vegetables}         0.005693950  0.5233645 2.7048291    56
    ## [788]  {chicken,                                                                                      
    ##         other vegetables}         => {root vegetables}          0.005693950  0.3181818 2.9191401    56
    ## [789]  {other vegetables,                                                                             
    ##         root vegetables}          => {chicken}                  0.005693950  0.1201717 2.8006834    56
    ## [790]  {chicken,                                                                                      
    ##         root vegetables}          => {whole milk}               0.005998983  0.5514019 2.1579934    59
    ## [791]  {chicken,                                                                                      
    ##         whole milk}               => {root vegetables}          0.005998983  0.3410405 3.1288554    59
    ## [792]  {root vegetables,                                                                              
    ##         whole milk}               => {chicken}                  0.005998983  0.1226611 2.8587018    59
    ## [793]  {chicken,                                                                                      
    ##         rolls/buns}               => {whole milk}               0.005287239  0.5473684 2.1422079    52
    ## [794]  {chicken,                                                                                      
    ##         whole milk}               => {rolls/buns}               0.005287239  0.3005780 1.6341542    52
    ## [795]  {chicken,                                                                                      
    ##         other vegetables}         => {whole milk}               0.008439248  0.4715909 1.8456413    83
    ## [796]  {chicken,                                                                                      
    ##         whole milk}               => {other vegetables}         0.008439248  0.4797688 2.4795197    83
    ## [797]  {other vegetables,                                                                             
    ##         whole milk}               => {chicken}                  0.008439248  0.1127717 2.6282229    83
    ## [798]  {other vegetables,                                                                             
    ##         white bread}              => {whole milk}               0.005897306  0.4296296 1.6814196    58
    ## [799]  {white bread,                                                                                  
    ##         whole milk}               => {other vegetables}         0.005897306  0.3452381 1.7842442    58
    ## [800]  {chocolate,                                                                                    
    ##         soda}                     => {whole milk}               0.005083884  0.3759398 1.4712966    50
    ## [801]  {chocolate,                                                                                    
    ##         whole milk}               => {soda}                     0.005083884  0.3048780 1.7483823    50
    ## [802]  {soda,                                                                                         
    ##         whole milk}               => {chocolate}                0.005083884  0.1269036 2.5575747    50
    ## [803]  {chocolate,                                                                                    
    ##         other vegetables}         => {whole milk}               0.005490595  0.4320000 1.6906964    54
    ## [804]  {chocolate,                                                                                    
    ##         whole milk}               => {other vegetables}         0.005490595  0.3292683 1.7017098    54
    ## [805]  {coffee,                                                                                       
    ##         yogurt}                   => {whole milk}               0.005083884  0.5208333 2.0383589    50
    ## [806]  {coffee,                                                                                       
    ##         whole milk}               => {yogurt}                   0.005083884  0.2717391 1.9479259    50
    ## [807]  {coffee,                                                                                       
    ##         other vegetables}         => {whole milk}               0.006405694  0.4772727 1.8678779    63
    ## [808]  {coffee,                                                                                       
    ##         whole milk}               => {other vegetables}         0.006405694  0.3423913 1.7695315    63
    ## [809]  {frozen vegetables,                                                                            
    ##         root vegetables}          => {other vegetables}         0.006100661  0.5263158 2.7200819    60
    ## [810]  {frozen vegetables,                                                                            
    ##         other vegetables}         => {root vegetables}          0.006100661  0.3428571 3.1455224    60
    ## [811]  {other vegetables,                                                                             
    ##         root vegetables}          => {frozen vegetables}        0.006100661  0.1287554 2.6771861    60
    ## [812]  {frozen vegetables,                                                                            
    ##         root vegetables}          => {whole milk}               0.006202339  0.5350877 2.0941455    61
    ## [813]  {frozen vegetables,                                                                            
    ##         whole milk}               => {root vegetables}          0.006202339  0.3034826 2.7842829    61
    ## [814]  {root vegetables,                                                                              
    ##         whole milk}               => {frozen vegetables}        0.006202339  0.1268191 2.6369262    61
    ## [815]  {frozen vegetables,                                                                            
    ##         yogurt}                   => {other vegetables}         0.005287239  0.4262295 2.2028204    52
    ## [816]  {frozen vegetables,                                                                            
    ##         other vegetables}         => {yogurt}                   0.005287239  0.2971429 2.1300292    52
    ## [817]  {other vegetables,                                                                             
    ##         yogurt}                   => {frozen vegetables}        0.005287239  0.1217799 2.5321457    52
    ## [818]  {frozen vegetables,                                                                            
    ##         yogurt}                   => {whole milk}               0.006100661  0.4918033 1.9247454    60
    ## [819]  {frozen vegetables,                                                                            
    ##         whole milk}               => {yogurt}                   0.006100661  0.2985075 2.1398111    60
    ## [820]  {whole milk,                                                                                   
    ##         yogurt}                   => {frozen vegetables}        0.006100661  0.1088929 2.2641900    60
    ## [821]  {frozen vegetables,                                                                            
    ##         rolls/buns}               => {whole milk}               0.005083884  0.5000000 1.9568245    50
    ## [822]  {frozen vegetables,                                                                            
    ##         whole milk}               => {rolls/buns}               0.005083884  0.2487562 1.3524143    50
    ## [823]  {frozen vegetables,                                                                            
    ##         other vegetables}         => {whole milk}               0.009659380  0.5428571 2.1245523    95
    ## [824]  {frozen vegetables,                                                                            
    ##         whole milk}               => {other vegetables}         0.009659380  0.4726368 2.4426606    95
    ## [825]  {other vegetables,                                                                             
    ##         whole milk}               => {frozen vegetables}        0.009659380  0.1290761 2.6838548    95
    ## [826]  {beef,                                                                                         
    ##         root vegetables}          => {other vegetables}         0.007930859  0.4561404 2.3574043    78
    ## [827]  {beef,                                                                                         
    ##         other vegetables}         => {root vegetables}          0.007930859  0.4020619 3.6886925    78
    ## [828]  {other vegetables,                                                                             
    ##         root vegetables}          => {beef}                     0.007930859  0.1673820 3.1903134    78
    ## [829]  {beef,                                                                                         
    ##         root vegetables}          => {whole milk}               0.008032537  0.4619883 1.8080601    79
    ## [830]  {beef,                                                                                         
    ##         whole milk}               => {root vegetables}          0.008032537  0.3779904 3.4678506    79
    ## [831]  {root vegetables,                                                                              
    ##         whole milk}               => {beef}                     0.008032537  0.1642412 3.1304493    79
    ## [832]  {beef,                                                                                         
    ##         yogurt}                   => {other vegetables}         0.005185562  0.4434783 2.2919646    51
    ## [833]  {beef,                                                                                         
    ##         other vegetables}         => {yogurt}                   0.005185562  0.2628866 1.8844677    51
    ## [834]  {other vegetables,                                                                             
    ##         yogurt}                   => {beef}                     0.005185562  0.1194379 2.2764964    51
    ## [835]  {beef,                                                                                         
    ##         yogurt}                   => {whole milk}               0.006100661  0.5217391 2.0419038    60
    ## [836]  {beef,                                                                                         
    ##         whole milk}               => {yogurt}                   0.006100661  0.2870813 2.0579045    60
    ## [837]  {whole milk,                                                                                   
    ##         yogurt}                   => {beef}                     0.006100661  0.1088929 2.0755075    60
    ## [838]  {beef,                                                                                         
    ##         rolls/buns}               => {other vegetables}         0.005795628  0.4253731 2.1983945    57
    ## [839]  {beef,                                                                                         
    ##         other vegetables}         => {rolls/buns}               0.005795628  0.2938144 1.5973825    57
    ## [840]  {other vegetables,                                                                             
    ##         rolls/buns}               => {beef}                     0.005795628  0.1360382 2.5928984    57
    ## [841]  {beef,                                                                                         
    ##         rolls/buns}               => {whole milk}               0.006812405  0.5000000 1.9568245    67
    ## [842]  {beef,                                                                                         
    ##         whole milk}               => {rolls/buns}               0.006812405  0.3205742 1.7428673    67
    ## [843]  {rolls/buns,                                                                                   
    ##         whole milk}               => {beef}                     0.006812405  0.1202873 2.2926844    67
    ## [844]  {beef,                                                                                         
    ##         other vegetables}         => {whole milk}               0.009252669  0.4690722 1.8357838    91
    ## [845]  {beef,                                                                                         
    ##         whole milk}               => {other vegetables}         0.009252669  0.4354067 2.2502495    91
    ## [846]  {other vegetables,                                                                             
    ##         whole milk}               => {beef}                     0.009252669  0.1236413 2.3566128    91
    ## [847]  {curd,                                                                                         
    ##         whipped/sour cream}       => {whole milk}               0.005897306  0.5631068 2.2038024    58
    ## [848]  {curd,                                                                                         
    ##         whole milk}               => {whipped/sour cream}       0.005897306  0.2256809 3.1483291    58
    ## [849]  {whipped/sour cream,                                                                           
    ##         whole milk}               => {curd}                     0.005897306  0.1829653 3.4340911    58
    ## [850]  {curd,                                                                                         
    ##         tropical fruit}           => {yogurt}                   0.005287239  0.5148515 3.6906446    52
    ## [851]  {curd,                                                                                         
    ##         yogurt}                   => {tropical fruit}           0.005287239  0.3058824 2.9150707    52
    ## [852]  {tropical fruit,                                                                               
    ##         yogurt}                   => {curd}                     0.005287239  0.1805556 3.3888624    52
    ## [853]  {curd,                                                                                         
    ##         tropical fruit}           => {other vegetables}         0.005287239  0.5148515 2.6608326    52
    ## [854]  {curd,                                                                                         
    ##         other vegetables}         => {tropical fruit}           0.005287239  0.3076923 2.9323196    52
    ## [855]  {other vegetables,                                                                             
    ##         tropical fruit}           => {curd}                     0.005287239  0.1473088 2.7648509    52
    ## [856]  {curd,                                                                                         
    ##         tropical fruit}           => {whole milk}               0.006507372  0.6336634 2.4799360    64
    ## [857]  {curd,                                                                                         
    ##         whole milk}               => {tropical fruit}           0.006507372  0.2490272 2.3732392    64
    ## [858]  {tropical fruit,                                                                               
    ##         whole milk}               => {curd}                     0.006507372  0.1538462 2.8875514    64
    ## [859]  {curd,                                                                                         
    ##         root vegetables}          => {other vegetables}         0.005490595  0.5046729 2.6082280    54
    ## [860]  {curd,                                                                                         
    ##         other vegetables}         => {root vegetables}          0.005490595  0.3195266 2.9314780    54
    ## [861]  {other vegetables,                                                                             
    ##         root vegetables}          => {curd}                     0.005490595  0.1158798 2.1749582    54
    ## [862]  {curd,                                                                                         
    ##         root vegetables}          => {whole milk}               0.006202339  0.5700935 2.2311457    61
    ## [863]  {curd,                                                                                         
    ##         whole milk}               => {root vegetables}          0.006202339  0.2373541 2.1775909    61
    ## [864]  {root vegetables,                                                                              
    ##         whole milk}               => {curd}                     0.006202339  0.1268191 2.3802788    61
    ## [865]  {curd,                                                                                         
    ##         yogurt}                   => {other vegetables}         0.006100661  0.3529412 1.8240549    60
    ## [866]  {curd,                                                                                         
    ##         other vegetables}         => {yogurt}                   0.006100661  0.3550296 2.5449825    60
    ## [867]  {other vegetables,                                                                             
    ##         yogurt}                   => {curd}                     0.006100661  0.1405152 2.6373420    60
    ## [868]  {curd,                                                                                         
    ##         yogurt}                   => {whole milk}               0.010066090  0.5823529 2.2791250    99
    ## [869]  {curd,                                                                                         
    ##         whole milk}               => {yogurt}                   0.010066090  0.3852140 2.7613555    99
    ## [870]  {whole milk,                                                                                   
    ##         yogurt}                   => {curd}                     0.010066090  0.1796733 3.3723037    99
    ## [871]  {curd,                                                                                         
    ##         rolls/buns}               => {whole milk}               0.005897306  0.5858586 2.2928449    58
    ## [872]  {curd,                                                                                         
    ##         whole milk}               => {rolls/buns}               0.005897306  0.2256809 1.2269607    58
    ## [873]  {rolls/buns,                                                                                   
    ##         whole milk}               => {curd}                     0.005897306  0.1041293 1.9544109    58
    ## [874]  {curd,                                                                                         
    ##         other vegetables}         => {whole milk}               0.009862735  0.5739645 2.2462956    97
    ## [875]  {curd,                                                                                         
    ##         whole milk}               => {other vegetables}         0.009862735  0.3774319 1.9506268    97
    ## [876]  {other vegetables,                                                                             
    ##         whole milk}               => {curd}                     0.009862735  0.1317935 2.4736429    97
    ## [877]  {napkins,                                                                                      
    ##         yogurt}                   => {whole milk}               0.006100661  0.4958678 1.9406524    60
    ## [878]  {napkins,                                                                                      
    ##         whole milk}               => {yogurt}                   0.006100661  0.3092784 2.2170208    60
    ## [879]  {whole milk,                                                                                   
    ##         yogurt}                   => {napkins}                  0.006100661  0.1088929 2.0795376    60
    ## [880]  {napkins,                                                                                      
    ##         rolls/buns}               => {whole milk}               0.005287239  0.4521739 1.7696500    52
    ## [881]  {napkins,                                                                                      
    ##         whole milk}               => {rolls/buns}               0.005287239  0.2680412 1.4572612    52
    ## [882]  {napkins,                                                                                      
    ##         other vegetables}         => {whole milk}               0.006812405  0.4718310 1.8465809    67
    ## [883]  {napkins,                                                                                      
    ##         whole milk}               => {other vegetables}         0.006812405  0.3453608 1.7848785    67
    ## [884]  {pork,                                                                                         
    ##         root vegetables}          => {other vegetables}         0.007015760  0.5149254 2.6612144    69
    ## [885]  {other vegetables,                                                                             
    ##         pork}                     => {root vegetables}          0.007015760  0.3239437 2.9720018    69
    ## [886]  {other vegetables,                                                                             
    ##         root vegetables}          => {pork}                     0.007015760  0.1480687 2.5683516    69
    ## [887]  {pork,                                                                                         
    ##         root vegetables}          => {whole milk}               0.006812405  0.5000000 1.9568245    67
    ## [888]  {pork,                                                                                         
    ##         whole milk}               => {root vegetables}          0.006812405  0.3073394 2.8196674    67
    ## [889]  {root vegetables,                                                                              
    ##         whole milk}               => {pork}                     0.006812405  0.1392931 2.4161341    67
    ## [890]  {pork,                                                                                         
    ##         rolls/buns}               => {other vegetables}         0.005592272  0.4954955 2.5607978    55
    ## [891]  {other vegetables,                                                                             
    ##         pork}                     => {rolls/buns}               0.005592272  0.2582160 1.4038441    55
    ## [892]  {other vegetables,                                                                             
    ##         rolls/buns}               => {pork}                     0.005592272  0.1312649 2.2768791    55
    ## [893]  {pork,                                                                                         
    ##         rolls/buns}               => {whole milk}               0.006202339  0.5495495 2.1507441    61
    ## [894]  {pork,                                                                                         
    ##         whole milk}               => {rolls/buns}               0.006202339  0.2798165 1.5212799    61
    ## [895]  {rolls/buns,                                                                                   
    ##         whole milk}               => {pork}                     0.006202339  0.1095153 1.8996166    61
    ## [896]  {other vegetables,                                                                             
    ##         pork}                     => {whole milk}               0.010167768  0.4694836 1.8373939   100
    ## [897]  {pork,                                                                                         
    ##         whole milk}               => {other vegetables}         0.010167768  0.4587156 2.3707136   100
    ## [898]  {other vegetables,                                                                             
    ##         whole milk}               => {pork}                     0.010167768  0.1358696 2.3567499   100
    ## [899]  {frankfurter,                                                                                  
    ##         tropical fruit}           => {whole milk}               0.005185562  0.5483871 2.1461946    51
    ## [900]  {frankfurter,                                                                                  
    ##         whole milk}               => {tropical fruit}           0.005185562  0.2524752 2.4060989    51
    ## [901]  {tropical fruit,                                                                               
    ##         whole milk}               => {frankfurter}              0.005185562  0.1225962 2.0788503    51
    ## [902]  {frankfurter,                                                                                  
    ##         root vegetables}          => {whole milk}               0.005083884  0.5000000 1.9568245    50
    ## [903]  {frankfurter,                                                                                  
    ##         whole milk}               => {root vegetables}          0.005083884  0.2475248 2.2709011    50
    ## [904]  {root vegetables,                                                                              
    ##         whole milk}               => {frankfurter}              0.005083884  0.1039501 1.7626712    50
    ## [905]  {frankfurter,                                                                                  
    ##         yogurt}                   => {whole milk}               0.006202339  0.5545455 2.1702963    61
    ## [906]  {frankfurter,                                                                                  
    ##         whole milk}               => {yogurt}                   0.006202339  0.3019802 2.1647050    61
    ## [907]  {whole milk,                                                                                   
    ##         yogurt}                   => {frankfurter}              0.006202339  0.1107078 1.8772608    61
    ## [908]  {frankfurter,                                                                                  
    ##         rolls/buns}               => {other vegetables}         0.005592272  0.2910053 1.5039606    55
    ## [909]  {frankfurter,                                                                                  
    ##         other vegetables}         => {rolls/buns}               0.005592272  0.3395062 1.8457950    55
    ## [910]  {other vegetables,                                                                             
    ##         rolls/buns}               => {frankfurter}              0.005592272  0.1312649 2.2258456    55
    ## [911]  {frankfurter,                                                                                  
    ##         rolls/buns}               => {whole milk}               0.005998983  0.3121693 1.2217211    59
    ## [912]  {frankfurter,                                                                                  
    ##         whole milk}               => {rolls/buns}               0.005998983  0.2920792 1.5879486    59
    ## [913]  {rolls/buns,                                                                                   
    ##         whole milk}               => {frankfurter}              0.005998983  0.1059246 1.7961524    59
    ## [914]  {frankfurter,                                                                                  
    ##         other vegetables}         => {whole milk}               0.007625826  0.4629630 1.8118745    75
    ## [915]  {frankfurter,                                                                                  
    ##         whole milk}               => {other vegetables}         0.007625826  0.3712871 1.9188696    75
    ## [916]  {other vegetables,                                                                             
    ##         whole milk}               => {frankfurter}              0.007625826  0.1019022 1.7279446    75
    ## [917]  {bottled beer,                                                                                 
    ##         bottled water}            => {soda}                     0.005083884  0.3225806 1.8499013    50
    ## [918]  {bottled beer,                                                                                 
    ##         soda}                     => {bottled water}            0.005083884  0.2994012 2.7089336    50
    ## [919]  {bottled water,                                                                                
    ##         soda}                     => {bottled beer}             0.005083884  0.1754386 2.1785841    50
    ## [920]  {bottled beer,                                                                                 
    ##         bottled water}            => {whole milk}               0.006100661  0.3870968 1.5149609    60
    ## [921]  {bottled beer,                                                                                 
    ##         whole milk}               => {bottled water}            0.006100661  0.2985075 2.7008472    60
    ## [922]  {bottled water,                                                                                
    ##         whole milk}               => {bottled beer}             0.006100661  0.1775148 2.2043661    60
    ## [923]  {bottled beer,                                                                                 
    ##         yogurt}                   => {whole milk}               0.005185562  0.5604396 2.1933637    51
    ## [924]  {bottled beer,                                                                                 
    ##         whole milk}               => {yogurt}                   0.005185562  0.2537313 1.8188395    51
    ## [925]  {bottled beer,                                                                                 
    ##         rolls/buns}               => {whole milk}               0.005388917  0.3955224 1.5479358    53
    ## [926]  {bottled beer,                                                                                 
    ##         whole milk}               => {rolls/buns}               0.005388917  0.2636816 1.4335591    53
    ## [927]  {bottled beer,                                                                                 
    ##         other vegetables}         => {whole milk}               0.007625826  0.4716981 1.8460609    75
    ## [928]  {bottled beer,                                                                                 
    ##         whole milk}               => {other vegetables}         0.007625826  0.3731343 1.9284162    75
    ## [929]  {other vegetables,                                                                             
    ##         whole milk}               => {bottled beer}             0.007625826  0.1019022 1.2654140    75
    ## [930]  {brown bread,                                                                                  
    ##         tropical fruit}           => {whole milk}               0.005693950  0.5333333 2.0872795    56
    ## [931]  {brown bread,                                                                                  
    ##         whole milk}               => {tropical fruit}           0.005693950  0.2258065 2.1519442    56
    ## [932]  {tropical fruit,                                                                               
    ##         whole milk}               => {brown bread}              0.005693950  0.1346154 2.0751447    56
    ## [933]  {brown bread,                                                                                  
    ##         root vegetables}          => {whole milk}               0.005693950  0.5600000 2.1916435    56
    ## [934]  {brown bread,                                                                                  
    ##         whole milk}               => {root vegetables}          0.005693950  0.2258065 2.0716478    56
    ## [935]  {root vegetables,                                                                              
    ##         whole milk}               => {brown bread}              0.005693950  0.1164241 1.7947197    56
    ## [936]  {brown bread,                                                                                  
    ##         soda}                     => {whole milk}               0.005083884  0.4032258 1.5780843    50
    ## [937]  {brown bread,                                                                                  
    ##         whole milk}               => {soda}                     0.005083884  0.2016129 1.1561883    50
    ## [938]  {soda,                                                                                         
    ##         whole milk}               => {brown bread}              0.005083884  0.1269036 1.9562640    50
    ## [939]  {brown bread,                                                                                  
    ##         yogurt}                   => {other vegetables}         0.005185562  0.3566434 1.8431883    51
    ## [940]  {brown bread,                                                                                  
    ##         other vegetables}         => {yogurt}                   0.005185562  0.2771739 1.9868844    51
    ## [941]  {other vegetables,                                                                             
    ##         yogurt}                   => {brown bread}              0.005185562  0.1194379 1.8411789    51
    ## [942]  {brown bread,                                                                                  
    ##         yogurt}                   => {whole milk}               0.007117438  0.4895105 1.9157723    70
    ## [943]  {brown bread,                                                                                  
    ##         whole milk}               => {yogurt}                   0.007117438  0.2822581 2.0233295    70
    ## [944]  {whole milk,                                                                                   
    ##         yogurt}                   => {brown bread}              0.007117438  0.1270417 1.9583943    70
    ## [945]  {brown bread,                                                                                  
    ##         rolls/buns}               => {whole milk}               0.005287239  0.4193548 1.6412077    52
    ## [946]  {brown bread,                                                                                  
    ##         whole milk}               => {rolls/buns}               0.005287239  0.2096774 1.1399544    52
    ## [947]  {brown bread,                                                                                  
    ##         other vegetables}         => {whole milk}               0.009354347  0.5000000 1.9568245    92
    ## [948]  {brown bread,                                                                                  
    ##         whole milk}               => {other vegetables}         0.009354347  0.3709677 1.9172190    92
    ## [949]  {other vegetables,                                                                             
    ##         whole milk}               => {brown bread}              0.009354347  0.1250000 1.9269201    92
    ## [950]  {domestic eggs,                                                                                
    ##         margarine}                => {whole milk}               0.005185562  0.6219512 2.4340988    51
    ## [951]  {margarine,                                                                                    
    ##         whole milk}               => {domestic eggs}            0.005185562  0.2142857 3.3774038    51
    ## [952]  {domestic eggs,                                                                                
    ##         whole milk}               => {margarine}                0.005185562  0.1728814 2.9518891    51
    ## [953]  {margarine,                                                                                    
    ##         root vegetables}          => {other vegetables}         0.005897306  0.5321101 2.7500277    58
    ## [954]  {margarine,                                                                                    
    ##         other vegetables}         => {root vegetables}          0.005897306  0.2989691 2.7428739    58
    ## [955]  {other vegetables,                                                                             
    ##         root vegetables}          => {margarine}                0.005897306  0.1244635 2.1251714    58
    ## [956]  {margarine,                                                                                    
    ##         yogurt}                   => {other vegetables}         0.005693950  0.4000000 2.0672622    56
    ## [957]  {margarine,                                                                                    
    ##         other vegetables}         => {yogurt}                   0.005693950  0.2886598 2.0692194    56
    ## [958]  {other vegetables,                                                                             
    ##         yogurt}                   => {margarine}                0.005693950  0.1311475 2.2392987    56
    ## [959]  {margarine,                                                                                    
    ##         yogurt}                   => {whole milk}               0.007015760  0.4928571 1.9288699    69
    ## [960]  {margarine,                                                                                    
    ##         whole milk}               => {yogurt}                   0.007015760  0.2899160 2.0782241    69
    ## [961]  {whole milk,                                                                                   
    ##         yogurt}                   => {margarine}                0.007015760  0.1252269 2.1382052    69
    ## [962]  {margarine,                                                                                    
    ##         rolls/buns}               => {other vegetables}         0.005185562  0.3517241 1.8177651    51
    ## [963]  {margarine,                                                                                    
    ##         other vegetables}         => {rolls/buns}               0.005185562  0.2628866 1.4292370    51
    ## [964]  {other vegetables,                                                                             
    ##         rolls/buns}               => {margarine}                0.005185562  0.1217184 2.0782990    51
    ## [965]  {margarine,                                                                                    
    ##         rolls/buns}               => {whole milk}               0.007930859  0.5379310 2.1052733    78
    ## [966]  {margarine,                                                                                    
    ##         whole milk}               => {rolls/buns}               0.007930859  0.3277311 1.7817774    78
    ## [967]  {rolls/buns,                                                                                   
    ##         whole milk}               => {margarine}                0.007930859  0.1400359 2.3910645    78
    ## [968]  {margarine,                                                                                    
    ##         other vegetables}         => {whole milk}               0.009252669  0.4690722 1.8357838    91
    ## [969]  {margarine,                                                                                    
    ##         whole milk}               => {other vegetables}         0.009252669  0.3823529 1.9760595    91
    ## [970]  {other vegetables,                                                                             
    ##         whole milk}               => {margarine}                0.009252669  0.1236413 2.1111323    91
    ## [971]  {butter,                                                                                       
    ##         domestic eggs}            => {whole milk}               0.005998983  0.6210526 2.4305820    59
    ## [972]  {butter,                                                                                       
    ##         whole milk}               => {domestic eggs}            0.005998983  0.2177122 3.4314091    59
    ## [973]  {domestic eggs,                                                                                
    ##         whole milk}               => {butter}                   0.005998983  0.2000000 3.6091743    59
    ## [974]  {butter,                                                                                       
    ##         whipped/sour cream}       => {other vegetables}         0.005795628  0.5700000 2.9458487    57
    ## [975]  {butter,                                                                                       
    ##         other vegetables}         => {whipped/sour cream}       0.005795628  0.2893401 4.0363970    57
    ## [976]  {other vegetables,                                                                             
    ##         whipped/sour cream}       => {butter}                   0.005795628  0.2007042 3.6218827    57
    ## [977]  {butter,                                                                                       
    ##         whipped/sour cream}       => {whole milk}               0.006710727  0.6600000 2.5830084    66
    ## [978]  {butter,                                                                                       
    ##         whole milk}               => {whipped/sour cream}       0.006710727  0.2435424 3.3975033    66
    ## [979]  {whipped/sour cream,                                                                           
    ##         whole milk}               => {butter}                   0.006710727  0.2082019 3.7571846    66
    ## [980]  {butter,                                                                                       
    ##         citrus fruit}             => {whole milk}               0.005083884  0.5555556 2.1742495    50
    ## [981]  {butter,                                                                                       
    ##         whole milk}               => {citrus fruit}             0.005083884  0.1845018 2.2292084    50
    ## [982]  {citrus fruit,                                                                                 
    ##         whole milk}               => {butter}                   0.005083884  0.1666667 3.0076453    50
    ## [983]  {bottled water,                                                                                
    ##         butter}                   => {whole milk}               0.005388917  0.6022727 2.3570841    53
    ## [984]  {butter,                                                                                       
    ##         whole milk}               => {bottled water}            0.005388917  0.1955720 1.7695034    53
    ## [985]  {bottled water,                                                                                
    ##         whole milk}               => {butter}                   0.005388917  0.1568047 2.8296781    53
    ## [986]  {butter,                                                                                       
    ##         tropical fruit}           => {other vegetables}         0.005490595  0.5510204 2.8477592    54
    ## [987]  {butter,                                                                                       
    ##         other vegetables}         => {tropical fruit}           0.005490595  0.2741117 2.6122949    54
    ## [988]  {other vegetables,                                                                             
    ##         tropical fruit}           => {butter}                   0.005490595  0.1529745 2.7605583    54
    ## [989]  {butter,                                                                                       
    ##         tropical fruit}           => {whole milk}               0.006202339  0.6224490 2.4360468    61
    ## [990]  {butter,                                                                                       
    ##         whole milk}               => {tropical fruit}           0.006202339  0.2250923 2.1451379    61
    ## [991]  {tropical fruit,                                                                               
    ##         whole milk}               => {butter}                   0.006202339  0.1466346 2.6461494    61
    ## [992]  {butter,                                                                                       
    ##         root vegetables}          => {other vegetables}         0.006609049  0.5118110 2.6451190    65
    ## [993]  {butter,                                                                                       
    ##         other vegetables}         => {root vegetables}          0.006609049  0.3299492 3.0270996    65
    ## [994]  {other vegetables,                                                                             
    ##         root vegetables}          => {butter}                   0.006609049  0.1394850 2.5171280    65
    ## [995]  {butter,                                                                                       
    ##         root vegetables}          => {whole milk}               0.008235892  0.6377953 2.4961069    81
    ## [996]  {butter,                                                                                       
    ##         whole milk}               => {root vegetables}          0.008235892  0.2988930 2.7421759    81
    ## [997]  {root vegetables,                                                                              
    ##         whole milk}               => {butter}                   0.008235892  0.1683992 3.0389098    81
    ## [998]  {butter,                                                                                       
    ##         yogurt}                   => {other vegetables}         0.006405694  0.4375000 2.2610681    63
    ## [999]  {butter,                                                                                       
    ##         other vegetables}         => {yogurt}                   0.006405694  0.3197970 2.2924220    63
    ## [1000] {other vegetables,                                                                             
    ##         yogurt}                   => {butter}                   0.006405694  0.1475410 2.6625056    63
    ## [1001] {butter,                                                                                       
    ##         yogurt}                   => {whole milk}               0.009354347  0.6388889 2.5003869    92
    ## [1002] {butter,                                                                                       
    ##         whole milk}               => {yogurt}                   0.009354347  0.3394834 2.4335417    92
    ## [1003] {whole milk,                                                                                   
    ##         yogurt}                   => {butter}                   0.009354347  0.1669691 3.0131038    92
    ## [1004] {butter,                                                                                       
    ##         rolls/buns}               => {other vegetables}         0.005693950  0.4242424 2.1925508    56
    ## [1005] {butter,                                                                                       
    ##         other vegetables}         => {rolls/buns}               0.005693950  0.2842640 1.5454594    56
    ## [1006] {other vegetables,                                                                             
    ##         rolls/buns}               => {butter}                   0.005693950  0.1336516 2.4118587    56
    ## [1007] {butter,                                                                                       
    ##         rolls/buns}               => {whole milk}               0.006609049  0.4924242 1.9271757    65
    ## [1008] {butter,                                                                                       
    ##         whole milk}               => {rolls/buns}               0.006609049  0.2398524 1.3040068    65
    ## [1009] {rolls/buns,                                                                                   
    ##         whole milk}               => {butter}                   0.006609049  0.1166966 2.1058917    65
    ## [1010] {butter,                                                                                       
    ##         other vegetables}         => {whole milk}               0.011489578  0.5736041 2.2448850   113
    ## [1011] {butter,                                                                                       
    ##         whole milk}               => {other vegetables}         0.011489578  0.4169742 2.1549874   113
    ## [1012] {other vegetables,                                                                             
    ##         whole milk}               => {butter}                   0.011489578  0.1535326 2.7706297   113
    ## [1013] {newspapers,                                                                                   
    ##         tropical fruit}           => {whole milk}               0.005083884  0.4310345 1.6869177    50
    ## [1014] {newspapers,                                                                                   
    ##         whole milk}               => {tropical fruit}           0.005083884  0.1858736 1.7713827    50
    ## [1015] {tropical fruit,                                                                               
    ##         whole milk}               => {newspapers}               0.005083884  0.1201923 1.5058488    50
    ## [1016] {newspapers,                                                                                   
    ##         root vegetables}          => {other vegetables}         0.005998983  0.5221239 2.6984175    59
    ## [1017] {newspapers,                                                                                   
    ##         other vegetables}         => {root vegetables}          0.005998983  0.3105263 2.8489051    59
    ## [1018] {other vegetables,                                                                             
    ##         root vegetables}          => {newspapers}               0.005998983  0.1266094 1.5862470    59
    ## [1019] {newspapers,                                                                                   
    ##         root vegetables}          => {whole milk}               0.005795628  0.5044248 1.9741415    57
    ## [1020] {newspapers,                                                                                   
    ##         whole milk}               => {root vegetables}          0.005795628  0.2118959 1.9440264    57
    ## [1021] {root vegetables,                                                                              
    ##         whole milk}               => {newspapers}               0.005795628  0.1185031 1.4846856    57
    ## [1022] {newspapers,                                                                                   
    ##         yogurt}                   => {rolls/buns}               0.005083884  0.3311258 1.8002336    50
    ## [1023] {newspapers,                                                                                   
    ##         rolls/buns}               => {yogurt}                   0.005083884  0.2577320 1.8475174    50
    ## [1024] {rolls/buns,                                                                                   
    ##         yogurt}                   => {newspapers}               0.005083884  0.1479290 1.8533524    50
    ## [1025] {newspapers,                                                                                   
    ##         yogurt}                   => {other vegetables}         0.005592272  0.3642384 1.8824408    55
    ## [1026] {newspapers,                                                                                   
    ##         other vegetables}         => {yogurt}                   0.005592272  0.2894737 2.0750537    55
    ## [1027] {other vegetables,                                                                             
    ##         yogurt}                   => {newspapers}               0.005592272  0.1288056 1.6137621    55
    ## [1028] {newspapers,                                                                                   
    ##         yogurt}                   => {whole milk}               0.006609049  0.4304636 1.6846834    65
    ## [1029] {newspapers,                                                                                   
    ##         whole milk}               => {yogurt}                   0.006609049  0.2416357 1.7321334    65
    ## [1030] {whole milk,                                                                                   
    ##         yogurt}                   => {newspapers}               0.006609049  0.1179673 1.4779729    65
    ## [1031] {newspapers,                                                                                   
    ##         rolls/buns}               => {other vegetables}         0.005490595  0.2783505 1.4385588    54
    ## [1032] {newspapers,                                                                                   
    ##         other vegetables}         => {rolls/buns}               0.005490595  0.2842105 1.5451689    54
    ## [1033] {other vegetables,                                                                             
    ##         rolls/buns}               => {newspapers}               0.005490595  0.1288783 1.6146725    54
    ## [1034] {newspapers,                                                                                   
    ##         rolls/buns}               => {whole milk}               0.007625826  0.3865979 1.5130086    75
    ## [1035] {newspapers,                                                                                   
    ##         whole milk}               => {rolls/buns}               0.007625826  0.2788104 1.5158100    75
    ## [1036] {rolls/buns,                                                                                   
    ##         whole milk}               => {newspapers}               0.007625826  0.1346499 1.6869833    75
    ## [1037] {newspapers,                                                                                   
    ##         other vegetables}         => {whole milk}               0.008337570  0.4315789 1.6890485    82
    ## [1038] {newspapers,                                                                                   
    ##         whole milk}               => {other vegetables}         0.008337570  0.3048327 1.5754229    82
    ## [1039] {other vegetables,                                                                             
    ##         whole milk}               => {newspapers}               0.008337570  0.1114130 1.3958564    82
    ## [1040] {domestic eggs,                                                                                
    ##         whipped/sour cream}       => {other vegetables}         0.005083884  0.5102041 2.6368141    50
    ## [1041] {domestic eggs,                                                                                
    ##         other vegetables}         => {whipped/sour cream}       0.005083884  0.2283105 3.1850125    50
    ## [1042] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {domestic eggs}            0.005083884  0.1760563 2.7748623    50
    ## [1043] {domestic eggs,                                                                                
    ##         whipped/sour cream}       => {whole milk}               0.005693950  0.5714286 2.2363709    56
    ## [1044] {domestic eggs,                                                                                
    ##         whole milk}               => {whipped/sour cream}       0.005693950  0.1898305 2.6482029    56
    ## [1045] {whipped/sour cream,                                                                           
    ##         whole milk}               => {domestic eggs}            0.005693950  0.1766562 2.7843161    56
    ## [1046] {domestic eggs,                                                                                
    ##         pip fruit}                => {whole milk}               0.005388917  0.6235294 2.4402753    53
    ## [1047] {domestic eggs,                                                                                
    ##         whole milk}               => {pip fruit}                0.005388917  0.1796610 2.3749544    53
    ## [1048] {pip fruit,                                                                                    
    ##         whole milk}               => {domestic eggs}            0.005388917  0.1790541 2.8221100    53
    ## [1049] {citrus fruit,                                                                                 
    ##         domestic eggs}            => {whole milk}               0.005693950  0.5490196 2.1486701    56
    ## [1050] {domestic eggs,                                                                                
    ##         whole milk}               => {citrus fruit}             0.005693950  0.1898305 2.2935910    56
    ## [1051] {citrus fruit,                                                                                 
    ##         whole milk}               => {domestic eggs}            0.005693950  0.1866667 2.9420940    56
    ## [1052] {domestic eggs,                                                                                
    ##         tropical fruit}           => {whole milk}               0.006914082  0.6071429 2.3761441    68
    ## [1053] {domestic eggs,                                                                                
    ##         whole milk}               => {tropical fruit}           0.006914082  0.2305085 2.1967547    68
    ## [1054] {tropical fruit,                                                                               
    ##         whole milk}               => {domestic eggs}            0.006914082  0.1634615 2.5763529    68
    ## [1055] {domestic eggs,                                                                                
    ##         root vegetables}          => {other vegetables}         0.007320793  0.5106383 2.6390582    72
    ## [1056] {domestic eggs,                                                                                
    ##         other vegetables}         => {root vegetables}          0.007320793  0.3287671 3.0162543    72
    ## [1057] {other vegetables,                                                                             
    ##         root vegetables}          => {domestic eggs}            0.007320793  0.1545064 2.4352096    72
    ## [1058] {domestic eggs,                                                                                
    ##         root vegetables}          => {whole milk}               0.008540925  0.5957447 2.3315356    84
    ## [1059] {domestic eggs,                                                                                
    ##         whole milk}               => {root vegetables}          0.008540925  0.2847458 2.6123830    84
    ## [1060] {root vegetables,                                                                              
    ##         whole milk}               => {domestic eggs}            0.008540925  0.1746362 2.7524788    84
    ## [1061] {domestic eggs,                                                                                
    ##         soda}                     => {other vegetables}         0.005083884  0.4098361 2.1180965    50
    ## [1062] {domestic eggs,                                                                                
    ##         other vegetables}         => {soda}                     0.005083884  0.2283105 1.3092908    50
    ## [1063] {other vegetables,                                                                             
    ##         soda}                     => {domestic eggs}            0.005083884  0.1552795 2.4473941    50
    ## [1064] {domestic eggs,                                                                                
    ##         soda}                     => {whole milk}               0.005185562  0.4180328 1.6360336    51
    ## [1065] {domestic eggs,                                                                                
    ##         whole milk}               => {soda}                     0.005185562  0.1728814 0.9914217    51
    ## [1066] {soda,                                                                                         
    ##         whole milk}               => {domestic eggs}            0.005185562  0.1294416 2.0401577    51
    ## [1067] {domestic eggs,                                                                                
    ##         yogurt}                   => {other vegetables}         0.005795628  0.4042553 2.0892544    57
    ## [1068] {domestic eggs,                                                                                
    ##         other vegetables}         => {yogurt}                   0.005795628  0.2602740 1.8657394    57
    ## [1069] {other vegetables,                                                                             
    ##         yogurt}                   => {domestic eggs}            0.005795628  0.1334895 2.1039565    57
    ## [1070] {domestic eggs,                                                                                
    ##         yogurt}                   => {whole milk}               0.007727504  0.5390071 2.1094846    76
    ## [1071] {domestic eggs,                                                                                
    ##         whole milk}               => {yogurt}                   0.007727504  0.2576271 1.8467658    76
    ## [1072] {whole milk,                                                                                   
    ##         yogurt}                   => {domestic eggs}            0.007727504  0.1379310 2.1739611    76
    ## [1073] {domestic eggs,                                                                                
    ##         rolls/buns}               => {other vegetables}         0.005897306  0.3766234 1.9464482    58
    ## [1074] {domestic eggs,                                                                                
    ##         other vegetables}         => {rolls/buns}               0.005897306  0.2648402 1.4398580    58
    ## [1075] {other vegetables,                                                                             
    ##         rolls/buns}               => {domestic eggs}            0.005897306  0.1384248 2.1817438    58
    ## [1076] {domestic eggs,                                                                                
    ##         rolls/buns}               => {whole milk}               0.006609049  0.4220779 1.6518648    65
    ## [1077] {domestic eggs,                                                                                
    ##         whole milk}               => {rolls/buns}               0.006609049  0.2203390 1.1979181    65
    ## [1078] {rolls/buns,                                                                                   
    ##         whole milk}               => {domestic eggs}            0.006609049  0.1166966 1.8392804    65
    ## [1079] {domestic eggs,                                                                                
    ##         other vegetables}         => {whole milk}               0.012302999  0.5525114 2.1623358   121
    ## [1080] {domestic eggs,                                                                                
    ##         whole milk}               => {other vegetables}         0.012302999  0.4101695 2.1198197   121
    ## [1081] {other vegetables,                                                                             
    ##         whole milk}               => {domestic eggs}            0.012302999  0.1644022 2.5911785   121
    ## [1082] {bottled water,                                                                                
    ##         fruit/vegetable juice}    => {soda}                     0.005185562  0.3642857 2.0890671    51
    ## [1083] {fruit/vegetable juice,                                                                        
    ##         soda}                     => {bottled water}            0.005185562  0.2817680 2.5493908    51
    ## [1084] {bottled water,                                                                                
    ##         soda}                     => {fruit/vegetable juice}    0.005185562  0.1789474 2.4753128    51
    ## [1085] {bottled water,                                                                                
    ##         fruit/vegetable juice}    => {whole milk}               0.005795628  0.4071429 1.5934142    57
    ## [1086] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {bottled water}            0.005795628  0.2175573 1.9684228    57
    ## [1087] {bottled water,                                                                                
    ##         whole milk}               => {fruit/vegetable juice}    0.005795628  0.1686391 2.3327216    57
    ## [1088] {fruit/vegetable juice,                                                                        
    ##         tropical fruit}           => {other vegetables}         0.006609049  0.4814815 2.4883712    65
    ## [1089] {fruit/vegetable juice,                                                                        
    ##         other vegetables}         => {tropical fruit}           0.006609049  0.3140097 2.9925242    65
    ## [1090] {other vegetables,                                                                             
    ##         tropical fruit}           => {fruit/vegetable juice}    0.006609049  0.1841360 2.5470849    65
    ## [1091] {fruit/vegetable juice,                                                                        
    ##         tropical fruit}           => {whole milk}               0.005998983  0.4370370 1.7104096    59
    ## [1092] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {tropical fruit}           0.005998983  0.2251908 2.1460774    59
    ## [1093] {tropical fruit,                                                                               
    ##         whole milk}               => {fruit/vegetable juice}    0.005998983  0.1418269 1.9618394    59
    ## [1094] {fruit/vegetable juice,                                                                        
    ##         root vegetables}          => {other vegetables}         0.006609049  0.5508475 2.8468653    65
    ## [1095] {fruit/vegetable juice,                                                                        
    ##         other vegetables}         => {root vegetables}          0.006609049  0.3140097 2.8808629    65
    ## [1096] {other vegetables,                                                                             
    ##         root vegetables}          => {fruit/vegetable juice}    0.006609049  0.1394850 1.9294441    65
    ## [1097] {fruit/vegetable juice,                                                                        
    ##         root vegetables}          => {whole milk}               0.006507372  0.5423729 2.1226571    64
    ## [1098] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {root vegetables}          0.006507372  0.2442748 2.2410847    64
    ## [1099] {root vegetables,                                                                              
    ##         whole milk}               => {fruit/vegetable juice}    0.006507372  0.1330561 1.8405163    64
    ## [1100] {fruit/vegetable juice,                                                                        
    ##         soda}                     => {yogurt}                   0.005083884  0.2762431 1.9802120    50
    ## [1101] {fruit/vegetable juice,                                                                        
    ##         yogurt}                   => {soda}                     0.005083884  0.2717391 1.5583407    50
    ## [1102] {soda,                                                                                         
    ##         yogurt}                   => {fruit/vegetable juice}    0.005083884  0.1858736 2.5711208    50
    ## [1103] {fruit/vegetable juice,                                                                        
    ##         soda}                     => {whole milk}               0.006100661  0.3314917 1.2973422    60
    ## [1104] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {soda}                     0.006100661  0.2290076 1.3132887    60
    ## [1105] {soda,                                                                                         
    ##         whole milk}               => {fruit/vegetable juice}    0.006100661  0.1522843 2.1064919    60
    ## [1106] {fruit/vegetable juice,                                                                        
    ##         yogurt}                   => {other vegetables}         0.008235892  0.4402174 2.2751120    81
    ## [1107] {fruit/vegetable juice,                                                                        
    ##         other vegetables}         => {yogurt}                   0.008235892  0.3913043 2.8050133    81
    ## [1108] {other vegetables,                                                                             
    ##         yogurt}                   => {fruit/vegetable juice}    0.008235892  0.1896956 2.6239884    81
    ## [1109] {fruit/vegetable juice,                                                                        
    ##         yogurt}                   => {whole milk}               0.009456024  0.5054348 1.9780943    93
    ## [1110] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {yogurt}                   0.009456024  0.3549618 2.5444968    93
    ## [1111] {whole milk,                                                                                   
    ##         yogurt}                   => {fruit/vegetable juice}    0.009456024  0.1687840 2.3347270    93
    ## [1112] {fruit/vegetable juice,                                                                        
    ##         rolls/buns}               => {whole milk}               0.005592272  0.3846154 1.5052496    55
    ## [1113] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {rolls/buns}               0.005592272  0.2099237 1.1412931    55
    ## [1114] {fruit/vegetable juice,                                                                        
    ##         other vegetables}         => {whole milk}               0.010472801  0.4975845 1.9473713   103
    ## [1115] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {other vegetables}         0.010472801  0.3931298 2.0317558   103
    ## [1116] {other vegetables,                                                                             
    ##         whole milk}               => {fruit/vegetable juice}    0.010472801  0.1399457 1.9358164   103
    ## [1117] {pip fruit,                                                                                    
    ##         whipped/sour cream}       => {other vegetables}         0.005592272  0.6043956 3.1236105    55
    ## [1118] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {pip fruit}                0.005592272  0.1936620 2.5600343    55
    ## [1119] {other vegetables,                                                                             
    ##         pip fruit}                => {whipped/sour cream}       0.005592272  0.2140078 2.9854844    55
    ## [1120] {pip fruit,                                                                                    
    ##         whipped/sour cream}       => {whole milk}               0.005998983  0.6483516 2.5374208    59
    ## [1121] {whipped/sour cream,                                                                           
    ##         whole milk}               => {pip fruit}                0.005998983  0.1861199 2.4603346    59
    ## [1122] {pip fruit,                                                                                    
    ##         whole milk}               => {whipped/sour cream}       0.005998983  0.1993243 2.7806450    59
    ## [1123] {citrus fruit,                                                                                 
    ##         whipped/sour cream}       => {other vegetables}         0.005693950  0.5233645 2.7048291    56
    ## [1124] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {citrus fruit}             0.005693950  0.1971831 2.3824272    56
    ## [1125] {citrus fruit,                                                                                 
    ##         other vegetables}         => {whipped/sour cream}       0.005693950  0.1971831 2.7507741    56
    ## [1126] {citrus fruit,                                                                                 
    ##         whipped/sour cream}       => {whole milk}               0.006304016  0.5794393 2.2677219    62
    ## [1127] {whipped/sour cream,                                                                           
    ##         whole milk}               => {citrus fruit}             0.006304016  0.1955836 2.3631016    62
    ## [1128] {citrus fruit,                                                                                 
    ##         whole milk}               => {whipped/sour cream}       0.006304016  0.2066667 2.8830733    62
    ## [1129] {sausage,                                                                                      
    ##         whipped/sour cream}       => {whole milk}               0.005083884  0.5617978 2.1986792    50
    ## [1130] {whipped/sour cream,                                                                           
    ##         whole milk}               => {sausage}                  0.005083884  0.1577287 1.6788548    50
    ## [1131] {sausage,                                                                                      
    ##         whole milk}               => {whipped/sour cream}       0.005083884  0.1700680 2.3725093    50
    ## [1132] {tropical fruit,                                                                               
    ##         whipped/sour cream}       => {yogurt}                   0.006202339  0.4485294 3.2152236    61
    ## [1133] {whipped/sour cream,                                                                           
    ##         yogurt}                   => {tropical fruit}           0.006202339  0.2990196 2.8496685    61
    ## [1134] {tropical fruit,                                                                               
    ##         yogurt}                   => {whipped/sour cream}       0.006202339  0.2118056 2.9547626    61
    ## [1135] {tropical fruit,                                                                               
    ##         whipped/sour cream}       => {other vegetables}         0.007829181  0.5661765 2.9260881    77
    ## [1136] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {tropical fruit}           0.007829181  0.2711268 2.5838485    77
    ## [1137] {other vegetables,                                                                             
    ##         tropical fruit}           => {whipped/sour cream}       0.007829181  0.2181303 3.0429952    77
    ## [1138] {tropical fruit,                                                                               
    ##         whipped/sour cream}       => {whole milk}               0.007930859  0.5735294 2.2445928    78
    ## [1139] {whipped/sour cream,                                                                           
    ##         whole milk}               => {tropical fruit}           0.007930859  0.2460568 2.3449307    78
    ## [1140] {tropical fruit,                                                                               
    ##         whole milk}               => {whipped/sour cream}       0.007930859  0.1875000 2.6156915    78
    ## [1141] {root vegetables,                                                                              
    ##         whipped/sour cream}       => {yogurt}                   0.006405694  0.3750000 2.6881378    63
    ## [1142] {whipped/sour cream,                                                                           
    ##         yogurt}                   => {root vegetables}          0.006405694  0.3088235 2.8332830    63
    ## [1143] {root vegetables,                                                                              
    ##         yogurt}                   => {whipped/sour cream}       0.006405694  0.2480315 3.4601273    63
    ## [1144] {root vegetables,                                                                              
    ##         whipped/sour cream}       => {other vegetables}         0.008540925  0.5000000 2.5840778    84
    ## [1145] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {root vegetables}          0.008540925  0.2957746 2.7135668    84
    ## [1146] {other vegetables,                                                                             
    ##         root vegetables}          => {whipped/sour cream}       0.008540925  0.1802575 2.5146562    84
    ## [1147] {root vegetables,                                                                              
    ##         whipped/sour cream}       => {whole milk}               0.009456024  0.5535714 2.1664843    93
    ## [1148] {whipped/sour cream,                                                                           
    ##         whole milk}               => {root vegetables}          0.009456024  0.2933754 2.6915550    93
    ## [1149] {root vegetables,                                                                              
    ##         whole milk}               => {whipped/sour cream}       0.009456024  0.1933472 2.6972619    93
    ## [1150] {soda,                                                                                         
    ##         whipped/sour cream}       => {whole milk}               0.005490595  0.4736842 1.8538337    54
    ## [1151] {whipped/sour cream,                                                                           
    ##         whole milk}               => {soda}                     0.005490595  0.1703470 0.9768879    54
    ## [1152] {soda,                                                                                         
    ##         whole milk}               => {whipped/sour cream}       0.005490595  0.1370558 1.9119775    54
    ## [1153] {whipped/sour cream,                                                                           
    ##         yogurt}                   => {other vegetables}         0.010167768  0.4901961 2.5334096   100
    ## [1154] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {yogurt}                   0.010167768  0.3521127 2.5240730   100
    ## [1155] {other vegetables,                                                                             
    ##         yogurt}                   => {whipped/sour cream}       0.010167768  0.2341920 3.2670620   100
    ## [1156] {whipped/sour cream,                                                                           
    ##         yogurt}                   => {whole milk}               0.010879512  0.5245098 2.0527473   107
    ## [1157] {whipped/sour cream,                                                                           
    ##         whole milk}               => {yogurt}                   0.010879512  0.3375394 2.4196066   107
    ## [1158] {whole milk,                                                                                   
    ##         yogurt}                   => {whipped/sour cream}       0.010879512  0.1941924 2.7090525   107
    ## [1159] {rolls/buns,                                                                                   
    ##         whipped/sour cream}       => {other vegetables}         0.006710727  0.4583333 2.3687380    66
    ## [1160] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {rolls/buns}               0.006710727  0.2323944 1.2634597    66
    ## [1161] {other vegetables,                                                                             
    ##         rolls/buns}               => {whipped/sour cream}       0.006710727  0.1575179 2.1974306    66
    ## [1162] {rolls/buns,                                                                                   
    ##         whipped/sour cream}       => {whole milk}               0.007829181  0.5347222 2.0927151    77
    ## [1163] {whipped/sour cream,                                                                           
    ##         whole milk}               => {rolls/buns}               0.007829181  0.2429022 1.3205877    77
    ## [1164] {rolls/buns,                                                                                   
    ##         whole milk}               => {whipped/sour cream}       0.007829181  0.1382406 1.9285050    77
    ## [1165] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {whole milk}               0.014641586  0.5070423 1.9843854   144
    ## [1166] {whipped/sour cream,                                                                           
    ##         whole milk}               => {other vegetables}         0.014641586  0.4542587 2.3476795   144
    ## [1167] {other vegetables,                                                                             
    ##         whole milk}               => {whipped/sour cream}       0.014641586  0.1956522 2.7294172   144
    ## [1168] {pastry,                                                                                       
    ##         pip fruit}                => {whole milk}               0.005083884  0.4761905 1.8636424    50
    ## [1169] {pip fruit,                                                                                    
    ##         whole milk}               => {pastry}                   0.005083884  0.1689189 1.8986486    50
    ## [1170] {pastry,                                                                                       
    ##         whole milk}               => {pip fruit}                0.005083884  0.1529052 2.0212670    50
    ## [1171] {citrus fruit,                                                                                 
    ##         pip fruit}                => {tropical fruit}           0.005592272  0.4044118 3.8540598    55
    ## [1172] {pip fruit,                                                                                    
    ##         tropical fruit}           => {citrus fruit}             0.005592272  0.2736318 3.3061046    55
    ## [1173] {citrus fruit,                                                                                 
    ##         tropical fruit}           => {pip fruit}                0.005592272  0.2806122 3.7094374    55
    ## [1174] {citrus fruit,                                                                                 
    ##         pip fruit}                => {other vegetables}         0.005897306  0.4264706 2.2040663    58
    ## [1175] {other vegetables,                                                                             
    ##         pip fruit}                => {citrus fruit}             0.005897306  0.2256809 2.7267469    58
    ## [1176] {citrus fruit,                                                                                 
    ##         other vegetables}         => {pip fruit}                0.005897306  0.2042254 2.6996725    58
    ## [1177] {citrus fruit,                                                                                 
    ##         pip fruit}                => {whole milk}               0.005185562  0.3750000 1.4676184    51
    ## [1178] {pip fruit,                                                                                    
    ##         whole milk}               => {citrus fruit}             0.005185562  0.1722973 2.0817493    51
    ## [1179] {citrus fruit,                                                                                 
    ##         whole milk}               => {pip fruit}                0.005185562  0.1700000 2.2472446    51
    ## [1180] {pip fruit,                                                                                    
    ##         sausage}                  => {whole milk}               0.005592272  0.5188679 2.0306669    55
    ## [1181] {pip fruit,                                                                                    
    ##         whole milk}               => {sausage}                  0.005592272  0.1858108 1.9777590    55
    ## [1182] {sausage,                                                                                      
    ##         whole milk}               => {pip fruit}                0.005592272  0.1870748 2.4729583    55
    ## [1183] {pip fruit,                                                                                    
    ##         tropical fruit}           => {root vegetables}          0.005287239  0.2587065 2.3734870    52
    ## [1184] {pip fruit,                                                                                    
    ##         root vegetables}          => {tropical fruit}           0.005287239  0.3398693 3.2389674    52
    ## [1185] {root vegetables,                                                                              
    ##         tropical fruit}           => {pip fruit}                0.005287239  0.2512077 3.3207366    52
    ## [1186] {pip fruit,                                                                                    
    ##         tropical fruit}           => {yogurt}                   0.006405694  0.3134328 2.2468017    63
    ## [1187] {pip fruit,                                                                                    
    ##         yogurt}                   => {tropical fruit}           0.006405694  0.3559322 3.3920477    63
    ## [1188] {tropical fruit,                                                                               
    ##         yogurt}                   => {pip fruit}                0.006405694  0.2187500 2.8916751    63
    ## [1189] {pip fruit,                                                                                    
    ##         tropical fruit}           => {other vegetables}         0.009456024  0.4626866 2.3912361    93
    ## [1190] {other vegetables,                                                                             
    ##         pip fruit}                => {tropical fruit}           0.009456024  0.3618677 3.4486132    93
    ## [1191] {other vegetables,                                                                             
    ##         tropical fruit}           => {pip fruit}                0.009456024  0.2634561 3.4826487    93
    ## [1192] {pip fruit,                                                                                    
    ##         tropical fruit}           => {whole milk}               0.008439248  0.4129353 1.6160839    83
    ## [1193] {pip fruit,                                                                                    
    ##         whole milk}               => {tropical fruit}           0.008439248  0.2804054 2.6722744    83
    ## [1194] {tropical fruit,                                                                               
    ##         whole milk}               => {pip fruit}                0.008439248  0.1995192 2.6374619    83
    ## [1195] {pip fruit,                                                                                    
    ##         root vegetables}          => {yogurt}                   0.005287239  0.3398693 2.4363079    52
    ## [1196] {pip fruit,                                                                                    
    ##         yogurt}                   => {root vegetables}          0.005287239  0.2937853 2.6953158    52
    ## [1197] {root vegetables,                                                                              
    ##         yogurt}                   => {pip fruit}                0.005287239  0.2047244 2.7062696    52
    ## [1198] {pip fruit,                                                                                    
    ##         root vegetables}          => {other vegetables}         0.008134215  0.5228758 2.7023036    80
    ## [1199] {other vegetables,                                                                             
    ##         pip fruit}                => {root vegetables}          0.008134215  0.3112840 2.8558569    80
    ## [1200] {other vegetables,                                                                             
    ##         root vegetables}          => {pip fruit}                0.008134215  0.1716738 2.2693710    80
    ## [1201] {pip fruit,                                                                                    
    ##         root vegetables}          => {whole milk}               0.008947636  0.5751634 2.2509877    88
    ## [1202] {pip fruit,                                                                                    
    ##         whole milk}               => {root vegetables}          0.008947636  0.2972973 2.7275363    88
    ## [1203] {root vegetables,                                                                              
    ##         whole milk}               => {pip fruit}                0.008947636  0.1829522 2.4184606    88
    ## [1204] {pip fruit,                                                                                    
    ##         yogurt}                   => {other vegetables}         0.008134215  0.4519774 2.3358895    80
    ## [1205] {other vegetables,                                                                             
    ##         pip fruit}                => {yogurt}                   0.008134215  0.3112840 2.2313984    80
    ## [1206] {other vegetables,                                                                             
    ##         yogurt}                   => {pip fruit}                0.008134215  0.1873536 2.4766438    80
    ## [1207] {pip fruit,                                                                                    
    ##         yogurt}                   => {whole milk}               0.009557702  0.5310734 2.0784351    94
    ## [1208] {pip fruit,                                                                                    
    ##         whole milk}               => {yogurt}                   0.009557702  0.3175676 2.2764410    94
    ## [1209] {whole milk,                                                                                   
    ##         yogurt}                   => {pip fruit}                0.009557702  0.1705989 2.2551617    94
    ## [1210] {pip fruit,                                                                                    
    ##         rolls/buns}               => {other vegetables}         0.005083884  0.3649635 1.8861882    50
    ## [1211] {other vegetables,                                                                             
    ##         pip fruit}                => {rolls/buns}               0.005083884  0.1945525 1.0577248    50
    ## [1212] {other vegetables,                                                                             
    ##         rolls/buns}               => {pip fruit}                0.005083884  0.1193317 1.5774566    50
    ## [1213] {pip fruit,                                                                                    
    ##         rolls/buns}               => {whole milk}               0.006202339  0.4452555 1.7425737    61
    ## [1214] {pip fruit,                                                                                    
    ##         whole milk}               => {rolls/buns}               0.006202339  0.2060811 1.1204021    61
    ## [1215] {rolls/buns,                                                                                   
    ##         whole milk}               => {pip fruit}                0.006202339  0.1095153 1.4476916    61
    ## [1216] {other vegetables,                                                                             
    ##         pip fruit}                => {whole milk}               0.013523132  0.5175097 2.0253514   133
    ## [1217] {pip fruit,                                                                                    
    ##         whole milk}               => {other vegetables}         0.013523132  0.4493243 2.3221780   133
    ## [1218] {other vegetables,                                                                             
    ##         whole milk}               => {pip fruit}                0.013523132  0.1807065 2.3887751   133
    ## [1219] {pastry,                                                                                       
    ##         sausage}                  => {whole milk}               0.005693950  0.4552846 1.7818239    56
    ## [1220] {pastry,                                                                                       
    ##         whole milk}               => {sausage}                  0.005693950  0.1712538 1.8228153    56
    ## [1221] {sausage,                                                                                      
    ##         whole milk}               => {pastry}                   0.005693950  0.1904762 2.1409524    56
    ## [1222] {pastry,                                                                                       
    ##         tropical fruit}           => {other vegetables}         0.005083884  0.3846154 1.9877521    50
    ## [1223] {other vegetables,                                                                             
    ##         pastry}                   => {tropical fruit}           0.005083884  0.2252252 2.1464051    50
    ## [1224] {other vegetables,                                                                             
    ##         tropical fruit}           => {pastry}                   0.005083884  0.1416431 1.5920680    50
    ## [1225] {pastry,                                                                                       
    ##         tropical fruit}           => {whole milk}               0.006710727  0.5076923 1.9869295    66
    ## [1226] {pastry,                                                                                       
    ##         whole milk}               => {tropical fruit}           0.006710727  0.2018349 1.9234941    66
    ## [1227] {tropical fruit,                                                                               
    ##         whole milk}               => {pastry}                   0.006710727  0.1586538 1.7832692    66
    ## [1228] {pastry,                                                                                       
    ##         root vegetables}          => {other vegetables}         0.005897306  0.5370370 2.7754909    58
    ## [1229] {other vegetables,                                                                             
    ##         pastry}                   => {root vegetables}          0.005897306  0.2612613 2.3969258    58
    ## [1230] {other vegetables,                                                                             
    ##         root vegetables}          => {pastry}                   0.005897306  0.1244635 1.3989700    58
    ## [1231] {pastry,                                                                                       
    ##         root vegetables}          => {whole milk}               0.005693950  0.5185185 2.0292995    56
    ## [1232] {pastry,                                                                                       
    ##         whole milk}               => {root vegetables}          0.005693950  0.1712538 1.5711580    56
    ## [1233] {root vegetables,                                                                              
    ##         whole milk}               => {pastry}                   0.005693950  0.1164241 1.3086071    56
    ## [1234] {pastry,                                                                                       
    ##         soda}                     => {rolls/buns}               0.005388917  0.2560386 1.3920067    53
    ## [1235] {pastry,                                                                                       
    ##         rolls/buns}               => {soda}                     0.005388917  0.2572816 1.4754309    53
    ## [1236] {rolls/buns,                                                                                   
    ##         soda}                     => {pastry}                   0.005388917  0.1405836 1.5801592    53
    ## [1237] {pastry,                                                                                       
    ##         soda}                     => {other vegetables}         0.005490595  0.2608696 1.3482145    54
    ## [1238] {other vegetables,                                                                             
    ##         pastry}                   => {soda}                     0.005490595  0.2432432 1.3949255    54
    ## [1239] {other vegetables,                                                                             
    ##         soda}                     => {pastry}                   0.005490595  0.1677019 1.8849689    54
    ## [1240] {pastry,                                                                                       
    ##         soda}                     => {whole milk}               0.008235892  0.3913043 1.5314279    81
    ## [1241] {pastry,                                                                                       
    ##         whole milk}               => {soda}                     0.008235892  0.2477064 1.4205205    81
    ## [1242] {soda,                                                                                         
    ##         whole milk}               => {pastry}                   0.008235892  0.2055838 2.3107614    81
    ## [1243] {pastry,                                                                                       
    ##         yogurt}                   => {rolls/buns}               0.005795628  0.3275862 1.7809897    57
    ## [1244] {pastry,                                                                                       
    ##         rolls/buns}               => {yogurt}                   0.005795628  0.2766990 1.9834803    57
    ## [1245] {rolls/buns,                                                                                   
    ##         yogurt}                   => {pastry}                   0.005795628  0.1686391 1.8955030    57
    ## [1246] {pastry,                                                                                       
    ##         yogurt}                   => {other vegetables}         0.006609049  0.3735632 1.9306328    65
    ## [1247] {other vegetables,                                                                             
    ##         pastry}                   => {yogurt}                   0.006609049  0.2927928 2.0988463    65
    ## [1248] {other vegetables,                                                                             
    ##         yogurt}                   => {pastry}                   0.006609049  0.1522248 1.7110070    65
    ## [1249] {pastry,                                                                                       
    ##         yogurt}                   => {whole milk}               0.009150991  0.5172414 2.0243012    90
    ## [1250] {pastry,                                                                                       
    ##         whole milk}               => {yogurt}                   0.009150991  0.2752294 1.9729451    90
    ## [1251] {whole milk,                                                                                   
    ##         yogurt}                   => {pastry}                   0.009150991  0.1633394 1.8359347    90
    ## [1252] {pastry,                                                                                       
    ##         rolls/buns}               => {other vegetables}         0.006100661  0.2912621 1.5052880    60
    ## [1253] {other vegetables,                                                                             
    ##         pastry}                   => {rolls/buns}               0.006100661  0.2702703 1.4693798    60
    ## [1254] {other vegetables,                                                                             
    ##         rolls/buns}               => {pastry}                   0.006100661  0.1431981 1.6095465    60
    ## [1255] {pastry,                                                                                       
    ##         rolls/buns}               => {whole milk}               0.008540925  0.4077670 1.5958569    84
    ## [1256] {pastry,                                                                                       
    ##         whole milk}               => {rolls/buns}               0.008540925  0.2568807 1.3965849    84
    ## [1257] {rolls/buns,                                                                                   
    ##         whole milk}               => {pastry}                   0.008540925  0.1508079 1.6950808    84
    ## [1258] {other vegetables,                                                                             
    ##         pastry}                   => {whole milk}               0.010574479  0.4684685 1.8334212   104
    ## [1259] {pastry,                                                                                       
    ##         whole milk}               => {other vegetables}         0.010574479  0.3180428 1.6436947   104
    ## [1260] {other vegetables,                                                                             
    ##         whole milk}               => {pastry}                   0.010574479  0.1413043 1.5882609   104
    ## [1261] {bottled water,                                                                                
    ##         citrus fruit}             => {other vegetables}         0.005083884  0.3759398 1.9429156    50
    ## [1262] {citrus fruit,                                                                                 
    ##         other vegetables}         => {bottled water}            0.005083884  0.1760563 1.5929292    50
    ## [1263] {bottled water,                                                                                
    ##         other vegetables}         => {citrus fruit}             0.005083884  0.2049180 2.4758831    50
    ## [1264] {bottled water,                                                                                
    ##         citrus fruit}             => {whole milk}               0.005897306  0.4360902 1.7067041    58
    ## [1265] {citrus fruit,                                                                                 
    ##         whole milk}               => {bottled water}            0.005897306  0.1933333 1.7492487    58
    ## [1266] {bottled water,                                                                                
    ##         whole milk}               => {citrus fruit}             0.005897306  0.1715976 2.0732957    58
    ## [1267] {citrus fruit,                                                                                 
    ##         tropical fruit}           => {root vegetables}          0.005693950  0.2857143 2.6212687    56
    ## [1268] {citrus fruit,                                                                                 
    ##         root vegetables}          => {tropical fruit}           0.005693950  0.3218391 3.0671389    56
    ## [1269] {root vegetables,                                                                              
    ##         tropical fruit}           => {citrus fruit}             0.005693950  0.2705314 3.2686441    56
    ## [1270] {citrus fruit,                                                                                 
    ##         tropical fruit}           => {yogurt}                   0.006304016  0.3163265 2.2675448    62
    ## [1271] {citrus fruit,                                                                                 
    ##         yogurt}                   => {tropical fruit}           0.006304016  0.2910798 2.7740019    62
    ## [1272] {tropical fruit,                                                                               
    ##         yogurt}                   => {citrus fruit}             0.006304016  0.2152778 2.6010528    62
    ## [1273] {citrus fruit,                                                                                 
    ##         tropical fruit}           => {other vegetables}         0.009049314  0.4540816 2.3467645    89
    ## [1274] {citrus fruit,                                                                                 
    ##         other vegetables}         => {tropical fruit}           0.009049314  0.3133803 2.9865262    89
    ## [1275] {other vegetables,                                                                             
    ##         tropical fruit}           => {citrus fruit}             0.009049314  0.2521246 3.0462480    89
    ## [1276] {citrus fruit,                                                                                 
    ##         tropical fruit}           => {whole milk}               0.009049314  0.4540816 1.7771161    89
    ## [1277] {citrus fruit,                                                                                 
    ##         whole milk}               => {tropical fruit}           0.009049314  0.2966667 2.8272448    89
    ## [1278] {tropical fruit,                                                                               
    ##         whole milk}               => {citrus fruit}             0.009049314  0.2139423 2.5849172    89
    ## [1279] {citrus fruit,                                                                                 
    ##         root vegetables}          => {other vegetables}         0.010371124  0.5862069 3.0296084   102
    ## [1280] {citrus fruit,                                                                                 
    ##         other vegetables}         => {root vegetables}          0.010371124  0.3591549 3.2950455   102
    ## [1281] {other vegetables,                                                                             
    ##         root vegetables}          => {citrus fruit}             0.010371124  0.2188841 2.6446257   102
    ## [1282] {citrus fruit,                                                                                 
    ##         root vegetables}          => {whole milk}               0.009150991  0.5172414 2.0243012    90
    ## [1283] {citrus fruit,                                                                                 
    ##         whole milk}               => {root vegetables}          0.009150991  0.3000000 2.7523321    90
    ## [1284] {root vegetables,                                                                              
    ##         whole milk}               => {citrus fruit}             0.009150991  0.1871102 2.2607232    90
    ## [1285] {citrus fruit,                                                                                 
    ##         yogurt}                   => {rolls/buns}               0.005795628  0.2676056 1.4548930    57
    ## [1286] {citrus fruit,                                                                                 
    ##         rolls/buns}               => {yogurt}                   0.005795628  0.3454545 2.4763451    57
    ## [1287] {rolls/buns,                                                                                   
    ##         yogurt}                   => {citrus fruit}             0.005795628  0.1686391 2.0375492    57
    ## [1288] {citrus fruit,                                                                                 
    ##         yogurt}                   => {other vegetables}         0.007625826  0.3521127 1.8197731    75
    ## [1289] {citrus fruit,                                                                                 
    ##         other vegetables}         => {yogurt}                   0.007625826  0.2640845 1.8930548    75
    ## [1290] {other vegetables,                                                                             
    ##         yogurt}                   => {citrus fruit}             0.007625826  0.1756440 2.1221855    75
    ## [1291] {citrus fruit,                                                                                 
    ##         yogurt}                   => {whole milk}               0.010269446  0.4741784 1.8557678   101
    ## [1292] {citrus fruit,                                                                                 
    ##         whole milk}               => {yogurt}                   0.010269446  0.3366667 2.4133503   101
    ## [1293] {whole milk,                                                                                   
    ##         yogurt}                   => {citrus fruit}             0.010269446  0.1833031 2.2147246   101
    ## [1294] {citrus fruit,                                                                                 
    ##         rolls/buns}               => {other vegetables}         0.005998983  0.3575758 1.8480071    59
    ## [1295] {citrus fruit,                                                                                 
    ##         other vegetables}         => {rolls/buns}               0.005998983  0.2077465 1.1294564    59
    ## [1296] {other vegetables,                                                                             
    ##         rolls/buns}               => {citrus fruit}             0.005998983  0.1408115 1.7013276    59
    ## [1297] {citrus fruit,                                                                                 
    ##         rolls/buns}               => {whole milk}               0.007219115  0.4303030 1.6840550    71
    ## [1298] {citrus fruit,                                                                                 
    ##         whole milk}               => {rolls/buns}               0.007219115  0.2366667 1.2866869    71
    ## [1299] {rolls/buns,                                                                                   
    ##         whole milk}               => {citrus fruit}             0.007219115  0.1274686 1.5401149    71
    ## [1300] {citrus fruit,                                                                                 
    ##         other vegetables}         => {whole milk}               0.013014743  0.4507042 1.7638982   128
    ## [1301] {citrus fruit,                                                                                 
    ##         whole milk}               => {other vegetables}         0.013014743  0.4266667 2.2050797   128
    ## [1302] {other vegetables,                                                                             
    ##         whole milk}               => {citrus fruit}             0.013014743  0.1739130 2.1012712   128
    ## [1303] {sausage,                                                                                      
    ##         shopping bags}            => {soda}                     0.005693950  0.3636364 2.0853432    56
    ## [1304] {shopping bags,                                                                                
    ##         soda}                     => {sausage}                  0.005693950  0.2314050 2.4630604    56
    ## [1305] {sausage,                                                                                      
    ##         soda}                     => {shopping bags}            0.005693950  0.2343096 2.3781580    56
    ## [1306] {sausage,                                                                                      
    ##         shopping bags}            => {rolls/buns}               0.005998983  0.3831169 2.0828936    59
    ## [1307] {rolls/buns,                                                                                   
    ##         shopping bags}            => {sausage}                  0.005998983  0.3072917 3.2707939    59
    ## [1308] {rolls/buns,                                                                                   
    ##         sausage}                  => {shopping bags}            0.005998983  0.1960133 1.9894641    59
    ## [1309] {sausage,                                                                                      
    ##         shopping bags}            => {other vegetables}         0.005388917  0.3441558 1.7786509    53
    ## [1310] {other vegetables,                                                                             
    ##         shopping bags}            => {sausage}                  0.005388917  0.2324561 2.4742491    53
    ## [1311] {other vegetables,                                                                             
    ##         sausage}                  => {shopping bags}            0.005388917  0.2000000 2.0299278    53
    ## [1312] {root vegetables,                                                                              
    ##         shopping bags}            => {other vegetables}         0.006609049  0.5158730 2.6661120    65
    ## [1313] {other vegetables,                                                                             
    ##         shopping bags}            => {root vegetables}          0.006609049  0.2850877 2.6155203    65
    ## [1314] {other vegetables,                                                                             
    ##         root vegetables}          => {shopping bags}            0.006609049  0.1394850 1.4157222    65
    ## [1315] {root vegetables,                                                                              
    ##         shopping bags}            => {whole milk}               0.005287239  0.4126984 1.6151567    52
    ## [1316] {shopping bags,                                                                                
    ##         whole milk}               => {root vegetables}          0.005287239  0.2157676 1.9795473    52
    ## [1317] {root vegetables,                                                                              
    ##         whole milk}               => {shopping bags}            0.005287239  0.1081081 1.0972582    52
    ## [1318] {shopping bags,                                                                                
    ##         soda}                     => {rolls/buns}               0.006304016  0.2561983 1.3928749    62
    ## [1319] {rolls/buns,                                                                                   
    ##         shopping bags}            => {soda}                     0.006304016  0.3229167 1.8518282    62
    ## [1320] {rolls/buns,                                                                                   
    ##         soda}                     => {shopping bags}            0.006304016  0.1644562 1.6691714    62
    ## [1321] {shopping bags,                                                                                
    ##         soda}                     => {other vegetables}         0.005388917  0.2190083 1.1318688    53
    ## [1322] {other vegetables,                                                                             
    ##         shopping bags}            => {soda}                     0.005388917  0.2324561 1.3330648    53
    ## [1323] {other vegetables,                                                                             
    ##         soda}                     => {shopping bags}            0.005388917  0.1645963 1.6705927    53
    ## [1324] {shopping bags,                                                                                
    ##         soda}                     => {whole milk}               0.006812405  0.2768595 1.0835309    67
    ## [1325] {shopping bags,                                                                                
    ##         whole milk}               => {soda}                     0.006812405  0.2780083 1.5942925    67
    ## [1326] {soda,                                                                                         
    ##         whole milk}               => {shopping bags}            0.006812405  0.1700508 1.7259538    67
    ## [1327] {shopping bags,                                                                                
    ##         yogurt}                   => {other vegetables}         0.005388917  0.3533333 1.8260816    53
    ## [1328] {other vegetables,                                                                             
    ##         shopping bags}            => {yogurt}                   0.005388917  0.2324561 1.6663310    53
    ## [1329] {other vegetables,                                                                             
    ##         yogurt}                   => {shopping bags}            0.005388917  0.1241218 1.2597912    53
    ## [1330] {shopping bags,                                                                                
    ##         yogurt}                   => {whole milk}               0.005287239  0.3466667 1.3567317    52
    ## [1331] {shopping bags,                                                                                
    ##         whole milk}               => {yogurt}                   0.005287239  0.2157676 1.5467017    52
    ## [1332] {rolls/buns,                                                                                   
    ##         shopping bags}            => {other vegetables}         0.005287239  0.2708333 1.3997088    52
    ## [1333] {other vegetables,                                                                             
    ##         shopping bags}            => {rolls/buns}               0.005287239  0.2280702 1.2399503    52
    ## [1334] {other vegetables,                                                                             
    ##         rolls/buns}               => {shopping bags}            0.005287239  0.1241050 1.2596210    52
    ## [1335] {rolls/buns,                                                                                   
    ##         shopping bags}            => {whole milk}               0.005287239  0.2708333 1.0599466    52
    ## [1336] {shopping bags,                                                                                
    ##         whole milk}               => {rolls/buns}               0.005287239  0.2157676 1.1730651    52
    ## [1337] {other vegetables,                                                                             
    ##         shopping bags}            => {whole milk}               0.007625826  0.3289474 1.2873845    75
    ## [1338] {shopping bags,                                                                                
    ##         whole milk}               => {other vegetables}         0.007625826  0.3112033 1.6083472    75
    ## [1339] {other vegetables,                                                                             
    ##         whole milk}               => {shopping bags}            0.007625826  0.1019022 1.0342703    75
    ## [1340] {bottled water,                                                                                
    ##         sausage}                  => {other vegetables}         0.005083884  0.4237288 2.1898964    50
    ## [1341] {other vegetables,                                                                             
    ##         sausage}                  => {bottled water}            0.005083884  0.1886792 1.7071393    50
    ## [1342] {bottled water,                                                                                
    ##         other vegetables}         => {sausage}                  0.005083884  0.2049180 2.1811351    50
    ## [1343] {sausage,                                                                                      
    ##         tropical fruit}           => {other vegetables}         0.005998983  0.4306569 2.2257020    59
    ## [1344] {other vegetables,                                                                             
    ##         sausage}                  => {tropical fruit}           0.005998983  0.2226415 2.1217822    59
    ## [1345] {other vegetables,                                                                             
    ##         tropical fruit}           => {sausage}                  0.005998983  0.1671388 1.7790154    59
    ## [1346] {sausage,                                                                                      
    ##         tropical fruit}           => {whole milk}               0.007219115  0.5182482 2.0282415    71
    ## [1347] {sausage,                                                                                      
    ##         whole milk}               => {tropical fruit}           0.007219115  0.2414966 2.3014719    71
    ## [1348] {tropical fruit,                                                                               
    ##         whole milk}               => {sausage}                  0.007219115  0.1706731 1.8166339    71
    ## [1349] {root vegetables,                                                                              
    ##         sausage}                  => {yogurt}                   0.005185562  0.3469388 2.4869846    51
    ## [1350] {sausage,                                                                                      
    ##         yogurt}                   => {root vegetables}          0.005185562  0.2642487 2.4243340    51
    ## [1351] {root vegetables,                                                                              
    ##         yogurt}                   => {sausage}                  0.005185562  0.2007874 2.1371689    51
    ## [1352] {root vegetables,                                                                              
    ##         sausage}                  => {other vegetables}         0.006812405  0.4557823 2.3555539    67
    ## [1353] {other vegetables,                                                                             
    ##         sausage}                  => {root vegetables}          0.006812405  0.2528302 2.3195755    67
    ## [1354] {other vegetables,                                                                             
    ##         root vegetables}          => {sausage}                  0.006812405  0.1437768 1.5303518    67
    ## [1355] {root vegetables,                                                                              
    ##         sausage}                  => {whole milk}               0.007727504  0.5170068 2.0233832    76
    ## [1356] {sausage,                                                                                      
    ##         whole milk}               => {root vegetables}          0.007727504  0.2585034 2.3716240    76
    ## [1357] {root vegetables,                                                                              
    ##         whole milk}               => {sausage}                  0.007727504  0.1580042 1.6817867    76
    ## [1358] {sausage,                                                                                      
    ##         soda}                     => {yogurt}                   0.005592272  0.2301255 1.6496243    55
    ## [1359] {sausage,                                                                                      
    ##         yogurt}                   => {soda}                     0.005592272  0.2849741 1.6342392    55
    ## [1360] {soda,                                                                                         
    ##         yogurt}                   => {sausage}                  0.005592272  0.2044610 2.1762701    55
    ## [1361] {sausage,                                                                                      
    ##         soda}                     => {rolls/buns}               0.009659380  0.3974895 2.1610335    95
    ## [1362] {rolls/buns,                                                                                   
    ##         sausage}                  => {soda}                     0.009659380  0.3156146 1.8099532    95
    ## [1363] {rolls/buns,                                                                                   
    ##         soda}                     => {sausage}                  0.009659380  0.2519894 2.6821598    95
    ## [1364] {sausage,                                                                                      
    ##         soda}                     => {other vegetables}         0.007219115  0.2970711 1.5353098    71
    ## [1365] {other vegetables,                                                                             
    ##         sausage}                  => {soda}                     0.007219115  0.2679245 1.5364652    71
    ## [1366] {other vegetables,                                                                             
    ##         soda}                     => {sausage}                  0.007219115  0.2204969 2.3469556    71
    ## [1367] {sausage,                                                                                      
    ##         soda}                     => {whole milk}               0.006710727  0.2761506 1.0807566    66
    ## [1368] {sausage,                                                                                      
    ##         whole milk}               => {soda}                     0.006710727  0.2244898 1.2873803    66
    ## [1369] {soda,                                                                                         
    ##         whole milk}               => {sausage}                  0.006710727  0.1675127 1.7829949    66
    ## [1370] {sausage,                                                                                      
    ##         yogurt}                   => {rolls/buns}               0.005998983  0.3056995 1.6619980    59
    ## [1371] {rolls/buns,                                                                                   
    ##         sausage}                  => {yogurt}                   0.005998983  0.1960133 1.4050953    59
    ## [1372] {rolls/buns,                                                                                   
    ##         yogurt}                   => {sausage}                  0.005998983  0.1745562 1.8579658    59
    ## [1373] {sausage,                                                                                      
    ##         yogurt}                   => {other vegetables}         0.008134215  0.4145078 2.1422406    80
    ## [1374] {other vegetables,                                                                             
    ##         sausage}                  => {yogurt}                   0.008134215  0.3018868 2.1640354    80
    ## [1375] {other vegetables,                                                                             
    ##         yogurt}                   => {sausage}                  0.008134215  0.1873536 1.9941807    80
    ## [1376] {sausage,                                                                                      
    ##         yogurt}                   => {whole milk}               0.008744281  0.4455959 1.7439058    86
    ## [1377] {sausage,                                                                                      
    ##         whole milk}               => {yogurt}                   0.008744281  0.2925170 2.0968694    86
    ## [1378] {whole milk,                                                                                   
    ##         yogurt}                   => {sausage}                  0.008744281  0.1560799 1.6613045    86
    ## [1379] {rolls/buns,                                                                                   
    ##         sausage}                  => {other vegetables}         0.008845958  0.2890365 1.4937858    87
    ## [1380] {other vegetables,                                                                             
    ##         sausage}                  => {rolls/buns}               0.008845958  0.3283019 1.7848806    87
    ## [1381] {other vegetables,                                                                             
    ##         rolls/buns}               => {sausage}                  0.008845958  0.2076372 2.2100781    87
    ## [1382] {rolls/buns,                                                                                   
    ##         sausage}                  => {whole milk}               0.009354347  0.3056478 1.1961984    92
    ## [1383] {sausage,                                                                                      
    ##         whole milk}               => {rolls/buns}               0.009354347  0.3129252 1.7012820    92
    ## [1384] {rolls/buns,                                                                                   
    ##         whole milk}               => {sausage}                  0.009354347  0.1651706 1.7580654    92
    ## [1385] {other vegetables,                                                                             
    ##         sausage}                  => {whole milk}               0.010167768  0.3773585 1.4768487   100
    ## [1386] {sausage,                                                                                      
    ##         whole milk}               => {other vegetables}         0.010167768  0.3401361 1.7578760   100
    ## [1387] {other vegetables,                                                                             
    ##         whole milk}               => {sausage}                  0.010167768  0.1358696 1.4461874   100
    ## [1388] {bottled water,                                                                                
    ##         tropical fruit}           => {soda}                     0.005185562  0.2802198 1.6069747    51
    ## [1389] {bottled water,                                                                                
    ##         soda}                     => {tropical fruit}           0.005185562  0.1789474 1.7053754    51
    ## [1390] {soda,                                                                                         
    ##         tropical fruit}           => {bottled water}            0.005185562  0.2487805 2.2509256    51
    ## [1391] {bottled water,                                                                                
    ##         tropical fruit}           => {yogurt}                   0.007117438  0.3846154 2.7570644    70
    ## [1392] {bottled water,                                                                                
    ##         yogurt}                   => {tropical fruit}           0.007117438  0.3097345 2.9517819    70
    ## [1393] {tropical fruit,                                                                               
    ##         yogurt}                   => {bottled water}            0.007117438  0.2430556 2.1991273    70
    ## [1394] {bottled water,                                                                                
    ##         tropical fruit}           => {rolls/buns}               0.005388917  0.2912088 1.5832164    53
    ## [1395] {bottled water,                                                                                
    ##         rolls/buns}               => {tropical fruit}           0.005388917  0.2226891 2.1222355    53
    ## [1396] {rolls/buns,                                                                                   
    ##         tropical fruit}           => {bottled water}            0.005388917  0.2190083 1.9815513    53
    ## [1397] {bottled water,                                                                                
    ##         tropical fruit}           => {other vegetables}         0.006202339  0.3351648 1.7321840    61
    ## [1398] {bottled water,                                                                                
    ##         other vegetables}         => {tropical fruit}           0.006202339  0.2500000 2.3825097    61
    ## [1399] {other vegetables,                                                                             
    ##         tropical fruit}           => {bottled water}            0.006202339  0.1728045 1.5635074    61
    ## [1400] {bottled water,                                                                                
    ##         tropical fruit}           => {whole milk}               0.008032537  0.4340659 1.6987817    79
    ## [1401] {bottled water,                                                                                
    ##         whole milk}               => {tropical fruit}           0.008032537  0.2337278 2.2274351    79
    ## [1402] {tropical fruit,                                                                               
    ##         whole milk}               => {bottled water}            0.008032537  0.1899038 1.7182193    79
    ## [1403] {bottled water,                                                                                
    ##         root vegetables}          => {other vegetables}         0.007015760  0.4480519 2.3156022    69
    ## [1404] {bottled water,                                                                                
    ##         other vegetables}         => {root vegetables}          0.007015760  0.2827869 2.5944114    69
    ## [1405] {other vegetables,                                                                             
    ##         root vegetables}          => {bottled water}            0.007015760  0.1480687 1.3397013    69
    ## [1406] {bottled water,                                                                                
    ##         root vegetables}          => {whole milk}               0.007320793  0.4675325 1.8297580    72
    ## [1407] {bottled water,                                                                                
    ##         whole milk}               => {root vegetables}          0.007320793  0.2130178 1.9543186    72
    ## [1408] {root vegetables,                                                                              
    ##         whole milk}               => {bottled water}            0.007320793  0.1496881 1.3543541    72
    ## [1409] {bottled water,                                                                                
    ##         soda}                     => {yogurt}                   0.007422471  0.2561404 1.8361081    73
    ## [1410] {bottled water,                                                                                
    ##         yogurt}                   => {soda}                     0.007422471  0.3230088 1.8523569    73
    ## [1411] {soda,                                                                                         
    ##         yogurt}                   => {bottled water}            0.007422471  0.2713755 2.4553613    73
    ## [1412] {bottled water,                                                                                
    ##         soda}                     => {rolls/buns}               0.006812405  0.2350877 1.2781027    67
    ## [1413] {bottled water,                                                                                
    ##         rolls/buns}               => {soda}                     0.006812405  0.2815126 1.6143886    67
    ## [1414] {rolls/buns,                                                                                   
    ##         soda}                     => {bottled water}            0.006812405  0.1777188 1.6079712    67
    ## [1415] {bottled water,                                                                                
    ##         soda}                     => {other vegetables}         0.005693950  0.1964912 1.0154972    56
    ## [1416] {bottled water,                                                                                
    ##         other vegetables}         => {soda}                     0.005693950  0.2295082 1.3161593    56
    ## [1417] {other vegetables,                                                                             
    ##         soda}                     => {bottled water}            0.005693950  0.1739130 1.5735371    56
    ## [1418] {bottled water,                                                                                
    ##         soda}                     => {whole milk}               0.007524148  0.2596491 1.0161755    74
    ## [1419] {bottled water,                                                                                
    ##         whole milk}               => {soda}                     0.007524148  0.2189349 1.2555247    74
    ## [1420] {soda,                                                                                         
    ##         whole milk}               => {bottled water}            0.007524148  0.1878173 1.6993401    74
    ## [1421] {bottled water,                                                                                
    ##         yogurt}                   => {rolls/buns}               0.007117438  0.3097345 1.6839353    70
    ## [1422] {bottled water,                                                                                
    ##         rolls/buns}               => {yogurt}                   0.007117438  0.2941176 2.1083433    70
    ## [1423] {rolls/buns,                                                                                   
    ##         yogurt}                   => {bottled water}            0.007117438  0.2071006 1.8738126    70
    ## [1424] {bottled water,                                                                                
    ##         yogurt}                   => {other vegetables}         0.008134215  0.3539823 1.8294356    80
    ## [1425] {bottled water,                                                                                
    ##         other vegetables}         => {yogurt}                   0.008134215  0.3278689 2.3502844    80
    ## [1426] {other vegetables,                                                                             
    ##         yogurt}                   => {bottled water}            0.008134215  0.1873536 1.6951453    80
    ## [1427] {bottled water,                                                                                
    ##         yogurt}                   => {whole milk}               0.009659380  0.4203540 1.6451180    95
    ## [1428] {bottled water,                                                                                
    ##         whole milk}               => {yogurt}                   0.009659380  0.2810651 2.0147778    95
    ## [1429] {whole milk,                                                                                   
    ##         yogurt}                   => {bottled water}            0.009659380  0.1724138 1.5599721    95
    ## [1430] {bottled water,                                                                                
    ##         rolls/buns}               => {other vegetables}         0.007320793  0.3025210 1.5634756    72
    ## [1431] {bottled water,                                                                                
    ##         other vegetables}         => {rolls/buns}               0.007320793  0.2950820 1.6042737    72
    ## [1432] {other vegetables,                                                                             
    ##         rolls/buns}               => {bottled water}            0.007320793  0.1718377 1.5547598    72
    ## [1433] {bottled water,                                                                                
    ##         rolls/buns}               => {whole milk}               0.008744281  0.3613445 1.4141757    86
    ## [1434] {bottled water,                                                                                
    ##         whole milk}               => {rolls/buns}               0.008744281  0.2544379 1.3833037    86
    ## [1435] {rolls/buns,                                                                                   
    ##         whole milk}               => {bottled water}            0.008744281  0.1543986 1.3969732    86
    ## [1436] {bottled water,                                                                                
    ##         other vegetables}         => {whole milk}               0.010777834  0.4344262 1.7001918   106
    ## [1437] {bottled water,                                                                                
    ##         whole milk}               => {other vegetables}         0.010777834  0.3136095 1.6207825   106
    ## [1438] {other vegetables,                                                                             
    ##         whole milk}               => {bottled water}            0.010777834  0.1440217 1.3030854   106
    ## [1439] {root vegetables,                                                                              
    ##         tropical fruit}           => {yogurt}                   0.008134215  0.3864734 2.7703835    80
    ## [1440] {tropical fruit,                                                                               
    ##         yogurt}                   => {root vegetables}          0.008134215  0.2777778 2.5484556    80
    ## [1441] {root vegetables,                                                                              
    ##         yogurt}                   => {tropical fruit}           0.008134215  0.3149606 3.0015870    80
    ## [1442] {root vegetables,                                                                              
    ##         tropical fruit}           => {rolls/buns}               0.005897306  0.2801932 1.5233281    58
    ## [1443] {rolls/buns,                                                                                   
    ##         tropical fruit}           => {root vegetables}          0.005897306  0.2396694 2.1988328    58
    ## [1444] {rolls/buns,                                                                                   
    ##         root vegetables}          => {tropical fruit}           0.005897306  0.2426778 2.3127291    58
    ## [1445] {root vegetables,                                                                              
    ##         tropical fruit}           => {other vegetables}         0.012302999  0.5845411 3.0209991   121
    ## [1446] {other vegetables,                                                                             
    ##         tropical fruit}           => {root vegetables}          0.012302999  0.3427762 3.1447798   121
    ## [1447] {other vegetables,                                                                             
    ##         root vegetables}          => {tropical fruit}           0.012302999  0.2596567 2.4745380   121
    ## [1448] {root vegetables,                                                                              
    ##         tropical fruit}           => {whole milk}               0.011997966  0.5700483 2.2309690   118
    ## [1449] {tropical fruit,                                                                               
    ##         whole milk}               => {root vegetables}          0.011997966  0.2836538 2.6023653   118
    ## [1450] {root vegetables,                                                                              
    ##         whole milk}               => {tropical fruit}           0.011997966  0.2453222 2.3379305   118
    ## [1451] {soda,                                                                                         
    ##         tropical fruit}           => {yogurt}                   0.006609049  0.3170732 2.2728970    65
    ## [1452] {tropical fruit,                                                                               
    ##         yogurt}                   => {soda}                     0.006609049  0.2256944 1.2942885    65
    ## [1453] {soda,                                                                                         
    ##         yogurt}                   => {tropical fruit}           0.006609049  0.2416357 2.3027975    65
    ## [1454] {soda,                                                                                         
    ##         tropical fruit}           => {rolls/buns}               0.005388917  0.2585366 1.4055872    53
    ## [1455] {rolls/buns,                                                                                   
    ##         tropical fruit}           => {soda}                     0.005388917  0.2190083 1.2559454    53
    ## [1456] {rolls/buns,                                                                                   
    ##         soda}                     => {tropical fruit}           0.005388917  0.1405836 1.3397667    53
    ## [1457] {soda,                                                                                         
    ##         tropical fruit}           => {other vegetables}         0.007219115  0.3463415 1.7899466    71
    ## [1458] {other vegetables,                                                                             
    ##         tropical fruit}           => {soda}                     0.007219115  0.2011331 1.1534370    71
    ## [1459] {other vegetables,                                                                             
    ##         soda}                     => {tropical fruit}           0.007219115  0.2204969 2.1013440    71
    ## [1460] {soda,                                                                                         
    ##         tropical fruit}           => {whole milk}               0.007829181  0.3756098 1.4700048    77
    ## [1461] {tropical fruit,                                                                               
    ##         whole milk}               => {soda}                     0.007829181  0.1850962 1.0614698    77
    ## [1462] {soda,                                                                                         
    ##         whole milk}               => {tropical fruit}           0.007829181  0.1954315 1.8624695    77
    ## [1463] {tropical fruit,                                                                               
    ##         yogurt}                   => {rolls/buns}               0.008744281  0.2986111 1.6234606    86
    ## [1464] {rolls/buns,                                                                                   
    ##         tropical fruit}           => {yogurt}                   0.008744281  0.3553719 2.5474363    86
    ## [1465] {rolls/buns,                                                                                   
    ##         yogurt}                   => {tropical fruit}           0.008744281  0.2544379 2.4248028    86
    ## [1466] {tropical fruit,                                                                               
    ##         yogurt}                   => {other vegetables}         0.012302999  0.4201389 2.1713431   121
    ## [1467] {other vegetables,                                                                             
    ##         tropical fruit}           => {yogurt}                   0.012302999  0.3427762 2.4571457   121
    ## [1468] {other vegetables,                                                                             
    ##         yogurt}                   => {tropical fruit}           0.012302999  0.2833724 2.7005496   121
    ## [1469] {tropical fruit,                                                                               
    ##         yogurt}                   => {whole milk}               0.015149975  0.5173611 2.0247698   149
    ## [1470] {tropical fruit,                                                                               
    ##         whole milk}               => {yogurt}                   0.015149975  0.3581731 2.5675162   149
    ## [1471] {whole milk,                                                                                   
    ##         yogurt}                   => {tropical fruit}           0.015149975  0.2704174 2.5770885   149
    ## [1472] {rolls/buns,                                                                                   
    ##         tropical fruit}           => {other vegetables}         0.007829181  0.3181818 1.6444131    77
    ## [1473] {other vegetables,                                                                             
    ##         tropical fruit}           => {rolls/buns}               0.007829181  0.2181303 1.1859102    77
    ## [1474] {other vegetables,                                                                             
    ##         rolls/buns}               => {tropical fruit}           0.007829181  0.1837709 1.7513436    77
    ## [1475] {rolls/buns,                                                                                   
    ##         tropical fruit}           => {whole milk}               0.010981190  0.4462810 1.7465872   108
    ## [1476] {tropical fruit,                                                                               
    ##         whole milk}               => {rolls/buns}               0.010981190  0.2596154 1.4114524   108
    ## [1477] {rolls/buns,                                                                                   
    ##         whole milk}               => {tropical fruit}           0.010981190  0.1938959 1.8478352   108
    ## [1478] {other vegetables,                                                                             
    ##         tropical fruit}           => {whole milk}               0.017081851  0.4759207 1.8625865   168
    ## [1479] {tropical fruit,                                                                               
    ##         whole milk}               => {other vegetables}         0.017081851  0.4038462 2.0871397   168
    ## [1480] {other vegetables,                                                                             
    ##         whole milk}               => {tropical fruit}           0.017081851  0.2282609 2.1753349   168
    ## [1481] {root vegetables,                                                                              
    ##         soda}                     => {other vegetables}         0.008235892  0.4426230 2.2875443    81
    ## [1482] {other vegetables,                                                                             
    ##         root vegetables}          => {soda}                     0.008235892  0.1738197 0.9968030    81
    ## [1483] {other vegetables,                                                                             
    ##         soda}                     => {root vegetables}          0.008235892  0.2515528 2.3078561    81
    ## [1484] {root vegetables,                                                                              
    ##         soda}                     => {whole milk}               0.008134215  0.4371585 1.7108848    80
    ## [1485] {root vegetables,                                                                              
    ##         whole milk}               => {soda}                     0.008134215  0.1663202 0.9537952    80
    ## [1486] {soda,                                                                                         
    ##         whole milk}               => {root vegetables}          0.008134215  0.2030457 1.8628305    80
    ## [1487] {root vegetables,                                                                              
    ##         yogurt}                   => {rolls/buns}               0.007219115  0.2795276 1.5197090    71
    ## [1488] {rolls/buns,                                                                                   
    ##         root vegetables}          => {yogurt}                   0.007219115  0.2970711 2.1295150    71
    ## [1489] {rolls/buns,                                                                                   
    ##         yogurt}                   => {root vegetables}          0.007219115  0.2100592 1.9271753    71
    ## [1490] {root vegetables,                                                                              
    ##         yogurt}                   => {other vegetables}         0.012913066  0.5000000 2.5840778   127
    ## [1491] {other vegetables,                                                                             
    ##         root vegetables}          => {yogurt}                   0.012913066  0.2725322 1.9536108   127
    ## [1492] {other vegetables,                                                                             
    ##         yogurt}                   => {root vegetables}          0.012913066  0.2974239 2.7286977   127
    ## [1493] {root vegetables,                                                                              
    ##         yogurt}                   => {whole milk}               0.014539908  0.5629921 2.2033536   143
    ## [1494] {root vegetables,                                                                              
    ##         whole milk}               => {yogurt}                   0.014539908  0.2972973 2.1311362   143
    ## [1495] {whole milk,                                                                                   
    ##         yogurt}                   => {root vegetables}          0.014539908  0.2595281 2.3810253   143
    ## [1496] {rolls/buns,                                                                                   
    ##         root vegetables}          => {other vegetables}         0.012201322  0.5020921 2.5948898   120
    ## [1497] {other vegetables,                                                                             
    ##         root vegetables}          => {rolls/buns}               0.012201322  0.2575107 1.4000100   120
    ## [1498] {other vegetables,                                                                             
    ##         rolls/buns}               => {root vegetables}          0.012201322  0.2863962 2.6275247   120
    ## [1499] {rolls/buns,                                                                                   
    ##         root vegetables}          => {whole milk}               0.012709710  0.5230126 2.0468876   125
    ## [1500] {root vegetables,                                                                              
    ##         whole milk}               => {rolls/buns}               0.012709710  0.2598753 1.4128652   125
    ## [1501] {rolls/buns,                                                                                   
    ##         whole milk}               => {root vegetables}          0.012709710  0.2244165 2.0588959   125
    ## [1502] {other vegetables,                                                                             
    ##         root vegetables}          => {whole milk}               0.023182511  0.4892704 1.9148326   228
    ## [1503] {root vegetables,                                                                              
    ##         whole milk}               => {other vegetables}         0.023182511  0.4740125 2.4497702   228
    ## [1504] {other vegetables,                                                                             
    ##         whole milk}               => {root vegetables}          0.023182511  0.3097826 2.8420820   228
    ## [1505] {soda,                                                                                         
    ##         yogurt}                   => {rolls/buns}               0.008642603  0.3159851 1.7179181    85
    ## [1506] {rolls/buns,                                                                                   
    ##         soda}                     => {yogurt}                   0.008642603  0.2254642 1.6162101    85
    ## [1507] {rolls/buns,                                                                                   
    ##         yogurt}                   => {soda}                     0.008642603  0.2514793 1.4421567    85
    ## [1508] {soda,                                                                                         
    ##         yogurt}                   => {other vegetables}         0.008337570  0.3048327 1.5754229    82
    ## [1509] {other vegetables,                                                                             
    ##         soda}                     => {yogurt}                   0.008337570  0.2546584 1.8254849    82
    ## [1510] {other vegetables,                                                                             
    ##         yogurt}                   => {soda}                     0.008337570  0.1920375 1.1012761    82
    ## [1511] {soda,                                                                                         
    ##         yogurt}                   => {whole milk}               0.010472801  0.3828996 1.4985348   103
    ## [1512] {soda,                                                                                         
    ##         whole milk}               => {yogurt}                   0.010472801  0.2614213 1.8739641   103
    ## [1513] {whole milk,                                                                                   
    ##         yogurt}                   => {soda}                     0.010472801  0.1869328 1.0720027   103
    ## [1514] {rolls/buns,                                                                                   
    ##         soda}                     => {other vegetables}         0.009862735  0.2572944 1.3297376    97
    ## [1515] {other vegetables,                                                                             
    ##         soda}                     => {rolls/buns}               0.009862735  0.3012422 1.6377653    97
    ## [1516] {other vegetables,                                                                             
    ##         rolls/buns}               => {soda}                     0.009862735  0.2315036 1.3276022    97
    ## [1517] {rolls/buns,                                                                                   
    ##         soda}                     => {whole milk}               0.008845958  0.2307692 0.9031498    87
    ## [1518] {soda,                                                                                         
    ##         whole milk}               => {rolls/buns}               0.008845958  0.2208122 1.2004908    87
    ## [1519] {rolls/buns,                                                                                   
    ##         whole milk}               => {soda}                     0.008845958  0.1561939 0.8957242    87
    ## [1520] {other vegetables,                                                                             
    ##         soda}                     => {whole milk}               0.013929842  0.4254658 1.6651240   137
    ## [1521] {soda,                                                                                         
    ##         whole milk}               => {other vegetables}         0.013929842  0.3477157 1.7970490   137
    ## [1522] {other vegetables,                                                                             
    ##         whole milk}               => {soda}                     0.013929842  0.1861413 1.0674634   137
    ## [1523] {rolls/buns,                                                                                   
    ##         yogurt}                   => {other vegetables}         0.011489578  0.3343195 1.7278153   113
    ## [1524] {other vegetables,                                                                             
    ##         yogurt}                   => {rolls/buns}               0.011489578  0.2646370 1.4387534   113
    ## [1525] {other vegetables,                                                                             
    ##         rolls/buns}               => {yogurt}                   0.011489578  0.2696897 1.9332351   113
    ## [1526] {rolls/buns,                                                                                   
    ##         yogurt}                   => {whole milk}               0.015556685  0.4526627 1.7715630   153
    ## [1527] {whole milk,                                                                                   
    ##         yogurt}                   => {rolls/buns}               0.015556685  0.2776770 1.5096478   153
    ## [1528] {rolls/buns,                                                                                   
    ##         whole milk}               => {yogurt}                   0.015556685  0.2746858 1.9690488   153
    ## [1529] {other vegetables,                                                                             
    ##         yogurt}                   => {whole milk}               0.022267412  0.5128806 2.0072345   219
    ## [1530] {whole milk,                                                                                   
    ##         yogurt}                   => {other vegetables}         0.022267412  0.3974592 2.0541308   219
    ## [1531] {other vegetables,                                                                             
    ##         whole milk}               => {yogurt}                   0.022267412  0.2975543 2.1329789   219
    ## [1532] {other vegetables,                                                                             
    ##         rolls/buns}               => {whole milk}               0.017895272  0.4200477 1.6439194   176
    ## [1533] {rolls/buns,                                                                                   
    ##         whole milk}               => {other vegetables}         0.017895272  0.3159785 1.6330258   176
    ## [1534] {other vegetables,                                                                             
    ##         whole milk}               => {rolls/buns}               0.017895272  0.2391304 1.3000817   176
    ## [1535] {fruit/vegetable juice,                                                                        
    ##         other vegetables,                                                                             
    ##         yogurt}                   => {whole milk}               0.005083884  0.6172840 2.4158327    50
    ## [1536] {fruit/vegetable juice,                                                                        
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {other vegetables}         0.005083884  0.5376344 2.7785782    50
    ## [1537] {fruit/vegetable juice,                                                                        
    ##         other vegetables,                                                                             
    ##         whole milk}               => {yogurt}                   0.005083884  0.4854369 3.4797900    50
    ## [1538] {other vegetables,                                                                             
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {fruit/vegetable juice}    0.005083884  0.2283105 3.1581347    50
    ## [1539] {other vegetables,                                                                             
    ##         root vegetables,                                                                              
    ##         whipped/sour cream}       => {whole milk}               0.005185562  0.6071429 2.3761441    51
    ## [1540] {root vegetables,                                                                              
    ##         whipped/sour cream,                                                                           
    ##         whole milk}               => {other vegetables}         0.005185562  0.5483871 2.8341498    51
    ## [1541] {other vegetables,                                                                             
    ##         whipped/sour cream,                                                                           
    ##         whole milk}               => {root vegetables}          0.005185562  0.3541667 3.2492809    51
    ## [1542] {other vegetables,                                                                             
    ##         root vegetables,                                                                              
    ##         whole milk}               => {whipped/sour cream}       0.005185562  0.2236842 3.1204741    51
    ## [1543] {other vegetables,                                                                             
    ##         whipped/sour cream,                                                                           
    ##         yogurt}                   => {whole milk}               0.005592272  0.5500000 2.1525070    55
    ## [1544] {whipped/sour cream,                                                                           
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {other vegetables}         0.005592272  0.5140187 2.6565286    55
    ## [1545] {other vegetables,                                                                             
    ##         whipped/sour cream,                                                                           
    ##         whole milk}               => {yogurt}                   0.005592272  0.3819444 2.7379181    55
    ## [1546] {other vegetables,                                                                             
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {whipped/sour cream}       0.005592272  0.2511416 3.5035137    55
    ## [1547] {other vegetables,                                                                             
    ##         pip fruit,                                                                                    
    ##         root vegetables}          => {whole milk}               0.005490595  0.6750000 2.6417131    54
    ## [1548] {pip fruit,                                                                                    
    ##         root vegetables,                                                                              
    ##         whole milk}               => {other vegetables}         0.005490595  0.6136364 3.1713682    54
    ## [1549] {other vegetables,                                                                             
    ##         pip fruit,                                                                                    
    ##         whole milk}               => {root vegetables}          0.005490595  0.4060150 3.7249607    54
    ## [1550] {other vegetables,                                                                             
    ##         root vegetables,                                                                              
    ##         whole milk}               => {pip fruit}                0.005490595  0.2368421 3.1308362    54
    ## [1551] {other vegetables,                                                                             
    ##         pip fruit,                                                                                    
    ##         yogurt}                   => {whole milk}               0.005083884  0.6250000 2.4460306    50
    ## [1552] {pip fruit,                                                                                    
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {other vegetables}         0.005083884  0.5319149 2.7490189    50
    ## [1553] {other vegetables,                                                                             
    ##         pip fruit,                                                                                    
    ##         whole milk}               => {yogurt}                   0.005083884  0.3759398 2.6948749    50
    ## [1554] {other vegetables,                                                                             
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {pip fruit}                0.005083884  0.2283105 3.0180562    50
    ## [1555] {citrus fruit,                                                                                 
    ##         other vegetables,                                                                             
    ##         root vegetables}          => {whole milk}               0.005795628  0.5588235 2.1870392    57
    ## [1556] {citrus fruit,                                                                                 
    ##         root vegetables,                                                                              
    ##         whole milk}               => {other vegetables}         0.005795628  0.6333333 3.2731652    57
    ## [1557] {citrus fruit,                                                                                 
    ##         other vegetables,                                                                             
    ##         whole milk}               => {root vegetables}          0.005795628  0.4453125 4.0854929    57
    ## [1558] {other vegetables,                                                                             
    ##         root vegetables,                                                                              
    ##         whole milk}               => {citrus fruit}             0.005795628  0.2500000 3.0205774    57
    ## [1559] {root vegetables,                                                                              
    ##         tropical fruit,                                                                               
    ##         yogurt}                   => {whole milk}               0.005693950  0.7000000 2.7395543    56
    ## [1560] {root vegetables,                                                                              
    ##         tropical fruit,                                                                               
    ##         whole milk}               => {yogurt}                   0.005693950  0.4745763 3.4019370    56
    ## [1561] {tropical fruit,                                                                               
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {root vegetables}          0.005693950  0.3758389 3.4481118    56
    ## [1562] {root vegetables,                                                                              
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {tropical fruit}           0.005693950  0.3916084 3.7320432    56
    ## [1563] {other vegetables,                                                                             
    ##         root vegetables,                                                                              
    ##         tropical fruit}           => {whole milk}               0.007015760  0.5702479 2.2317503    69
    ## [1564] {root vegetables,                                                                              
    ##         tropical fruit,                                                                               
    ##         whole milk}               => {other vegetables}         0.007015760  0.5847458 3.0220571    69
    ## [1565] {other vegetables,                                                                             
    ##         tropical fruit,                                                                               
    ##         whole milk}               => {root vegetables}          0.007015760  0.4107143 3.7680737    69
    ## [1566] {other vegetables,                                                                             
    ##         root vegetables,                                                                              
    ##         whole milk}               => {tropical fruit}           0.007015760  0.3026316 2.8840907    69
    ## [1567] {other vegetables,                                                                             
    ##         tropical fruit,                                                                               
    ##         yogurt}                   => {whole milk}               0.007625826  0.6198347 2.4258155    75
    ## [1568] {tropical fruit,                                                                               
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {other vegetables}         0.007625826  0.5033557 2.6014206    75
    ## [1569] {other vegetables,                                                                             
    ##         tropical fruit,                                                                               
    ##         whole milk}               => {yogurt}                   0.007625826  0.4464286 3.2001640    75
    ## [1570] {other vegetables,                                                                             
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {tropical fruit}           0.007625826  0.3424658 3.2637119    75
    ## [1571] {other vegetables,                                                                             
    ##         root vegetables,                                                                              
    ##         yogurt}                   => {whole milk}               0.007829181  0.6062992 2.3728423    77
    ## [1572] {root vegetables,                                                                              
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {other vegetables}         0.007829181  0.5384615 2.7828530    77
    ## [1573] {other vegetables,                                                                             
    ##         root vegetables,                                                                              
    ##         whole milk}               => {yogurt}                   0.007829181  0.3377193 2.4208960    77
    ## [1574] {other vegetables,                                                                             
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {root vegetables}          0.007829181  0.3515982 3.2257165    77
    ## [1575] {other vegetables,                                                                             
    ##         rolls/buns,                                                                                   
    ##         root vegetables}          => {whole milk}               0.006202339  0.5083333 1.9894383    61
    ## [1576] {rolls/buns,                                                                                   
    ##         root vegetables,                                                                              
    ##         whole milk}               => {other vegetables}         0.006202339  0.4880000 2.5220599    61
    ## [1577] {other vegetables,                                                                             
    ##         root vegetables,                                                                              
    ##         whole milk}               => {rolls/buns}               0.006202339  0.2675439 1.4545571    61
    ## [1578] {other vegetables,                                                                             
    ##         rolls/buns,                                                                                   
    ##         whole milk}               => {root vegetables}          0.006202339  0.3465909 3.1797776    61
    ## [1579] {other vegetables,                                                                             
    ##         rolls/buns,                                                                                   
    ##         yogurt}                   => {whole milk}               0.005998983  0.5221239 2.0434097    59
    ## [1580] {rolls/buns,                                                                                   
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {other vegetables}         0.005998983  0.3856209 1.9929489    59
    ## [1581] {other vegetables,                                                                             
    ##         whole milk,                                                                                   
    ##         yogurt}                   => {rolls/buns}               0.005998983  0.2694064 1.4646832    59
    ## [1582] {other vegetables,                                                                             
    ##         rolls/buns,                                                                                   
    ##         whole milk}               => {yogurt}                   0.005998983  0.3352273 2.4030322    59

``` r
inspect(subset(grocery_rules, subset=lift > 4))
```

    ##     lhs                   rhs                      support confidence     lift count
    ## [1] {ham}              => {white bread}        0.005083884  0.1953125 4.639851    50
    ## [2] {white bread}      => {ham}                0.005083884  0.1207729 4.639851    50
    ## [3] {butter,                                                                        
    ##      other vegetables} => {whipped/sour cream} 0.005795628  0.2893401 4.036397    57
    ## [4] {citrus fruit,                                                                  
    ##      other vegetables,                                                              
    ##      whole milk}       => {root vegetables}    0.005795628  0.4453125 4.085493    57

``` r
inspect(subset(grocery_rules, subset=confidence > 0.6))
```

    ##      lhs                        rhs                    support confidence     lift count
    ## [1]  {onions,                                                                           
    ##       root vegetables}       => {other vegetables} 0.005693950  0.6021505 3.112008    56
    ## [2]  {curd,                                                                             
    ##       tropical fruit}        => {whole milk}       0.006507372  0.6336634 2.479936    64
    ## [3]  {domestic eggs,                                                                    
    ##       margarine}             => {whole milk}       0.005185562  0.6219512 2.434099    51
    ## [4]  {butter,                                                                           
    ##       domestic eggs}         => {whole milk}       0.005998983  0.6210526 2.430582    59
    ## [5]  {butter,                                                                           
    ##       whipped/sour cream}    => {whole milk}       0.006710727  0.6600000 2.583008    66
    ## [6]  {bottled water,                                                                    
    ##       butter}                => {whole milk}       0.005388917  0.6022727 2.357084    53
    ## [7]  {butter,                                                                           
    ##       tropical fruit}        => {whole milk}       0.006202339  0.6224490 2.436047    61
    ## [8]  {butter,                                                                           
    ##       root vegetables}       => {whole milk}       0.008235892  0.6377953 2.496107    81
    ## [9]  {butter,                                                                           
    ##       yogurt}                => {whole milk}       0.009354347  0.6388889 2.500387    92
    ## [10] {domestic eggs,                                                                    
    ##       pip fruit}             => {whole milk}       0.005388917  0.6235294 2.440275    53
    ## [11] {domestic eggs,                                                                    
    ##       tropical fruit}        => {whole milk}       0.006914082  0.6071429 2.376144    68
    ## [12] {pip fruit,                                                                        
    ##       whipped/sour cream}    => {other vegetables} 0.005592272  0.6043956 3.123610    55
    ## [13] {pip fruit,                                                                        
    ##       whipped/sour cream}    => {whole milk}       0.005998983  0.6483516 2.537421    59
    ## [14] {fruit/vegetable juice,                                                            
    ##       other vegetables,                                                                 
    ##       yogurt}                => {whole milk}       0.005083884  0.6172840 2.415833    50
    ## [15] {other vegetables,                                                                 
    ##       root vegetables,                                                                  
    ##       whipped/sour cream}    => {whole milk}       0.005185562  0.6071429 2.376144    51
    ## [16] {other vegetables,                                                                 
    ##       pip fruit,                                                                        
    ##       root vegetables}       => {whole milk}       0.005490595  0.6750000 2.641713    54
    ## [17] {pip fruit,                                                                        
    ##       root vegetables,                                                                  
    ##       whole milk}            => {other vegetables} 0.005490595  0.6136364 3.171368    54
    ## [18] {other vegetables,                                                                 
    ##       pip fruit,                                                                        
    ##       yogurt}                => {whole milk}       0.005083884  0.6250000 2.446031    50
    ## [19] {citrus fruit,                                                                     
    ##       root vegetables,                                                                  
    ##       whole milk}            => {other vegetables} 0.005795628  0.6333333 3.273165    57
    ## [20] {root vegetables,                                                                  
    ##       tropical fruit,                                                                   
    ##       yogurt}                => {whole milk}       0.005693950  0.7000000 2.739554    56
    ## [21] {other vegetables,                                                                 
    ##       tropical fruit,                                                                   
    ##       yogurt}                => {whole milk}       0.007625826  0.6198347 2.425816    75
    ## [22] {other vegetables,                                                                 
    ##       root vegetables,                                                                  
    ##       yogurt}                => {whole milk}       0.007829181  0.6062992 2.372842    77

``` r
inspect(subset(grocery_rules, subset=lift > 2.5 & confidence > 0.6))
```

    ##     lhs                     rhs                    support confidence     lift count
    ## [1] {onions,                                                                        
    ##      root vegetables}    => {other vegetables} 0.005693950  0.6021505 3.112008    56
    ## [2] {butter,                                                                        
    ##      whipped/sour cream} => {whole milk}       0.006710727  0.6600000 2.583008    66
    ## [3] {butter,                                                                        
    ##      yogurt}             => {whole milk}       0.009354347  0.6388889 2.500387    92
    ## [4] {pip fruit,                                                                     
    ##      whipped/sour cream} => {other vegetables} 0.005592272  0.6043956 3.123610    55
    ## [5] {pip fruit,                                                                     
    ##      whipped/sour cream} => {whole milk}       0.005998983  0.6483516 2.537421    59
    ## [6] {other vegetables,                                                              
    ##      pip fruit,                                                                     
    ##      root vegetables}    => {whole milk}       0.005490595  0.6750000 2.641713    54
    ## [7] {pip fruit,                                                                     
    ##      root vegetables,                                                               
    ##      whole milk}         => {other vegetables} 0.005490595  0.6136364 3.171368    54
    ## [8] {citrus fruit,                                                                  
    ##      root vegetables,                                                               
    ##      whole milk}         => {other vegetables} 0.005795628  0.6333333 3.273165    57
    ## [9] {root vegetables,                                                               
    ##      tropical fruit,                                                                
    ##      yogurt}             => {whole milk}       0.005693950  0.7000000 2.739554    56

``` r
plot(grocery_rules, measure = c("support", "lift"), shading = "confidence", jitter = 0)
```

![](Henson_Q6_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
saveAsGraph(head(grocery_rules, n = 750, by = "lift"), file = "grocery_rules.graphml")
```

``` r
knitr::include_graphics("grocery_list.svg")
```

![](grocery_list.svg)<!-- -->
