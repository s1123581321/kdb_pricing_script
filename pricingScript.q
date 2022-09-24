//Interest rate functions
//periodicInterest[[r]ate;[t]ime;[n]umber of periods]
periodicInterest:{[r;t;n]
    xexp[1+r%n;n*t]
    };

//Future cash flows function, uses ACT/365 daycount and continuous compounding and a flat yield curve (constant risk free interest rate)
pvCashflowsConstantDiscount:{[fvList;fvDates;pvDate;r]
    fvList*exp[-1*r*(fvDates-pvDate)%365]
    };

//Future cash flows function, uses ACT/365 daycount and continuous compounding and a step dictionary for the interest rates
//The interest rate used is the one thats equal to or immediatly less than the rate in the dictionary for the tenor required
pvCashflows:{[fvList;fvDates;pvDate;rDict]
    rList:rDict (fvDates-pvDate)%365;
    fvList*exp[-1*rList*(fvDates-pvDate)%365]
    };

//Example, constant risk free interest rate: r = 0.1
//pvCashflowsConstantDiscount[100 100;2023.01.01 2024.01.01;2022.01.01;0.1]
//Example, varying risk free interest rate:r = `s#(`float$1+til 10)!(xexp[0.1+0.08*til 10;2])
//pvCashflows[100 100;2023.01.01 2024.01.01;2022.01.01;`s#(`float$1+til 10)!(xexp[0.1+0.08*til 10;2])]


//Forward/futures functions
//Forward/delivery price pricing with zero forward value at inception
forwardPrice:{[S;I;U;r;u;i;y;t]
    (S-I+U)*exp[t*r+u-i-y]
    };

//Forward/delivery price of a forex forward
forwardFxPrice:{[S;r;rf;t]
    S*exp[t*r-rf]
    };

//Forward/future value
forwardValue:{[F;F2;r;t]
    exp[-1*r*t]*F-F2
    };

//Example, stock forward
//Spot price = £200
//Forward start date = 2022.08.01
//Dividend incomes = £2 per share on 2023.01.01, £2 per share on 2024.01.01
//Risk free interest rate = 1.25%
//Delivery date = 2024.08.01
//forwardPrice[200;sum pvCashflows[2 2;2023.01.01 2024.01.01;2022.08.01;0.0125];0;0.0125;0;0;0;(2024.08.01-2022.08.01)%365]


//Swap functions
//Interest rate curves
riskFreeRateTable:([]tenor:0.01 0.1 0.25 0.5 0.75 1 1.25 1.5 1.75 2 2.25;interestRate:0.58 2.52 3.53 3.54 3.59 3.63 3.67 3.70 3.75 3.78 3.83);
forward3MonthTable:([]forwardStart:0 0.25 0.5 0.75 1 1.25 1.5 1.75 2;forwardRate:3.53 3.55 3.64 3.67 3.71 3.73 3.80 3.81 3.84);
riskFreeDict:`s#(exec tenor from riskFreeRateTable)!0.01*exec interestRate from riskFreeRateTable;
forward3MonthDict:`s#(exec forwardStart from forward3MonthTable)!0.01*exec forwardRate from forward3MonthTable;

//Discounted future floating rate cash flows with a step dictionary for forward rates and a constant tenor
floatingRateCashflows:{[N;tenor;frDict;payDates;pvDate;rDict]
    L:neg[tenor]+(payDates-pvDate)%365;
    //Replacing negative numbers with 0 in case the first leg length is shorter than the overall tenor
    frList:frDict .[L;where L<0;:;0f];
    //Feeding in the deltas to get a proper time multiplyer of ACT/365 for the interest
    cashflowList:N*frList*deltas (payDates-pvDate)%365;
    pvCashflows[cashflowList;payDates;pvDate;rDict]
    };
fixedRateCashflows:{[N;fixedRate;payDates;pvDate;rDict]
    cashflowList:N*fixedRate*deltas (payDates-pvDate)%365;
    pvCashflows[cashflowList;payDates;pvDate;rDict]
    };

//Retunrns the monitary value of a swap by valuing the fixed and floating future cash flows present value
swapValue:{[swapDict]
    floatingCashflowsPV:floatingRateCashflows[swapDict`N;swapDict`tenor;swapDict`frDict;swapDict`floatingPayDates;swapDict`pvDateFloating;swapDict`rDict];
    fixedRateCashflowsPV:fixedRateCashflows[swapDict`N;swapDict`fixedRate;swapDict`fixedPayDates;swapDict`pvDateFixed;swapDict`rDict];
    sum (`fixed`floating!((sum floatingCashflowsPV)-sum fixedRateCashflowsPV;(sum fixedRateCashflowsPV)-sum floatingCashflowsPV))swapDict`paying
    };

//Returns the value of the swap in terms of the yield on the notional of the swap, does not return a percentage
swapYieldValue:{[swapDict]
    swapValue[swapDict]%swapDict`N
    };

//Example dictionary for swap functions
swapInputDict:`N`tenor`frDict`floatingPayDates`pvDateFloating`rDict`fixedRate`fixedPayDates`pvDateFixed`paying!(1000000;0.25;forward3MonthDict;2007.08.22 2007.11.23 2008.02.22 2008.02.29;2007.05.20;riskFreeDict;0.039;2007.08.22 2007.11.23 2008.02.22 2008.02.29;2007.05.20;`floating);

//Example, forward rate step dicitonary for tenor used: `s#(`float$0.125*til 10)!(xexp[0.1+0.08*til 10;2])
//Risk free interest rate step dictionary used: `s#(`float$1+til 10)!(xexp[0.1+0.07*til 10;2])
//Notional = 100
//tenor = 3 months
//pv date = 2022.01.01
//Pay dates = 2022.04.01 2022.07.01
//floatingRateCashflows[100;0.25;`s#(`float$0.125*til 10)!(xexp[0.1+0.08*til 10;2]);2022.04.01 2022.07.01;2022.01.01;`s#(`float$til 10)!(xexp[0.1+0.07*til 10;2])]

//Example executions of floatingRateCashflows with the step dictionaries defined above
//floatingRateCashflows[1000000;0.25;forward3MonthDict;2007.08.22 2007.11.23 2008.02.22 2008.02.29;2007.05.20;riskFreeDict]
//floatingRateCashflows[1000000;0.25;forward3MonthDict;2007.08.22 2007.11.23 2008.02.22 2008.02.29;2007.07.20;riskFreeDict]
//floatingRateCashflows[1000000;0.25;forward3MonthDict;2007.08.22 2007.11.23 2008.02.22 2008.02.29;2007.02.20;riskFreeDict]

//Example of full swap value calculation
//fixedRateCashflows[1000000;0.25;0.039;2007.08.22 2007.11.23 2008.02.22 2008.02.29;2007.05.20;riskFreeDict]
//floatingRateCashflows[1000000;0.25;forward3MonthDict;2007.08.22 2007.11.23 2008.02.22 2008.02.29;2007.05.20;riskFreeDict]
//swapValue[swapInputDict]
//swapYieldValue[swapInputDict]
//swapValue[swapInputDict];

//Feed in the dictionary with a null fixed rate
swapInputDict:`N`tenor`frDict`floatingPayDates`pvDateFloating`rDict`fixedRate`fixedPayDates`pvDateFixed`paying!(1000000;0.25;forward3MonthDict;2007.08.22 2007.11.23 2008.02.22 2008.02.29;2007.05.20;riskFreeDict;0N;2007.08.22 2007.11.23 2008.02.22 2008.02.29;2007.05.20;`floating);

//Function to find the fixed rate of a swap given it's value
swapFixedRateCalc:{[swapInputDict;targetValue;lowInput;highInput;accuracy]
    (swapInputDict`fixedRate):lowInput;
    valueLow:swapValue[swapInputDict];
    (swapInputDict`fixedRate):highInput;
    valueHigh:swapValue[swapInputDict];
    if[accuracy>abs lowInput-highInput;:lowInput];
    $[(abs valueLow-targetValue) <= abs valueHigh-targetValue;highInput:avg[(lowInput;highInput)];lowInput:avg[(lowInput;highInput)]];
    .z.s[swapInputDict;targetValue;lowInput;highInput;accuracy]
    };

//Example execution, finding the fixed rate to 0.0001 precision with a loss of £10,000 in commision
//swapFixedRateCalc[swapInputDict;-10000;0;1;0.0001]




//Option pricing

//Binomial tree option pricing
binomialOptionPricing:{[steps;t;S0;K;vol;r;optionType]
    N:steps+1;
    u:exp[vol*sqrt t%steps];
    d:exp[-1*vol*sqrt t%steps];
    //Risk free probabilities
    pf:(exp[r*t%steps]-d)%u-d;
    qf:1-pf;
    //The final prices
    S:{[S0;u;d;N;n]S0*xexp[u;n]*xexp[d;-1+N-n]}[S0;u;d;N;] each til N;
    //The final option payoffs
    payoff:(`put`call!(max (K-S;N#0);max (S-K;N#0)))optionType;
    //Function that outputs the option value at the start
    {[r;t;steps;pf;qf;payoff]
        Vx:exp[-1*r*t%steps]*{[pf;qf;x](pf*x[0])+qf*x[1]}[pf;qf;] each 1_(,)prior payoff;
        $[2<>count payoff;.z.s[r;t;steps;pf;qf;Vx];Vx]}[r;t;steps;pf;qf;payoff]
    };
//binomialOptionPricing[3;1;2.05;1;0.08'`call]
//binomialOptionPricing[1000;1;2.05;2;0.25;0.08;`call]
//binomialOptionPricing[100;6;100;105;0.10;0.07;`call]



//Black Scholes

//Normal distribution
normDist:{[av;sd;x]
    (1%sd*2.50662828)*exp[-0.5*xexp[(x-av)%sd;2]]
    };
//normalDistribution[5;1;5]

//Cumulative normal distribution
cumulativeNormDist:{[width;av;sd;x]
    n:abs floor(av-x)%width;
    L:sum width*normDist[av;sd;x+(av-x)*(til n)%n];
    $[x>av;0.5+L;0.5-L]
    };
//cumulativeNormDist[0.0001;4;2;3]
//cumulativeNormDist[0.1;0;1;0.28]

//Black Scholes model
blackScholes:{[S;K;t;vol;r;width;putCall]
    d1:(log[S%K]+t*r+0.5*xexp[vol;2])%vol*xexp[t;0.5];
    d2:d1-vol*xexp[t;0.5];
    if[putCall=`call;:(S*cumulativeNormDist[width;0;1;d1])-K*exp[neg r*t]*cumulativeNormDist[width;0;1;d2]];
    if[putCall=`put;:(K*exp[neg r*t]*cumulativeNormDist[width;0;1;neg d2])-S*cumulativeNormDist[width;0;1;neg d1]];
    };

//blackScholes[100;105;6;0.10;0.07;0.0001;`call]
//(binomialOptionPricing[100;6;100;105;0.10;0.07;`call];blackScholesEuNoDiv[100;105;6;0.10;0.07;0.00001;`call])
//With dividend adjusted stock price
//blackScholes[100-0.5*sum pvCashflowsConstantDiscount[5;2022.07.01;2022.01.01;0.07];105;0.5;0.10;0.07;0.0001;`call]
//blackScholes[100-sum pvCashflowsConstantDiscount[5 5 5;2023.01.01 2024.01.01 2025.01.01;2022.01.01;0.07];105;3;0.10;0.07;0.0001;`put]
//With continuous dividend
//blackScholes[100*exp[neg 0.05*3];105;3;0.10;0.07;0.0001;`call]
