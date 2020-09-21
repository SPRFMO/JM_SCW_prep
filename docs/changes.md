In .dat file:

Now, ***Population weight at age*** & ***Maturity at age*** are in .ctl
file

Before After

![](media/image1.png)

In .ctl file:

1.  You have to specify ***the number and name of the stocks.***

2.  In the ***matrix of selectivity*** add a row before, where row 1 is
    index of stock.

3.  After the matrix of selectivity, specify in a vector the ***number
    of regimes*** (by stock).

Before After

![](media/image2.png)

With the number of stocks and the number of regimes by stock, we
construct three ragged arrays (recruitment, growth and natural
mortality). Each row is a stock and each column is a regime.

These arrays specify if the regime is using a new set of recruitment,
growth or natural mortality parameters.

Recruitment array:

1.  Specify the ***recruitment array*** (number of stocks, number
    of regimes)

2.  ***Steepness*** (cv and phase) by each set of parameters in the
    recruitment array.

3.  ***SigmaR*** (cv and phase) by each set of parameters in the
    recruitment array.

4.  ***Phase\_R*** by each stock.

5.  Number of years that we want to fit in the SR-curve.

6.  Years that we want to fit in the SR-curve.

7.  Years where there are a regime shift.

Before After

![](media/image3.png)

Growth array:

1.  Specify the ***growth array*** (number of stocks, number of regimes)

2.  ***Linf*** (cv and phase) by each set of parameters in the
    growth array.

3.  ***K*** (cv and phase) by each set of parameters in the
    growth array.

4.  ***Lo\_Len*** (cv and phase) by each set of parameters in the
    growth array.

5.  ***Sigma\_Len*** (cv and phase) each set of parameters in the
    growth array.

Before After

![](media/image4.png)

Natural mortality array:

1.  Specify the ***natural mortality array*** (number of stocks, number
    of regimes)

2.  ***Natural mortality*** (cv and phase) by each set of parameters in
    the natural mortality array.

Before After

![](media/image5.png)

Finally, add the ***Population weight at age*** and ***Maturity at
age*** vector in the .ctl file

Before After

![](media/image6.png)
