
x = normrnd(0,1,1,100);

[H, pValue, W] = swtest(x,0.05)

% test polynomial
n = 100;
i = ((1:n)' - 3/8) / (n + 1/4)
mtilde  =   norminv(((1:n)' - 3/8) / (n + 1/4));
c    =   1/sqrt(mtilde'*mtilde) * mtilde;
    
m = c(n)

PolyCoef_1   =   [-2.706056 , 4.434685 , -2.071190 , -0.147981 , 0.221157 , m]
u    =   1/sqrt(n)

% The polyval evalulates coefficients from highest order to lowest order
% with lowest coefficient being the n+1th coefficient.
% p(x) = p_1 x^n + p_2 x^n-1 + ... + p_n x^1 + p_{n+1}
% its the reverse of 
% p(x) = c + p_1 x^1 + p_2 x^2 + p_3 x^3 + ... p_n x^n
% p(x) = c + \sum_{i=1}^n p_i x^i
% we can rewrite the function by reversing the coefficients and taking
% the first coefficient as a constant.

testpoly = polyval(PolyCoef_1, u)