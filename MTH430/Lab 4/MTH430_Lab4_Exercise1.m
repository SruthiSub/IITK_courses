function [y,yM1,yM2]= lab4_ex1(y0,h,N)
yM1=zeros(1,N+1);
yM2=zeros(1,N+1);
f= @(t,y) -100*y+100*t+101;
yM1(1)=y0;
yM2(1)=y0;
for n=1:N
    yM1(n+1)=yM1(n)+h*f(h*(n-1),yM1(n))+h*h*(100+-100*f(h*(n-1),yM1(n)))/2;
end
for n=1:N
    yM2(n+1)=yM2(n)+h*f(h*(n-1),yM2(n))+h*h*(100+-100*f(h*(n-1),yM2(n)))/2 +h*h*h*(-100*100+100*100*f(h*(n-1),yM2(n)))/6;
end

