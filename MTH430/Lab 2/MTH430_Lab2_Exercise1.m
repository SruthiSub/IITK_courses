function[] = lab2_exercise1(y0,h,N)
YE=zeros(N+1,1);
YBE=zeros(N+1,1);
time=linspace(0,N*h,N+1);
YE(1)=y0;
YBE(1)=y0;
for n=1:N
    t=n*h;
    YE(n+1)=YE(n)+h*(-100*YE(n)+100*(t-h)+101);
    YBE(n+1)=(YBE(n)+h*(100*t+101))/(1+100*h);
end
tiledlayout(2,1)
nexttile
plot(time,YE);
nexttile
plot(time,YBE);
end


lab2_exercise1(1.01,0.1,10);

