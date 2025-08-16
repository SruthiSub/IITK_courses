H=[1,0.5,0.25,0.2,0.125,0.1,0.05,0.025,0.01,0.001];
Eh=[0,0,0,0,0,0,0,0,0,0];
for i=1:10
    h=H(i);
    Eh(i)=lab3_ex1(1.01,h,1/h);
end
plot(H,Eh);

function [E]= lab3_ex1(y0,h,N)
Y=zeros(1,N+1);
Ye=zeros(1,N+1);
Er=zeros(1,N);
R=zeros(1,N+1);
f = @(t,y) -100*y+100*t+101;
Y(1)=y0;
yexac = @(t) t+1-exp(-100*t)*(y0-1);
%2nd order Adam-Moulton method. How much order do we need in the
%initializer for stability? => 1 (p-1)?
Y(2)=yexac(h);
for n=2:N
    Y(n+1)=(Y(n)+5*h*(100*h*n+101)/12+h*(8*f(h*(n-1),Y(n))-f(h*(n-2),Y(n-1)))/12)/(1+125*h/3);
end
for n=0:N
    t=n*h;
    Ye(n+1)=yexac(t);
    Er(n+1)=abs(Ye(n+1)-Y(n+1));
end
for n=1:N
    R(n)=Er(n+1)/Er(n);
end
E=max(abs(Er));
end


