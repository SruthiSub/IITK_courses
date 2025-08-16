function [y,y1,y2,y3,y4] = lab4a(y0,h,N)
f = @(t,y) -100*y+100*t+101;
y=zeros(1,N+1);
%exact solution
yexac = @(t) t+1-exp(-100*t)*(y0-1);
y(1,1)=y0;
for i=1:N
    y(1,i+1)=yexac(i*h);
end
y1=zeros(1,N+1);
y2=zeros(1,N+1);
y3=zeros(1,N+1);
y4=zeros(1,N+1);
y1(1,1)=y0;
y2(1,1)=y0;
y3(1,1)=y0;
y4(1,1)=y0;
%Heun's Method
for n = 1:N
    t=h*n-h;
    p1=y1(1,n);
    k1=f(t,p1);
    p2=p1+h*k1;
    k2=f(t+h,p2);
    y1(1,1+n)=y1(1,n)+h*(k1+k2)/2;
end
%Modified Euler's Method
for n=1:N
    t=n*h-h;
    p1=y2(1,n);
    k1=f(t,p1);
    p2=p1+h*k1/2;
    k2=f(t+h/2,p2);
    y2(1,n+1)=y2(1,n+1)+h*k2;
end
%Heun's 3-stage method
for n=1:N
    t=n*h-h;
    p1=y3(1,n);
    k1=f(t,p1);
    p2=p1+h*k1/2;
    k2=f(t+h/2,p2);
    p3=p1+h*(-1*k1+2*k2);
    k3=f(t+h,p3);
    y3(1,n+1)=y3(1,n)+h*(k1/6+k2*2/3+k3/6);
end
%Runge-Kutta-Simpson 4-stage method
for n=1:N
    t=n*h-h;
    p1=y4(1,n);
    k1=f(t,p1);
    p2=p1+h*k1/2;
    k2=f(t+h/2,p2);
    p3=p1+h*k2/2;
    k3=f(t+h/2,p3);
    p4=p1+h*k3;
    k4=f(t+h,p4);
    y4(1,n+1)=y4(1,n)+h*(k1/6+k2/3+k3/3+k4/6);
end
y
y1
y2
y3
y4

end