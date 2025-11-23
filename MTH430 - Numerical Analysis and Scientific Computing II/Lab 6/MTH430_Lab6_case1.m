%solve u''+u=t^2+1 using finite difference method
uexac= @(t) t*t-1;
N=20;
Q=30;
t0=-1;
t1=1;
a=0;
b=0;
h=(t1-t0)/(N+1);
f= @(t,y,y1) t*t+1-y;
T=zeros(N,1);
U=zeros(N,Q);
A=zeros(N,N);
Uexac=zeros(N,1);
F1=zeros(N,N);
g=zeros(N,1);
g(1,1)=-a/(h*h);
g(N,1)=-b/(h*h);
F=zeros(N,1);
for i=1:N
    T(i)=t0+h*i;
    A(i,i)=-2;
    U(i,1)=a;
    F1(i,i)=-1;
    Uexac(i)=uexac(T(i));
end
A(1,2) =1;
A(N,N-1)=1;
for i=2:N-1
    A(i,i+1)=1;
    A(i,i-1)=1;
end
for m=1:Q
    u=U(:,m);
    for k=2:N-1
        F(k,1)=f(T(k),u(k),(u(k+1)-u(k-1))/h);
    end
    F(1,1)=f(T(1),u(1),(u(2)-a)/h);
    F(N,1)=f(T(N),u(N),(b-u(N-1))/h);
    unew=u-(A/(h*h) - F1)\(((A/(h*h))*u)-F-g);
end

plot(T,unew,T,Uexac);
unew
Uexac