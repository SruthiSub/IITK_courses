Q=200;
N=200;
t0=-1;
t1=1;
h=(t1-t0)/(Q+1);
s0=0;
a=0;
b=0;
s=zeros(1,N+1);
Y=zeros(1,N);
Yexac=zeros(1,N);
T=zeros(1,N);
yexac= @(t) t*t-1 ;
s(1,1)=s0;
for n=1:N
    T(n)=t0+h*n;
    Yexac(n)=yexac(T(n));
end
for n=1:N
    S=s(1,n);
    f1 = @(t,y1,y2) y2;
    f2 = @(t,y1,y2) -y1+t*t+1;
    z=[0;1];
    y0=[a;s(1,n)];
    u=y0;
    for k=1:Q
        u1=u(1,1)+h*f1(t0+k*h,u(1,1),u(2,1));
        u2=u(2,1)+h*f2(t0+k*h,u(1,1),u(2,1));
        u=[u1 ; u2];
        f3 = @(t,z1,z2) z2;
    f4 = @(t,z1,z2) -z1;
        z1=z(1,1)+h*f3(t0+k*h,z(1,1),z(2,1));
        z2=z(2,1)+h*f4(t0+k*h,z(1,1),z(2,1));
        z=[z1;z2];
        Y(1,k)=u1;
    end
    Snew=S-(u1-b)/z1;
    s(1,n+1)=Snew;
end
plot(T,Yexac,T,Y);
Y