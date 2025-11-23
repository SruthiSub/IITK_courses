%Lab 7 exercise 1
%Collocation method with monomial basis
N=10;
h=2/N;
yexac = @(t) 1/(exp(t)+exp(-t));
Yexac=zeros(N+1,1);
a=1/(exp(1)+exp(-1));
b=1/(exp(1)+exp(-1));
f = @(t,u,v) -u+2*v*v/u;
T=linspace(-1,1,N+1);
A=zeros(N+1,N+1);
F=zeros(N+1,1);
F1=zeros(N+1,N+1);
y=zeros(N+1,1);
u=zeros(N+1,1);
z=zeros(N+1,1);
f2=zeros(N+1,1);
f3=zeros(N+1,1);
%fill in first and last equations - boundary conditions
for i =1:N+1
    A(1,i) = (-1)^(i-1);
    A(N+1,i) = 1;
    Yexac(i,1)=yexac(T(i));
    y(i,1)=0.1;
end
F(1,1)=a;
F(N+1,1)=b;
u(1,1)=a;
u(N+1,1)=b;
%fill in other equations - obtained by making r=0 on mesh points
for i =2:N
    for j=1:N+1
        t=T(i);
        A(i,j)=(j-1)*(j-2)*(t^(j-3));
        if j<3
            A(i,j)=0;
        end
    end
end
%Observe that A is constant but F keeps changing, so update F at each step
%Now do Newton's iteration (non-linear system)
Q=20;
for i=1:Q
    %u at this iteration, and z is u' (set values into z now)
    for j=2:N
        g=0;
        t=T(j);
        for m=2:N+1
            g=g+(m-1)*y(m,1)*((t)^(m-2));
        end
        z(j,1)=g;
    end
    for j=2:N
        g=0;
        for m=1:N+1
            g=g+y(m,1)*(t^(m-1));
        end
        u(j,1)=g;
    end
    %now update F and F', then do the Newton iteration step
    for j=2:N
        F(j,1)=f(T(j),u(j,1),z(j,1));
        f2(j,1)=-1-2*(z(j,1)*z(j,1))/((u(j,1)*u(j,1)));
        f3(j,1)= 4*z(j,1)/u(j,1);
        t=T(j);
        F1(j,1)=f2(j,1);
        for k=2:N+1
            F1(j,k)=f2(j,1)*(t^(k-1))+f3(j,1)*(t^(k-2))*(k-1);
        end
    end
    ynew=y-(A - F1)\(A*y-F);
    y=ynew;
end
u=zeros(N+1,1);
for i=1:N+1
    t=T(i);
    g=0;
    for j=1:N+1
        g=g+y(j,1)*(t^(j-1));
    end
    u(i)=g;
end
u
Yexac
plot(T,u,T,Yexac);
