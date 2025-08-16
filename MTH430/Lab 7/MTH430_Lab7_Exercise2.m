%Lab 7 exercise 2
%Collocation method with lagrange basis

N= 35;
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
%Let us first define functions which return values of the lagrange
%polynomials at given points, and the same for it's derivatives (so that we
%don't have to keep calculating that at each step!!

function L=l(p,i,N)
T=linspace(-1,1,N+1);
%evaluate li(t) where t is the time T(p)
L=1;
t=T(p);
for j=1:N+1
    if i~=j
        L=L*(t-T(j))/(T(i)-T(j));
    end
end
end
function L=l1(p,i,N)
T=linspace(-1,1,N+1);
%evaluate li'(t) where t is the time T(p)
L=0;
C=1;
t=T(p);
for j=1:N+1
    if i~=j
        C=C/(T(i)-T(j));
    end
end
for k=1:N+1
    elem=1;
    for j=1:N+1
        if (j~=i) && (j~=k)
            elem=elem*(t-T(j));
        end
    end 
    if k~=i
        L=L+elem;
    end
end
L=L*C;
end

function L=l2(p,i,N)
T=linspace(-1,1,N+1);
%evaluate li'(t) where t is the time T(p)
L=0;
C=1;
t=T(p);
for j=1:N+1
    if i~=j
        C=C/(T(i)-T(j));
    end
end

for k=1:N+1
    K=0;
    for l=1:N+1
        elem=1;
        for j=1:N+1
            if (j~=i) && (j~=k) && (j~=l)
                elem=elem*(t-T(j));
            end
        end 
        if l~=i && k~=l
            K=K+elem;
        end
    end
    if k~=i 
        L=L+K;
    end
end
L=L*C;
end

%fill in first and last equations - boundary conditions
for i =1:N+1
    A(1,i) = l(1,i,N);
    A(N+1,i) = l(N+1,i,N);
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
        A(i,j)=l2(i,j,N);
    end
end
%Observe that A is constant but F keeps changing, so update F at each step
%Now do Newton's iteration (non-linear system)
Q=40;
for i=1:Q
    %u at this iteration, and z is u' (set values into z now)
    for j=2:N
        g=0;
        t=T(j);
        for m=2:N+1
            g=g+y(m,1)*l1(j,m,N);
        end
        z(j,1)=g;
    end
    for j=2:N
        g=0;
        for m=1:N+1
            g=g+y(m,1)*l(j,m,N);
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
            F1(j,k)=f2(j,1)*l(j,k,N)+f3(j,1)*l1(j,k,N);
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
        g=g+y(j,1)*l(i,j,N);
    end
    u(i)=g;
end
u
Yexac
plot(T,u,T,Yexac);