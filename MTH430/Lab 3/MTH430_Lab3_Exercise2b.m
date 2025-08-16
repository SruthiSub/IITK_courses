function [E,A] = lab3_ex2b(P,N,M,k,e)
Y=zeros(4,N+1);
r = @(x,y) (x*x+y*y)^(0.5);
h=2*pi/N;
N=N*P;
t=linspace(0,N*h,N+1);
Y(1,1)=1-e;
Y(2,1)=0;
Y(3,1)=0;
Y(4,1)=((1+e)/(1-e))^0.5 ;
if (k==1)
    for n=1:N
        rd=r(Y(1,n),Y(3,n));
        Y(1,n+1)=(Y(1,n)+h*Y(2,n))/(1+(h*h/(rd*rd*rd)));
        Y(3,n+1)=(Y(3,n)+h*Y(4,n))/(1+(h*h/(rd*rd*rd)));
        Y(2,n+1)=Y(2,n)-h*Y(1,n+1)/(rd*rd*rd);
        Y(4,n+1)=Y(4,n)-h*Y(3,n+1)/(rd*rd*rd);
    end
elseif (k==2)
    for n=1:N
        X=Y(:,n);
        rd=r(X(1),X(3));
        a=X(1)+h*X(2)/2;
        b=X(2)-h*X(1)/(2*rd*rd*rd);
        c=X(3)+h*X(4)/2;
        d=X(4)-h*X(3)/(2*rd*rd*rd);
        %to solve g(x) = x
        for i=1:M
            rd=r(X(1),X(3));
            Y(1,n+1)=a+h*X(2)/2;
            Y(2,n+1)=b-h*X(1)/(2*rd*rd*rd);
            Y(3,n+1)=c+h*X(4)/2;
            Y(4,n+1)=d-h*X(3)/(2*rd*rd*rd);
            X=Y(:,n+1);
        end
    end
else
    X=Y(:,1);
    rd=r(X(1),X(3));
    a=X(1)+h*X(2)/2;
    b=X(2)-h*X(1)/(2*rd*rd*rd);
    c=X(3)+h*X(4)/2;
    d=X(4)-h*X(3)/(2*rd*rd*rd);
    %to solve g(x) = x
    for i=1:M
        rd=r(X(1),X(3));
        Y(1,2)=a+h*X(2)/2;
        Y(2,2)=b-h*X(1)/(2*rd*rd*rd);
        Y(3,2)=c+h*X(4)/2;
        Y(4,2)=d-h*X(3)/(2*rd*rd*rd);
        X=Y(:,2);
    end
    for n=2:N
        X=Y(:,n);
        W=Y(:,n-1);
        rd=r(X(1),X(3));
        d=r(W(1),W(3));
        a=X(1)+ 2*h*X(2)/3-h*W(2)/12;
        b=X(2)-2*h*X(1)/(3*rd*rd*rd)-h*W(1)/(12*d*d*d);
        c=X(3)+2*h*X(4)/3+h*W(4)/12;
        d=X(4)-2*h*X(3)/(3*rd*rd*rd) - h*W(3)/(12*d*d*d);
        %to solve g(x) = x
        for i=1:M
            rd=r(X(1),X(3));
            Y(1,n+1)=a+5*h*X(2)/12;
            Y(2,n+1)=b-5*h*X(1)/(12*rd*rd*rd);
            Y(3,n+1)=c+5*h*X(4)/12;
            Y(4,n+1)=d-5*h*X(3)/(12*rd*rd*rd);
            X=Y(:,n+1);
        end
    end
end
x=Y(1,:);
x1=Y(2,:);
y=Y(3,:);
y1=Y(4,:);

%checking angular momentum and energy conservation:
fE = @(x,y,r) ((x*x+y*y)/2)- (1/r);
fA = @(x,x1,y,y1) x*y1-y*x1;
E = zeros(1,N+1);
A=zeros(1,N+1);
for n=1:N+1
    R= r(x(n),y(n));
    E(n) = fE(x1(n),y1(n),R);
    A(n) = fA(x(n),x1(n),y(n),y1(n));
end
tiledlayout(5,1);
nexttile;
plot(t,x);
nexttile;
plot(t,y);
nexttile;
plot(y,x);
nexttile;
plot(t,E);
nexttile;
plot(t,A);
