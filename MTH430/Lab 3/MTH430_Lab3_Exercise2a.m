function [E,A] = MTH430_Lab3_Exercise2(P,N,k,e)
r = @(x,y) (x*x+y*y)^(0.5);
f = @(t,y) [y(2); -y(1)/(r(y(1),y(3))*r(y(1),y(3))*r(y(1),y(3))); y(4); -y(3)/(r(y(1),y(3))*r(y(1),y(3))*r(y(1),y(3)))];
Y=zeros(4,N+1);
h=2*pi/N;
N=N*P;
t=linspace(0,P*2*pi,N+1);
Y(:,1) = [1-e,0,0,((1+e)/(1-e))^(0.5)];
if (k==1)
    for n=1:N
        Y(:,n+1)=Y(:,n)+h*f(h*(n-1),Y(:,n));
    end
elseif (k==2)
        Y(:,2) = Y(:,1) +h*(f(0,Y(:,1)));
        for n=3:N+1
            Y(:,n)=Y(:,n-1)+h*(3*f(h*(n-1),Y(:,n-1))-f(h*(n-2),Y(:,n-2)))/2;
        end
else
    Y(:,2) = Y(:,1) +h*(f(0,Y(:,1)));
    Y(:,3) = Y(:,2)+h*(3*f(h*2,Y(:,2))-f(h,Y(:,1)))/2;
    for n=3:N
        Y(:,n+1) = Y(:,n)+ h*(23*f(h*(n-1),Y(:,n))-16*f(h*(n-2),Y(:,n-1))+5*f(h*(n-3),Y(:,n-2)))/12;
    end
end
x=Y(1,:)
x1=Y(2,:);
y=Y(3,:)
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
x
y

