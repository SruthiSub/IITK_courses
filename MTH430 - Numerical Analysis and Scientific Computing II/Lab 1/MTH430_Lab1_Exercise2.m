function []=lab1_exercise2(N,T,a1,a2,b1,b2,y10,y20)
times=linspace(0,T,N+1);
X=[y10;y20];
Y=zeros(2,N+1);
h=T/N;
Y(:,1)=X;
for i=1:N
    Y(:,i+1)=X+h*[X(1)*(a1-b1*X(2)); X(2)*(-a2+b2*X(1))];
    X=Y(:,i+1);
end

y1=Y(1,:);
y2=Y(2,:);

tiledlayout(2,1);
nexttile
plot(times,y1,times,y2);
nexttile
plot(y1,y2);
end

lab1_exercise2(1000,25,1,0.5,0.1,0.02,100,10)