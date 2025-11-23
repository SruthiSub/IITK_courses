function []=lab2_exercise2(N,T,a1,a2,b1,b2,y10,y20)
Y=zeros(2,N+1);
Y(1,1)=y10;
Y(2,1)=y20;
h=T/N;
time=linspace(0,T,N+1);
for n=1:N
    X=Y(:,n);
    x0=Y(1,n);
    y0=Y(2,n);
    d_f11 = @(x,y) 1-h*(a1-b1*y);
    d_f12 = @(x,y) h*b1*x;
    d_f21 = @(x,y) -h*b2*y;
    d_f22 = @(x,y) 1+h*(a2-b2*x);
    F= @(x,y) [x-x0-h*x*(a1-b1*y); y-y0-h*y*(-a2+b2*x)];
    for k=1:500
        A=[d_f11(X(1),X(2)) d_f12(X(1),X(2)); d_f21(X(1),X(2)) d_f22(X(1),X(2))];
        if rcond(A)<exp(-10)
            break
        end
        Y=A\F(X(1),X(2));
        X=X-Y;
    end
    Y(1,n+1)=X(1);
    Y(2,n+1)=X(2);
end

tiledlayout(2,1);
nexttile
plot(time,Y(1,:),time,Y(2,:));
nexttile
plot(Y(1,:),Y(2,:));
end

lab2_exercise2(3500,250,1,0.5,0.1,0.02,100,10)