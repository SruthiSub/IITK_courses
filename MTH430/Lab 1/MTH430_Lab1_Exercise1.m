function [E] = lab1_exercise1(J)
A=[0 1 0;0 0 1;-2 -5 -4];
Y0=[1;0;-1];
E=zeros(J+1,1);
R=zeros(J,1);
for j=0:J
    N=2^j;
    h=1/N;
    t=linspace(0,1,N);
    Y=Y0;
    for T=t
        X=Y+h*(A*Y+[0;0;-4*sin(T)-2*cos(T)]);
        Y=X;
    end
    error=abs(Y(1)-cos(1));
    E(j+1)=error;
end
for k=1:J
    R(k)=E(k)/E(k+1);
end
Y
end

lab1_exercise1(10)
