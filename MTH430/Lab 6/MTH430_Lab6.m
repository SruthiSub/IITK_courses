function [] = MTH430_Lab6()
    N=100;
    Q=100;
    h=2/(N+1);
    f = @(t,y,y1) (2*y1*y1)/y - y;
    yexac = @(t) 1/(exp(t)+exp(-t));
    u0=1/(exp(1)+exp(-1));
    a=u0;
    b=u0;
    A=zeros(N,N);
    T=zeros(N,1);
    U=zeros(N,Q);
    for i=1:N
        A(i,i)=-2/(h*h);
        T(i)=i*h-1;
        U(i,1)=u0;
    end
    Uexac=zeros(N,1);
    for i=1:N
        Uexac(i,1)=yexac(T(i,1));
    end
    A(1,2)=1/(h*h);
    A(N,N-1)=1/(h*h);
    g=zeros(N,1);
    g(1)=-a/(h*h);
    g(N)=-b/(h*h);
    for i=2:N-1
        A(i,i-1)=1/(h*h);
        A(i,i+1)=1/(h*h);
    end
    F=zeros(N,1);
    for n=1:Q
        u=U(:,n);
        F1=zeros(N);
        F(1,1)=f(T(1),u(1),(u(2)-a)/(2*h));
        F1(1,1)=-1-((u(2)-a)^2)/(u(1)*u(1)*2*h*h);
        F1(1,2)=(u(2)-a)/(u(1)*h*h);
        F(N,1)=f(T(N),u(N),(b-u(N-1))/(2*h));
        F1(N,N-1)=-(b-u(N-1))/(u(N)*h*h);
        F1(N,N)=-1-(b-u(N-1))^2/(u(N)*u(N)*2*h*h);
        for i=2:N-1
            F(i,1)=f(T(i),u(i),(u(i+1)-u(i-1))/(2*h));
            F1(i,i)=-1-((u(i+1)-u(i-1))^2)/(2*h*h*u(i)*u(i));
            F1(i,i-1)=-(u(i+1)-u(i-1))/(u(i)*h*h);
            F1(i,i+1)=(u(i+1)-u(i-1))/(u(i)*h*h);
        end
        unew=u-(A - F1)\((A)*u-F-g);
        U(:,n+1)=unew;
    end
    plot(T,Uexac,T,U(:,Q+1));
