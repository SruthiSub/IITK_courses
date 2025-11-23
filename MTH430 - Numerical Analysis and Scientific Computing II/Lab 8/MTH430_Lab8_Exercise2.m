%Lab Assignment 8 Exercise 2
function [x,y,u] = lab8_exercise2(N)
h=1/(2*N);
f= @(x,y) log((x-3)^2+(y-2)^2);
K=N*(N-1)+(N-1)*(2*N-1); %number of variables
y=zeros(K,1);
A=zeros(K);
B=zeros(K,1);
D=(N-1)*(2*N-1);
%Boundary value points - store on (2N+1)x(2N+1) mesh:
BC=zeros(2*N+1,2*N+1);
Exac=zeros(2*N+1,2*N+1);
for i=1:2*N+1
    for j=1:2*N+1
        Exac(i,j)=f((i-1)*h,(j-1)*h);
        if i==1
            BC(i,j)=Exac(i,j);
        end
        if j==2*N+1
            BC(i,j)=Exac(i,j);
        end
        if j==1 && i<N+2
            BC(i,j)=Exac(i,j);
        end
        if i==2*N+1 && j>N
            BC(i,j)=Exac(i,j);
        end
        if j==N+1 && i>N+1
            BC(i,j)=Exac(i,j);
        end
        if i==N+1 && j<N+2
            BC(i,j)=Exac(i,j);
        end
    end
end
%Setting up the system of equations to solve:
%Filling in the equations for interior points:
for i=2:N-2
    for j=2:2*N-2
        p=(i-1)*(2*N-1)+j;
        A(p,p)=-4;
        A(p,p+1)=1;
        A(p,p-1)=1;
        A(p,p+(2*N-1))=1;
        A(p,p-(2*N-1))=1;
    end
end
i=N-1;
for j=N+3:2*N-2
    p=(i-1)*(2*N-1)+j;
    A(p,p)=-4;
    A(p,p+1)=1;
    A(p,p-1)=1;
    A(p,p+(N-1))=1;
    A(p,p-(2*N-1))=1;
end
for i=1:N-1
    for j=2:N-2
        p=D+j+(N-1)*(i-1);
        A(p,p)=-4;
        A(p,p+1)=1;
        A(p,p-1)=1;
        A(p,p+(N-1))=1;
        A(p,p-(N-1))=1;
    end
end
%For boundary points:
%Outer Corners First: 
%Left most corner:
A(1,1)=-4;
A(1,2)=1;
A(1,2*N)=1;
B(1,1)=-BC(1,2)-BC(2,1);
%Right most corner:
A(2*N-1,2*N-1)=-4;
A(2*N-1,(2*N-1)*2)=1;
A(2*N-1,2*N-2)=1;
B(2*N-1,1)=-BC(1,2*N)-BC(2,2*N+1);
%Bottom most corner
p=D+N*(N-1);
A(p,p)=-4;
A(p,p-1)=1;
A(p,p-(N-1))=1;
B(p,1)=-BC(2*N+1,2*N)-BC(2*N,2*N+1);
%Inner corners
%Left most corner
p=(2*N-1)*(N-2)+1;
A(p,p)=-4;
A(p,p+1)=1;
A(p,p-(2*N-1))=1;
B(p,1)=-BC(N,1)-BC(N+1,2);
%Right most corner
p=D+(N-1)*(N-1)+1;
A(p,p)=-4;
A(p,p+1)=1;
A(p,p-(N-1))=1;
B(p,1)=-BC(2*N,N+1)-BC(2*N+1,N+2);
%Filling in equations for border rows:
%Rows
for j=2:2*N-2
    A(j,j)=-4;
    A(j,j-1)=1;
    A(j,j+1)=1;
    A(j,j+(2*N-1))=1;
    B(j,1)=-BC(1,j+1);
end
for j=2:N+1 
    p=j+(N-2)*(2*N-1);
    A(p,p)=-4;
    A(p,p-1)=1;
    A(p,p+1)=1;
    A(p,p-(2*N-1))=1;
    B(p,1)=-BC(N+1,j+1);
end
p=(N-2)*(2*N-1)+N+2;
A(p,p)=-4;
A(p,p+1)=1;
A(p,p-1)=1;
A(p,p-(2*N-1))=1;
A(p,p+(N-1))=1;
for j=2:N-2
    p=D+j+(N-1)*(N-1);
    A(p,p)=-4;
    A(p,p+1)=1;
    A(p,p-1)=1;
    A(p,p-(N-1))=1;
    B(p,1)=-BC(2*N+1,j+N+1);
end
%For columns
for j=2:N-2
    p=(j-1)*(2*N-1)+1;
    A(p,p)=-4;
    A(p,p-(2*N-1))=1;
    A(p,p+(2*N-1))=1;
    A(p,p+1)=1;
    B(p,1)=-BC(j+1,1);
end
for j=1:N-1
    p=D+(N-1)*(j-1)+1;
    A(p,p)=-4;
    A(p,p-(N-1))=1;
    A(p,p+(N-1))=1;
    A(p,p+1)=1;
    B(p,1)=-BC(N+j,1);
end
for j=2:N-2
    p=(2*N-1)*j;
    A(p,p)=-4;
    A(p,p-(2*N-1))=1;
    A(p,p+(2*N-1))=1;
    A(p,p-1)=1;
    B(p,1)=-BC(j+1,2*N+1);
end
j=N-1;
p=(2*N-1)*j;
A(p,p)=-4;
A(p,p-(2*N-1))=1;
A(p,p+(N-1))=1;
A(p,p-1)=1;
B(p,1)=-BC(N,2*N+1);
for j=1:N-1
    p=D+j*(N-1);
    A(p,p)=-4;
    A(p,p-1)=1;
    A(p,p-(N-1))=1;
    A(p,p+(N-1))=1;
    B(p,1)=-BC(N+j,2*N+1);
end
y=A\B;
%put extimated y values into BC matrix:
for i=1:N-1
    for j=1:2*N-1
        BC(i+1,j+1)=y((i-1)*(2*N-1)+j);
    end
end
for i=1:N
    for j=1:N-1
        BC(i+N,j+N+1)=y(D+(i-1)*(N-1)+j);
    end
end
tiledlayout(2,1)
nexttile
surf(BC)
nexttile
surf(Exac)


BC
Exac

