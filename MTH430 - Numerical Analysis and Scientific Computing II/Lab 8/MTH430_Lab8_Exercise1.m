%Lab Assignment 8 Exercise 1
function [x,y,u] = lab8_exercise1(N)
f = @(x,y) log((x-3)*(x-3)+(y-2)*(y-2));
BC=zeros(N+1); %contains boundary values
h=2/(N);
for i=1:N+1
    BC(1,i)=f(1,-1+h*(i-1));
    BC(N+1,i)=f(-1,-1+h*(i-1));
end
for i=2:N
    BC(i,1)=f(1-h*(i-1),-1);
    BC(i,N+1)=f(1-h*(i-1),1);
end
y=zeros((N-1)*(N-1),1);
A=zeros((N-1)*(N-1),(N-1)*(N-1));
B=zeros((N-1)*(N-1),1);
%First set up equations for interior points
for i=2:N-2
    for j=2:N-2
        p=(i-1)*(N-1)+j;
        A(p,p)=-4;
        A(p,p-1)=1;
        A(p,p+1)=1;
        A(p,p-(N-1))=1;
        A(p,p+(N-1))=1;
    end
end
%set up equations for boundary points. Use A for variables and add boundary
%point constants to B while setting up
%For corner points:
A(1,1)=-4;
A(1,2)=1;
A(1,N)=1;
B(1,1)= -BC(1,2)-BC(2,1);
A(N-1,N-1)=-4;
A(N-1,N-2)=1;
A(N-1,2*(N-1))=1;
B(N-1,1)= -BC(1,N)-BC(2,N+1);
A((N-2)*(N-1)+1,(N-2)*(N-1)+1)=-4;
A((N-2)*(N-1)+1,(N-2)*(N-1)+2)=1;
A((N-2)*(N-1)+1,(N-3)*(N-1)+1)=1;
B((N-2)*(N-1)+1)=-BC(N,1)-BC(N+1,2);
A((N-1)*(N-1),(N-1)*(N-1))=-4;
A((N-1)*(N-1),(N-1)*(N-1)-1)=1;
A((N-1)*(N-1),(N-2)*(N-1))=1;
B((N-1)*(N-1))=-BC(N+1,N)-BC(N,N+1);
%First row:
for i=2:N-2
    A(i,i)=-4;
    A(i,i-1)=1;
    A(i,i+1)=1;
    A(i,i+(N-1))=1;
    B(i,1)=-BC(1,i+1);
end
%Last row:
for i=2:N-2
    j=i+(N-1)*(N-2);
    A(j,j)=-4;
    A(j,j+1)=1;
    A(j,j-1)=1;
    A(j,j-(N-1))=1;
    B(j,1)=-BC(N+1,i+1);
end
%For first column
for i=2:N-2
    j=(i-1)*(N-1)+1;
    A(j,j)=-4;
    A(j,j-(N-1))=1;
    A(j,j+(N+1))=1;
    A(j,j+1)=1;
    B(j,1)=-BC(i+1,1);
end
%For last column
for i=2:N-2
    j=(i)*(N-1);
    A(j,j)=-4;
    A(j,j+(N-1))=1;
    A(j,j-(N-1))=1;
    A(j,j-1)=1;
    B(j,1)=-BC(i+1,N+1);
end
A
B
y=inv(A)*(B);
exac=zeros(N+1);
for i =2:N
    for j =2:N
        BC(i,j)=y((i-2)*(N-1)+j-1);
    end
end
for i=1:N+1
    for j=1:N+1
        exac(j,i)=f(1-h*(j-1),-1+(i-1)*h);
    end
end
BC
exac
tiledlayout(1,2)
nexttile
surf(BC)
nexttile
surf(exac)