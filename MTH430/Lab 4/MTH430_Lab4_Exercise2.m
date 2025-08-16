function [Px,Py,Pz] = lab4_ex2(lat0,lon0,v0,gam0,phi0,h,T)
r0=6378137;
mu = 3986004.418*10^8;
J2=1091.3*10^(-6);
r= @(x,y,z) (x*x+y*y+z*z)^0.5;
gP1 = @(px,py,pz,r) -mu*px*(r0*r0/(r*r) + 3*J2*r0*r0*r0*r0*(1 - 5*pz*pz/(r*r)))/(r*r*r*r*2)/(r0*r0*r);
gP2 = @(px,py,pz,r) -mu*py*(r0*r0/(r*r) + 3*J2*r0*r0*r0*r0*(1 - 5*pz*pz/(r*r)))/(r*r*r*r*2)/(r0*r0*r);
gP3 = @(px,py,pz,r) -mu*pz*(r0*r0/(r*r) + 3*J2*r0*r0*r0*r0*(1 - 5*pz*pz/(r*r)))/(r*r*r*r*2)/(r0*r0*r);
v=6000;
P0=[r0;0;0];
V0=[v*sin(20);0;v*cos(20)];
N=T/h;
YP=zeros(3,N+1);
YU=zeros(3,N+1);
YP(:,1)=P0;
YU(:,1)=V0;
for n=1:N
    px=YP(1,n);
    py=YP(2,n);
    pz=YP(3,n);
    rd=r(px,py,pz);
    YP(:,n+1)=YP(:,n)+h*YU(:,n);
    YU(:,n+1)=YU(:,n)+h*[gP1(px,py,pz,rd);gP2(px,py,pz,rd);gP3(px,py,pz,rd)];
end
plot3(YP(1,:),YP(2,:),YP(3,:));