program  formula
end program formula
subroutine fcn(n,x,y,f,rpar,ipar)
  integer*4 n,par,ipar
  real*8 y,f,x,rpar
  dimension y(n),f(n),rpar(20) 


f(1+0)=-(ellpsi+ellpsid*y(7+0)+ellpid*y(1+0)+2*y(8+2)*ellpipp+ellpi*y(8+0)-2*y(8+2)*nupidpipp*y(1+0)+y(8+1)**2*(-nupipp)-2*y(8+0)*y(8+2)*nupipipp-y(8+1)*nupippipd*y(1+1)-2*y(8+2)*y(8+1)**2*nupippippipp-2*y(14+0)*y(8+2)*nupsipipp-y(14+1)*y(8+1)*nupsippip)







f(1+1)=-(ellpsi+ellpsid*y(7+1)+ellpid*y(1+1)+6*y(8+3)*ellpipp+ellpi*y(8+1)-6*y(8+3)*nupidpipp*y(1+0)-2*y(8+2)*nupidpipp*y(1+1)-4*y(8+2)*y(8+1)*nupipp-2*y(8+2)*y(8+1)*nupipipp-6*y(8+0)*y(8+3)*nupipipp-2*y(8+1)*nupippipd*y(1+2)-2*y(8+2)*nupippipd*y(1+1)-6*y(8+3)*y(8+1)**2*nupippippipp-8*y(8+2)**2*y(8+1)*nupippippipp-6*y(14+0)*y(8+3)*nupsipipp-2*y(14+1)*y(8+2)*nupsippip-2*y(14+1)*y(8+2)*nupsipipp-2*y(14+2)*y(8+1)*nupsippip)







f(1+2)=-(ellpsi+ellpsid*y(7+2)+ellpid*y(1+2)+12*y(8+4)*ellpipp+ellpi*y(8+2)-2*y(8+2)*nupidpipp*y(1+2)-12*y(8+4)*nupidpipp*y(1+0)-6*y(8+3)*nupidpipp*y(1+1)-4*y(8+2)**2*nupipp-6*y(8+1)*y(8+3)*nupipp-2*y(8+2)**2*nupipipp-6*y(8+1)*y(8+3)*nupipipp-12*y(8+0)*y(8+4)*nupipipp-4*y(8+2)*nupippipd*y(1+2)-3*y(8+3)*nupippipd*y(1+1)-3*y(8+1)*nupippipd*y(1+3)-8*y(8+2)**3*nupippippipp-36*y(8+1)*y(8+3)*y(8+2)*nupippippipp-12*y(8+1)**2*y(8+4)*nupippippipp-12*y(14+0)*y(8+4)*nupsipipp-3*y(14+1)*y(8+3)*nupsippip-6*y(14+1)*y(8+3)*nupsipipp-4*y(14+2)*y(8+2)*nupsippip-2*y(14+2)*y(8+2)*nupsipipp-3*y(14+3)*y(8+1)*nupsippip)







f(1+3)=-(ellpsi+ellpsid*y(7+3)+ellpid*y(1+3)+ellpi*y(8+3)-2*y(8+2)*nupidpipp*y(1+3)-12*y(8+4)*nupidpipp*y(1+1)-6*y(8+3)*nupidpipp*y(1+2)-12*y(8+3)*y(8+2)*nupipp-8*y(8+1)*y(8+4)*nupipp-8*y(8+3)*y(8+2)*nupipipp-12*y(8+1)*y(8+4)*nupipipp-6*y(8+2)*nupippipd*y(1+3)-4*y(8+4)*nupippipd*y(1+1)-6*y(8+3)*nupippipd*y(1+2)-4*y(8+1)*nupippipd*y(1+4)-48*y(8+3)*y(8+2)**2*nupippippipp-64*y(8+1)*y(8+4)*y(8+2)*nupippippipp-36*y(8+1)*y(8+3)**2*nupippippipp-4*y(14+1)*y(8+4)*nupsippip-12*y(14+1)*y(8+4)*nupsipipp-6*y(14+2)*y(8+3)*nupsippip-6*y(14+2)*y(8+3)*nupsipipp-6*y(14+3)*y(8+2)*nupsippip-2*y(14+3)*y(8+2)*nupsipipp-4*y(14+4)*y(8+1)*nupsippip)







f(1+4)=-(ellpsi+ellpsid*y(7+4)+ellpid*y(1+4)+ellpi*y(8+4)-2*y(8+2)*nupidpipp*y(1+4)-12*y(8+4)*nupidpipp*y(1+2)-6*y(8+3)*nupidpipp*y(1+3)-16*y(8+4)*y(8+2)*nupipp-9*y(8+3)**2*nupipp-14*y(8+4)*y(8+2)*nupipipp-6*y(8+3)**2*nupipipp-8*y(8+2)*nupippipd*y(1+4)-8*y(8+4)*nupippipd*y(1+2)-9*y(8+3)*nupippipd*y(1+3)-80*y(8+4)*y(8+2)**2*nupippippipp-90*y(8+3)**2*y(8+2)*nupippippipp-120*y(8+1)*y(8+3)*y(8+4)*nupippippipp-8*y(14+2)*y(8+4)*nupsippip-12*y(14+2)*y(8+4)*nupsipipp-9*y(14+3)*y(8+3)*nupsippip-6*y(14+3)*y(8+3)*nupsipipp-8*y(14+4)*y(8+2)*nupsippip-2*y(14+4)*y(8+2)*nupsipipp)



end subroutine fcn
