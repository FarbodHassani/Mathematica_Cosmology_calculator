program schema
  !  a'=F(a(x))
  !  b'=G(b(x),H(x))
  ! since H(x)=a'(x)/a(x))
  ! this leads to "1"<=> a
  !               "2"<=>b
  !
  ! f(1)=F(y(1))
  ! f(2)=G(y(2),f(1)/y(1))
  !
  ! note that H'(x)= Q(a(x))
  ! so that if b'=G(b(x),H(x),H'(x))
  ! then
  !
  ! f(2)=G(y(2),f(1)/y(1),Q(y(1)))


  !compute a
  f(aa)=h0*sqrt(Olambda*y(aa)**(1-3*w)+Omatter*y(aa)+Oradiation)

  !the function Q
  Q=-h0**2/4*((4*y(aa)+3*w-1)/y(aa)**(2+3*w)*Olambda+(4/y(aa)-1/y(aa)**2)*Omatter&
       &+4/y(aa)**3*Oradiation)


  ! how to determine tau for a given z
  ! this needs integration
  ! using equ A.1



  !!

  do kw=1,nw
     w=...
     !determine tau for z=0 and z=1000
     ! this uses wz2tau.f90

     do kc=1,nc
        !        now integrate
        !       uses test0115.f90
        ! the initializations are in variables.f90
        ! the time dependent parts should be done in fcn,
        ! with hh(x)=f(aa)/y(aa)
        ! and hhprime(x)=Q

     enddo
  enddo


end program schema
        
subroutine bigfcn
  f(aa)=h0*sqrt(Olambda*y(aa)**(1-3*w)+Omatter*y(aa)+Oradiation)
  ! Q does not depend on f(aa)!
 Q=-h0**2/4*((4*y(aa)+3*w-1)/y(aa)**(2+3*w)*Olambda&
      &+(4/y(aa)-1/y(aa)**2)*Omatter&
       &+4/y(aa)**3*Oradiation)
 hh=f(aa)/y(aa)
ellpid= (1-3*w)*hh
ellpi=(1-3*cs2)*Q+3*(cs2-w)*hh**2
ellpsi= 3*(w-cs2)*hh
ellpsid=-(1+3cs2)
ellpipp= -cs2

nupipipp=-hh*(cs2-1+3*cs2*(1+w))
nupidpipp=-(cs2-1)
nupipp=-hh*(5*cs2+3*w-2)
nupippipd=2*(1-cs2)
nupsipipp=(cs2-1)
nupsippip=(cs2-1)
nupippippipp=3*(cs2-1)/2

!now we can use the vector field

! this is test0115.f90

!

end subroutine bigfcn
