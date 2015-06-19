!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c     This is a solver for burgers equation.                          c
!c     Using linear 2nd central and 2nd upwind flux reconstruction     c
!c     Author: tuxiongbiao from LSEC.                                  c
!c     Email:tuxb@lsec.cc.ac.cn           04/01/2013                   c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      program main
      use modl  
      call setup()
      call init(uc,x,nx,dx,cdx)

      call sav(uc,x,nx,tend,dx)
!      write(*,*)'timesteps=',nt
!      write(*,*)'the final time is',t
      if(nx==10*2**(len-1))then
      call order()
      end if
      stop
      end program main
       

      
      subroutine init(uc,x,nx,dx,cdx)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)  
      integer::i,nx
      double precision,dimension(-2:nx+3)::x,uc
      double precision::xl,xr,dt,dx,cdx,pi=4.0D+00*atan(1.0D+00)
      xl=0
      xr=2*pi
      dx=(xr-xl)/nx
      cdx=1.0d0/dx
      do i=0,nx
         x(i)=i*dx
!        uc(i)=1.0D+00/2*(1.0D+00/3*(u(x(i))+u(x(i)+dx)) + 4.0D+00/3*u(x(i)+0.5D+00*dx) )                             !3 points
        uc(i)=1.0D+00/2*(1.0D+00/6*(u(x(i))+u(x(i)+dx))+ &
         & 5.0D+00/6*(u(x(i)+0.5D+00*(1.0D+00-sqrt(5.0D+00)/5)*dx)+u(x(i)+0.5D+00*(1.0D+00+sqrt(5.0D+00)/5)*dx))  )   !4 points
!         uc(i)=1.0D+00/2*( 1.0D+00/10*(u(x(i))+u(x(i)+dx))+ 3.2D+01/45*u(x(i)+0.5D+00*dx)+ &
!         & 4.9D+01/90*(u(x(i)+0.5D+00*(1.0D+00-sqrt(3.0D+00/7))*dx)+u(x(i)+0.5D+00*(1.0D+00+sqrt(3.0D+00/7))*dx))  ) !5 points
      end do
      return
      end subroutine init


       
      subroutine sav(uc,x,nx,tend,dx)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      double precision,external::u
      double precision::ex,dx,x1,x2,x0,ul,ur,tend,erro,e1=0,e8=0,e=0
      integer::i,nx
      double precision,dimension(-2:nx+3)::uc,x
      do i=0,nx-1
         ex=(-cos(x(i+1))+cos(x(i)))/dx
         erro=abs(uc(i)-ex)
         e1=e1+erro*dx
         e8=max(e8,erro)
       enddo
      write(*,*) nx,e1,e8!,e
      write(1,*) nx,e1,e8!,e
      if(nx<=320)then
!      do i=1,nx
!      write(nx,*)x(i),uc(i)
!      end do
      end if
      return
      end subroutine sav
    

      double precision function f(x)
      double precision::x
      f=0.5d0*x**2
      return
      end function f
      

      double precision function u(x)
      double precision::x
      u=sin(x)
      end function u
 
