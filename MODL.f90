module modl
    implicit double precision (a-h,o-z)
    integer::i,nx,len,nt=0
    integer,allocatable::wge(:)
    double precision,allocatable::data_e1(:),data_e8(:),data_e(:),order_e1(:),order_e8(:),order_e(:)
    double precision,allocatable::uc(:),u1(:),u2(:),u3(:),x(:),flux(:)
    double precision::t=0,dt,dx,x0,tend,e=0,em=0,cdx,cfl=0.1,pi=4.*atan(1.)        
    character*8::arg
    
 contains 
    subroutine setup()
     
     open(unit=1,file='./data/error.dat', position="append")  
     call getarg(1,arg)
     read(arg,*) nx            ! convert string to integer
     call getarg(2,arg)
     read(arg,*) tend
     call getarg(3,arg)
     read(arg,*) len
     allocate(uc(-2:nx+3))
     allocate(u1(-2:nx+3))
     allocate(u2(-2:nx+3))
     allocate(u3(-2:nx+3))
     allocate(x(-2:nx+3))
     allocate(flux(0:nx))
     write(*,*)'grid number= ',nx
     write(*,*)'final time = ',tend
     return
    end subroutine setup
    
    subroutine order()
     allocate(wge(1:len))
     allocate(data_e1(1:len))
     allocate(data_e8(1:len))
     allocate(data_e(1:len))
     allocate(order_e1(2:len))
     allocate(order_e8(2:len))
     allocate(order_e(2:len))
     close(1)
     open(1, file='./data/error.dat') 
     
     do i=1,len
      read (1,*) wge(i),data_e1(i), data_e8(i),data_e(i)
     end do
     close(1)
     order_e1(2:len)=log(data_e1(1:len-1)/data_e1(2:len))/log(2.)
     order_e8(2:len)=log(data_e8(1:len-1)/data_e8(2:len))/log(2.)
     order_e(2:len)=log(data_e(1:len-1)/data_e(2:len))/log(2.)
     
     do i=2,len
     write (*, *) wge(i),order_e1(i), order_e8(i)!, order_e(i)
        end do
    end subroutine order
end module modl
