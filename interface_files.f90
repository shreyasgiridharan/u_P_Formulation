subroutine IntfUMAT_Sand(dt, props, dEps, Sig, Statv)
    !**********************************************************************
    !
    !   Function : 
    !
    !
    !**********************************************************************
     
    implicit none
        
    double precision, intent(in) :: dt, props(26), dEps(4)
    double precision, intent(inout) :: Sig(4), Statv(*)
    ! local variables
    character*80 C80dum
    double precision :: dtime,props16(16),dEps6(6),Sig6(6),Statv14(14), &
        Ddum,Ddum2(2),Ddum3(3),Ddum33(3,3),Ddum6(6),Ddum66(6,6)

    Ddum=0.d0; Ddum2=0.d0; Ddum3=0.d0; Ddum33=0.d0; Ddum6=0.d0
    Ddum66=0.d0

   props16(1:14)=props(11:24)
   props16(15)=props(26)
   props16(16)=props(25)
   
   props16(15) = 2200000.d0
            
   Sig6(1:2)=Sig(1:2); Sig6(4)=Sig(3); Sig6(3)=Sig(4)
   Sig6(5:6)=0.d0

   dEps6(1:2)=dEps(1:2);dEps6(4)=dEps(3);dEps6(3)=dEps(4)
   dEps6(5:6)=0.d0

   Statv14(1:4)=Statv(1:4)      ! delta tensor
   Statv14(5:6)=0.d0           ! delta tensor 3D
   Statv14(7)=Statv(5)          ! void ratio
   Statv14(8)=Statv(10)             ! not used, pore
   Statv14(9)=(Sig(1)+Sig(2)+Sig(3))/3.d0  ! mean stress
   Statv14(13)=Statv(8)                     ! time step

   dtime=dt
   call umat(Sig6,Statv14,Ddum66,Ddum,Ddum,Ddum,Ddum,Ddum6,Ddum6,&
       Ddum,Ddum6,dEps6,Ddum2,dtime,Ddum,Ddum,Ddum,Ddum,C80dum,&
       3,1,4,14,props16,16,Ddum3,Ddum33,1._8,Ddum,Ddum33,Ddum33,&
       1,1,0,0,0,0)

   Sig(1:2)=Sig6(1:2); Sig(4)=Sig6(3); Sig(3)=Sig6(4)
   Statv(1:4)=Statv14(1:4)! delta tensor
   Statv(5)=Statv14(7)    ! void ratio
   Statv(6)=Statv14(11)   ! mobilized friction angle
   Statv(7)=Statv14(12)   ! normalised length rho
   Statv(8)=Statv14(13)   ! time step
   Statv(9)=Statv14(10)   ! number of evaluation
   Statv(10) = Statv14(8)   !pore

    end subroutine IntfUMAT_Sand

    subroutine IntfUMAT_Clay(dt, props, dEps, Sig, Statv)
    !**********************************************************************
    !
    !   Function : 
    !
    !
    !**********************************************************************
     
    implicit none
        
    double precision, intent(in) :: dt, props(32), dEps(4)
    double precision, intent(inout) :: Sig(4), Statv(*)
    ! local variables
    character*80 C80dum
    double precision :: dtime,props22(22),dEps6(6),Sig6(6),Statv16(16), &
        Ddum,Ddum2(2),Ddum3(3),Ddum33(3,3),Ddum6(6),Ddum66(6,6)

    Ddum=0.d0; Ddum2=0.d0; Ddum3=0.d0; Ddum33=0.d0; Ddum6=0.d0
    Ddum66=0.d0

   props22(1:22)=props(11:32)
            
   Sig6(1:2)=Sig(1:2); Sig6(4)=Sig(3); Sig6(3)=Sig(4)
   Sig6(5:6)=0.d0

   dEps6(1:2)=dEps(1:2);dEps6(4)=dEps(3);dEps6(3)=dEps(4)
   dEps6(5:6)=0.d0

   Statv16(1:4) = Statv(1:4)      ! intergranular strain tensor
   Statv16(5:6) = 0.d0            ! delta tensor 3D
   Statv16(7)   = Statv(5)        ! void ratio
   Statv16(8)   = Statv(12)       ! inital excess pore pressure   
   Statv16(9)   = (Sig(1)+Sig(2)+Sig(3))/3.d0 ! Effective mean stress
   Statv16(13)  = Statv(8)          !dtsub time substep
   Statv16(14)  = Statv(13)         ! sensitivity
   dtime=dt
   
   call umat_hcea(Sig6,Statv16,Ddum66,Ddum,Ddum,Ddum,Ddum,Ddum6,Ddum6,&
       Ddum,Ddum6,dEps6,Ddum2,dtime,Ddum,Ddum,Ddum,Ddum,C80dum,&
       3,1,4,16,props22,22,Ddum3,Ddum33,1._8,Ddum,Ddum33,Ddum33,&
       1,1,0,0,0,0)

   Sig(1:2) = Sig6(1:2); Sig(4)=Sig6(3); Sig(3)=Sig6(4)
   Statv(1:4) = Statv16(1:4)! delta tensor
   Statv(5) = Statv16(7)    ! void ratio
   Statv(6) = Statv16(11)   ! mobilized friction angle
   Statv(7) = Statv16(12)   ! normalised length rho
   Statv(8) = Statv16(13)   ! time step
   Statv(9) = Statv16(10)   ! number of evaluation
   Statv(10) = Statv16(15)   ! ocr 
   Statv(11) = Statv16(16) ! boundary cutoff
   Statv(12) = Statv16(8)   ! excess pore pressure
   Statv(13) = Statv16(14)  ! sensitivity
   end subroutine IntfUMAT_Clay
    
