!-----------------------------------------------------------
!
!     LiebJADdiag
!
!     program uses JADAMILU package of Bollhoefer           
!                  roemer's makeanderson matrix             
!                           converted from base 0 to base 1 
! ----------------------------------------------------------

PROGRAM LiebJADdia

  USE MyNumbers  
  USE CConstants
  USE IConstants
  USE IPara
  USE DPara
  USE IChannels
  USE RNG_MT
  USE mt95
  USE SETBINS
  !USE RConstants					   
  !USE EigenPara
  !USE IEigenChannels
  !USE SimPara

  IMPLICIT NONE

  ! ----------------------------------------------------------
  ! variable declaration
  ! ----------------------------------------------------------

  ! Parameters for Lieb matrix
  INTEGER(KIND=IKIND) IWidth, ISSeed(5)
  
  INTEGER(KIND=IKIND) i, j, k, LSize, CSize, LARGE
       
  ! arguments to pass to the JD routine
  INTEGER(KIND=IKIND) &
       NEIG, MADSPACE, ISEARCH, NINIT, ICNTL(5), &
       ITER, IPRINT, INFO, IJOB, NDX1, NDX2, IErr, sumIErr, maxsp, &
       VECS_size       ! optimal workspace (N.B.: here, maxsp*maxsp>maxeig)

  PARAMETER (maxsp=20, LARGE=2147483647)

  REAL(KIND=RKIND) &
       SIGMA, TOL, DELTA, SHIFT, GAP, MEM, DROPTOL, drandval

  ! output arguments from the Lieb routine
  INTEGER(KIND=IKIND) nz
  
  REAL(KIND=RKIND), DIMENSION(:), ALLOCATABLE:: &
       ao

  INTEGER(KIND=IKIND), DIMENSION(:), ALLOCATABLE:: &
       iao,jao

  ! arguments from the JD routine
  REAL(KIND=RKIND), DIMENSION(:), ALLOCATABLE:: &
       EIGS, & !computed eigenvalues
       RES,&   !computed residues: |Ax-EIGS x|
       VECS, &    !computed eigenvectors
       a,a_w
  
  INTEGER(KIND=IKIND), DIMENSION(:), ALLOCATABLE:: &
       ia, ja
  
  ! some local variables
  INTEGER(KIND=IKIND)  i1, i2, i3, Inum
  
  REAL(KIND=RKIND) SugTarE
  CHARACTER*100 str
  
  !  REAL(KIND=RKIND), DIMENSION(:,:), ALLOCATABLE::mat
  
  !  Character*68 Matrixname

!!$  ! Setting Target Energy
!!$
!!$  INTEGER(KIND=IKIND) L
!!$
!!$  INTEGER(KIND=IKIND), Dimension(:,:), ALLOCATABLE:: TarStore
  
  ! ----------------------------------------------------------
  ! start of main code
  ! ----------------------------------------------------------
  
  ! ----------------------------------------------------------
  ! protocol feature via git
  ! set: git tag -a v0.0 -m 'Version 0.0'
  ! ----------------------------------------------------------
#ifdef git
  PRINT*,"LiebSparseDiag (", TRIM("GITVERSION"), ")"
#else
  PRINT*,"LiebSparseDiag()"
#endif
  
  ! ----------------------------------------------------------
  ! inout handling
  ! ----------------------------------------------------------

  PRINT*,"main: calling Input()"
  CALL  Input(IErr)
  IF(IErr.NE.0) THEN
     PRINT*,"main: Input() finds IErr=", IErr
     STOP
  ENDIF
  
  ! ----------------------------------------------------------
  ! start of main IWidth loop
  ! ----------------------------------------------------------
  
  DO IWidth= Width0, Width1, dWidth
  
       PRINT*,"main: IWidth=", IWidth

     ! ----------------------------------------------------------
     IF(IWriteFlag.GE.0) THEN
        PRINT*, "START@ IWidth=", IWidth
     ENDIF
     
     ! ----------------------------------------------------------
     ! defining the size of arrays
     ! ----------------------------------------------------------
     
     LSize     = (Dim*Nx+1)*(IWidth**Dim)
     VECS_size = Lsize*(3*maxsp+NEVals+1)+4*maxsp*maxsp
     CSize     = (2*Dim*Nx+Dim+1)*(IWidth**Dim) 
     
     IF(IWriteFlag.GE.2) THEN
        PRINT*,"main: cube is ",Lsize," by ",Lsize
        PRINT*,"main: VEC size = ",VECS_size
     ENDIF
     
     ! ----------------------------------------------------------
     ! ALLOCATing memory
     ! ----------------------------------------------------------
     
     sumIErr= 0
     ALLOCATE(EIGS(NEVals), STAT=IErr); sumIErr=sumIErr+IErr
     ALLOCATE(RES(NEVals), STAT=IErr); sumIErr=sumIErr+IErr
     ALLOCATE(VECS(VECS_size), STAT=IErr); sumIErr=sumIErr+IErr
     ALLOCATE(ao(0:CSize), STAT=IErr); sumIErr=sumIErr+IErr
     ALLOCATE(iao(0:LSize), STAT=IErr); sumIErr=sumIErr+IErr
     ALLOCATE(jao(0:CSize), STAT=IErr); sumIErr=sumIErr+IErr
     ALLOCATE(a(CSize), STAT=IErr); sumIErr=sumIErr+IErr
     ALLOCATE(a_w(CSize), STAT=IErr); sumIErr=sumIErr+IErr
     ALLOCATE(ia(LSize+1), STAT=IErr); sumIErr=sumIErr+IErr
     ALLOCATE(ja(CSize), STAT=IErr); sumIErr=sumIErr+IErr
     
     IF(IErr.NE.0) THEN
        PRINT*,"main: Input() finds sumIErr=", sumIErr
        STOP
     ENDIF
     
     ! ----------------------------------------------------------
     ! making the Lieb matrix
     ! ----------------------------------------------------------

     PRINT*,"main: calling MakeCompactRowLiebMat()"
     CALL MakeCompactRowLiebMat(Dim, Nx, IWidth, LSize, CSize, iao, jao, ao, nz )
     !output NZ = # of nonzero and diagonal elements
     !output ia must have size of N+1

     IF(IWriteFlag.GE.4) PRINT*,&
          "main: There are",nz,"nonzero elements of array a()"

     ! ----------------------------------------------------------
     ! converting arrays from base 0 to base 1
     ! ----------------------------------------------------------
     
     DO i=1,nz
        ja(i)=jao(i-1)
!!$           IF(IWriteFlag.GE.4) PRINT*,i,ja(i)
     ENDDO
     
     DO i=1,nz
        a(i)=ao(i-1)
!!$           IF(IWriteFlag.GE.4) PRINT*,i,a(i)
     ENDDO
     
     DO i=1,Lsize+1
        ia(i)=iao(i-1)
!!$           IF(IWriteFlag.GE.4) PRINT*,i,ia(i)
     ENDDO
     
     ! -----------------------------------------------------------------
     ! start of CubeDis loop, not finished yet, just keep CubeDis0= CubeDis1
     ! -----------------------------------------------------------------
       
     DO CubeDis= CubeDis0, CubeDis1 + dCubeDis/2.0, dCubeDis
    
        PRINT*,"main: CubeDis=", CubeDis

        ! ----------------------------------------------------------
        ! start of Energy loop
        ! ----------------------------------------------------------
        
        DO Energy= Energy0, Energy1 + dEnergy/2.0, dEnergy
           
           PRINT*,"main: Energy=", Energy

           CALL GetDirec(Dim, Nx, IWidth, CubeDis, LiebDis, CubeConPot, LiebConPot, Energy, str)
           
           ! ----------------------------------------------------------
           ! start of ISeed loop
           ! ----------------------------------------------------------
           
           DO Seed= ISeed, ISeed+ NConfig -1
              
              ! ----------------------------------------------------------
              ! Compute actual seed
              ! ----------------------------------------------------------

              ISSeed(1)= Seed
              ISSeed(2)= IWidth
              ISSeed(3)= NINT(Energy*1000.)
              ISSeed(4)= NINT(CubeDis*1000.)
              ISSeed(5)= NINT(LiebDis*1000.)

!              CALL genrand_int31(ISSeed) ! MT95 with 5 seeds

              SELECT CASE(IWriteFlag)
              CASE(1,2)
                 PRINT*, "-- Seed=", Seed
                 PRINT*, "-> ISSeed=", ISSeed
              CASE(3,4)
!!$                 PRINT*, "IS: IW=", IWidth, "hD=", NINT(CubeDis*1000.), "E=", NINT(Energy*1000.), &
!!$                      "S=", Seed, "IS=", ISSeed
                 CALL genrand_int31(ISSeed) ! MT95 with 5 seeds
                 CALL genrand_real1(drandval)
                 CALL SRANDOM5(ISSeed)
                 drandval=DRANDOM5(ISSeed)
                 WRITE(*, '(A7,I3,A4,F6.3,A4,F5.3,A16,F6.3,A16,F6.3,A3,F6.3,A3,I5,A4,F16.10)') &
                      "IS: IW=", IWidth, " hD=", CubeDis, " rD=", LiebDis, " CubeConPot=", CubeConPot, &
                      " LiebConPot=", LiebConPot, " E=", Energy, &
                      " S=", Seed, " R=", drandval
                 PRINT*, "ISSeed=", ISSeed
              CASE DEFAULT
                 PRINT*,"main: Seed=", Seed
              END SELECT              

              ! ----------------------------------------------------------
              ! CHECK if same exists and can be overwritten
              ! ----------------------------------------------------------

              SELECT CASE(IKeepFlag)
              CASE(1)
                 CALL CheckOutput( Dim,Nx, IWidth, Energy, CubeDis, LiebDis, CubeConPot, LiebConPot,&
                      Seed, str, IErr )
                 IF(IErr.EQ.2) CYCLE
              END SELECT
              
              !CALL genrand_int31(ISSeed) ! MT95 with 5 seeds, before: CALL SRANDOM(ISSeed
              CALL SRANDOM5(ISSeed) ! MT95 with 5 seeds, before: CALL SRANDOM(ISSeed)
              
              ! ----------------------------------------------------------
              ! ENTER random values into matrix
              ! ----------------------------------------------------------
              
              ! keep array a Lieb matrix form, for each disorder circle, only change the a_w
              a_w(:) = a(:) 
              
              ! Give the Lieb matrix different onsite potensial
              DO i=1, IWidth**Dim
                 
                 k= (i-1)*(Nx*Dim+1) + 1
                 !CALL genrand_real1(drandval)
                 drandval= DRANDOM5(ISSeed)
                 a_w(ia(k)) = CubeConPot + CubeDis*(drandval - 0.5D0)
                 
                 DO j=2, (Nx*Dim +1)
                    
                    k = (i-1)*(Nx*Dim+1) + j
                    !CALL genrand_real1(drandval)
                    drandval= DRANDOM5(ISSeed)
                    a_w(ia(k)) = LiebConPot + LiebDis*(drandval - 0.5D0)
                    
                 END DO
                 
              END DO
           
              !---------------------------------------------------------
              ! Transform the format of Sparse matrix to Full matrix
              !--------------------------------------------------------
              
!!$           mat(:,:)=0.0D0
!!$           DO i=1, LSize ! the horizontal of matrix
!!$
!!$              DO j=ia(i),ia(i+1)-1
!!$
!!$                 mat(i,ja(j)) = a_w(j)
!!$
!!$              END DO
!!$
!!$           END DO
!!$
!!$           ! Generate the file containing full matrix 
!!$
!!$           WRITE(Matrixname, '(A11,I1,I1,A2,I4.4,A6,I4,A4)') &
!!$                "SparseMat-L", Dim, Nx, "-M", IWidth,  &
!!$                "-Seed-", Seed, &
!!$                ".txt"
!!$
!!$           OPEN(UNIT= 10, FILE=Matrixname)   
!!$
!!$           DO i=1, LSize
!!$              DO j=1, LSize
!!$                 WRITE(10,102) mat(i,j)              
!!$              END DO
!!$              WRITE(10,*) " "
!!$           END DO
!!$
!!$102        FORMAT(1x,F15.6\)
!!$
!!$           CLOSE(10)

              ! ----------------------------------------------------------
              ! interface to the JADAMILU code
              ! ----------------------------------------------------------
              ! the matrix is already in the required format

              ! standard report on standard output
              IPRINT=6

              ! we want the eigenvalues near target sigma=0         
              ! (then, SHIFT need not to be set)       
              ISEARCH=2      
              SIGMA=Energy
                             
              ! elbow space factor for the fill computed during the ILU
              MEM=200.0 !Memory
              ! tolerence for discarded fill
              DROPTOL=1.d-3

              ! number of wanted eigenvalues
              NEIG=NEVals

              ! no initial approximate eigenvectors
              NINIT=0
              ! desired size of the search space
              MADSPACE=maxsp
              ! maximum number of iteration steps
              ITER=20000
              ! tolerance for the eigenvector residual
              TOL=1.0d-10
              
              ! additional parameters set to default
              ICNTL(1)=0
              ICNTL(2)=0    ! switch ON for ADAPTIVE precon =0
              ICNTL(3)=0
              ICNTL(4)=0
              ICNTL(5)=0

!!$              RES=0
!!$              VECS=0
!!$              EIGS=0

              ! ----------------------------------------------------------
              !  call to PJD that computes eigenvalues & eigenvectors
              ! ----------------------------------------------------------

              IF(IWriteFlag.GE.2) PRINT*,"main: calling PJD()"
              CALL PJD(Lsize, a_w, ja, ia, EIGS, RES, VECS, VECS_size, NEIG,&
                   SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,&
                   SHIFT, DROPTOL, MEM, ICNTL,&
                   IPRINT, INFO, GAP)

              ! We set a large NEvals to guarantee get all eigenvalues at a certain droptol
              ! When it get the number of eigenvalues is less than NEvals which we set,
              ! it will return the actually number INFO(which included one unconverged)
              IF(INFO.NE.0) THEN
                 NEIG= INFO - 1
              END IF

              !to be used if you want a new preconditioner in every iteration
              CALL PJDCLEANUP   

              ! ----------------------------------------------------------
              ! write results into files
              ! ---------------------------------------------------------
              
              IF(NEIG==0)THEN
                 PRINT*,"main: PJD() did not find any eigenvalues!"
              ELSE IF(NEIG.LT.0)THEN
                 PRINT*,"main: PJD() reported ERROR: #", NEIG
              ELSE
                 PRINT*,"main: PJD() found eigenvalues, these will now be saved into file"
                 CALL WriteOutputEVal( Dim, Nx, NEIG, EIGS, &
                      IWidth, Energy, CubeDis, LiebDis, CubeConPot, LiebConPot, Seed, str, IErr)
                 IF(IStateFlag.NE.0)THEN
                    PRINT*,"main: PJD() found eigenvectors, these will now be saved into file"
                    DO Inum= 1,NEIG
                       Call WriteOutputEVec(Dim, Nx, Inum, NEIG, Lsize, &
                            VECS, VECS_size, IWidth, Energy, CubeDis, & 
                            LiebDis, CubeConPot, LiebConPot, Seed, str, IErr)
                    END DO
                 END IF !IStateFlag IF
              END IF
              
           END DO !Seed loop
           
        END DO !Energy loop
        
     END DO !CubeDis loop
     
     ! ----------------------------------------------------------
     ! DEALLOCATE memory
     
     DEALLOCATE(EIGS,RES,VECS,ia,iao,ja,jao,a,a_w,ao,STAT=IErr)
     
     IF(IErr.NE.0) THEN
        PRINT*,"main: DEALLOCATE() finds IErr=", IErr
        !STOP
     ENDIF

  END DO ! Width loop
  
END PROGRAM LiebJADdia   !main program






