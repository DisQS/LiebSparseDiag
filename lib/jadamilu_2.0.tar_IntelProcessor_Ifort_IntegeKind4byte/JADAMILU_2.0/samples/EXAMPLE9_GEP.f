      PROGRAM EXAMPLE9_GEP
*     simple program to illustrate basic usage of PJDREVCOM_GEP with
*     diagonal preconditioning
      IMPLICIT NONE
*     needed parameters: 
*              N: size of the problem
*         MAXEIG: max. number of wanteg eig (NEIG<=MAXEIG)
*          MAXSP: max. value of MADSPACE
      INTEGER    N, MAXEIG, MAXSP
      PARAMETER (N=1000,MAXEIG=5,MAXSP=20)
*     optimal workspace (here MAXSP*MAXSP>MAXEIG)
      INTEGER LX
      PARAMETER (LX=N*(4*MAXSP+2*MAXEIG+1)+4*MAXSP*MAXSP)
      DOUBLE PRECISION   EIGS(MAXEIG), RES(MAXEIG), X(LX), D(N), DB(N)
*     arguments to pass to the routines
      INTEGER            NEIG, MADSPACE, ISEARCH, NINIT, ICNTL(5)
      INTEGER            ITER, IPRINT, INFO
      DOUBLE PRECISION   SIGMA, TOL, GAP, MEM, DROPTOL, SHIFT
      INTEGER            IJOB, NDX1, NDX2, NDX3

*     some local variables
      INTEGER I,J,K
*     let us define a very simple matrix:
*
*              1     5      0      ...
*              5     2      5       0     ...
*         A =  0     5      3       5      0     ...
*              0     0      5       4      5      0     ...
*             ...   ...   
*
*      and a very simple mass matrix
*
*              1    0.25     0      ...
*            0.25     1    0.25      0     ...
*         B =  0    0.25     1     0.25      0     ...
*              0     0     0.25      1     0.25      0     ...
*             ...   ...   
*
*     we use the storage scheme needed by PJD
*     (only the upper triangular part is referenced)
      DOUBLE PRECISION A(2*N), B(2*N)
      INTEGER IA(N+1), JA(2*N), IB(N+1), JB(2*N)
      K=1
      DO I=1,N
         IA(I)=K
         JA(K)=I
         A(K)=DBLE(I)
           IB(I)=K
           JB(K)=I
           B(K)=1.0d0
         K=K+1
         IF (I .LT. N) THEN
            JA(K)=I+1
            A(K)=5.0D0
              JB(K)=I+1
              B(K)=0.25D0
            K=K+1
         END IF
      END DO
      IA(N+1)=K
      IB(N+1)=K
*
*
      PRINT *, 
     $'================================================================'
      PRINT *, 
     $'Computing the smallest eigenvalues with diagonal preconditioning'
      PRINT *, 
     $'----------------------------------------------------------------'
*
c     use a copy of the diagonal entries to be passed to PJDREVCOM
      DO I=1,N
         D(I)=0.0
         DO J=IA(I),IA(I+1)-1
            K=JA(J)
            IF (K.EQ.I) D(I)=A(J)
            K=JB(J)
            IF (K.EQ.I) DB(I)=B(J)
         END DO
      END DO
c     set input variables
c     the matrix is already in the required format
c   
c     standard report on standard output
      IPRINT=6
c
c     we want the smallest eigenvalues
      ISEARCH=0
c     number of wanted eigenvalues
      NEIG=maxeig
c     no initial approximate eigenvectors
      NINIT=0
c     desired size of the search space
      MADSPACE=maxsp
c     maximum number of iteration steps
      ITER=1000
c     tolerance for the eigenvector residual
      TOL=1.0d-10
c     additional parameters set to default
      ICNTL(1)=0
      ICNTL(2)=0
      ICNTL(3)=0
      ICNTL(4)=0
      ICNTL(5)=0

      IJOB=0
 10   CALL DPJDREVCOM_GEP( N,D,-1,-1,DB,-1,-1,EIGS, RES, X, LX, NEIG, 
     $                 SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $                 SHIFT, DROPTOL, MEM, ICNTL,
     $                 IJOB, NDX1, NDX2,NDX3, IPRINT, INFO, GAP)
c
      IF (IJOB.eq.1 .OR. IJOB.eq.3 .OR. IJOB.eq.4) THEN
c     your private matrix-vector multiplications
         IF (IJOB.eq.1 .OR. IJOB.eq.3) 
c        Matrix A: X(NDX1) input,  X(NDX2) output
     $    call matvec(N,IA,JA,A,X(NDX1),X(NDX2))
         IF (IJOB.eq.1 .OR. IJOB.eq.4) 
c        Matrix B: X(NDX1) input,  X(NDX3) output
     $    call matvec(N,IB,JA,B,X(NDX1),X(NDX3))
         GOTO 10
      END IF
c
c     release internal memory and discard preconditioner
      CALL DPJDCLEANUP
c
      END


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Matrix-Vector Multiplication
c     (This routine should work for any matrix a, ja, ia compliant to 
c     the format required by PJD/PJDREVCOM) 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine matvec(n,ia,ja,a,x,y) 
      integer n, ia(n+1), ja(*)
      double precision a(*),x(n),y(n)

      integer i,k,j

      do i=1,n
         y(i)=0.0
      end do

      do i=1,n
         do k=ia(i),ia(i+1)-1
           j=ja(k)
           if (i.eq.j) then
             y(i)=y(i)+a(k)*x(i)
           else
             y(i)=y(i)+a(k)*x(j)
             y(j)=y(j)+a(k)*x(i)
           end if
         end do
      end do
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
