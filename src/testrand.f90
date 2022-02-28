  program testrand
    USE RNG_MT
    USE MyNumbers
    
    INTEGER(IKIND) ind, irand
    REAL(RKIND) drand

    DO ind=1,1000000,1
       
       CALL SRANDOM(ind)

       irand= IRANDOM( ind )
       drand= DRANDOM( ind )

       PRINT*, ind, irand, drand
    END DO

  end program testrand
