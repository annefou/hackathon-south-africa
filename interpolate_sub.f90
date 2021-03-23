 PROGRAM INTER_SUB
   IMPLICIT NONE
   INTEGER, PARAMETER :: NSTAT=5 ! number of stations
   INTEGER, PARAMETER :: WRF_NLONS=147 ! number of grid points in x direction
   INTEGER, PARAMETER :: WRF_NLATS=159 ! number of grid points in y direction
   ! Each of the variables declared below has a value for each of the five stations
   REAL :: LON(NSTAT)
   REAL :: LAT(NSTAT)
   REAL :: TMIN_STAT(NSTAT) ! not used really in this programme
   REAL :: TMAX_STAT(NSTAT) ! not used really in this programme
   REAL :: TMIN_WRF(NSTAT)
   REAL :: TMAX_WRF(NSTAT)
   ! Each of the variables declared below are associated with 2-D model data
   REAL :: WRF_LON(WRF_NLONS)
   REAL :: WRF_LAT(WRF_NLATS)
   REAL :: TMIN(WRF_NLONS,WRF_NLATS)
   REAL :: TMAX(WRF_NLONS,WRF_NLATS)
   ! Integers used for looping
   INTEGER :: I
   INTEGER :: J
   INTEGER :: K
   ! Date variables, they will be read in and are not really used
   INTEGER :: YR
   INTEGER :: MON
   INTEGER :: DY
   CHARACTER :: STID*8

   !*********************************************************************
   ! READ in WRF Latitude and longitude data
   !*********************************************************************
     WRF_LON(1)=24.57633
     DO I=2,WRF_NLONS
       WRF_LON(I)=WRF_LON(I-1)+0.09348
     ENDDO

     WRF_LAT(1)=-35.03672
     DO I=2,WRF_NLATS
       WRF_LAT(I)=WRF_LAT(I-1)+0.08727
     ENDDO

   PRINT*,WRF_LON(1:10), WRF_LAT(1:10)
   !*********************************************************************
   ! Read in WRF Tmin and Tmax data
   ! Generated first with the max and min function in grads
   !*********************************************************************
    OPEN(9,FILE="WRF_TMIN-TMAX.dat",ACCESS='direct',&
            recl=WRF_NLONS*WRF_NLATS*4)
      READ(9,rec=1)((TMAX(I,J),I=1,WRF_NLONS),J=1,WRF_NLATS)
      READ(9,rec=2)((TMIN(I,J),I=1,WRF_NLONS),J=1,WRF_NLATS)
    close(9)

    PRINT*,WRF_LON(1), WRF_LAT(1),TMAX(1,1),TMIN(1,1)
    PRINT*,WRF_LON(1), WRF_LAT(2),TMAX(1,2),TMIN(1,2)
    PRINT*,WRF_LON(2), WRF_LAT(1),TMAX(2,1),TMIN(2,1)

   !*********************************************************************
   ! Read in the station longitude and latitude
   !*********************************************************************
    OPEN(12,FILE="MaxTemp.txt")
      DO K=1,NSTAT
        READ(12,*) STID, LON(K), LAT(K), YR, MON, DY, TMAX_STAT(K)
   !*********************************************************************
   ! Determine I and J in model data close to the station
   !*********************************************************************
        I=0
100     I=I+1
        IF(WRF_LON(I).LT.LON(K))GO TO 100
        J=0
101     J=J+1
        IF(WRF_LAT(J).LT.LAT(K))GO TO 101
    !******************************************************************
    ! Now do bilinear interpolation based on 4 grid points
    !******************************************************************
        CALL INTERP(I,J,K,TMAX(I-1:I,J-1:J),WRF_LON(I-1:I),WRF_LAT(J-1:J),LON(K),LAT(K),TMAX_WRF(K))
        PRINT*,"MAX", LON(K), LAT(K), TMAX_STAT(K), TMAX_WRF(K)
      ENDDO
    CLOSE(12)

    !******************************************************************
    ! Now do bilinear interpolation based on 4 grid points
    !******************************************************************
    OPEN(12,FILE="MinTemp.txt")
      DO K=1,NSTAT
        READ(12,*) STID, LON(K), LAT(K), YR, MON, DY, TMIN_STAT(K)
        I=0
102     I=I+1
        IF(WRF_LON(I).LT.LON(K))GO TO 102
        J=0
103     J=J+1
        IF(WRF_LAT(J).LT.LAT(K))GO TO 103
    !******************************************************************
    ! Now do bilinear interpolation based on 4 grid points
    !******************************************************************
        CALL INTERP(I,J,K,TMIN(I-1:I,J-1:J),WRF_LON(I-1:I),WRF_LAT(J-1:J),LON(K),LAT(K),TMIN_WRF(K))
        PRINT*,"MIN", LON(K), LAT(K), TMIN_STAT(K), TMIN_WRF(K)
      ENDDO
    CLOSE(12)

    CONTAINS

   SUBROUTINE INTERP(I,J,K,TEMP,LONS,LATS,LONSTAT,LATSTAT,NEW)
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: I
     INTEGER, INTENT(IN) :: J
     INTEGER, INTENT(IN) :: K
     REAL, INTENT(IN) :: TEMP(I-1:I,J-1:J)
     REAL, INTENT(IN) :: LONS(I-1:I)
     REAL, INTENT(IN) :: LATS(J-1:J)
     REAL, INTENT(IN) :: LONSTAT(K:K)
     REAL, INTENT(IN) :: LATSTAT(K:K)
     REAL, INTENT(OUT) :: NEW(K:K)
!    REAL, INTENT(INOUT) :: LATSSTAT
     REAL :: VAR1
     REAL :: VAR2
       
       VAR1=(LONS(I)-LONSTAT(K))/(LONS(I)-LONS(I-1))*TEMP(I-1,J-1)+&
            (LONSTAT(K)-LONS(I-1))/(LONS(I)-LONS(I-1))*TEMP(I,J-1)
       VAR2=(LONS(I)-LONSTAT(K))/(LONS(I)-LONS(I-1))*TEMP(I-1,J)+&
            (LONSTAT(K)-LONS(I-1))/(LONS(I)-LONS(I-1))*TEMP(I-1,J)
       NEW(K)=(LATS(J)-LATSTAT(K))/(LATS(J)-LATS(J-1))*VAR1+&
            (LATSTAT(K)-LATS(J-1))/(LATS(J)-LATS(J-1))*VAR2

    END SUBROUTINE INTERP

   END PROGRAM INTER_SUB
