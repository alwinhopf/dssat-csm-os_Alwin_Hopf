C=======================================================================
!  Quality Subroutine based on RStages.for which is modified from
C      STAGES Subroutine, J. W. Jones
C  Output of fruit cohort data.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/01/2019 Written by AH, KB
C  03/22/2020 included "Multihar" to have harvest indicator in output file
C-----------------------------------------------------------------------
!     Called from: PODS
!     Calls:       None
C=======================================================================


      SUBROUTINE COHORT(DAS,YRDOY,YRPLT,PAGE,NAGE,WTSD,WTSHE,NPP,
     & NR2TIM,SDNO,SHELN,LNGPEG,EO,EOP,TMIN,TMAX)

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      !Alwin Hopf - for cohort output
      !multiharvest
      USE Multihar
      !Alwin Hopf - end
      USE CsvOutput !for csv output
      IMPLICIT NONE
      SAVE


      INTEGER DYNAMIC, I, J, LAST_DAY, HARVESTED, RAPID_GROWTH
      INTEGER DAS, YRDOY, YRPLT, DAP, DOY, YEAR, TIMDIF, NPP, NR2TIM, NAGE
      REAL PAGE, WTSD, WTSHE
      REAL SDNO, SHELN
      !Alwin Hopf - new variable. Evapotranspiration
      REAL LNGPEG
      REAL EO,EOP,TMIN,TMAX

      !output array
      !seed numer, shell numer, page, 
      !define as empty in initial run
      !REAL XWTSD(I,J), XWTSHE(I,J), XSDNO(I,J), XSHELN(I,J), XPAGE(I,J)


!     For output file:
      CHARACTER*11 Filename
      INTEGER ERRNUM, LUN, RUN
      LOGICAL FEXIST, FOPEN

      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      DAS     = CONTROL % DAS

!     Output file:
      Filename = 'COHORT.OUT'
      CALL GETLUN('Filename', LUN)

      INQUIRE (FILE = Filename, EXIST = FEXIST)
      INQUIRE (FILE = Filename, OPENED = FOPEN)
      IF (FEXIST) THEN
        IF (.NOT. FOPEN) THEN
          OPEN (UNIT = LUN, FILE = Filename, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          INQUIRE (FILE = Filename, NUMBER = LUN)
        ENDIF
      ELSE
        OPEN (UNIT = LUN, FILE = Filename, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        !WRITE(LUN,'("*Filename OUTPUT FILE")')
      ENDIF

      !For sequenced run, use replicate
      ! number instead of run number in header.
 !     CALL HEADER(SEASINIT, LUN, RUN)


       DAP   = MAX(0,TIMDIF(YRPLT,YRDOY))
       CALL YR_DOY(YRDOY, YEAR, DOY) 
!       DAS = 1


       !@YEAR     YEAR
       !DOY       Day of Year
       !DAP       Day after planting
       !PAGE      Photothermal age of each cohort (Photo-thermal days)
       !WTSD(J)   Seed mass  for cohort J (g/m2)
       !WTSHE(J)  Shell mass  for cohort J (g/m2)
       !DAS       Day after start of simulation
       !NPP       Cohort number used as index in loops 
       !

       !new Alwin Hopf - indicator for last day of fruit cohort.
       !cohorttracking
       !if Last_Day = 1, means that next day this fruit will be gone 
      If ((page >= xmpage).AND.(HARV_AH==1).AND.(WTSD>0)) THEN
            LAST_DAY = 1
      Else
            LAST_DAY = 0
      End IF

      !If ((page >= xmpage).AND.(HARV==1).AND.(WTSD==0))then
      If ((page >= xmpage).AND.(WTSD==0)) THEN
            HARVESTED = 1            !HARVESTED = DAP 
      Else
            HARVESTED = 0
      End IF

      If ((page >= LNGPEG)) THEN
            RAPID_GROWTH = 1            !HARVESTED = DAP 
      Else
            RAPID_GROWTH = 0
      End IF
      !new Alwin Hopf - end
      
      IF (NR2TIM .EQ. 1) THEN
      WRITE (LUN,120)
  120 FORMAT('YEAR  DOY  DAS  DAP  NPP  PAGE  NAGE  WTSD  WTSHE  SDNO  SHELN  NR2TIM HARV_AH HARVESTED LAST_DAY')
      ENDIF

       !
       !XWTSHE[NPP,DAP] = WTSHE

       !create an array instead of wrtiting. two dimensional for day and npp(cohort)
       !double-loop, write npp and j
!             WRITE (LUN,120)
!  120 FORMAT('@YEAR  DOY  DAP  NPP  PAGE   WTSD   WTSHE    DAS')

      WRITE (LUN,300) YEAR, DOY, DAS, DAP, NPP, PAGE, NAGE, WTSD, WTSHE,
     &             SDNO, SHELN, NR2TIM, HARV_AH, HARVESTED, Last_Day
  300 FORMAT(
     &I4,2X, !YEAR
     &I3.2,  !DOY 
     &I5,  !DAS
     &I5, !DAP
     &I4,1X, !NPP
     &F6.2,X, !PAGE
     &I4,1X, !NAGE
     &F6.3,1X, !WTSD
     &F6.3,1X, !WTSHE
     &F6.3,1X, !SDNO
     &F6.3,1X, !SHELN
     &I4,1X, !NR2TIM
     &I4,4X, !HARV_AH
     &I4,6X, !HARVESTED
     &I4) !Last_Day
   

      !    Alwin Hopf - CSV output corresponding to Cohort.OUT
      IF (FMOPT == 'C') THEN    
      CALL CsvOut_Cohort(YEAR, DOY, DAP, NPP, PAGE, NAGE, WTSD, WTSHE, 
     &SDNO, SHELN, NR2TIM, HARV_AH, HARVESTED, Last_Day, RAPID_GROWTH,
     &EO, EOP, vCsvlineCohort, vpCsvlineCohort, vlngthCohort)
 
      CALL LinklstCohort(vCsvlineCohort)
      END IF
      !Alwin Hopf - end

      RETURN


      
      END SUBROUTINE COHORT

!------------------------------------------------------------------------
!     COHORT Variables:
!------------------------------------------------------------------------
! DAS       Days after start of simulation (days)
! FNSTR(I)  Nitrogen stress function (0 to 1) for phase I 
! FPSTR(I)  Phosphorus stress function (0 to 1) for phase I 
! FSW(I)    Water stress function (0.0 to 1.0) for phase I 
! FT(I)     Temperature function (0-1) for phase I 
! FUDAY(I)  Effect of daylength on development progress (0-1) for phase I 
! ISIMI     Start of simulation code:     E = On reported emergence day, I 
!             = When initial conditions measured, P = On reported planting 
!             date, S = On specified date 
! JPEND     Day when juvenile phase ends and plants first become sensitive 
!             to photoperiod (days)
! NDLEAF    Day when leaf expansion ceased (days)
! NDSET     Normal time by which a pod load (full number) should be 
!             achieved with no water stress (days)
! NDVST     Day on which last main stem node formed (days)
! NPRIOR(I) The phase of growth at which phase I accumulator can start 
! NR0       Day when floral induction occurs (days)
! NR1       Day when 50% of plants have at least one flower (days)
! NR2       Day when 50% of plants have one peg (peanuts only) (days)
! NR3       Day when 50% of plants have at least one pod (days)
! NR5       Day when 50% of plants have pods with beginning seeds (days)
! NR7       Day when 50% of plants first have yellowing or maturing pods
!             (days)
! NVALP0    Set to 100,000 in PHENOLOG, used for comparison of times of 
!             plant stages  (days)
! NVALPH(I) Day when stage (I) occurred. (days)
! NVEG0     Day of emergence (days)
! NVEG1     1st day with 50% of plants w/ completely unrolled leaf at 
!             unifoliate node (days)
! PHTEM     Threshold time for emergence (thermal days)
! PHTHRS(I) Threshold time that must accumulate in phase I for the next 
!             stage to occur  (thermal or photothermal days)
! PHZACC(I) Cumulative. time of progression from the start of phase I
!             (thermal or photothermal days)
! PLME      Planting method; T = transplant, S = seed, P = pre-germinated 
!             seed, N = nursery 
! PROG (I)  Thermal or photo-thermal time that occurs in a real day for 
!             Phase I (Thermal or photothermal days)
! REM (I)   Remainder of thermal or photo-thermal time after a threshold is 
!             reached on a day (to be used to start progression into the 
!             next phase) (thermal or photothermal days / day)
! RSTAGE    Number of RSTAGES which have occurred. 
! SDEPTH    Planting depth (cm)
! STGDOY(I) Day when stage I occurred (YYDDD)
! TIMDIF    Integer function which calculates the number of days between 
!             two Julian dates (da)
! YRDOY     Current day of simulation (YYDDD)
! YREMRG    Day of emergence (YYDDD)
! YRNR1     Day when 50% of plants have at least one flower (YYDDD)
! YRNR2     Day when 50% of plants have one peg (peanuts only) (YYDDD)
! YRNR3     Day when 50% of plants have at least one beginning pod (YYDDD)
! YRNR5     Day when 50% of plants have pods with beginning seeds (YYDDD)
! YRNR7     Day when 50% of plants first have yellowing or maturing pods
!             (YYDDD)
! MDATE     Date of harvest maturity (YYDDD)
! YRPLT     Planting date (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!     End Subroutine COHORT
!-----------------------------------------------------------------------
