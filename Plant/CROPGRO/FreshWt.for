!=======================================================================
!  FreshWt, Subroutine, C.H.Porter, K.J.Boote, J.I.Lizaso
!-----------------------------------------------------------------------
!  Computes fresh pod weigt
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  05/09/2007     Written. KJB, CHP, JIL, RR
!  02/27/2008     Added pod quality for snap bean. JIL
!-----------------------------------------------------------------------
!  Called from:  PODS
!=======================================================================

      SUBROUTINE FreshWt(DYNAMIC, ISWFWT, 
     &    NR2TIM,       ! Days since NR2 (first peg) (d), input
     &    PHTIM,        ! Cumulative photothermal time ages of seeds and shells, input
     &    SDNO,         ! Number of seeds for cohort J (#/m2)
     &    SHELN,        ! Number of shells for cohort J (#/m2)
     &    WTSD,         ! Seed mass  for cohort J (g/m2)
     &    WTSHE,        ! Shell mass  for cohort J (g/m2)
     &    YRPLT)

!-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData

!      VSH
      Use MultiHar

!Alwin Hopf - Fresh Weight CSV Output
      USE CsvOutput
      
      IMPLICIT NONE
      SAVE

      CHARACTER*1   ISWFWT
      CHARACTER*2   CROP
      CHARACTER*7   ERRKEY
      PARAMETER (ERRKEY = 'FreshWt')
      CHARACTER*11, PARAMETER :: FWFile = "FreshWt.OUT"
      CHARACTER*16 CROPD
      CHARACTER*78 MSG(3)

      INTEGER DAP, DAS, DOY, DYNAMIC, ERRNUM, I
      INTEGER NOUTPF, NPP, NR2TIM, TIMDIF
      INTEGER YEAR, YRDOY, YRPLT

      REAL AvgDMC, AvgDPW, AvgFPW, PodDiam, PodLen
      REAL PAGE, PodAge, PODNO, SEEDNO, SHELPC
      REAL TDPW, TFPW, TDSW
      REAL CLASS(7)

      REAL, DIMENSION(NCOHORTS) :: DMC, DryPodWt, FreshPodWt, PHTIM
      REAL, DIMENSION(NCOHORTS) :: SDNO, SHELN, WTSD, WTSHE, XPAGE
      
      ! VSH
      !new variables for FreshWt.out file 
      Real :: TWTSH
      Real :: AvgRDSD, PRDSH, PRDSD, AvgRDSP, AvgRSNP
      Real :: HRSN, HRDSD, HRDSH 

      LOGICAL FEXIST

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      CALL GET(CONTROL)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CALL GET(ISWITCH)
      
!     Switch for fresh weight calculations
      IF (INDEX('Y',ISWFWT) < 1 .OR. 
     &    INDEX('N,0',ISWITCH%IDETL) > 0) RETURN

      CROP   = CONTROL % CROP

!     Currently only works for tomato.  Add other crops later. 
!     Send a message if not available crop
      !added strawberry (SR)
      IF (INDEX('TM,SR,GB',CROP) < 0) THEN
        CALL GET_CROPD(CROP, CROPD)
        WRITE(MSG(1),'(A)') 
     &  "Fresh weight calculations not currently available for "
        WRITE(MSG(2),'(A2,1X,A16)') CROP, CROPD
        CALL INFO(2,ERRKEY,MSG)
      ENDIF

!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      CALL GETLUN(FWFile, NOUTPF)
      INQUIRE (FILE= FWFile, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN(UNIT = NOUTPF, FILE = FWFile, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = NOUTPF, FILE = FWFile, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(NOUTPF,'("*Fresh Weight Output File")')
      ENDIF

      !Write headers
      CALL HEADER(SEASINIT, NOUTPF, CONTROL%RUN)

!     Change header to PWAD1 (was PWAD) because GBuild requires 
!     unique headers (PlantGro also lists PWAD).  Should have same
!     value, but slightly off. Why?

!     Need to look at how GBuild handles P#AD and SH%D here, too.

        SELECT CASE (CROP)
          CASE ('TM')       ! Tomato
            WRITE (NOUTPF,230)
          CASE ('SR')       ! Strawberry
            WRITE (NOUTPF,230)
          CASE ('GB')       ! Snap bean
            WRITE (NOUTPF,231)
        END SELECT
        
!       VSH
!  230 FORMAT('@YEAR DOY   DAS   DAP',
!     &    '   FPWAD   PDMCD   AFPWD',
!     &    '   ADPWD   PAGED')
! Alwin Hopf - previous .out output version
!  230 FORMAT('@YEAR DOY   DAS   DAP',
!     &    '   FPWAD   PDMCD   AFPWD',
!     &    '   ADPWD   PAGED   RTFPW    HARV   RTDPW   HARV_AH')
  230 FORMAT('@YEAR DOY   DAS   DAP',
     &    '   FPWAD   PDMCD   AFPWD',
     &    '   ADPWD   PAGED   HARV   HARV_AH   P#AD     G#AD   TWTSH    TDSW', 
     &    '    TDPW  RPODNO  RSEEDN   RTDSH   RTDSD   RTDPW',
     &    '   RUDPW   RTFPW      HRVF     CHRVF'   
     &    '    HRVD   CHRVD ARFPW ARDPW ARDSD ',
     &    'PRDSH PRDSD ARDSP ARSNP   HRSN   HRPN  HRDSD  HRDSH  NR2TIM')
     
  231 FORMAT('@YEAR DOY   DAS   DAP',
     &    '   FPWAD   PDMCD   AFPWD',
     &    '   ADPWD   PAGED',
     &    ' FCULD FSZ1D FSZ2D FSZ3D FSZ4D FSZ5D FSZ6D')
  
      AvgDMC = 0.0
      AvgDPW = 0.0
      AvgFPW = 0.0
      PodAge = 0.0
      PODNO  = 0.0
      SEEDNO = 0.0
      SHELPC = 0.0
      TDPW   = 0.0
      TFPW   = 0.0
      
!     VSH initialization at the begining
      TWTSH  = 0.0
      RTDSD   = 0.0 
      RTDSH   = 0.0
      RTFPW   = 0.0
      RTDPW   = 0.0
      RPODNO  = 0.0
      RSEEDNO = 0.0

      HRVD = 0.0
      HRVF = 0.0
      CHRVD = 0.0
      CHRVF = 0.0
      HRSN = 0.0
      HRPN = 0.0
      HRDSD = 0.0
      HRDSH = 0.0


            !alwin new
      !RTFPW =  0
      !RTDPW =   0
      !RTDSD =   0
      !RTDSH =   0

      !RPODNO =  0
      !RSEEDNO =  0
      !alwin new end 

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (INDEX('Y',ISWFWT) < 1 .OR. 
     &    INDEX('N,0',ISWITCH%IDETL) > 0) RETURN

!     Calculate number of pods, including those with and without seeds
      SEEDNO = 0.0
      PODNO  = 0.0
      TFPW   = 0.0
      TDPW   = 0.0
      TDSW   = 0.0

      !      VSH initialization at the begining
      TWTSH = 0.0
      RTDSD   = 0.0 
      RTDSH   = 0.0
      RTFPW   = 0.0
      RTDPW   = 0.0
      RPODNO  = 0.0
      RSEEDNO = 0.0
      DO I = 1, 7
        CLASS(I) = 0.0
      ENDDO
!-----------------------------------------------------------------------
      
      
      DO NPP = 1, NR2TIM + 1
        PAGE = PHTIM(NR2TIM + 1) - PHTIM(NPP)
        XPAGE(NPP) = PAGE

!       Dry matter concentration (fraction)
!       DMC(NPP) = (5. + 7.2 * EXP(-7.5 * PAGE / 40.)) / 100.
        SELECT CASE (CROP)
          CASE ('TM')       ! Tomato
            DMC(NPP) = (5. + 7.2 * EXP(-7.5 * PAGE / 40.)) / 100.
          CASE ('SR')       ! Strawberry
                        !DMC(NPP) = (5. + 7.2 * EXP(-7.5 * PAGE / 40.)) / 100.  !original
            !DMC(NPP) = (5. + 7.2 * EXP(-7.5 * PAGE / 40.)) / 34.  !changed Alwin Hopf. Ratio Dry:Fresh Weight was about 3 times too high
            DMC(NPP) = 0.16 !fixed value for Strwawberry. From Code from Ken Boote / VSH
          CASE ('GB')       ! Snap bean
!           DMC(NPP) = 0.0465 + 0.0116 * EXP(0.161 * PAGE)
            DMC(NPP) = 0.023 + 0.0277 * EXP(0.116 * PAGE)
        END SELECT

!       Fresh weight (g/pod)
        IF (SHELN(NPP) > 1.E-6) THEN
          FreshPodWt(NPP) = (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP) /
     &                          SHELN(NPP)  !g/pod
          DryPodWt(NPP) = (WTSD(NPP) + WTSHE(NPP))/SHELN(NPP) !g/pod
        ELSE
          FreshPodWt(NPP) = 0.0
        ENDIF

!       Snap bean quality
        IF (CROP .EQ. 'GB') THEN
          PodDiam = 8.991 *(1.0-EXP(-0.438*(FreshPodWt(NPP)+0.5)))  !(mm/pod)
          PodLen  = 14.24 *(1.0-EXP(-0.634*(FreshPodWt(NPP)+0.46))) !(cm/pod)

          IF (PodDiam .LT. 4.7625) THEN
!           Culls
            CLASS(7) = CLASS(7) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
          ELSEIF (PodDiam .LT. 5.7547) THEN
!           Sieve size 1
            CLASS(1) = CLASS(1) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP) 

            ELSEIF (PodDiam .LT. 7.3422) THEN
!           Sieve size 2
            CLASS(2) = CLASS(2) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP) 
          ELSEIF (PodDiam .LT. 8.3344) THEN
!           Sieve size 3
            CLASS(3) = CLASS(3) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
          ELSEIF (PodDiam .LT. 9.5250) THEN
!           Sieve size 4
            CLASS(4) = CLASS(4) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
          ELSEIF (PodDiam .LT. 10.7156) THEN
!           Sieve size 5
            CLASS(5) = CLASS(5) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
          ELSE
!           Sieve size 6
            CLASS(6) = CLASS(6) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
          ENDIF
        ENDIF

        TFPW = TFPW + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
        TDPW = TDPW + WTSD(NPP) + WTSHE(NPP)
        TDSW = TDSW + WTSD(NPP)

        ! VSH
        TWTSH = TWTSH + WTSHE(NPP)
        PODNO = PODNO + SHELN(NPP)
        SEEDNO = SEEDNO + SDNO(NPP)
        
        ! VSH accumulating in the basket for harvesting
        If (page >= xmpage) Then

        !AH = correction 2
        !PAGE = PHTIM(NR2TIM + 1) - PHTIM(NPP)
        !If ((page >= xmpage).AND.(HARV==1)) Then 

           RTFPW = RTFPW + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP) !fresh weight of mature fruits
           RTDPW = RTDPW + WTSD(NPP) + WTSHE(NPP) !dry weight of mature fruits (seed and shell)
           RTDSD = RTDSD + WTSD(NPP) !seed mass of mature fruits - wtsd = seed mass for cohort
           RTDSH = RTDSH + WTSHE(NPP) !shell mass of mature fruits - wtshe = shell mass for cohort

           RPODNO = RPODNO + SHELN(NPP)
           RSEEDNO = RSEEDNO + SDNO(NPP)      
   
        End if
        
        ! outputs
        ! Reset 5 vars to zero after print and initialization
        ! exect R values for cumulative
        ! plus average values
      
      ENDDO  ! NPP

      PodAge = XPAGE(1)
      IF (PODNO > 1.E-6) THEN
        AvgFPW = TFPW / PODNO
        AvgDPW = TDPW / PODNO
      ELSE
        AvgFPW = 0.0
        AvgDPW = 0.0
      ENDIF
      IF (TFPW > 1.E-6) THEN
        AvgDMC = TDPW / TFPW
      ELSE
        AvgDMC = 0.0
      ENDIF
      IF (TDPW > 1.E-6) THEN
        ShelPC = TDSW / TDPW * 100.
      ELSE
        ShelPC = 0.0
      ENDIF

!     VSH
!     for new FreshWt.for output
      IF (RPODNO > 1.E-6) THEN
        AvgRFPW = RTFPW / RPODNO
        AvgRDPW = RTDPW / RPODNO
        AvgRDSP = RTDSD / RPODNO
        AvgRSNP = RSEEDNO / RPODNO
      ELSE
        AvgRFPW = 0.0
        AvgRDPW = 0.0
        AvgRDSP = 0.0
        AvgRSNP = 0.0
      ENDIF
      IF (RSEEDNO > 1.E-6) THEN
        AvgRDSD = 1000.0* RTDSD / RSEEDNO
      ELSE
        AvgRDSD = 0.0
      ENDIF
      IF (RTDPW > 1.E-6) THEN
        PRDSH = 100.0* RTDSH / RTDPW
        PRDSD = 100.0* RTDSD / RTDPW
      ELSE
        PRDSH = 0.0
        PRDSD = 0.0
      ENDIF

!     VSH
! Alwin Hopf new
      if (HARV_AH==1) Then
        HRVD = RTDPW 
        HRVF = RTFPW 
        CHRVD = CHRVD + HRVD
        CHRVF = CHRVF + HRVF
        HRSN = RSEEDNO
        HRPN = RPODNO
        HRDSD = RTDSD
        HRDSH = RTDSH
      else
        HRVD = 0.0
        HRVF = 0.0
!         RTDPW = 0
        HRSN = 0.0
        HRPN = 0.0
        HRDSD = 0.0
        HRDSH = 0.0
      end if
      RUDPW = TDPW - RTDPW
! Alwin Hopf new

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      IF (INDEX('Y',ISWFWT) < 1 .OR. 
     &    INDEX('N,0',ISWITCH%IDETL) > 0) RETURN

      YRDOY = CONTROL % YRDOY
      IF (YRDOY .LT. YRPLT .OR. YRPLT .LT. 0) RETURN

!     DAS = MAX(0,TIMDIF(YRSIM,YRDOY))
      DAS = CONTROL % DAS
        
      RTDSD = RTDSD + WTSD(NPP) !seed mass of mature fruits - wtsd = seed mass for cohort
      RTDSH = RTDSH + WTSHE(NPP) !shell mass of mature fruits - wtshe = shell mass for cohort
!     Daily output every FROP days
      IF (MOD(DAS,CONTROL%FROP) == 0) THEN  

        CALL YR_DOY(YRDOY, YEAR, DOY) 
        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        IF (DAP > DAS) DAP = 0
      
        SELECT CASE (CROP)
        
        ! add aditional values  for more than one harvest
        
          CASE ('TM')       ! Tomato
!          VSH added additional outputs
!            WRITE(NOUTPF, 1000) YEAR, DOY, DAS, DAP, 
!     &      NINT(TFPW * 10.), AvgDMC, AvgFPW, AvgDPW, 
!     &      PodAge
            WRITE(NOUTPF, 1000) YEAR, DOY, DAS, DAP, 
     &      NINT(TFPW * 10.), AvgDMC, AvgFPW, AvgDPW, 
     &      PodAge, NINT(RTFPW*10.), HARV, NINT(RTDPW*10.)

!          CASE ('SR')       ! Strawberry
      !          VSH added additional outputs
      !            WRITE(NOUTPF, 1000) YEAR, DOY, DAS, DAP, 
      !     &      NINT(TFPW * 10.), AvgDMC, AvgFPW, AvgDPW, 
      !     &      PodAge

!            !old output - Alwin Hopf
!                  WRITE(NOUTPF, 1000) YEAR, DOY, DAS, DAP, 
!     &      NINT(TFPW * 10.), AvgDMC, AvgFPW, AvgDPW, 
!     &      PodAge, NINT(RTFPW*10.), HARV, NINT(RTDPW*10.), HARV_AH
!            !old output - Alwin Hopf

!           new Strawberry output - from DSSAT 4.6 files from VSH
          CASE ('SR')       ! Strawberry
            WRITE(NOUTPF, 1000) YEAR, DOY, DAS, DAP, 
     &      NINT(TFPW * 10.), AvgDMC, AvgFPW, AvgDPW, 
     &      PodAge, HARV, HARV_AH, PODNO, SEEDNO, TWTSH*10.,TDSW*10.,TDPW*10., 
     &      RPODNO, RSEEDNO, RTDSH*10., RTDSD*10., RTDPW*10.,  
     &      RUDPW*10., RTFPW*10., HRVF*10.0, CHRVF*10.0,
     &      HRVD*10.0, CHRVD*10.0, AvgRFPW, 
     &      AvgRDPW, AvgRDSD, PRDSH, PRDSD, AvgRDSP, AvgRSNP, 
     &      HRSN, HRPN, HRDSD*10.0, HRDSH*10.0, NR2TIM
!           new output end - from DSSAT 4.6 files from VSH
     
          CASE ('GB')       ! Snap bean
            WRITE(NOUTPF, 2000) YEAR, DOY, DAS, DAP, 
     &      NINT(TFPW * 10.), AvgDMC, AvgFPW, AvgDPW, 
     &      PodAge,NINT(CLASS(7)*10.),NINT(CLASS(1)*10.),
     &      NINT(CLASS(2)*10.),NINT(CLASS(3)*10.),NINT(CLASS(4)*10.),
     &      NINT(CLASS(5)*10.),NINT(CLASS(6)*10.)
        END SELECT
        !RTDPW = 0.0 
!        VSH added additional formats
! 1000   FORMAT(1X,I4,1X,I3.3,2(1X,I5),
!     &    I8,F8.3,F8.1,F8.2,F8.1)

! old FreshWt output
! 1000   FORMAT(1X,I4,1X,I3.3,2(1X,I5),
!     &    I8,F8.3,F8.1,F8.2,F8.1,I8,I8, I8, I8)

! Alwin Hopf / VSH - for FreshWt output
 1000   FORMAT(1X,I4,1X,I3.3,2(1X,I5),
     &    I8,F8.3,F8.1,F8.2,F8.1,I7,I10,F7.2,F9.2,3(F8.1),7(F8.2),
     &    2(F10.2),2(F8.2),2(F6.1),
     &    F6.1, 2(F6.1), 2(F6.1), 2(F7.1), 2(F7.1), I8)
     
 2000   FORMAT(1X,I4,1X,I3.3,2(1X,I5),
     &    I8,F8.3,F8.1,F8.2,F8.1,
     &    7(1X,I5))
   
 !    Alwin Hopf - CSV output corresponding to FreshWt.OUT
      IF (FMOPT == 'C') THEN    
!      CALL CsvOut_FreshWt(YEAR, DOY, DAS, DAP, TFPW, AvgDMC,
!     &AvgFPW, AvgDPW, PodAge, RTFPW, HARV, RTDPW, HARV_AH,  
!     &vCsvlineFreshWt, vpCsvlineFreshWt, vlngthFreshWt)
!
      CALL CsvOut_FreshWt(YEAR, DOY, DAS, DAP, 
     &NINT(TFPW * 10.), AvgDMC, AvgFPW, AvgDPW, 
     &PodAge, HARV, HARV_AH, PODNO, SEEDNO, TWTSH*10.,TDSW*10.,TDPW*10., 
     &RPODNO, RSEEDNO, RTDSH*10., RTDSD*10., RTDPW*10.,  
     &RUDPW*10., RTFPW*10., HRVF*10.0, CHRVF*10.0,
     &HRVD*10.0, CHRVD*10.0, AvgRFPW, 
     &AvgRDPW, AvgRDSD, PRDSH, PRDSD, AvgRDSP, AvgRSNP, 
     &HRSN, HRPN, HRDSD*10.0, HRDSH*10.0, NR2TIM,
     &vCsvlineFreshWt, vpCsvlineFreshWt, vlngthFreshWt)
 
      CALL LinklstFreshWt(vCsvlineFreshWt)
      END IF
      !Alwin Hopf - end

!       VSH  removing cohorts when harvesting
        Do NPP = 1, NR2TIM + 1
            PAGE = PHTIM(NR2TIM + 1) - PHTIM(NPP)
            if ((page >= xmpage).AND.(HARV==1))then 
               SDNO(NPP) = 0
               !AH: correction multiharvest. Shell number should not be resetted to 0
               !otherwise total pod number is not correct
               !SHELN(NPP)= 0
               WTSD(NPP) = 0.0
               WTSHE(NPP)= 0.0  
            end if
        End do     
      
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASONAL SUMMARY
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------

      CLOSE (NOUTPF)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE FreshWt
!=======================================================================

