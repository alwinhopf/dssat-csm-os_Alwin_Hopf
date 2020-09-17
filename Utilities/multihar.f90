Module MultiHar
!
!
!
Implicit None

   Integer       :: NHAR1
   Character     :: symbol
   Integer, Dimension(:), Allocatable :: HDATE1
   Integer       :: Lindex
   Integer       :: HARV, HARV_AH            ! trigger true or false for harvesting
   Integer, save :: iHARV = 1
   
   !  cultivar parameter from cultivar file --> move to cultivar file. xmpage = required age for harvest
   Real    :: xmpage = 10.0 !6 seems to be better? depend on experiment - not sure
   
   Real    :: RTDSD 
   Real    :: RTDSH
   !for FreshWt.for output
   Real    :: HRPN, AvgRFPW, AvgRDPW
   
   !Alwim Hopf - old
!   Real    :: RTFPW
!   Real    :: RTDPW  
!   Integer :: RPODNO 
!   Integer :: RSEEDNO
!   Integer :: NPP0
   !Alwim Hopf - old

   !new format - changed Integer to Real. Otherwise error later on
   Real    :: RTFPW
   Real    :: RTDPW  
   Real    :: RPODNO 
   Real    :: RSEEDNO
   Integer :: NPP0
   !end

   !for FreshWt.for output
   Real :: HRVD   ! dry weight of harvested fruit
   Real :: HRVF   ! harvest fresh weight of mature fruit 
   Real :: DIFR
   Real :: RUDPW
   Real :: CHRVD   ! Cumulative dry weight of harvested fruit 
   Real :: CHRVF   ! Cumulative fresh weight of harvested fruit 
     
End Module MultiHar