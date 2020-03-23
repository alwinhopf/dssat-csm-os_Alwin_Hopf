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
   Real    :: xmpage = 12 !6 seems to be better? depend on experiment - not sure
   
   Real    :: RTDSD 
   Real    :: RTDSH
   
   Real    :: RTFPW
   Real    :: RTDPW  
   Integer :: RPODNO 
   Integer :: RSEEDNO
   Integer :: NPP0
     
End Module MultiHar