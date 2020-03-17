Module MultiHar
!
!
!
Implicit None

   Integer       :: NHAR1
   Character     :: symbol
   Integer, Dimension(:), Allocatable :: HDATE1
   Integer       :: Lindex
   Integer       :: HARV            ! triger true or false for harvesting
   Integer, save :: iHARV = 1
   
   !  cultivar parameter from cultivar file --> move to cultivar file. xmpage = required age for harvest
   Real    :: xmpage = 7.5
   
   Real    :: RTDSD 
   Real    :: RTDSH
   
   Real    :: RTFPW
   Real    :: RTDPW  
   Integer :: RPODNO 
   Integer :: RSEEDNO
   Integer :: NPP0
     
End Module MultiHar