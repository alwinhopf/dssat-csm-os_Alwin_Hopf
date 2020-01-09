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
   
   !  cultivar parameter from cultivar file
   Real    :: xmpage = 12.0
   
   Real    :: RTDSD 
   Real    :: RTDSH
   
   Real    :: RTFPW
   Real    :: RTDPW  
   Integer :: RPODNO 
   Integer :: RSEEDNO
   Integer :: NPP0
     
End Module MultiHar