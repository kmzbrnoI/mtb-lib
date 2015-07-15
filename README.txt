-----------------------   MTB COMMUNICATION LIBRARY ----------------------------
                                DESCRIPTION

    - This library works as a middleman between MTB-USB module and
       computer software. Is simplifies communication with the MTB.
    - Implemented in Object Pascal, last compilation and editation in
       Delphi 2009 32-bit
    - Is usually exported as .dll file.
    - MTB documentation is availabe somewhere nearby this library. If not,
      send an e-mail to authors and they will send you the documentation.
      See http://mtb.kmz-brno.cz/
    - Most of the comments are written in czech (or better in czenglish),
       so, be letient please.
    - !! This library supports JUST MTB-UNI, MTB-TTL and MTB-REG modules.

 Small how-to:
  1) connect MTB-USB board via USB
  2) install modified FTDI drivers (see MTBD2XXUnit.pas)
  3) load the library
  4) select MTB-USB board
  5) open device (by calling Open() from parent software)
  6) configure modules (configuration will be saved)
  7) start communication (by calling Start() from parent software)
  8) use the bus  (SetInput, GetOutput, ...)
  ...
  9) stop communication by calling Stop()
  10)close device by calling Close()

 For more information, see src/MTB.dpr
