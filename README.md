MTB COMMUNICATION LIBRARY
=========================

# Description
    - This library is a middleware used for communicating with MTB-USB
       module and your program. Is simplifies communication with the MTB.
    - Library is implemented in Object Pascal, last compiled and edited in
       Delphi 2009 32-bit.
    - Dll is available in [output](output/) directory.
    - MTB documentation is availabe at http://mtb.kmz-brno.cz/. If not,
      send e-mail to author(s).
    - Most of the comments are written in czech, please be letient.
    - This library supports JUST MTB-UNI, MTB-TTL and MTB-REG modules!

# How-to
  1.  connect MTB-USB board via USB
  2.  install modified FTDI drivers (see MTBD2XXUnit.pas)
  3.  load the library
  4.  select MTB-USB board
  5.  open device (by calling Open() from parent software)
  6.  configure modules (configuration will be saved)
  7.  start communication (by calling Start() from parent software)
  8.  use the bus  (SetOutput, GetInput, ...)
  ...
  9.  stop communication by calling Stop()
  10. close device by calling Close()

# Authors
 - Jan Horacek (jan.horacek@kmz-brno.cz)
 - Michal Petrilak (engineercz@gmail.com)
 - Petr Travnik (petr.travnik@kmz-brno.cz)

This software is distributed as open source under Apache License v2.0.

See src/MTB.dpr for further information.
