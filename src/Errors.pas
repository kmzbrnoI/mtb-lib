////////////////////////////////////////////////////////////////////////////////
// Errors.pas
//  MTB communication library
//  Error codes definiton
//   (c) Jan Horacek (jan.horacek@kmz-brno.cz),
////////////////////////////////////////////////////////////////////////////////

{
   LICENSE:

   Copyright 2016 Jan Horacek

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
  limitations under the License.
}

{
  DESCRIPTION:

  This file defines library error codes.
}

unit Errors;

interface

const
 MTB_GENERAL_EXCEPTION = 1000;
 MTB_FT_EXCEPTION = 1001;       // device is always closed when this exception happens
 MTB_FILE_CANNOT_ACCESS = 1010;
 MTB_FILE_DEVICE_OPENED = 1011;
 MTB_MODULE_INVALID_ADDR = 1100;
 MTB_MODULE_FAILED = 1102;
 MTB_PORT_INVALID_NUMBER = 1103;
 MTB_MODULE_UNKNOWN_TYPE = 1104;
 MTB_INVALID_SPEED = 1105;
 MTB_INVALID_SCOM_CODE = 1106;
 MTB_INVALID_MODULES_COUNT = 1107;

 MTB_ALREADY_OPENNED = 2001;
 MTB_CANNOT_OPEN_PORT = 2002;
 MTB_FIRMWARE_TOO_LOW = 2003;
 MTB_DEVICE_DISCONNECTED = 2004;
 MTB_SCANNING_NOT_FINISHED = 2010;
 MTB_NOT_OPENED = 2011;
 MTB_ALREADY_STARTED = 2012;
 MTB_OPENING_NOT_FINISHED = 2021;
 MTB_NO_MODULES = 2025;
 MTB_NOT_STARTED = 2031;

 MTB_MODULE_NOT_ANSWERED_CMD = 2101;
 MTB_MODULE_NOT_ANSWERED_CMD_GIVING_UP = 2102;
 MTB_MODULE_SUM_ERROR = 2106;
 MTB_MODULE_SUM_ERROR_GIVING_UP = 2107;
 MTB_INVALID_PACKET = 2108;

implementation

end.
