////////////////////////////////////////////////////////////////////////////////
// MTB.dpr
//  MTB communication library
//  Main Library file.
//  (c) Petr Travnik (petr.travnik@kmz-brno.cz),
//      Jan Horacek (jan.horacek@kmz-brno.cz),
//      Michal Petrilak (engineercz@gmail.com)
////////////////////////////////////////////////////////////////////////////////

{
   LICENSE:

   Copyright 2015-2016 Petr Travnik, Michal Petrilak, Jan Horacek

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

{ DESCRIPTON:
    - This library works as a middleman between MTB-USB module and
       computer software. Is simplifies communication with the MTB.
    - Implemented in Object Pascal, last compilation and editation in Delphi 2009 32-bit
    - Is usually exported as .dll file.
    - MTB documentation is availabe somewhere nearby this library. If not,
      send an e-mail to authors and they will send you the documentation.
    - Most of the comments are written in czech (or better in czenglish),
       so, be letient please.
    - This file actually defines functions exported to parent application,
       all technology is done in MTBusb.pas
    - Errors are not reported via exceptions, but just via result values.
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

}


// JCL_DEBUG_EXPERT_INSERTJDBG OFF
library MTB;

uses
  SysUtils,
  ExtCtrls,
  Math,
  Windows,
  Forms,
  Classes,
  FFormConfig in 'FFormConfig.pas' {FormConfig},
  MTBD2XXUnit in 'MTBD2XXUnit.pas',
  FFormModule in 'FFormModule.pas' {FormModule},
  About in 'About.pas' {F_About},
  MTBusb in 'MTBusb.pas',
  LibraryEvents in 'LibraryEvents.pas',
  Errors in 'Errors.pas',
  LibCML in 'LibCML.pas';

{$R *.res}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Configuration files:

function LoadConfig(filename:PChar):Integer; stdcall;
begin
 try
  MTBdrv.LoadConfig(filename);
  Result := 0;
 except
  Result := MTB_GENERAL_EXCEPTION;
 end;
end;

function SaveConfig(filename:PChar):Integer; stdcall;
begin
 try
  MTBdrv.SaveConfig(filename);
  Result := 0;
 except
  Result := MTB_GENERAL_EXCEPTION;
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// Logging:

procedure SetLogLevel(loglevel:Cardinal); stdcall;
begin
 try
  MTBdrv.LogLevel := TLogLevel(loglevel);
  FormConfig.CB_LogLevel.ItemIndex := Integer(MTBdrv.LogLevel);
 except

 end;
end;

function GetLogLevel():Cardinal; stdcall;
begin
 try
  Result := Cardinal(MTBdrv.LogLevel);
 except
  Result := MTB_GENERAL_EXCEPTION;
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// Dialogs:

procedure ShowConfigDialog(); stdcall;
begin
  try
    FormConfig.Show();
  finally

  end;

end;

procedure HideConfigDialog(); stdcall;
begin
  try
    FormConfig.Hide();
  finally

  end;
end;

////////////////////////////////////////////////////////////////////////////////
// MTB open/close start/stop:

function Open():Integer; stdcall;
begin
  try
    MTBdrv.Open(MTBdrv.UsbSerial);
    Result := 0;
  except
    on E:EAlreadyOpened do
      Result := MTB_ALREADY_OPENNED;
    on E:EAlreadyStarted do
      Result := MTB_ALREADY_STARTED;
    on E:ECannotOpenPort do
      Result := MTB_CANNOT_OPEN_PORT;
    on E:EFtGeneral do
      Result := MTB_FT_EXCEPTION;
    on E:Exception do
      Result := MTB_GENERAL_EXCEPTION;
   end;
end;

function OpenDevice(device:PChar; persist:boolean):Integer; stdcall;
begin
  try
    MTBdrv.Open(device);
    Result := 0;
  except
    on E:EAlreadyOpened do
      Result := MTB_ALREADY_OPENNED;
    on E:EAlreadyStarted do
      Result := MTB_ALREADY_STARTED;
    on E:ECannotOpenPort do
      Result := MTB_CANNOT_OPEN_PORT;
    on E:EFtGeneral do
      Result := MTB_FT_EXCEPTION;
    on E:Exception do
      Result := MTB_GENERAL_EXCEPTION;
  end;
end;

function Close():Integer; stdcall;
begin
  try
    if (MTBdrv.OpeningScanning) then Exit(MTB_SCANNING_NOT_FINISHED);
    if (not MTBdrv.Openned) then Exit(MTB_NOT_OPENED);
    
    MTBdrv.Close();
    Result := 0;
  except
    on E:EFtGeneral do
      Result := MTB_FT_EXCEPTION;
    on E:Exception do
      Result := MTB_GENERAL_EXCEPTION;
  end;
end;

function Openned():Boolean; stdcall;
begin
 try
   Result := MTBdrv.Openned;
 except
   Result := false;
 end;
end;

function Start():Integer; stdcall;
begin
  try
    if (MTBdrv.Scanning) then Exit(MTB_ALREADY_STARTED);
    if (not MTBdrv.Openned) then Exit(MTB_NOT_OPENED);
    if (MTBdrv.HWVersionInt < MIN_FW_VERSION) then Exit(MTB_FIRMWARE_TOO_LOW);
    if (MTBdrv.OpeningScanning) then Exit(MTB_SCANNING_NOT_FINISHED);
    if (MTBdrv.ModuleCount = 0) then Exit(MTB_NO_MODULES);

    MTBdrv.Start();
    Result := 0;
  except
    on E:EFtGeneral do
      Result := MTB_FT_EXCEPTION;
    on E:Exception do
      Result := MTB_GENERAL_EXCEPTION;
  end;
end;

function Stop():Integer; stdcall;
begin
  try
    if (not MTBdrv.Scanning) then Exit(MTB_NOT_STARTED);
    MTBdrv.Stop();
    Result := 0;
  except
    on E:EFtGeneral do
      Result := MTB_FT_EXCEPTION;
    on E:Exception do
      Result := MTB_GENERAL_EXCEPTION;
  end;
end;

function Started():Boolean; stdcall;
begin
 try
   Result := MTBdrv.Scanning;
 except
   Result := false;
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// MTB IO functions:

function GetInput(module, port: Cardinal): Integer; stdcall;
begin
 try
   if ((not InRange(module, Low(TAddr), High(TAddr))) or (not MTBdrv.IsModule(module))) then Exit(MTB_MODULE_INVALID_ADDR);
   if (MTBdrv.IsModuleFailure(module)) then Exit(MTB_MODULE_FAILED);
   if (not InRange(port, Low(TIOchann), High(TIOchann))) then Exit(MTB_PORT_INVALID_NUMBER);

   Result := 0;

   case (MTBdrv.GetModuleType(Module)) of
     TModulType.idMTB_POT_ID  : Result := MTBdrv.GetPotChannel(Module, Port);
     TModulType.idMTB_REGP_ID : Result := MTBdrv.GetRegChannel(Module, Port);

     TModulType.idMTB_UNI_ID, TModulType.idMTB_UNIOUT_ID,
     TModulType.idMTB_TTL_ID, TModulType.idMTB_TTLOUT_ID:begin
       case (MTBdrv.GetInputIO(Module, Port)) of
        false : Result := 0;
        true  : Result := 1;
       end;
     end;
   end;

 except
  // ft exception cannot happen here
  Result := MTB_GENERAL_EXCEPTION;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function GetOutput(module, port: Cardinal): Integer; stdcall;
var MTBport:TPortValue;
begin
  try
    if ((not InRange(module, Low(TAddr), High(TAddr))) or (not MTBdrv.IsModule(module))) then Exit(MTB_MODULE_INVALID_ADDR);
    if (MTBdrv.IsModuleFailure(module)) then Exit(MTB_MODULE_FAILED);
    if (not InRange(port, Low(TIOchann), High(TIOchann))) then Exit(MTB_PORT_INVALID_NUMBER);

    MTBport := Module*16 + Port;
    Result := 0;
    if (MTBdrv.IsScomOut(MTBport)) then
      Result := MTBdrv.GetScomCode(MTBport)
    else begin
      case (MTBdrv.GetOutPort(MTBport)) of
        false: Result := 0;
        true : Result := 1;
      end;//case
    end;

  except
    // ft exception cnnot happen here
    Result := MTB_GENERAL_EXCEPTION;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function SetOutput(module, port: Cardinal; state: Integer): Integer; stdcall;
begin
  try
    if ((not InRange(module, Low(TAddr), High(TAddr))) or (not MTBdrv.IsModule(module))) then Exit(MTB_MODULE_INVALID_ADDR);
    if (MTBdrv.IsModuleFailure(module)) then Exit(MTB_MODULE_FAILED);
    if (not InRange(port, Low(TIOchann), High(TIOchann))) then Exit(MTB_PORT_INVALID_NUMBER);

    case (MTBdrv.GetModuleType(Module)) of
     TModulType.idMTB_UNI_ID, TModulType.idMTB_UNIOUT_ID,
     TModulType.idMTB_TTL_ID, TModulType.idMTB_TTLOUT_ID:begin
             if (MTBdrv.IsScomOut((module shl 4) or (port))) then
              begin
               //pokud je port SCom
               MTBdrv.SetScomCode((module shl 4) or (port), State);
              end else begin//SCom
               //pokud port neni SCom
               case (State) of
                0:MTBdrv.SetOutputIO(Module,Port,false);
                1:MTBdrv.SetOutputIO(Module,Port,true);
               end;
              end;//else SCom
            end;//case 1..127
     end;

    Result := 0;
  except
   // FT exception cannot happen here
   Result := MTB_GENERAL_EXCEPTION;
  end;
end;//function

////////////////////////////////////////////////////////////////////////////////
// MTB-USB board

function GetDeviceCount():Integer; stdcall;
begin
 try
   Result := MTBdrv.GetDeviceCount;
 except
    on E:EFtGeneral do
      Result := MTB_FT_EXCEPTION;
    on E:Exception do
      Result := MTB_GENERAL_EXCEPTION;
 end;
end;

procedure GetDeviceSerial(index:Integer; serial:PChar; serialLen:Cardinal); stdcall;
begin
 try
   if ((index < 0) or (index >= MTBdrv.GetDeviceCount)) then Exit();
   StrPLCopy(serial, MTBdrv.GetDeviceSerial(index), serialLen);
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////
// MTB modules

function IsModule(module:Cardinal):Boolean; stdcall;
begin
 try
   if (not InRange(module, Low(TAddr), High(TAddr))) then Exit(false);
   Result := MTBdrv.IsModule(module) and (not MTBdrv.IsModuleFailure(module));
 except
   Result := false;
 end;
end;

function IsModuleFailure(module:Cardinal):Boolean; stdcall;
begin
 try
   if (not InRange(module, Low(TAddr), High(TAddr))) then Exit(false);
   Result := MTBdrv.IsModuleFailure(module);
 except
   Result := false;
 end;
end;

function GetModuleCount():Cardinal; stdcall;
begin
 try
   Result := MTBdrv.ModuleCount;
 except
   Result := MTB_GENERAL_EXCEPTION;
 end;
end;

function GetModuleType(module:Cardinal):Integer; stdcall;
begin
 try
   if ((not InRange(module, Low(TAddr), High(TAddr))) or (not MTBdrv.IsModule(module))) then Exit(MTB_MODULE_INVALID_ADDR);
   Result := Integer(MTBdrv.GetModuleInfo(module).typ);
 except
   Result := MTB_GENERAL_EXCEPTION;
 end;
end;

function GetModuleName(module:Cardinal; name:PChar; nameLen:Cardinal):Integer; stdcall;
begin
 try
   if (not InRange(module, Low(TAddr), High(TAddr))) then Exit(MTB_MODULE_INVALID_ADDR);
   StrPLCopy(name, MTBdrv.GetModuleInfo(module).name, nameLen);
   Result := 0;
 except
   Result := MTB_GENERAL_EXCEPTION;
 end;
end;

function GetModuleFW(module:Cardinal; fw:PChar; fwLen:Cardinal):Integer; stdcall;
begin
 try
   if ((not InRange(module, Low(TAddr), High(TAddr))) or (not MTBdrv.IsModule(module))) then Exit(MTB_MODULE_INVALID_ADDR);
   StrPLCopy(fw, MTBdrv.GetModuleInfo(module).firmware, fwLen);
   Result := 0;
 except
   Result := MTB_GENERAL_EXCEPTION;
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// Library version functions:

function GetDeviceVersion(version:PChar; versionLen:Cardinal):Integer; stdcall;
begin
 try
   if (not MTBdrv.Openned) then Exit(MTB_DEVICE_DISCONNECTED);
   StrPLCopy(version, MTBdrv.HWVersion, versionLen);
   Result := 0;
 except
   Result := MTB_GENERAL_EXCEPTION;
 end;
end;

procedure GetDriverVersion(version:PChar; versionLen:Cardinal); stdcall;
begin
 try
   StrPLCopy(version, SW_VERSION, versionLen);
 finally

 end;
end;

procedure GetLibVersion(version:PChar; versionLen:Cardinal); stdcall;
begin
 try
   StrPLCopy(version, _VERSION, versionLen);
 finally

 end;
end;

////////////////////////////////////////////////////////////////////////////////
// Event binders:

procedure BindBeforeOpen(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
 LibEvents.BeforeOpen.data  := data;
 LibEvents.BeforeOpen.event := event;
end;

procedure BindAfterOpen(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
 LibEvents.AfterOpen.data  := data;
 LibEvents.AfterOpen.event := event;
end;

procedure BindBeforeClose(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
 LibEvents.BeforeClose.data  := data;
 LibEvents.BeforeClose.event := event;
end;

procedure BindAfterClose(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
 LibEvents.AfterClose.data  := data;
 LibEvents.AfterClose.event := event;
end;

procedure BindBeforeStart(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
 LibEvents.BeforeStart.data  := data;
 LibEvents.BeforeStart.event := event;
end;

procedure BindAfterStart(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
 LibEvents.AfterStart.data  := data;
 LibEvents.AfterStart.event := event;
end;

procedure BindBeforeStop(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
 LibEvents.BeforeStop.data  := data;
 LibEvents.BeforeStop.event := event;
end;

procedure BindAfterStop(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
 LibEvents.AfterStop.data  := data;
 LibEvents.AfterStop.event := event;
end;

procedure BindOnError(event:TStdErrorEvent; data:Pointer); stdcall;
begin
 LibEvents.OnError.data  := data;
 LibEvents.OnError.event := event;
end;

procedure BindOnLog(event:TStdLogEvent; data:Pointer); stdcall;
begin
 LibEvents.OnLog.data  := data;
 LibEvents.OnLog.event := event;
end;

procedure BindOnInputChanged(event:TStdModuleChangeEvent; data:Pointer); stdcall;
begin
 LibEvents.OnInputChanged.data  := data;
 LibEvents.OnInputChanged.event := event;
end;

procedure BindOnOutputChanged(event:TStdModuleChangeEvent; data:Pointer); stdcall;
begin
 LibEvents.OnOutputChanged.data  := data;
 LibEvents.OnOutputChanged.event := event;
end;

////////////////////////////////////////////////////////////////////////////////
// Dll exported functions:

exports
  LoadConfig, SaveConfig,
  SetLogLevel, GetLogLevel,
  ShowConfigDialog, HideConfigDialog,
  Open, OpenDevice, Close, Openned, Start, Stop, Started,
  GetInput, GetOutput, SetOutput,
  GetDeviceCount, GetDeviceSerial,
  IsModule, IsModuleFailure, GetModuleCount, GetModuleType, GetModuleName, GetModuleFW,
  GetDeviceVersion, GetDriverVersion, GetLibVersion,
  BindBeforeOpen, BindAfterOpen, BindBeforeClose, BindAfterClose,
  BindBeforeStart, BindAfterStart, BindBeforeStop, BindAfterStop,
  BindOnError, BindOnLog, BindOnInputChanged, BindOnOutputChanged;


begin
  Application.CreateForm(TFormConfig, FormConfig);
  Application.CreateForm(TFormModule, FormModule);
  Application.CreateForm(TF_About, F_About);
end.

