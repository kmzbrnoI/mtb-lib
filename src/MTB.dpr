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

   Copyright 2015-2019 Petr Travnik, Michal Petrilak, Jan Horacek

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
  DESCRIPTON:

  This file defines functions exported to parent application, all logic is done
  in MTBusb.pas. Errors are not reported via exceptions, but just via result values.
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
  IniFiles,
  FFormConfig in 'FFormConfig.pas' {FormConfig},
  MTBD2XXUnit in 'MTBD2XXUnit.pas',
  FFormModule in 'FFormModule.pas' {FormModule},
  MTBusb in 'MTBusb.pas',
  LibraryEvents in 'LibraryEvents.pas',
  Errors in 'Errors.pas',
  LibCML in 'LibCML.pas',
  Version in 'Version.pas';

const
  API_SUPPORTED_VERSIONS: array[0..0] of Cardinal = (
  	$0301 // v1.3
  );

{$R *.res}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Configuration files:

function LoadConfig(filename:PChar):Integer; stdcall;
begin
 try
  MTBdrv.LoadConfig(filename);
  MTBdrv.ConfigFn := filename;
  FormConfig.OnConfigLoad();
  Result := 0;
 except
  on E:EIniFileException do
    Result := MTB_FILE_CANNOT_ACCESS;
  on E:Exception do
    Result := MTB_GENERAL_EXCEPTION;
 end;
end;

function SaveConfig(filename:PChar):Integer; stdcall;
begin
 try
  MTBdrv.SaveConfig(filename);
  Result := 0;
 except
  on E:EIniFileException do
    Result := MTB_FILE_CANNOT_ACCESS;
  on E:Exception do
    Result := MTB_GENERAL_EXCEPTION;
 end;
end;

procedure SetConfigFileName(filename:PChar);
begin
 try
   MTBdrv.ConfigFn := filename;
 except

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
    on E:ECannotOpenPort do
      Result := MTB_CANNOT_OPEN_PORT;
    on E:EFtGeneral do
      Result := MTB_FT_EXCEPTION;
    on E:Exception do
      Result := MTB_GENERAL_EXCEPTION;
   end;

  try
    if (Result <> 0) then
      FormConfig.AfterClose(nil);
  finally

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
    on E:ECannotOpenPort do
      Result := MTB_CANNOT_OPEN_PORT;
    on E:EFtGeneral do
      Result := MTB_FT_EXCEPTION;
    on E:Exception do
      Result := MTB_GENERAL_EXCEPTION;
  end;

  try
    if (Result <> 0) then
      FormConfig.AfterClose(nil);
  finally

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

function Opened():Boolean; stdcall;
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

  try
    if (Result <> 0) then
      FormConfig.AfterStop(nil);
  finally

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
   if (not MTBdrv.Scanning) then Exit(MTB_NOT_STARTED);   
   if ((not InRange(module, Low(TAddr), High(TAddr))) or (not MTBdrv.IsModule(module))) then Exit(MTB_MODULE_INVALID_ADDR);
   if (MTBdrv.IsModuleFailure(module)) then Exit(MTB_MODULE_FAILED);
   if (not InRange(port, Low(TIOchann), High(TIOchann))) then Exit(MTB_PORT_INVALID_NUMBER);
   if (not MTBdrv.GetModuleInfo(module).inputStateKnown) then Exit(MTB_INPUT_NOT_YET_SCANNED);

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
    if (not MTBdrv.Scanning) then Exit(MTB_NOT_STARTED);
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
    if (not MTBdrv.Scanning) then Exit(MTB_NOT_STARTED);
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

function GetInputType(module, port: Cardinal): Integer; stdcall;
begin
  try
    if ((not InRange(module, Low(TAddr), High(TAddr))) or (not MTBdrv.IsModule(module))) then Exit(MTB_MODULE_INVALID_ADDR);
    if (not InRange(port, Low(TIOchann), High(TIOchann))) then Exit(MTB_PORT_INVALID_NUMBER);

    if (MTBdrv.IsIRIn(Module*16 + Port)) then
      Result := Integer(TRCSIPortType.iptIR)
    else
      Result := Integer(TRCSIPortType.iptPlain);
  except
    Result := MTB_GENERAL_EXCEPTION;
  end;
end;

function GetOutputType(module, port: Cardinal): Integer; stdcall;
begin
  try
    if ((not InRange(module, Low(TAddr), High(TAddr))) or (not MTBdrv.IsModule(module))) then Exit(MTB_MODULE_INVALID_ADDR);
    if (not InRange(port, Low(TIOchann), High(TIOchann))) then Exit(MTB_PORT_INVALID_NUMBER);

    if (MTBdrv.IsScomOut(Module*16 + Port)) then
      Result := Integer(TRCSOPortType.optScom)
    else
      Result := Integer(TRCSIPortType.iptPlain);
  except
    Result := MTB_GENERAL_EXCEPTION;
  end;
end;

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

function GetMaxModuleAddr():Cardinal;
begin
 Result := MTBusb._ADDR_MAX_NUM;
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
   StrPLCopy(name, MTBdrv.GetModuleInfo(module).popis, nameLen);
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

function GetModuleInputsCount(module:Cardinal):Cardinal;
begin
 if (module > MTBusb._ADDR_MAX_NUM) then
   Exit(MTB_MODULE_INVALID_ADDR);

 if (MTBdrv.IsModule(module)) then
  begin
   case (MTBdrv.GetModuleType(Module)) of
    TModulType.idMTB_POT_ID: Result := High(TPotInp)+1;
    TModulType.idMTB_UNI_ID, TModulType.idMTB_TTL_ID: Result := High(TIOchann)+1;
   else
    Result := 0;
   end;
  end else begin
   Result := High(TIOchann)+1; // assume all modules IO
  end;
end;

function GetModuleOutputsCount(module:Cardinal):Cardinal;
begin
 if (module > MTBusb._ADDR_MAX_NUM) then
   Exit(MTB_MODULE_INVALID_ADDR);

 if (MTBdrv.IsModule(module)) then
  begin
   case (MTBdrv.GetModuleType(Module)) of
    TModulType.idMTB_REGP_ID: Result := High(TRegOut)+1;
    TModulType.idMTB_UNI_ID, TModulType.idMTB_UNIOUT_ID,
      TModulType.idMTB_TTL_ID, TModulType.idMTB_TTLOUT_ID: Result := High(TIOchann)+1;
   else
    Result := 0;
   end;
  end else begin
   Result := High(TIOchann)+1; // assume all modules IO
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Library version functions:

function ApiSupportsVersion(version:Cardinal):Boolean; stdcall;
var i:Integer;
begin
 for i := Low(API_SUPPORTED_VERSIONS) to High(API_SUPPORTED_VERSIONS) do
   if (API_SUPPORTED_VERSIONS[i] = version) then
     Exit(true);
 Result := false;
end;

function ApiSetVersion(version:Cardinal):Integer; stdcall;
begin
 if (not ApiSupportsVersion(version)) then
   Exit(MTB_UNSUPPORTED_API_VERSION);

 CML.api_version := version;
 Result := 0;
end;

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
   StrPLCopy(version, GetLibVersion(), versionLen);
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

procedure BindOnScanned(event:TStdNotifyEvent; data:Pointer); stdcall;
begin
 LibEvents.OnScanned.data  := data;
 LibEvents.OnScanned.event := event;
end;

////////////////////////////////////////////////////////////////////////////////
// Dll exported functions:

exports
  LoadConfig, SaveConfig, SetConfigFileName,
  SetLogLevel, GetLogLevel,
  ShowConfigDialog, HideConfigDialog,
  Open, OpenDevice, Close, Opened, Start, Stop, Started,
  GetInput, GetOutput, SetOutput,
  GetDeviceCount, GetDeviceSerial,
  IsModule, IsModuleFailure, GetModuleCount, GetModuleType, GetModuleName, GetModuleFW,
  GetModuleInputsCount, GetModuleOutputsCount, GetMaxModuleAddr,
  ApiSupportsVersion, ApiSetVersion, GetDeviceVersion, GetDriverVersion,
  BindBeforeOpen, BindAfterOpen, BindBeforeClose, BindAfterClose,
  BindBeforeStart, BindAfterStart, BindBeforeStop, BindAfterStop,
  BindOnError, BindOnLog, BindOnInputChanged, BindOnOutputChanged,
  BindOnScanned, GetInputType, GetOutputType;


begin
  Application.CreateForm(TFormConfig, FormConfig);
  Application.CreateForm(TFormModule, FormModule);
end.

