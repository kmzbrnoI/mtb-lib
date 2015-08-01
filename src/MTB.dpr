////////////////////////////////////////////////////////////////////////////////
// MTB.dpr
//  MTB communication library
//  Main Library file.
//  (c) Petr Travnik (petr.travnik@kmz-brno.cz),
//      Jan Horacek (jan.horacek@kmz-brno.cz),
//      Michal Petrilak (engineercz@gmail.com)
// 01.08.2015
////////////////////////////////////////////////////////////////////////////////

{
   LICENSE:

   Copyright 2015 Petr Travnik, Michal Petrilak, Jan Horacek

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
  MTBusb in 'MTBusb.pas';

{$R *.res}

////////////////////////////////////////////////////////////////////////////////

// This function should be called before unloading.
function OnUnload(): Integer; stdcall;
begin
  FreeAndNil(MTBdrv);
  FreeAndNil(FormModule);
  FreeAndNil(FormConfig);
  Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// Config dialog showing/hiding:

function ShowConfigDialog(): Integer; stdcall;
begin
  FormConfig.Show;
  Result := 0;
end;

function HideConfigDialog(): Integer; stdcall;
begin
  FormConfig.Hide;
  Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// Show about dialog:

function ShowAboutDialog(): Integer; stdcall;
begin
  F_About.ShowModal;
  Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// Start/stop communication (device must be openned):

procedure Start(); stdcall;
begin
  MTBdrv.Start;
end;

procedure Stop(); stdcall;
begin
  MTBdrv.Stop;
end;

////////////////////////////////////////////////////////////////////////////////
// Get MTB module input state.
//  Result values: [0 - 1] : MTB-UNI, MTB-TTL
//                 [-15 - 15] : MTB-REG, MTB-POT

function GetInput(Module, Port: Integer): Integer; stdcall;
begin
  if ((Module < 1) or (Module > 255)) then Exit(-2);
  if (not MTBdrv.IsModule(Module)) then Exit(-2);
  if (MTBdrv.IsModuleFailure(Module)) then Exit(-2);
  if ((port < 0) or (port > 15)) then Exit(-2);

  Result := -2;

  case (MTBdrv.GetModuleType(Module)) of
   TModulType.idMTB_POT_ID  : Result := MTBdrv.GetPotChannel(Module,Port);
   TModulType.idMTB_REGP_ID : Result := MTBdrv.GetRegChannel(Module,Port);

   TModulType.idMTB_UNI_ID, TModulType.idMTB_UNIOUT_ID,
   TModulType.idMTB_TTL_ID, TModulType.idMTB_TTLOUT_ID:begin
     case (MTBdrv.GetInputIO(Module,Port)) of
      false : Result := 0;
      true  : Result := 1;
     end;
   end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// Get MTB module output port state.
//  Result values: [0 - 1] : MTB-UNI, MTB-TTL standard outputs
//                 SCom code for MTB-UNI SCom outputs

function GetOutput(Module, Port: Integer): Integer; stdcall;
var MTBport:TPortValue;
begin
  if ((Module < 1) or (Module > 255)) then Exit(-2);
  if (not MTBdrv.IsModule(Module)) then Exit(-2);
  if (MTBdrv.IsModuleFailure(Module)) then Exit(-2);
  if ((port < 0) or (port > 15)) then Exit(-2);

  MTBport := Module*16 + Port;
  if (MTBdrv.IsScomOut(MTBport)) then
    Result := MTBdrv.GetScomCode(MTBport)
  else begin
    case (MTBdrv.GetOutPort(MTBport)) of
      false: Result := 0;
      true : Result := 1;
    end;//case
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// Returns versions:

function GetDeviceVersion:string; stdcall;
 begin
  Result := MTBdrv.HWVersion;
 end;

function GetDriverVersion:string; stdcall;
 begin
  Result := SW_VERSION;
 end;

function GetLibVersion: string; stdcall;
begin
 Result := _VERSION;
end;

////////////////////////////////////////////////////////////////////////////////
// Returns true of module exists, therwise false.

function GetModuleExists(Module:integer):boolean;stdcall;
 begin
  Result := (MTBdrv.IsModule(Module) and (not MTBdrv.IsModuleFailure(Module)));
 end;

////////////////////////////////////////////////////////////////////////////////
// Opens and closes MTB device.

function Open:integer;stdcall;
 begin
  MTBdrv.Open(MTBdrv.UsbSerial);
  result := 0;
end;

function Close:integer;stdcall;
 begin
  MTBdrv.Close;
  result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// Returns module type (as string)

function GetModuleType(Module:Integer):string; stdcall;
 begin
  if (MTBdrv.IsModule(Module)) then
    Result := MTBdrv.GetModuleTypeName(Module)
  else
    Result := 'modul neexistuje';
 end;//function

////////////////////////////////////////////////////////////////////////////////
// Returns module name (as string)

function GetModuleName(Module:Integer):string; stdcall;
 begin
  Result := MTBdrv.GetModuleCfg(Module).CFGpopis;
 end;//function

////////////////////////////////////////////////////////////////////////////////
// Returns module firmware version (as string)

function GetModuleFirmware(Module:integer):string; stdcall;
 begin
  if (MTBdrv.IsModule(Module)) then
    Result := MTBdrv.GetModuleCfg(Module).CFGfw
  else
    Result := 'modul neexistuje';
 end;//function

////////////////////////////////////////////////////////////////////////////////
// Sets module name

function SetModuleName(Module:Integer;Name:string):Integer; stdcall;
 begin
  MTBdrv.GetModuleCfg(Module);
  MTBdrv.WrCfgData.CFGdata[0] := MTBdrv.RdCfgdata.CFGdata[0];
  MTBdrv.WrCfgData.CFGdata[1] := MTBdrv.RdCfgdata.CFGdata[1];
  MTBdrv.WrCfgData.CFGdata[2] := MTBdrv.RdCfgdata.CFGdata[2];
  MTBdrv.WrCfgData.CFGpopis := Name;
  MTBdrv.SetModuleCfg(Module);
  Result := 0;
 end;

////////////////////////////////////////////////////////////////////////////////
// Sets MTB speed (must be called before open)

function SetMtbSpeed(Speed:Integer):Integer; stdcall;
 begin
  if (Speed <= 2) then
   begin
    MTBdrv.MtbSpeed := TMtbSpeed(Speed+2);
    FormConfig.cb_speed.ItemIndex := Speed;
    Result := 0;
   end else
    Result := 50;
 end;

////////////////////////////////////////////////////////////////////////////////
// Sets output

function SetOutput(Module, Port, State: Integer): Integer; stdcall;
 begin
  if (MTBdrv.IsModule(Module)) then
   begin
    if (port >= 0) AND (port <= 15) then
     begin
      case (MTBdrv.GetModuleType(Module)) of
       TModulType.idMTB_POT_ID  : ;
       TModulType.idMTB_REGP_ID : ;

       TModulType.idMTB_UNI_ID, TModulType.idMTB_UNIOUT_ID,
       TModulType.idMTB_TTL_ID, TModulType.idMTB_TTLOUT_ID:begin
               if (MTBdrv.IsScomOut((module shl 4) or (port))) then
                begin
                 //pokud je port SCom
                 MTBdrv.SetScomCode((module shl 4) or (port),State);
                end else begin//SCom
                 //pokud port neni SCom
                 case (State) of
                  0:MTBdrv.SetOutputIO(Module,Port,false);
                  1:MTBdrv.SetOutputIO(Module,Port,true);
                 end;
                end;//else SCom
              end;//case 1..127
       end;
     end;//case GetModuleType
    Result := 0;
   end else Result := -2;
 end;//function

////////////////////////////////////////////////////////////////////////////////
// Is device open?

function IsOpen():Boolean; stdcall;
begin
 Result := MTBdrv.Openned;
end;//function

////////////////////////////////////////////////////////////////////////////////
// Is communication started?

function IsStart():Boolean; stdcall;
begin
 Result := MTBdrv.Scanning;
end;//function

////////////////////////////////////////////////////////////////////////////////
// ----- setting callback events begin -----

procedure SetBeforeOpen(ptr:TStdNotifyEvent); stdcall;
begin
 FormConfig.PrgEvents.prgBeforeOpen := ptr;
end;//function

procedure SetAfterOpen(ptr:TStdNotifyEvent); stdcall;
begin
 FormConfig.PrgEvents.prgAfterOpen := ptr;
end;//function

procedure SetBeforeClose(ptr:TStdNotifyEvent); stdcall;
begin
 FormConfig.PrgEvents.prgBeforeClose := ptr;
end;//function

procedure SetAfterClose(ptr:TStdNotifyEvent); stdcall;
begin
 FormConfig.PrgEvents.prgAfterClose := ptr;
end;//function

procedure SetBeforeStart(ptr:TStdNotifyEvent); stdcall;
begin
 FormConfig.PrgEvents.prgBeforeStart := ptr;
end;//function

procedure SetAfterStart(ptr:TStdNotifyEvent); stdcall;
begin
 FormConfig.PrgEvents.prgAfterStart := ptr;
end;//function

procedure SetBeforeStop(ptr:TStdNotifyEvent); stdcall;
begin
 FormConfig.PrgEvents.prgBeforeStop := ptr;
end;//function

procedure SetAfterStop(ptr:TStdNotifyEvent); stdcall;
begin
 FormConfig.PrgEvents.prgAfterStop := ptr;
end;//function

procedure SetOnError(ptr:TMyErrorEvent); stdcall;
begin
 FormConfig.PrgEvents.prgError := ptr;
end;//function

procedure SetOnInputChange(ptr:TMyModuleChangeEvent); stdcall;
begin
 FormConfig.PrgEvents.prgInputChanged := ptr;
end;//function

procedure SetOnOutputChange(ptr:TMyModuleChangeEvent); stdcall;
begin
 FormConfig.PrgEvents.prgOutputChanged := ptr;
end;//function

// ----- setting callback events end -----

////////////////////////////////////////////////////////////////////////////////
// Exported functions (dll exported):

exports
  OnUnload name 'onunload',
  Start name 'start',
  Stop name 'stop',
  GetInput name 'getinput',
  SetOutput name 'setoutput',
  GetOutput name 'getoutput',
  ShowConfigDialog name 'showconfigdialog',
  HideConfigDialog name 'hideconfigdialog',
  ShowAboutDialog name 'showaboutdialog',
  GetDriverVersion name 'getdriverversion',
  GetLibVersion name 'getlibversion',
  GetDeviceVersion name 'getdeviceversion',
  GetModuleFirmware name 'getmodulefirmware',
  GetModuleExists name 'getmoduleexists',
  GetModuleType name 'getmoduletype',
  GetModuleName name 'getmodulename',
  SetModuleName name 'setmodulename',
  SetMtbSpeed name 'setmtbspeed',
  Open name 'open',
  Close name 'close',
  IsOpen name 'isopen',
  IsStart name 'isstart',

  //events
  SetBeforeOpen name 'setbeforeopen',
  SetAfterOpen name 'setafteropen',
  SetBeforeClose name 'setbeforeclose',
  SetAfterClose name 'setafterclose',
  SetBeforeStart name 'setbeforestart',
  SetAfterStart name 'setafterstart',
  SetBeforeStop name 'setbeforestop',
  SetAfterStop name 'setafterstop',
  SetOnError name 'setonerror',
  SetOnInputChange name 'setoninputchange',
  SetOnOutputChange name 'setonoutputchange';

begin
  MTBdrv := TMTBusb.Create(nil, 'mtb');

  Application.CreateForm(TFormConfig, FormConfig);
  Application.CreateForm(TFormModule, FormModule);
  Application.CreateForm(TF_About, F_About);

  if Assigned(MTBdrv) then
   begin
    if (FormConfig.cb_mtbName.ItemIndex = -1) then
      FormConfig.OnLog(FormConfig,'ERR: nelze otevrit zarizeni, zarizeni '+MTBdrv.UsbSerial+' neexistuje')
   end else
    FormConfig.OnLog(FormConfig,'ERR: objekt MTBdrv nebyl vytvoren !');

end.//unit

