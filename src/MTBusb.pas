////////////////////////////////////////////////////////////////////////////////
// MTBusb.pas
//  MTB communication library
//  Main MTB technology
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

{
  DESCRIPTION:

  This is main MTB technology unit. TMTBusb class operates with MTB. Methods
  can be called to the class and events can be handled from the class. This
  class does almost everything: it creates messages for MTB-USB, parses it,
  cares about modules, ...
}

unit MTBusb;

interface

uses
  Windows, SysUtils, Classes, IniFiles, ExtCtrls, Forms, math, MTBD2XXUnit,
  Version;

// Verze komponenty
const
  MIN_FW_VERSION: Integer = 920;

// verze pro FW 0.9.6 a vyšší

const
  _ADDR_MAX_NUM = 191;     // maximalni pocet adres;
  _PORT_MAX_NUM = 4095;    // 256*16-1;
  _PORTOVER_MAX_NUM = 255;    // 32*8-1;
  _DEFAULT_USB_NAME = 'MTB03001';
  _DEFAULT_ERR_ADDR = 255;
  _CONFIG_FN = 'mtbcfg.ini';

// konstanty pro MTB-USB modul
const
  _USB_SET   =  $0C8;
  _MTB_SET   =  $0D0;
  _FB_LIST   =  $0D0;
  _USB_RST   =  $0D8;
  _SEND_CMD  =  $0E0;
  _SCAN_MTB  =  $0E8;
  _COM_OFF   =  $0F0;
  _COM_ON    =  $0F8;

        {konstanty odpovedi}
  _RST_OK    =  $040;
  _LIST_OK   =  $050;
  _MTB_CFG   =  $060;
  _MTB_DATA  =  $070;
  _MTB_ID    =  $0A0;
  _MTB_SEARCH_END  =  $0B0;
  _ERR_CODE  =  $0C0;
  _USB_CFGQ  =  $0F0;

// konstanty pro MTB moduly
// prikazy:
const
  __PWR_ON        = $1*8;
  __PWR_OFF       = $2*8;
  __BYPASS_RST    = $4*8;
  __IDLE          = $8*8;
  __SEND_ID       = $9*8;
  __SET_CFG       = $a*8;

  __SET_OUTB      = $10*8;     {nastaveni jednoho bitu}
  __READ_FEED     = $11*8;
  __SET_REG_U     = $12*8;
  __SET_OUTW      = $13*8;     {nastaveni vsech 16 vystupu- puvodni OPkod = 12h!!!, novy=13h}
  __FLICK_OUT     = $14*8;
  __SET_NAVEST    = $15*8;      {nastaveni kodu navesti S-com}
  __READ_POT      = $16*8;      {cteni stavu potenciometru}

// ID modulu
const
  MTB_POT_ID     = $10;
  MTB_REGP_ID    = $30;
  MTB_UNI_ID     = $40;
  MTB_UNIOUT_ID  = $50;
  MTB_TTL_ID     = $60;
  MTB_TTLOUT_ID  = $70;

// Konstanty programu
const _MTB_MAX_ADDR = 191;

const
  _REG_ADDR = $80;    // 128..159      REG 32 adres
  _POT_ADDR = $a0;    // 160..175      POT 16 adres
  _REG_CHANN = 256;
  _POT_CHANN = 64;

  _END_BYTE = 11;  // hodnota posledniho byte v paketu

type
  // Ranges & sub-types:
  TAddr       = 0..191;   // Rozsah povolenych adres
  TIOaddr     = 0..191;  // Rozsah adres UNI, TTL
  TIOchann    = 0..15;   // Rozsah kanalu UNI,TTL
  TregAddr    = 128..159;  // Rezsah Reg adres
  TRegChann   = 0..255;  // Rozsah kanalu regulatoru
  TRegOut     = 0..7;    // Rozsah vystupu regulatoru
  TRegSpeed   = 0..15;   // Rychlost regulatoru
  TRegDirect  = 0..1;    // Smer Regulatoru
  TRegAcctime = 0..20;   // Casova rampa regulatoru
  TPotAddr    = 160..175; // Rozsah pro adresy POT
  TPotChann   = 0..63;   // Hodnota kanálu potenciometru
  TPotInp     = 0..3;     // Rozsah pro vstupy POT
  TPotValue   = 0..15;   // Hodnota potenciometru
  TPotDirect  = 0..1;    // Smer potenciometru
  TPortValue  = 0..4095; // Hodnota Portu

  // Exceptions
  EAlreadyOpened = class(Exception);
  ECannotOpenPort = class(Exception);
  EFirmwareTooLog = class(Exception);
  ENotOpened = class(Exception);
  EAlreadyStarted = class(Exception);
  EOpeningNotFinished = class(Exception);
  ENoModules = class(Exception);
  ENotStarted = class(Exception);
  EInvalidScomCode = class(Exception);

  TLogLevel = (llNo = 0, llError = 1, llChange = 2, llCmd = 3, llRawCmd = 4,
    llDebug = 5);

  TModulType = (idNone = $0, idMTB_POT_ID = $10, idMTB_REGP_ID = $30, idMTB_UNI_ID = $40,
        idMTB_UNIOUT_ID = $50, idMTB_TTL_ID = $60, idMTB_TTLOUT_ID = $70);

  TFlickType = (flOff = 0, fl33 = 1, fl55 = 2);

  TTimerInterval = (ti50 = 50, ti100 = 100, ti200 = 200, ti250 = 250);

  TMtbSpeed = (sp38400 = 2, sp57600 = 3, sp115200 = 4);

  TModulConfigGet = record
    CFGdata: array[0..3] of byte;
    CFGnum: byte;
    CFGpopis : string;
    CFGfw : string;
  end;
  TModulConfigSet = record
    CFGdata: array[0..3] of byte;
    CFGpopis : string;
  end;
  TPort = record
    value: word;
    PortName: Array[0..15] of string;
    changed: boolean;
  end;
  TPortIn = record
    value : boolean;
    inUp  : boolean;
    inDn  : boolean;
  end;
  TPortOut = record
    value : boolean;
    flick : TFlickType;
    flickChange : boolean;
  end;
  TReg = record
    RegSpeed   : TRegSpeed;
    RegDirect  : TRegDirect;
    RegAcctime : TRegAcctime;
  end;
  TPortRegOver = record
    value : boolean;
    inUp  : boolean;
  end;
  TPot = record
    PotValue : TPotValue;
    PotDirect: TPotDirect;
  end;
  TScom = record
    ScomCode: Array[0..7] of byte;       // kod vystupu
    ScomName: Array[0..15] of string;    // nazev vystupu
    ScomActive: array[0..7] of Boolean;  // true, pokud je Scom vystup aktivni
    changed: array[0..7] of Boolean;     // nastavi priznak pri zmene
  end;
  TModule = object
    name: string;
    popis: string;
    Input: TPort;
    Output: TPort;
    Status : byte;
    Sum : Byte;
    Scom: TScom;
    OPData: Array[0..3] of byte;
    CFData: Array[0..3] of byte;      // konfigurace
    CFGnum: byte;                     // poèet konfig. dat
    ErrTX: byte;                      // poslední chyba
    ID: byte;                        // (ID + cfg num)
    typ: TModulType;                 //(ID only)
    firmware: string;                   // verze FW
    setting : byte;
    available: boolean;
    failure : Boolean;        // vypadek modulu - porucha - neodpovida
    revived : Boolean;        // modul oziven po vypadku - zacal odpovidat
    inputStateKnown : Boolean;
  end;

  TNotifyEventError = procedure (Sender: TObject; errValue: word; errAddr: byte) of object;
  TNotifyEventLog = procedure (Sender: TObject; logLevel: TLogLevel; msg:string) of object;
  TNotifyEventChange = procedure (Sender: TObject; module: byte) of object;

  TMTBusb = class(TComponent)
  private
    FTimer: TTimer;
    FSpeed: TMtbSpeed;
    FScanInterval: TTimerInterval;
    FMyDir : String;
    FOpenned: boolean;
    FScanning: boolean;
    FusbName: String;
    FModule: Array[0..255] of TModule;
    FPot : array[TPotChann] of TPot;
    FPortIn : array[0.._PORT_MAX_NUM] of TPortIn;
    FPortOut: array[0.._PORT_MAX_NUM] of TPortOut;
    FRegOver: array[0.._PORTOVER_MAX_NUM] of TPortRegOver;
    FReg: Array[0..255] of TReg;
    FPotChanged   : Boolean;    // zmena vstupu POT
    FOverChanged  : Boolean;    // zmena vstupu REG - pretizeni
    FInputChanged : Boolean;    // zmena vstupu IO - UNI/TTL

    FModuleCount: Cardinal;       // pocet nalezenych modulu na sbernici
    FScan_flag: boolean;
    FSeznam_flag: boolean;
    FDataInputErr: Boolean;  // chyba posledniho byte paketu
    FHWVersion: string;
    FHWVersion_Major: byte;
    FHWVersion_Minor: byte;
    FHWVersion_Release: byte;
    FHWVersionInt : Integer;  // verze FW ve tvaru int, bez teèek

    FCmdCount: word;      // pocet cmd/sec
    FCmdCountCounter: word;  // citac 1 sec v timeru
    FCmdCounter_flag : boolean;  // priznak zmeny poctu cmd
    FCmdSecCycleNum : word;
    FCyclesFbData : Byte;  // hodnota pro opakovane zasilani FB dat do PC  v sec
                           // 0 .. data se zasilaji jen pri zmene
                           // 1 .. 25  = 1sec .. 25sec
                           // do modulu se zasila x*10

    FXRamAddr  : Word;  // Adresa XRAM
    FXRamValue : Byte;  // Hodnota bunky

    FLogLevel : TLogLevel;

    FWaitingOnScan : boolean;

    FErrAddress: byte;
    FBeforeOpen,
    FAfterOpen,
    FBeforeClose,
    FBeforeStart,
    FAfterStart,
    FBeforeStop,
    FAfterStop,
    FAfterClose,
    FOnScanned : TNotifyEvent;
    FOnError : TNotifyEventError;
    FOnLog : TNotifyEventLog;
    FOnScan : TNotifyEvent;
    FOnChange : TNotifyEvent;
    FOnInputChange : TNotifyEventChange;
    FOnOutputChange : TNotifyEventChange;
    function MT_send_packet(buf: array of byte; count:byte): integer;

    function GetSpeedStr: cardinal;

    procedure SetScanInterval(interval: TTimerInterval);

    procedure MtbScan(Sender: TObject);
    procedure LogWrite(ll:TLogLevel; msg:string);
    procedure WriteError(errValue: word; errAddr: byte);

    procedure GetCmdNum;

    procedure SendCyclesData;

    function GetPotValue(chann: TPotChann): TPot;

    function GetInPortDifUp(Port: TPortValue): boolean;
    function GetInPortDifDn(Port: TPortValue): boolean;
    function GetInPort(Port: TPortValue): boolean;

    procedure SetOutPort(Port: TPortValue; state: boolean);
    procedure SetInPort(Port: TPortValue; state: boolean);

    function GetRegOverValue(Port: TRegChann): TPortRegOver;

    function GetModuleStatus(addr :TAddr): byte;
    procedure ResetPortsStatus();

    procedure LogDataOut(bufLen:Integer);
    procedure LogDataIn(bufLen:Integer; bufStart:Integer = 0);

  protected
    { Protected declarations }

  public
    { Public declarations }

    WrCfgData: TModulConfigSet;
    RdCfgdata: TModulConfigGet;

    constructor Create(AOwner: TComponent; MyDir:string); reintroduce;
    destructor Destroy; override;

    function GetDeviceCount: Integer;
    function GetDeviceSerial(index: Integer): string;

    procedure Open(serial_num: String);
    procedure Close();
    procedure Start();
    procedure Stop();

    function GetPortNum(addr : TIOaddr; channel: TIOchann): word;
    function GetChannel(Port: TPortValue): byte;
    function GetPotChannel(potAddr: TPotAddr; potInp: TPotInp): word;
    function GetRegChannel(regAddr: TregAddr; regOut: TRegOut): word;

    function GetExistsInPort(Port: TPortValue): boolean;
    function GetExistsOutPort(Port: TPortValue): boolean;

    function GetAdrr(Port: TPortValue): byte;
    function IsModule(addr: TAddr): boolean;
    function IsModuleConfigured(addr: TAddr): boolean;  // modul ma konfiguracni data
    function IsModuleRevived(addr: TAddr): boolean;     // modul oziven po vypadku
    function IsModuleFailure(addr: TAddr): boolean;     // modul v poruse
    function IsModuleScanned(addr: TAddr): boolean;
    function AreAllModulesScanned(): boolean;

    function GetModuleInfo(addr: TAddr): TModule; // informace o modulu
    function GetModuleType(addr: TAddr): TModulType; // Typ modulu
    function GetModuleTypeName(addr: TAddr): string; // Typ modulu - nazev

    function GetModuleCfg(addr: TAddr): TModulConfigGet; // Vrati cfg data
    procedure SetModuleCfg(addr: TAddr); // Vlozi cfg data z pole FSetCfg[]

    function GetModuleEnable(addr: TIOaddr):boolean;
    procedure SetModuleEnable(addr: TIOaddr; enable:Boolean); // On/off modulu

    procedure SetCfg(addr: TAddr; config: cardinal);
    function GetCfg(addr: TAddr): cardinal;

    procedure SetPortIName(Port: TPortValue; name: string);
    function GetPortIName(Port: TPortValue): string;

    procedure SetPortOName(Port: TPortValue; name: string);
    function GetPortOName(Port: TPortValue): string;

    function GetErrString(err: word): string;

    procedure SetOutputIO(addr : TIOaddr; channel: TIOchann; value: boolean);
    function GetInputIO(addr : TIOaddr; channel: TIOchann): boolean;

    function GetIModule(addr: TIOaddr): word;
    function GetOModule(addr: TIOaddr): word;
    procedure SetOModule(addr: TIOaddr; state: word);
    function GetOutPort(Port: TPortValue): boolean;

    procedure SetOutPortFlick(Port: TPortValue; state: TFlickType);
    function GetOutPortFlick(Port: TPortValue): TFlickType;

    function IsScomOut(Port: TPortValue): Boolean;
    procedure SetScomCode(Port: TPortValue; code: byte);
    function GetScomCode(Port: TPortValue): byte;

    procedure SetRegSpeed(Rchan: TRegChann; Rspeed: TRegSpeed; Rdirect: TRegDirect; AccTime : TRegAcctime);
    function GetRegSpeed(Rchan: TRegChann): TReg;

    procedure GetFbData;   // zadost o FB data

    procedure SetCyclesData(value: Byte);

    procedure XMemWr(mAddr: word; mData : Byte);
    procedure XMemRd(mAddr: word);

    procedure LoadConfig(fn:string);
    procedure SaveConfig(fn:string);

    procedure CheckAllScannedEvent();

    property ModuleCount: Cardinal read FModuleCount;
    property Openned: boolean read FOpenned;
    property Scanning: boolean read FScanning;
    property ErrAddres: byte read FErrAddress;
    property OpeningScanning: boolean read FScan_flag;

    property CmdCounter_flag: boolean read FCmdCounter_flag;
    property CmdCount : word read FCmdCount;
    property HWVersion: string read FHWVersion;
    property HWVersionInt: Integer read FHWVersionInt;

    property InputChanged : boolean read FInputChanged;
    property PotChanged: boolean read FPotChanged;
    property OverChanged: boolean read FOverChanged;

    property Pot[chann : TPotChann]: TPot read GetPotValue;
    property InPortDifUp[port : TPortValue]: boolean read GetInPortDifUp;
    property InPortDifDn[port : TPortValue]: boolean read GetInPortDifDn;
    property InPortValue[port : TPortValue]: boolean read GetInPort write SetInPort;
    property OutPortValue[port : TPortValue]: boolean read GetOutPort write SetOutPort;

    property ModuleStatus[addr : TAddr]: byte read GetModuleStatus;
    property RegOver[chann : TRegChann]: TPortRegOver read GetRegOverValue;
    property LogLevel: TLogLevel read FLogLevel write FLogLevel;

  published

    { Published declarations }
    property UsbSerial: string read FusbName write FusbName;

    property MtbSpeed: TMtbSpeed read FSpeed write FSpeed default sp38400;

    property ScanInterval: TTimerInterval read FScanInterval write SetScanInterval default ti100;

    property OnError : TNotifyEventError read FOnError write FOnError;
    property OnLog : TNotifyEventLog read FOnLog write FOnLog;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnScan : TNotifyEvent read FOnScan write FOnScan;
    property OnInputChange : TNotifyEventChange read FOnInputChange write FOnInputChange;
    property OnOutputChange : TNotifyEventChange read FOnOutputChange write FOnOutputChange;

    property BeforeOpen : TNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen : TNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeClose : TNotifyEvent read FBeforeClose write FBeforeClose;
    property AfterClose : TNotifyEvent read FAfterClose write FAfterClose;

    property BeforeStart : TNotifyEvent read FBeforeStart write FBeforeStart;
    property AfterStart : TNotifyEvent read FAfterStart write FAfterStart;
    property BeforeStop : TNotifyEvent read FBeforeStop write FBeforeStop;
    property AfterStop : TNotifyEvent read FAfterStop write FAfterStop;
    property OnScanned : TNotifyEvent read FOnScanned write FOnScanned;

  end;

implementation

uses Variants, Errors;

function TMTBusb.GetPotValue(chann: TPotChann): TPot;
begin
  Result.PotValue :=  FPot[chann].PotValue;
  Result.PotDirect := FPot[chann].PotDirect;
end;

procedure TMTBusb.LogWrite(ll:TLogLevel; msg:string);
begin
  if ((ll <= Self.LogLevel) and (Assigned(OnLog))) then
    OnLog(Self, ll, msg);
end;

procedure TMTBusb.WriteError(errValue: word; errAddr: byte);
begin
 if (Assigned(OnError)) then OnError(Self, errValue, errAddr);
end;

// Vrati pocet pripojenych zarizeni
function TMTBusb.GetDeviceCount: Integer;
begin
  try
    GetFTDeviceCount;
    Result := FT_Device_Count;
  except
    on E:Exception do begin
      Self.LogWrite(llError, 'GetDeviceCount exception: '+E.ClassName+' - '+E.Message);
      raise;
     end;
  end;
end;

// Vrati seriove cislo modulu
function TMTBusb.GetDeviceSerial(index: Integer): string;
begin
  try
    GetFTDeviceSerialNo(index);
    Result := FT_Device_String;
  except
    on E:Exception do begin
      Self.LogWrite(llError, 'GetDeviceSerial exception: '+E.ClassName+' - '+E.Message);
      raise;
     end;
  end;
end;

// vrati stav vsech vstupu jako word
function TMTBusb.GetIModule(addr: TIOaddr): word;
begin
  if (FModule[addr].available) then begin
    Result := FModule[addr].Input.value;
   end else begin
    Result := 0;
  end;  
end;

//Chybove zpravy
function TMTBusb.GetErrString(err: word): string;
begin
    case err of
      // mtb moduly 101-200
      101: Result := 'Modul neodpovìdìl na pøíkaz - CMD';
      102: Result := 'Modul neodpovìdìl na pøíkaz - CMD, poslední pokus';
      106: Result := 'Chybný SUM pøijatých dat - CMD';
      107: Result := 'Chybný SUM pøijatých dat - CMD, poslední pokus';
      108: Result := 'Chybný SUM odeslaných dat - CMD';
      109: Result := 'Chybný SUM odeslaných dat - CMD, poslední pokus';

      121: Result := 'Modul neodpovìdìl na pøíkaz - FB';
      122: Result := 'Modul neodpovìdìl na pøíkaz - FB, poslední pokus';
      126: Result := 'Chybný SUM pøijatých dat - FB';
      127: Result := 'Chybný SUM pøijatých dat - FB, poslední pokus';
      128: Result := 'Chybný SUM odeslaných dat - FB';
      129: Result := 'Chybný SUM odeslaných dat - FB, poslední pokus';

      125: Result := 'Chybný SUM - FB';
      131: Result := 'Modul neodpovídá - PWR_ON';
      136: Result := 'Chybný SUM pøijatých dat - POWER ON - konfigurace';
      137: Result := 'Chybný SUM pøijatých dat - POWER ON - konfigurace, posledni pokus';
      138: Result := 'Chybný SUM odeslaných dat - POWER ON - konfigurace';
      139: Result := 'Chybný SUM odeslaných dat - POWER ON - konfigurace, posledni pokus';

      141: Result := 'Modul nekomunikuje';
      142: Result := 'Modul komunikuje';
      145: Result := 'Chybný SUM - Modul obdržel chybná data';

      151: Result := 'Nelze spustit komunikaci - neprobìhl scan sbìrnice';

      162: Result := 'Chybný SUM pøijatých dat - oživení modulu';
      163: Result := 'Chybný SUM odeslaných dat - oživení modulu';

      166: Result := 'Chybný SUM pøijatých dat - SCAN sbìrnice';
      167: Result := 'Chybný SUM pøijatých dat - SCAN sbìrnice - posledni pokus';
      168: Result := 'Chybný SUM odeslaných dat - SCAN sbìrnice';
      169: Result := 'Chybný SUM odeslaných dat - SCAN sbìrnice - posledni pokus';

      176: Result := 'Chybný SUM pøijatých dat - SC konfigurace';
      177: Result := 'Chybný SUM pøijatých dat - SC konfigurace - posledni pokus';
      178: Result := 'Chybný SUM odeslaných dat - SC konfigurace';
      179: Result := 'Chybný SUM odeslaných dat - SC konfigurace - posledni pokus';

      else Result := 'Neznámá chyba';
    end;
end;

// odesle paket pro modul
function TMTBusb.MT_send_packet(buf: array of byte; count:byte): integer;
var
  i, sum: word;
  s : string;
begin
  s := ' ';
  Result := 1;
  FT_Out_Buffer[0] := _SEND_CMD + count + 1;
  FT_Out_Buffer[1] := buf[0];
  FT_Out_Buffer[2] := buf[1] or (count-2);       // pøidá poèet dat
  sum := FT_Out_Buffer[1]+FT_Out_Buffer[2];
  for i := 2 to count-1 do begin
    FT_Out_Buffer[i+1] := buf[i];
    s := s + IntToHex(buf[i],2) + ' ';
    sum := sum + buf[i];
  end;
  FT_Out_Buffer[count+1] := Lo($8000-sum);
  s := s + IntToHex(FT_Out_Buffer[count+1],2);
  LogDataOut(count+2);
  Write_USB_Device_Buffer(count+2);
end;

// pokud je vystupu prirazen Scom port tak True, 15.1.2012
function TMTBusb.IsScomOut(Port: TPortValue): Boolean;
var
  adresa : Byte;
begin
  Result := False;
  adresa := GetAdrr(Port);
  if (adresa < 1) or (adresa > 127) then Exit;
  if not IsModule(adresa) then Exit;
  if GetChannel(Port) > 7 then Exit;
  Result := FModule[adresa].Scom.ScomActive[GetChannel(Port)];
end;

// nastavi Scom kod pro vysilani
procedure TMTBusb.SetScomCode(Port: TPortValue; code: byte);
begin
  if not IsScomOut(Port) then Exit;
  if code > 15 then begin
    FModule[GetAdrr(Port)].Scom.ScomCode[GetChannel(Port)] := 0;
    raise EInvalidScomCode.Create('Scom code must be between 0 and 15');
  end else begin
    FModule[GetAdrr(Port)].Scom.ScomCode[GetChannel(Port)] := code;
  end;
  FModule[GetAdrr(Port)].Scom.changed[GetChannel(Port)] := True;
end;

// vrati hodnotu Scom kodu, kdery je nastaven na vystup
function TMTBusb.GetScomCode(Port: TPortValue): byte;
var
  adresa, channel : Byte;
begin
  Result := 0;
  if not IsScomOut(Port) then Exit;
  adresa := GetAdrr(Port);
  channel := GetChannel(Port);
  Result := FModule[adresa].Scom.ScomCode[channel];
end;

// ziska adresu modulu z cisla portu
function TMTBusb.GetAdrr(Port: TPortValue): byte;
begin
  Result := Port DIV 16;
end;

// ziska cislo kanalu z cisla portu
function TMTBusb.GetChannel(Port: TPortValue): byte;
begin
  Result := Port MOD 16;
end;

function TMTBusb.GetPotChannel(potAddr: TPotAddr;potInp : TPotInp): word;
begin
  Result := ((potAddr - _POT_ADDR)*4)+potInp;
end;

function TMTBusb.GetRegChannel(regAddr: TregAddr;regOut : TRegOut): word;
begin
  Result := ((regAddr - _REG_ADDR)*8)+regOut;
end;

// ziska nazev inp portu
function TMTBusb.GetPortIName(Port: TPortValue): string;
begin
  Result := FModule[Port DIV 16].Input.PortName[Port MOD 16]
end;

// nastavi nazev inp portu
procedure TMTBusb.SetPortIName(Port: TPortValue; name: string);
begin
  FModule[Port DIV 16].Input.PortName[Port MOD 16] := name;
end;

// ziska nazev out portu
function TMTBusb.GetPortOName(Port: TPortValue): string;
begin
  Result := FModule[Port DIV 16].Output.PortName[Port MOD 16]
end;

// vlozi nazev out portu
procedure TMTBusb.SetPortOName(Port: TPortValue; name: string);
begin
  FModule[Port DIV 16].Output.PortName[Port MOD 16] := name;
end;

// spocita adresu portu z adresy a kanalu
function TMTBusb.GetPortNum(addr: TIOaddr; channel: TIOchann): word;
begin
  Result := 0;
  if addr > 0 then Result := addr*16+channel;
end;

// vlozi konfigurace pro modul - cardinal
procedure TMTBusb.SetCfg(addr: TAddr; config: cardinal);
begin
  FModule[addr].CFData[0] := (config AND $000000FF);
  FModule[addr].CFData[1] := (config AND $0000FF00) div $100;
  FModule[addr].CFData[2] := (config AND $00FF0000) div $10000;
  FModule[addr].CFData[3] := (config AND $FF000000) div $1000000;
end;

// ziska konfiguraci modulu - cardinal
function TMTBusb.GetCfg(addr: TAddr): cardinal;
begin
  Result := 0;
  Result := Result OR (FModule[addr].CFData[0]);
  Result := Result OR (FModule[addr].CFData[1] * $100);
  Result := Result OR (FModule[addr].CFData[2] * $10000);
  Result := Result OR (FModule[addr].CFData[3] * $1000000);
end;

// nastavi in port na hodnotu  - pouze pro testovani
procedure TMTBusb.SetInPort(Port: TPortValue; state: boolean);
var
  X: word;
  last: word;
begin
  X := Round(Power(2,Port MOD 16));
  last := FModule[Port DIV 16].Input.value;
  if state then begin
    FModule[Port DIV 16].Input.value := FModule[Port DIV 16].Input.value OR X;
   end else begin
    FModule[Port DIV 16].Input.value := FModule[Port DIV 16].Input.value AND ($FFFF-X);
  end;
  if (last <> FModule[Port DIV 16].Input.value) then FModule[Port DIV 16].Input.changed := true;
end;

// nastavi out port na hodnotu
procedure TMTBusb.SetOutPort(Port: TPortValue; state: boolean);
var
  X: word;
  last: word;
begin
  X := Round(Power(2,Port MOD 16));
  last := FModule[Port DIV 16].Output.value;
  if state then begin
    FModule[Port DIV 16].Output.value := FModule[Port DIV 16].Output.value OR X;
   end else begin
    FModule[Port DIV 16].Output.value := FModule[Port DIV 16].Output.value AND ($FFFF-X);
  end;
  if (last <> FModule[Port DIV 16].Output.value) then FModule[Port DIV 16].Output.changed := true;
end;

// nastavi blikani pro out port
procedure TMTBusb.SetOutPortFlick(Port: TPortValue; state: TFlickType);
var
  adresa : word;
begin
  adresa := Port div 16;
  if (adresa in [1.._MTB_MAX_ADDR]) and (IsModule(adresa)) then begin
    if FPortOut[Port].flick <> state then begin
      FPortOut[Port].flick := state;
      FPortOut[Port].flickChange := True;
    end;
  end;
end;

// vrati hodnotu nastaveni blikani
function TMTBusb.GetOutPortFlick(Port: TPortValue): TFlickType;
begin
  Result := FPortOut[Port].flick;
end;

// nastavi vystupy vsech kanalu 0-15 pro dany modul (adresu)
procedure TMTBusb.SetOModule(addr: TIOaddr; state: Word);
begin
  if IsModule(addr) then begin
    if (state <> FModule[addr].Output.value) then begin
      FModule[addr].Output.changed := True;
      FModule[addr].Output.value := state;
    end;
  end;
end;

// ziska stav vystupu vsech out kanalu dane adresy modulu
function TMTBusb.GetOModule(addr: TIOaddr): word;
begin
  Result := FModule[addr].Output.value;
end;

// ziska stav vstupu dane adresy a kanalu
function TMTBusb.GetInputIO(addr : TIOaddr; channel: TIOchann): boolean;
var
  X: word;
  Port : word;
begin
  Port := addr*16+channel;
  X := Round(Power(2,Port MOD 16));
  Result := (X = (FModule[Port DIV 16].Input.value AND X));
end;

// nastavi out adresu a channel na hodnotu
procedure TMTBusb.SetOutputIO(addr : TIOaddr; channel: TIOchann; value: boolean);
var
  X: word;
  last: word;
  Port: word;
begin
  Port := addr*16+channel;
  X := Round(Power(2,Port MOD 16));
  last := FModule[Port DIV 16].Output.value;
  if value then begin
    FModule[Port DIV 16].Output.value := FModule[Port DIV 16].Output.value OR X;
   end else begin
    FModule[Port DIV 16].Output.value := FModule[Port DIV 16].Output.value AND ($FFFF-X);
  end;
  if (last <> FModule[Port DIV 16].Output.value) then FModule[Port DIV 16].Output.changed := true;
end;


// ziska stav vstupu daneho inp portu
function TMTBusb.GetInPort(Port: TPortValue): boolean;
var X: word;
begin
  X := Round(Power(2,Port MOD 16));
  //Result := (X = (FModule[Port DIV 16].Input.value AND X));
  if (X = (FModule[Port DIV 16].Input.value AND X)) then Result := true else Result :=false;
end;

// ziska stav vystupu daneho out portu
function TMTBusb.GetOutPort(Port: TPortValue): boolean;
var X: word;
begin
  X := Round(Power(2,Port MOD 16));
  if (X = (FModule[Port DIV 16].Output.value AND X)) then Result := true else Result :=false;
end;

// Vrati True, pokud prave prisel signal na vstup
function TMTBusb.GetInPortDifUp(Port: TPortValue): boolean;
begin
  If FPortIn[Port].inUp then Result := True else Result := False;
end;

// Vrati True, pokud prave zmizel signal na vstupu
function TMTBusb.GetInPortDifDn(Port: TPortValue): boolean;
begin
  If FPortIn[Port].inDn then Result := True else Result := False;
end;

// Vrati stav pretizeni daneho kanalu regulatoru
function TMTBusb.GetRegOverValue(Port: TRegChann): TPortRegOver;
begin
  Result.value := FRegOver[Port].value;
  Result.inUp  := FRegOver[Port].inUp;
end;

// vrati, zda je adresa obsazena
function TMTBusb.IsModule(addr: TAddr): boolean;
begin
  Result := False;
  if addr in [1.._MTB_MAX_ADDR] then begin
    if FModule[addr].typ <> idNone then begin
      Result := True;
    end;
  end;
end;

// vrati status modulu 19.4.2007
function TMTBusb.GetModuleStatus(addr :TAddr): byte;
begin
  Result:= FModule[addr].status;
end;

// vrati, zda in port existuje
function TMTBusb.GetExistsInPort(Port: TPortValue): boolean;
begin
  case GetModuleType(Port div 16) of
      idMTB_POT_ID: Result := False;
      idMTB_REGP_ID: Result := True;
      idMTB_UNI_ID: Result := True;
      idMTB_UNIOUT_ID: Result := False;
      idMTB_TTL_ID: Result := True;
      idMTB_TTLOUT_ID: Result := False;
      else Result := False;
  end;
end;

// modul ma konfiguracni data - true
function TMTBusb.IsModuleConfigured(addr: TAddr): boolean;
begin
  Result := False;
  if IsModule(addr) then begin
    Result := FModule[addr].Status and $FA = $40;
    //Result := not FModule[addr].Status and $2 = $2;
  end;
end;

// modul oziven po vypadku
function TMTBusb.IsModuleRevived(addr: TAddr): boolean;
begin
  Result := False;
  if IsModule(addr) then begin
    Result := FModule[addr].revived;
  end;
end;

// modul v poruse
function TMTBusb.IsModuleFailure(addr: TAddr): boolean;
begin
  Result := False;
  if IsModule(addr) then begin
    Result := FModule[addr].failure;
  end;
end;

function TMTBusb.IsModuleScanned(addr: TAddr): boolean;
begin
  Result := False;
  if IsModule(addr) then begin
    Result := FModule[addr].inputStateKnown;
  end;
end;

// vrati, zda out port existuje
function TMTBusb.GetExistsOutPort(Port: TPortValue): boolean;
begin
  case GetModuleType(Port div 16) of
      idMTB_POT_ID: Result := False;
      idMTB_REGP_ID: Result := False;
      idMTB_UNI_ID: Result := True;
      idMTB_UNIOUT_ID: Result := True;
      idMTB_TTL_ID: Result := True;
      idMTB_TTLOUT_ID: Result := True;
      else Result := False;
  end;
end;

function TMTBusb.GetModuleType(addr: TAddr): TModulType; // Typ modulu
begin
  result := idNone;
  if GetModuleInfo(addr).ID <> 0 then begin
    case GetModuleInfo(addr).typ of
      idMTB_POT_ID: Result := idMTB_POT_ID;
      idMTB_REGP_ID: Result := idMTB_REGP_ID;
      idMTB_UNI_ID: Result := idMTB_UNI_ID;
      idMTB_UNIOUT_ID: Result := idMTB_UNIOUT_ID;
      idMTB_TTL_ID: Result := idMTB_TTL_ID;
      idMTB_TTLOUT_ID: Result := idMTB_TTLOUT_ID;
      else Result := idNone;
    end;
  end;
end;

// nastavi rychlost MTB-REG
procedure TMTBusb.SetRegSpeed(Rchan: TRegChann; Rspeed: TRegSpeed; Rdirect: TRegDirect; AccTime: TRegAcctime);
var
  adresa : byte;
  mdata: Array[0..7] of byte;
begin
  if AccTime > 20 then AccTime := 20;
  adresa := (Rchan DIV 8)+ _REG_ADDR;
  if (IsModule(adresa)) then begin
    if (GetModuleType(adresa) = idMTB_REGP_ID) then begin
      // kontrola zmeny parametru
      if (FReg[Rchan].RegSpeed <> Rspeed) or (FReg[Rchan].RegDirect <> Rdirect) then begin
        // odeslat data pro REG
        FReg[Rchan].RegSpeed := Rspeed;
        FReg[Rchan].RegDirect := Rdirect;
        mdata[0] := adresa;
        mdata[1] := __SET_REG_U;
        mdata[2] := (Rchan shl 5) + (Rdirect shl 4) + Rspeed;
        mdata[3] := AccTime;
        MT_send_packet(mdata, 4);
      end else begin
        // parametry beze zmen

      end;
    end else begin
      // adresa neobsahuje REG

    end;
  end else begin
    // adresa neni obsazena
    
  end;
end;

// vrati hodnoty regulatoru (odeslane???)
function TMTBusb.GetRegSpeed(Rchan: TRegChann): TReg;
begin
  Result.RegSpeed := FReg[Rchan].RegSpeed;
  Result.RegDirect := FReg[Rchan].RegDirect;
  Result.RegAcctime := FReg[Rchan].RegAcctime;
end;

// vrati informace o modulu v Tmodule
function TMTBusb.GetModuleInfo(addr: TAddr): TModule;
begin
  Result := FModule[addr];
end;


// vrati informace o modulu v Tmodule
function TMTBusb.GetModuleCfg(addr: TAddr): TModulConfigGet;
begin
  RdCfgdata.CFGnum := GetModuleInfo(addr).CFGnum;
  RdCfgdata.CFGdata[0] := GetModuleInfo(addr).CFdata[0];
  RdCfgdata.CFGdata[1] := GetModuleInfo(addr).CFdata[1];
  RdCfgdata.CFGdata[2] := GetModuleInfo(addr).CFdata[2];
  RdCfgdata.CFGdata[3] := GetModuleInfo(addr).CFdata[3];
  RdCfgdata.CFGfw := GetModuleInfo(addr).firmware;
  RdCfgdata.CFGpopis := GetModuleInfo(addr).popis;
  Result := RdCfgdata;
end;

// Vlozi cfg data z pole FSetCfg[]
procedure TMTBusb.SetModuleCfg(addr: TAddr);
begin
  FModule[addr].CFData[0] := WrCfgData.CFGdata[0];
  FModule[addr].CFData[1] := WrCfgData.CFGdata[1];
  FModule[addr].CFData[2] := WrCfgData.CFGdata[2];
  FModule[addr].CFData[3] := WrCfgData.CFGdata[3];
  FModule[addr].popis := WrCfgData.CFGpopis;
end;

// zaradi nebo vyradi modul z cinnosti - nutno pred spustenim komunikace
procedure TMTBusb.SetModuleEnable(addr: TIOaddr; enable:Boolean); // On/off modulu
begin
  if enable then FModule[addr].setting := FModule[addr].setting or 1
    else FModule[addr].setting := FModule[addr].setting and ($FF-1);
end;

// zjisti, zda je modul zaøazen nebo vyøazen
function TMTBusb.GetModuleEnable(addr: TIOaddr): boolean;
begin
  if (FModule[addr].setting and 1)=1 then Result := True
    else Result := False;
end;

// vrátí název modul dle typu
function TMTBusb.GetModuleTypeName(addr: TAddr): string;
begin
  Result := '';
  if GetModuleInfo(addr).ID <> 0 then begin
    case GetModuleInfo(addr).typ of
      idMTB_POT_ID: Result := 'MTB-POT';
      idMTB_REGP_ID: Result := 'MTB-REG puls';
      idMTB_UNI_ID: Result := 'MTB-UNI';
      idMTB_UNIOUT_ID: Result := 'MTB-UNI out';
      idMTB_TTL_ID: Result := 'MTB-TTL';
      idMTB_TTLOUT_ID: Result := 'MTB-TTL out';
      else Result := 'Neznamý modul';
    end;
  end;
end;

// vrati rychlost v str
function TMTBusb.GetSpeedStr: Cardinal;
begin
  Case FSpeed of
    sp38400: Result := 38400;
    sp57600: Result := 57600;
    sp115200: Result := 115200;
    else Result := 0;
  end;
end;

// nastavi rychlost kontroly prijatych dat - timer
procedure TMTBusb.SetScanInterval(interval: TTimerInterval);
begin
  // Kontrola rychlosti
  FScanInterval := interval;
  case interval of
    ti50: FCmdSecCycleNum := 20;
    ti100: FCmdSecCycleNum := 10;
    ti200: FCmdSecCycleNum := 5;
    ti250: FCmdSecCycleNum := 4;
  end;
  FTimer.Interval := ord(FScanInterval);
end;

// pozadavek na zaslani FB data
procedure TMTBusb.GetFbData;
begin
  if FScanning then begin
    FT_Out_Buffer[0] := _USB_SET + 1;
    FT_Out_Buffer[1] := 5;
    LogDataOut(2);
    Write_USB_Device_Buffer(2);
  end;
end;

// pozadavek na zaslani CMD count -pocet prikazu
procedure TMTBusb.GetCmdNum;
begin
  if FScanning then begin
    FT_Out_Buffer[0] := _USB_SET + 1;
    FT_Out_Buffer[1] := 21;
    LogDataOut(2);
    Write_USB_Device_Buffer(2);
  end;
end;

// Nastavi hodnotu poctu cyklu pro refresh FB dat do PC
procedure TMTBusb.SetCyclesData(value: Byte);
begin
  FCyclesFbData := 0;
  if value > 25 then FCyclesFbData := 25
  else FCyclesFbData := value;
end;

// Odesle hodnotu poctu cyklu pro refresh FB dat do PC
procedure TMTBusb.SendCyclesData;
begin
  if FOpenned then begin
    FT_Out_Buffer[0] := _USB_SET + 2;
    FT_Out_Buffer[1] := 31;
    FT_Out_Buffer[2] := FCyclesFbData * 10;
    LogWrite(llCmd, 'Cycles FB Data');
    LogDataOut(3);
    Write_USB_Device_Buffer(3); // Nastavi CyclesData
  end;
end;

// Ulozit data do pameti
procedure TMTBusb.XMemWr(mAddr: word; mData : Byte);
begin
  if FOpenned then begin
    FT_Out_Buffer[0] := _USB_SET + 4;
    FT_Out_Buffer[1] := 10;
    FT_Out_Buffer[2] := Hi(mAddr);
    FT_Out_Buffer[3] := Lo(mAddr);
    FT_Out_Buffer[4] := mData;
    LogDataOut(5);
    Write_USB_Device_Buffer(5); // Data do XRAM
  end;
end;

// Nacist data z pameti
procedure TMTBusb.XMemRd(mAddr: word);
begin
  if FOpenned then begin
    FT_Out_Buffer[0] := _USB_SET + 3;
    FT_Out_Buffer[1] := 11;
    FT_Out_Buffer[2] := Hi(mAddr);
    FT_Out_Buffer[3] := Lo(mAddr);
    LogDataOut(4);
    Write_USB_Device_Buffer(4); // Data do XRAM
  end;
end;

// Scan sbernice, zjisteni pripojenych modulu
procedure TMTBusb.MtbScan(Sender: TObject);
var
  i,j,channel,x: word;
  adresa: byte;
  port : TPortValue;
  changed: boolean;
  odpoved: boolean;
  PortStatus: FT_Result;
  paketNum, datNum: byte;
  mdata: Array[0..7] of byte;
  potValue : TPotValue;
  potDirect :TPotDirect;
  inDataNew1 : word;
  inDataOld1 : word;
  pocetModulu : Word;
  errId : Word;
begin
  try
    odpoved := false;
    Get_USB_Device_QueueStatus;

    // open
    if (FScan_flag) then begin
      while (FT_Q_Bytes>=8) do begin
        Read_USB_Device_Buffer(8);
        if FT_In_Buffer[7] = _END_BYTE then begin

          Self.LogDataIn(8);

          case (FT_In_Buffer[0]) and $F0 of
            _MTB_ID: begin
              if (FModuleCount = 256) then begin
                // something went wrong
                FModuleCount := 0;
                WriteError(MTB_INVALID_MODULES_COUNT, _DEFAULT_ERR_ADDR);
                LogWrite(llError, 'MTB-USB deska odpovídá opakovaným nalezením modulù!');
                Self.Close();
              end;

              Inc(FModuleCount);
              adresa := FT_In_Buffer[1];
              FModule[adresa].ID := FT_In_Buffer[2];
              FModule[adresa].firmware := IntToHex(FT_In_Buffer[3],2);
              FModule[adresa].Status := FT_In_Buffer[4];
              FModule[adresa].Sum := FT_In_Buffer[5];
              FModule[adresa].available := true;
              case FModule[adresa].ID and $F0 of
                MTB_POT_ID:FModule[adresa].typ :=  idMTB_POT_ID;
                MTB_REGP_ID: FModule[adresa].typ := idMTB_REGP_ID;
                MTB_UNI_ID: FModule[adresa].typ := idMTB_UNI_ID;
                MTB_UNIOUT_ID: FModule[adresa].typ := idMTB_UNIOUT_ID;
                MTB_TTL_ID: FModule[adresa].typ := idMTB_TTL_ID;
                MTB_TTLOUT_ID: FModule[adresa].typ := idMTB_TTLOUT_ID;
                else FModule[adresa].typ := idNone;
              end;
              FModule[adresa].CFGnum := FModule[adresa].ID and $0F;
              LogWrite(llChange, 'Nalezen modul - adresa: '+ IntToStr(adresa) + ' - ' + GetModuleTypeName(adresa)
                 + ' FW: ' + FModule[adresa].firmware);
            end;

            _MTB_SEARCH_END: begin
              pocetModulu := FT_In_Buffer[1];
              FScan_flag := false;
              FSeznam_flag := True;
              FOpenned := True;
              LogWrite(llChange, 'Ukonèeno hledání modulù');
              if FModuleCount <> pocetModulu then begin
                FModuleCount := 0;
                WriteError(MTB_INVALID_MODULES_COUNT, _DEFAULT_ERR_ADDR);
                LogWrite(llError, 'Nespravný poèet nalezených modulù');
                Self.Close();
              end else
                if Assigned(AfterOpen) then AfterOpen(Self);
            end;

            _RST_OK: begin

            end;

            _USB_CFGQ: begin
              case FT_In_Buffer[2] of
                1:begin
                  FHWVersion_Major := FT_In_Buffer[3];
                  FHWVersion_Minor := FT_In_Buffer[4];
                  FHWVersion_Release := FT_In_Buffer[5];
                  FHWVersion := IntToStr(FHWVersion_Major)+'.'+IntToStr(FHWVersion_Minor)+'.'+IntToStr(FHWVersion_Release);
                  FHWVersionInt := StrToInt(Format('%d%.2d%.2d',[FHWVersion_Major, FHWVersion_Minor, FHWVersion_Release]));
                  LogWrite(llChange, 'Verze FW: '+FHWVersion);
                end;
                11:begin
                  FXRamAddr := (FT_In_Buffer[3]*256)+FT_In_Buffer[4];
                  FXRamValue := FT_In_Buffer[5];
                  LogWrite(llChange, 'XRamRD: '+IntToStr(FXRamAddr)+':'+IntToStr(FXRamValue));
                end;
              end;
            end;
            _ERR_CODE: begin
              FErrAddress := (FT_In_Buffer[1]);
              errId := (FT_In_Buffer[2]);
              Self.WriteError(3000+errId, FErrAddress);
              Self.LogWrite(llError, 'Chyba '+intToStr(errId)+ ' modulu '+IntToStr(FErrAddress)+': '+Self.GetErrString(errId));
            end;
          end;
          Get_USB_Device_QueueStatus;
        end else begin // kontrola paketu
          FDataInputErr := True; // prisel chybný paket
            LogWrite(llError, 'Chyba pøijmutého paketu: '+IntToHex(FT_In_Buffer[0],2)
               +' '+IntToHex(FT_In_Buffer[1],2)+' '+IntToHex(FT_In_Buffer[2],2)
               +' '+IntToHex(FT_In_Buffer[3],2)+' '+IntToHex(FT_In_Buffer[4],2)
               +' '+IntToHex(FT_In_Buffer[5],2)+' '+IntToHex(FT_In_Buffer[6],2)
               +' '+IntToHex(FT_In_Buffer[7],2));
        end;
      end;  // while
      if FDataInputErr then begin
        FScan_flag := false;
        LogWrite(llError, 'Chyba paketu pri scan modulu');
        FOpenned := False;
        FScan_flag := False;
        Purge_USB_Device_Out;
        // ?? vymazat buffer???
        FDataInputErr := false;
      end;
    end;
  // #########################################################################
    // spustena komunikace
    if (FScanning) then begin
      FDataInputErr := False;
      if FCmdCounter_flag then FCmdCounter_flag := False;
      if FInputChanged then begin
        for i := 0 to _PORT_MAX_NUM do begin
          FPortIn[i].inUp := false;
          FPortIn[i].inDn := false;
        end;
      end;
      FInputChanged := False;
      if FOverChanged then begin
        for i := 0 to _PORTOVER_MAX_NUM do begin
          FRegOver[i].inUp := false;
        end;
      end;
      FOverChanged := False;
      Get_USB_Device_QueueStatus;
      if (FT_Q_Bytes >= 8) then begin
        paketNum := FT_Q_Bytes div 8;
        Read_USB_Device_Buffer(paketNum * 8);
        for i := 0 to paketNum-1 do begin
          if FT_In_Buffer[i*8+7] = _END_BYTE then begin

            if ((FT_In_Buffer[i*8] and $F0) <> _USB_CFGQ) and (FT_In_Buffer[i*8+2] <> 21) then begin
              LogDataIn(8, i*8);
            end;

            case (FT_In_Buffer[i*8] and $F0) of
              _USB_CFGQ: begin
                case FT_In_Buffer[i*8+2] of
                  1:begin   // verze FW
                    FHWVersion_Major := FT_In_Buffer[i*8+3];
                    FHWVersion_Minor := FT_In_Buffer[i*8+4];
                    FHWVersion_Release := FT_In_Buffer[i*8+5];
                    FHWVersion := IntToStr(FHWVersion_Major)+'.'+IntToStr(FHWVersion_Minor)+'.'+IntToStr(FHWVersion_Release);
                    LogWrite(llChange, 'Verze FW: '+FHWVersion);
                    LogWrite(llRawCmd, '_USB_CFGQ: '+' - '+ IntToHex(FT_In_Buffer[i*8],2)+' '+ IntToHex(FT_In_Buffer[i*8+1],2)+' '+IntToHex(FT_In_Buffer[i*8+2],2)
                      +' '+IntToHex(FT_In_Buffer[i*8+3],2)+' '+IntToHex(FT_In_Buffer[i*8+4],2)+' '+IntToHex(FT_In_Buffer[i*8+5],2)
                      +' '+IntToHex(FT_In_Buffer[i*8+6],2)+' '+IntToHex(FT_In_Buffer[i*8+7],2));
                  end;
                  21:begin  // Pocet prikazu
                    FCmdCount := FT_In_Buffer[i*8+4] OR FT_In_Buffer[i*8+3] * $100;
                    FCmdCounter_flag := True;
                  end else begin
                    LogWrite(llRawCmd, '_USB_CFGQ ?: '+' - '+ IntToHex(FT_In_Buffer[i*8],2)+' '+ IntToHex(FT_In_Buffer[i*8+1],2)+' '+IntToHex(FT_In_Buffer[i*8+2],2)
                      +' '+IntToHex(FT_In_Buffer[i*8+3],2)+' '+IntToHex(FT_In_Buffer[i*8+4],2)+' '+IntToHex(FT_In_Buffer[i*8+5],2)
                      +' '+IntToHex(FT_In_Buffer[i*8+6],2)+' '+IntToHex(FT_In_Buffer[i*8+7],2));
                  end;
                end;
                // ostatni navratove hodnoty doplnit pozdeji
                //Beep;
              end;
              _MTB_DATA: begin
                datNum := FT_In_Buffer[i*8] and $0F;
                adresa := FT_In_Buffer[i*8+1];
                mdata[0] := FT_In_Buffer[i*8+2];
                mdata[1] := FT_In_Buffer[i*8+3];
                mdata[2] := FT_In_Buffer[i*8+4];
                mdata[3] := FT_In_Buffer[i*8+5];

                FModule[adresa].Sum := FT_In_Buffer[i*8+datNum];
                FModule[adresa].Status := FT_In_Buffer[i*8+datNum-1];
                if (datNum>3) then begin
                  case (FModule[adresa].typ) of
                    // vyhodnoceni prijatych dat
                    // moduly I/O
                    idMTB_UNI_ID,idMTB_TTL_ID: begin
                      inDataOld1 := FModule[adresa].Input.value;
                      FModule[adresa].inputStateKnown := true;
                      Self.CheckAllScannedEvent();
                      inDataNew1 := FT_In_Buffer[i*8+2] OR FT_In_Buffer[i*8+3] * $100;
                      if inDataNew1 <> inDataOld1 then begin
                        FModule[adresa].Input.changed := true;
                        for j := 0 to 15 do begin
                          x := Round(Power(2,j));
                          if (inDataOld1 and x) <> (inDataNew1 and x) then begin
                            if (inDataNew1 and x) = 0 then begin
                              // 1 -> 0
                              FPortIn[adresa*16+j].value :=  False;
                              FPortIn[adresa*16+j].inUp := False;
                              FPortIn[adresa*16+j].inDn  :=  True;
                            end else begin
                              // 0 -> 1
                              FPortIn[adresa*16+j].value :=  True;
                              FPortIn[adresa*16+j].inUp := True;
                              FPortIn[adresa*16+j].inDn  :=  False;
                            end;
                          end;
                        end;
                        FModule[adresa].Input.value := inDataNew1;
                      end;
                    end;
                    // potenciometry
                    idMTB_POT_ID: begin
                      // channel n+0
                      channel := (adresa - _POT_ADDR)*4; // vypocita channel
                      potValue := (mdata[0] and 15);
                      potDirect:= (mdata[2] and 1);
                      if (FPot[channel].PotValue <> potValue) or (FPot[channel].PotDirect <> potDirect) then begin
                        FPot[channel].PotValue := potValue;
                        FPot[channel].PotDirect:= potDirect;
                        FModule[adresa].Input.changed := true;
                      end;
                      // channel n+1
                      Inc(channel);
                      potValue := (mdata[0] shr 4)and 15;
                      potDirect:= (mdata[2] shr 1)and 1;
                      if (FPot[channel].PotValue <> potValue) or (FPot[channel].PotDirect <> potDirect) then begin
                        FPot[channel].PotValue := potValue;
                        FPot[channel].PotDirect:= potDirect;
                        FModule[adresa].Input.changed := true;
                      end;
                      // channel n+2
                      Inc(channel);
                      potValue := (mdata[1] and 15);
                      potDirect:= (mdata[2] shr 2)and 1;
                      if (FPot[channel].PotValue <> potValue) or (FPot[channel].PotDirect <> potDirect) then begin
                        FPot[channel].PotValue := potValue;
                        FPot[channel].PotDirect:= potDirect;
                        FModule[adresa].Input.changed := true;
                      end;
                      // channel n+3
                      Inc(channel);
                      potValue := (mdata[1] shr 4)and 15;
                      potDirect:= (mdata[2] shr 3)and 1;
                      if (FPot[channel].PotValue <> potValue) or (FPot[channel].PotDirect <> potDirect) then begin
                        FPot[channel].PotValue := potValue;
                        FPot[channel].PotDirect:= potDirect;
                        FModule[adresa].Input.changed := true;
                      end;
                    end;
                    // regulatory
                    idMTB_REGP_ID: begin
                      channel := (adresa - _REG_ADDR)*8;
                      inDataOld1 := FModule[adresa].Input.value;
                      inDataNew1 := FT_In_Buffer[i*8+2] OR FT_In_Buffer[i*8+3] * $100;
                      if inDataNew1 <> inDataOld1 then begin
                        FModule[adresa].Input.changed := true;
                        for j := 0 to 7 do begin
                          x := Round(Power(2,j));
                          if (inDataOld1 and x) <> (inDataNew1 and x) then begin
                            if (inDataNew1 and x) = 0 then begin
                              // 1 -> 0
                              FRegOver[channel+j].value :=  False;
                              FRegOver[channel+j].inUp := False;
                            end else begin
                              // 0 -> 1
                              FRegOver[channel+j].value :=  True;
                              FRegOver[channel+j].inUp := True;
                            end;
                          end;
                        end;
                        FModule[adresa].Input.value := inDataNew1
                      end;
                    end;
                  end;  // case module typ
                end else
                if datNum = 3 then begin   // upravit po zmene FW
                  // jen odpoved po prikazu (adresa, status a SUM)
                  odpoved := true;
                  // doplnit kontrolu statusu
                end else begin  // doslo mene nez 3 data
                  LogWrite(llCmd, 'Status <3B addr: '+IntToStr(adresa)+'  status: '+IntToStr(FModule[adresa].Status));
                end;
              end;
              _ERR_CODE: begin
                FErrAddress := (FT_In_Buffer[(i*8)+1]);
                errId := (FT_In_Buffer[(i*8)+2]);
                Self.WriteError(3000+errId, FErrAddress);
                Self.LogWrite(llError, 'Chyba '+intToStr(errId)+ ' modulu '+IntToStr(FErrAddress)+': '+Self.GetErrString(errId));
                case errId of
                  141:begin // modul nekomunikuje
                    FModule[FErrAddress].revived := False;
                    FModule[FErrAddress].failure := True;
                    FModule[FErrAddress].Status := 0;
                    FModule[FErrAddress].inputStateKnown := false;

                    // nastaveni null hodnot do mudulu, ktere jsou v poruse
                    adresa := FErrAddress;
                    case (FModule[adresa].typ) of
                      // vyhodnoceni prijatych dat
                      // moduly I/O
                      idMTB_UNI_ID,idMTB_TTL_ID: begin
                        inDataOld1 := FModule[adresa].Input.value;
                        //inDataNew1 := FT_In_Buffer[i*8+2] OR FT_In_Buffer[i*8+3] * $100;
                        inDataNew1 := 0;
                        if inDataNew1 <> inDataOld1 then begin
                          FModule[adresa].Input.changed := true;
                          for j := 0 to 15 do begin
                            x := Round(Power(2,j));
                            if (inDataOld1 and x) <> (inDataNew1 and x) then begin
                              if (inDataNew1 and x) = 0 then begin
                                // 1 -> 0
                                FPortIn[adresa*16+j].value :=  False;
                                FPortIn[adresa*16+j].inUp := False;
                                FPortIn[adresa*16+j].inDn  :=  True;
                              end else begin
                                // 0 -> 1
                                FPortIn[adresa*16+j].value :=  True;
                                FPortIn[adresa*16+j].inUp := True;
                                FPortIn[adresa*16+j].inDn  :=  False;
                              end;
                            end;
                          end;
                          FModule[adresa].Input.value := inDataNew1
                        end;
                      end;
                      // potenciometry
                      idMTB_POT_ID: begin
                        // channel n+0
                        channel := (adresa - _POT_ADDR)*4; // vypocita channel
                        potValue := 0;
                        potDirect:= 0;
                        if (FPot[channel].PotValue <> potValue) or (FPot[channel].PotDirect <> potDirect) then begin
                          FPot[channel].PotValue := potValue;
                          FPot[channel].PotDirect:= potDirect;
                          FModule[adresa].Input.changed := true;
                        end;
                        // channel n+1
                        Inc(channel);
                        potValue := 0;
                        potDirect:= 0;
                        if (FPot[channel].PotValue <> potValue) or (FPot[channel].PotDirect <> potDirect) then begin
                          FPot[channel].PotValue := potValue;
                          FPot[channel].PotDirect:= potDirect;
                          FModule[adresa].Input.changed := true;
                        end;
                        // channel n+2
                        Inc(channel);
                        potValue := 0;
                        potDirect:= 0;
                        if (FPot[channel].PotValue <> potValue) or (FPot[channel].PotDirect <> potDirect) then begin
                          FPot[channel].PotValue := potValue;
                          FPot[channel].PotDirect:= potDirect;
                          FModule[adresa].Input.changed := true;
                        end;
                        // channel n+3
                        Inc(channel);
                        potValue := 0;
                        potDirect:= 0;
                        if (FPot[channel].PotValue <> potValue) or (FPot[channel].PotDirect <> potDirect) then begin
                          FPot[channel].PotValue := potValue;
                          FPot[channel].PotDirect:= potDirect;
                          FModule[adresa].Input.changed := true;
                        end;
                      end;
                      // regulatory - overit funkcnost
                      idMTB_REGP_ID: begin
                        channel := (adresa - _REG_ADDR)*8;
                        inDataOld1 := FModule[adresa].Input.value;
                        inDataNew1 := 0;
                        if inDataNew1 <> inDataOld1 then begin
                          FModule[adresa].Input.changed := true;
                          for j := 0 to 7 do begin
                            x := Round(Power(2,j));
                            if (inDataOld1 and x) <> (inDataNew1 and x) then begin
                              if (inDataNew1 and x) = 0 then begin
                                // 1 -> 0
                                FRegOver[channel+j].value :=  False;
                                FRegOver[channel+j].inUp := False;
                              end else begin
                                // 0 -> 1
                                FRegOver[channel+j].value :=  True;
                                FRegOver[channel+j].inUp := True;
                              end;
                            end;
                          end;
                          FModule[adresa].Input.value := inDataNew1
                        end;
                      end;
                    end;  // case module typ



                  end;
                  142:begin // modul oziven
                    FModule[FErrAddress].revived := True;
                  end;
                end;
                if FHWVersionInt = 905 then begin
                  case errId of
                    11:errId := 101;
                     1:errId := 102;
                     2:errId := 103;
                    else errId := 200;
                  end;
                  Self.WriteError(3000+errId, FErrAddress);
                end;
              end;
            end; // case
          end else begin
            FDataInputErr := True;
            LogWrite(llError, 'Chyba pøijmutého paketu: '+IntToHex(FT_In_Buffer[i*8+0],2)
               +' '+IntToHex(FT_In_Buffer[i*8+1],2)+' '+IntToHex(FT_In_Buffer[i*8+2],2)
               +' '+IntToHex(FT_In_Buffer[i*8+3],2)+' '+IntToHex(FT_In_Buffer[i*8+4],2)
               +' '+IntToHex(FT_In_Buffer[i*8+5],2)+' '+IntToHex(FT_In_Buffer[i*8+6],2)
               +' '+IntToHex(FT_In_Buffer[i*8+7],2));
          end;  // if kontrola posledniho byte paketu
        end;
      end;
      // priprava pro odeslani dat pri oziveni modulu
      for i := 1 to _ADDR_MAX_NUM do begin
        if IsModule(i) then begin
          if IsModuleRevived(i) then begin
            if IsModuleConfigured(i) then begin
              case FModule[i].typ of
                idMTB_UNI_ID,idMTB_UNIOUT_ID,idMTB_TTL_ID,idMTB_TTLOUT_ID:begin
                  LogWrite(llCmd, 'obnovit data na vystup adr:'+IntToStr(i));
                  FModule[i].revived := False;
                  FModule[i].failure := False;
                  // uni - vystup
                  FModule[i].Output.changed := True;
                  // uni - blikani vystupu
                  for j := 0 to 15 do begin
                    port := GetPortNum(i,j);
                    if FPortOut[port].flick in [fl33, fl55] then FPortOut[port].flickChange := True;
                  end;
                  // Scom
                  for j := 0 to 7 do begin
                    if FModule[i].Scom.ScomActive[j] then FModule[i].Scom.changed[j] := True;
                  end;
                end;
              end;
            end else begin
              // poslat idle - ziskat config
              mdata[0] := i;
              mdata[1] := __IDLE;
              MT_send_packet(mdata, 2);
            end;
          end;
        end;
      end;

      // odeslani pri zmene dat na vystupu
      for i := 1 to _MTB_MAX_ADDR do begin
        if IsModule(i) then begin
          if FModule[i].Output.changed then begin
            case (FModule[i].ID AND $F0) of
              MTB_UNI_ID, MTB_UNIOUT_ID, MTB_TTL_ID, MTB_TTLOUT_ID: begin
                mdata[0] := i;
                mdata[1] := __SET_OUTW + 2;
                mdata[2] := Lo(FModule[i].Output.value);
                mdata[3] := Hi(FModule[i].Output.value);
                MT_send_packet(mdata, 4);
                FModule[i].Output.changed := false;
                if (Assigned(OnOutputChange)) then OnOutputChange(Self, i);
              end;
            end;
          end;
        end;
      end;
      if FCmdCountCounter > 1 then begin
        dec(FCmdCountCounter)
      end else begin
        FCmdCountCounter := FCmdSecCycleNum;
        GetCmdNum;
      end;

      // nastaveni blikani vystupu
      for i := 0 to _PORT_MAX_NUM do begin
        if FModule[i div 16].typ in [idMTB_UNI_ID,idMTB_TTL_ID,idMTB_UNIOUT_ID,idMTB_TTLOUT_ID] then begin
          if FPortOut[i].flickChange then begin
            mdata[0] := (i div 16);
            mdata[1] := __FLICK_OUT;
            channel := GetChannel(i);
            mdata[2] := (channel and 7) or ((channel and 8)shl 1);
            case FPortOut[i].flick of
              fl33:mdata[2] := mdata[2] or $20;
              fl55:mdata[2] := mdata[2] or $40;
              else mdata[2] := mdata[2] or $0;
            end;
            MT_send_packet(mdata, 3);
            FPortOut[i].flickChange := False;
          end;
        end;
      end;

      // Scom data
      for i := 1 to _ADDR_MAX_NUM do begin
        if IsModule(i) then begin
          if FModule[i].typ in [idMTB_UNI_ID,idMTB_TTL_ID,idMTB_UNIOUT_ID,idMTB_TTLOUT_ID] then begin
            for j := 0 to 7 do begin
              if (FModule[i].Scom.ScomActive[j]) and (FModule[i].Scom.changed[j]) then begin
                mdata[0] := i;               // adresa
                mdata[1] := __SET_NAVEST;    // CMD
                mdata[2] := j;               // port
                mdata[3] := FModule[i].Scom.ScomCode[j];    // Scom code
                MT_send_packet(mdata, 4);
                FModule[i].Scom.changed[j] := False;
                if (Assigned(OnOutputChange)) then OnOutputChange(Self, i);
              end;
            end;
          end;
        end;
      end;

      // kontrola vstupu
      changed := false;
      FPotChanged := false;
      FOverChanged := false;
      FInputChanged := false;
      for i := 1 to _MTB_MAX_ADDR do begin
        if IsModule(i) then begin
          if (FModule[i].Input.changed) then begin
            changed := true;
            Case FModule[i].typ of
              idMTB_UNI_ID,idMTB_TTL_ID : FInputChanged := True;              // Nastavi pri zmene na vstupech I/O
              idMTB_POT_ID              : FPotChanged   := True;              // Nastavi pri zmene POT
              idMTB_REGP_ID             : FOverChanged  := True;              // Nastavi pri pretizeni REG
            end;
            FModule[i].Input.changed := false;
            if (Assigned(OnInputChange)) then OnInputChange(Self, i);
            if ((FModule[i].failure) and (Assigned(OnOutputChange))) then OnOutputChange(Self, i);
          end;
        end;
      end;

      if FCmdCounter_flag then changed := True;
      if FDataInputErr then begin
        Get_USB_Device_QueueStatus;
        if (FT_Q_Bytes > 0) then begin
         x := FT_Q_Bytes;
         Read_USB_Device_Buffer(x);
        end;
        GetFbData;  // znovu zaslat FB data
        Self.WriteError(MTB_INVALID_PACKET, _DEFAULT_ERR_ADDR);
        LogWrite(llError, 'Neplatný paket!');
      end;
      if odpoved then changed := True;
      // Pøi zmìnì vyvolá událost
      if (changed AND Assigned(OnChange)) then OnChange(Self);
    end; // FScaning

    if Assigned(OnScan) then OnScan(Self);
  except
   on E:EFtGeneral do
    begin
      Self.LogWrite(llError, 'EXCEPTION (mtbScan): '+e.ClassName+' - '+e.Message);
      Self.WriteError(MTB_FT_EXCEPTION, _DEFAULT_ERR_ADDR);
      LogWrite(llError, 'FTDI výjimka!');
      try
        if FScanning then Stop;
      except

      end;

      try
        Close
      except

      end;
    end;
   on e:Exception do
     Self.LogWrite(llError, 'EXCEPTION (mtbScan): '+e.ClassName+' - '+e.Message);
  end;
end;

// otevre zarizeni
procedure TMTBusb.Open(serial_num: String);
var
  i: word;
begin
  if ((FOpenned) or (FScan_flag)) then raise EAlreadyOpened.Create('MTB already opened');

  LogWrite(llCmd, 'Otevírám zaøízení ' + UsbSerial);
  if Assigned(BeforeOpen) then BeforeOpen(Self);

  try
    Open_USB_Device_By_Serial_Number(serial_num);
  except
    on E:EFtDeviceNotFound do
      raise ECannotOpenPort.Create('Cannot open port');
    on E:Exception do begin
      Self.LogWrite(llError, 'Open exception: '+E.ClassName+' - '+E.Message);
      raise;
    end;
  end;

  try
    // Nulovat poc. hodnoty
    FModuleCount := 0;
    for i:= 1 to _MTB_MAX_ADDR do begin
      FModule[i].CFGnum := 0;
      FModule[i].ID := 0;
      FModule[i].typ := idNone;
      FModule[i].available := false;
      FModule[i].firmware := '';
    end;

    FTimer.Enabled := true;

    // set FIFO timeout
    Set_USB_Device_TimeOuts(20,200);
    Purge_USB_Device_Out;

    // get FW version
    FT_Out_Buffer[0] := _USB_SET + 1;
    FT_Out_Buffer[1] := 1;
    Self.LogDataOut(2);
    Write_USB_Device_Buffer(2);

    FScan_flag := true;
    FDataInputErr := false;

    // begin scanning
    FT_Out_Buffer[0]:= _SCAN_MTB;
    Self.LogDataOut(1);
    Write_USB_Device_Buffer(1);

    LogWrite(llCmd, 'Otevøeno zaøízení ' + UsbSerial);
    LogWrite(llCmd, 'Library v' + Version.GetLibVersion());
  except
   on E:Exception do begin
     Self.LogWrite(llError, 'Open exception, closing: '+E.ClassName+' - '+E.Message);
     Self.Close();
     raise;
    end;
  end;
end;

// uzavre zarizeni
procedure TMTBusb.Close();
var i:Integer;
begin
 try
  if Assigned(BeforeClose) then BeforeClose(Self);

  // Reset MTB-USB
  try
    FT_Out_Buffer[0] := _USB_RST;
    LogDataOut(1);
    Write_USB_Device_Buffer(1);
  except

  end;

  Close_USB_Device;
  FTimer.Enabled := false;
  FOpenned := false;
  FScanning := false;
  FWaitingOnScan := false;
  LogWrite(llCmd, 'Uzavøení zaøízení');

  for i:= 1 to _MTB_MAX_ADDR do begin
    FModule[i].Status := 0;
    FModule[i].CFGnum := 0;
    FModule[i].ID := 0;
    FModule[i].typ := idNone;
    FModule[i].available := false;
    FModule[i].firmware := '';
  end;

  if Assigned(AfterClose) then AfterClose(Self);
 except
   on E:Exception do begin
     Self.LogWrite(llError, 'Close exception: '+E.ClassName+' - '+E.Message);
     raise;
    end;
 end;
end;

// spusti komunikaci
procedure TMTBusb.Start();
var
  i: word;
begin
  if (not FOpenned) then raise ENotOpened.Create('Device not opened, cannot start!');
  if (FScanning) then raise EAlreadyStarted.Create('Scanning already started!');
  if (FHWVersionInt < MIN_FW_VERSION) then raise EFirmwareTooLog.Create('FW < 0.9.20, cannot start!');
  if (FModuleCount = 0) then raise ENoModules.Create('No modules found on bus, cannot start!');

  if (Assigned(BeforeStart)) then BeforeStart(Self);

  try
    Purge_USB_Device_Out; // vyprazdnit FIFO out

    Self.ResetPortsStatus();

    FT_Out_Buffer[0] := _MTB_SET + 1;
    FT_Out_Buffer[1] := 0;
    LogDataOut(2);
    Write_USB_Device_Buffer(2); // Reset XRAM

    // send configuration to modules
    for i := 1 to _MTB_MAX_ADDR do begin
      if (FModule[i].available) then begin
        FT_Out_Buffer[0] := _MTB_SET + 7;
        FT_Out_Buffer[1] := i;
        FT_Out_Buffer[2] := FModule[i].ID;
        FT_Out_Buffer[3] := FModule[i].setting;  // Bit 0 ==> používat modul?
        FT_Out_Buffer[4] := FModule[i].CFData[0];
        FT_Out_Buffer[5] := FModule[i].CFData[1];
        FT_Out_Buffer[6] := FModule[i].CFData[2];
        FT_Out_Buffer[7] := FModule[i].CFData[3];
        LogDataOut(8);
        Write_USB_Device_Buffer(8);
      end;
    end;

    SendCyclesData;

    FT_Out_Buffer[0] := _USB_SET + 1;
    FT_Out_Buffer[1] := Ord(FSpeed);
    LogDataOut(2);
    Write_USB_Device_Buffer(2); // Nastavit rychlost komunikace

    FT_Out_Buffer[0] := _COM_ON;                  // spuštìní skenování
    LogDataOut(1);
    Write_USB_Device_Buffer(1);

    FScanning := true;
    FWaitingOnScan := true;
    LogWrite(llChange, 'Spuštìní komunikace');
    LogWrite(llChange, 'Speed: '+ IntToStr(GetSpeedStr)+'bd     ScanInterval: ' + IntToStr(ord(ScanInterval)));
    GetCmdNum;

    if (Assigned(AfterStart)) then AfterStart(Self);
  except
   on E:Exception do begin
     Self.LogWrite(llError, 'Start exception, closing: '+E.ClassName+' - '+E.Message);
     Self.Close();
     raise;
    end;
  end;
end;

// zastavi komunikaci
procedure TMTBusb.Stop();
var
 i : word;
begin
  if (not FScanning) then raise ENotStarted.Create('Communication not started, cannot close!');
  if (not FOpenned) then raise ENotOpened.Create('Device not opened, cannot stop communication!');

  if (Assigned(BeforeStop)) then BeforeStop(Self);

  try
    Self.ResetPortsStatus();

    FT_Out_Buffer[0] := _COM_OFF;                  // zastavení skenování
    LogDataOut(1);
    Write_USB_Device_Buffer(1);

    FScanning := false;
    FWaitingOnScan := false;
    LogWrite(llChange, 'Zastavení komunikace');
    for i := 1 to _MTB_MAX_ADDR do begin
      FModule[i].Status := 0;
    end;

    if (Assigned(AfterStop)) then AfterStop(Self);
  except
   on E:Exception do begin
     Self.LogWrite(llError, 'Stop exception, closing: '+E.ClassName+' - '+E.Message);
     Self.Close();
     raise;
    end;
  end;
end;

constructor TMTBusb.Create(AOwner: TComponent; MyDir:string);
begin
  inherited Create(AOwner);

  // Default hodnoty
  FLogLevel := logLevel;

  FOpenned := false;
  FScanning := false;
  FWaitingOnScan := false;
  FModuleCount := 0;
  FCmdSecCycleNum := 10;
  FCyclesFbData := 0;

  FTimer := TTimer.Create(self);
  FTimer.Enabled := false;
  FTimer.Interval := ord(FScanInterval);
  FTimer.OnTimer := MtbScan;
  FTimer.SetSubComponent(True);

  FMyDir := MyDir;
  CreateDir(MyDir);

  try
    Self.LoadConfig(FMyDir+'/'+_CONFIG_FN);
  except
    on E:Exception do
      Self.LogWrite(llError, 'Nelze naèíst konfiguraci : ' + E.Message);
  end;
end;

destructor TMTBusb.Destroy;
begin
  if (FScanning and FOpenned) then begin
    try
      Self.Stop();
    except

    end;
  end;
  if (FOpenned) then begin
    try
      Self.Close();
    except

    end;
  end;

  try
    Self.SaveConfig(FMyDir+'\'+_CONFIG_FN);
  except
    on E:Exception do
      Self.LogWrite(llError, 'Nelze uložit konfiguraci : ' + E.Message);
  end;

  inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMTBusb.LogDataOut(bufLen:Integer);
var s:string;
    i:Integer;
begin
 for i := 0 to bufLen-1 do
   s := s + IntToHex(FT_Out_Buffer[i], 2) + ' ';
 Self.LogWrite(llRawCmd, 'DATA OUT: '+s);
end;

procedure TMTBusb.LogDataIn(bufLen:Integer; bufStart:Integer = 0);
var s:string;
    i:Integer;
begin
 for i := 0 to bufLen-1 do
   s := s + IntToHex(FT_In_Buffer[i], 2) + ' ';
 Self.LogWrite(llRawCmd, 'DATA IN: '+s);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMTBusb.LoadConfig(fn:string);
var ini:TMemIniFile;
    i:Integer;
    name:string;
begin
 ini := TMemIniFile.Create(fn, TEncoding.UTF8);

 for i := 1 to _MTB_MAX_ADDR do
 begin
  name := 'Modul '+intToStr(i);
  Self.SetCfg(i, ini.ReadInteger(name, 'cfg', 0));
  FModule[i].setting := ini.ReadInteger(name, 'setting', 1);
  FModule[i].popis := ini.ReadString(name, 'popis', '');
 end;
 Self.FSpeed        := TMtbSpeed(ini.ReadInteger('MTB', 'speed', 4));
 Self.FScanInterval := TTimerInterval(ini.ReadInteger('MTB', 'timer', 100));
 Self.FusbName      := ini.ReadString('MTB', 'device', _DEFAULT_USB_NAME);
 Self.FLogLevel     := TLogLevel(ini.ReadInteger('MTB', 'LogLevel', Integer(llNo)));
 ini.Free;
end;

procedure TMTBusb.SaveConfig(fn:string);
var ini:TMemIniFile;
    i:Integer;
    name:string;
begin
 ini := TMemIniFile.Create(fn, TEncoding.UTF8);

 for i := 1 to _MTB_MAX_ADDR do
 begin
  name := 'Modul '+intToStr(i);
  ini.WriteInteger(name, 'cfg', Self.GetCfg(i));
  ini.WriteInteger(name, 'setting', FModule[i].Setting);
  ini.WriteString(name, 'popis', FModule[i].popis);
 end;

 ini.WriteInteger('MTB', 'speed', Integer(Self.FSpeed));
 ini.WriteInteger('MTB', 'timer', Integer(Self.FScanInterval));
 ini.WriteString('MTB', 'device', Self.FusbName);
 ini.WriteInteger('MTB', 'LogLevel', Integer(Self.FLogLevel));
 ini.UpdateFile;
 ini.Free;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMTBusb.ResetPortsStatus();
var i, j:Integer;
begin
  for i := 1 to _MTB_MAX_ADDR do begin
    FModule[i].revived := False;
    FModule[i].failure := False;

    FModule[i].inputStateKnown := false;
    FModule[i].Input.value := 0;
    FModule[i].Input.changed := False;
    FModule[i].Output.value := 0;
    FModule[i].Output.changed := False;
    for j := 0 to 7 do begin
      FModule[i].Scom.ScomCode[j] := 0;
      FModule[i].Scom.changed[j] := False;
      FModule[i].Scom.ScomActive[j] := False;
    end;
    if FModule[i].CFData[0] and $1 = 1 then begin
      FModule[i].Scom.ScomActive[0] := True;
      FModule[i].Scom.ScomActive[1] := True;
    end;
    if FModule[i].CFData[0] and $2 = 2 then begin
      FModule[i].Scom.ScomActive[2] := True;
      FModule[i].Scom.ScomActive[3] := True;
    end;
    if FModule[i].CFData[0] and $4 = 4 then begin
      FModule[i].Scom.ScomActive[4] := True;
      FModule[i].Scom.ScomActive[5] := True;
    end;
    if FModule[i].CFData[0] and $8 = 8 then begin
      FModule[i].Scom.ScomActive[6] := True;
      FModule[i].Scom.ScomActive[7] := True;
    end;
  end;
  for i := 0 to _PORT_MAX_NUM do begin
    FPortIn[i].value := False;
    FPortIn[i].inUp := False;
    FPortIn[i].inDn := False;
  end;
  for i := 0 to _POT_CHANN-1 do begin
    FPot[i].PotValue := 0;
    FPot[i].PotDirect := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TMTBusb.AreAllModulesScanned(): boolean;
var i:Integer;
begin
  for i := 1 to _MTB_MAX_ADDR do begin
    if ((IsModule(i)) and (not IsModuleFailure(i)) and
        ((not IsModuleConfigured(i)) or (not IsModuleScanned(i)))) then
     Exit(false);
  end;

 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMTBusb.CheckAllScannedEvent();
begin
 if ((Self.FWaitingOnScan) and (AreAllModulesScanned())) then begin
   Self.FWaitingOnScan := false;
   if (Assigned(Self.FOnScanned)) then Self.FOnScanned(Self);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

end.
