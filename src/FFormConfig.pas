////////////////////////////////////////////////////////////////////////////////
// FFormConfig.pas
//  MTB communication library
//  Main config form
//  (c) Petr Travnik (petr.travnik@kmz-brno.cz),
//      Jan Horacek (jan.horacek@kmz-brno.cz),
//      Michal Petrilak (engineercz@gmail.com)
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

{
  DESCRIPTION:

  TFormConfig is a class representing main configuration form.
}

unit FFormConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus, StrUtils,
  FFormModule, ExtCtrls, MTBusb;

type

   TFormConfig = class(TForm)
    pm_mod: TPopupMenu;
    pm_mod_nastaveni: TMenuItem;
    PC_Main: TPageControl;
    TS_Stav: TTabSheet;
    TS_Device: TTabSheet;
    TS_Modules: TTabSheet;
    TS_Log: TTabSheet;
    l_3: TLabel;
    l_modcount: TLabel;
    lv_modules: TListView;
    Label1: TLabel;
    Label2: TLabel;
    L_Openned: TLabel;
    L_Started: TLabel;
    cb_mtbName: TComboBox;
    cb_speed: TComboBox;
    b_ScanBrd: TButton;
    l_2: TLabel;
    RG_TimerInterval: TRadioGroup;
    MM_Main: TMainMenu;
    MI_Window: TMenuItem;
    PM_Close: TMenuItem;
    PM_Help: TMenuItem;
    PM_About: TMenuItem;
    LV_Log: TListView;
    Label3: TLabel;
    Label4: TLabel;
    B_DeleteNonExist: TButton;
    L_LogLevel: TLabel;
    CB_LogLevel: TComboBox;
    procedure b_ScanBrdClick(Sender: TObject);
    procedure lv_modulesDblClick(Sender: TObject);
    procedure cb_mtbNameChange(Sender: TObject);
    procedure pm_mod_nastaveniClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RG_TimerIntervalClick(Sender: TObject);
    procedure pm_modPopup(Sender: TObject);
    procedure cb_speedChange(Sender: TObject);//cteni verze z nastaveni

    procedure OnScanned(Sender:TObject);
    procedure OnError(Sender: TObject; errValue: word; errAddr: byte);

    procedure BeforeOpen(Sender:TObject);
    procedure AfterOpen(Sender:TObject);
    procedure BeforeClose(Sender:TObject);
    procedure AfterClose(Sender:TObject);

    procedure BeforeStart(Sender:TObject);
    procedure AfterStart(Sender:TObject);
    procedure BeforeStop(Sender:TObject);
    procedure AfterStop(Sender:TObject);

    procedure OnLog(Sender: TObject; logLevel:TLogLevel; logValue: string);
    procedure PM_AboutClick(Sender: TObject);
    procedure PM_CloseClick(Sender: TObject);
    procedure LV_LogCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure B_DeleteNonExistClick(Sender: TObject);
    procedure CB_LogLevelChange(Sender: TObject);
  private

  public

  end;

var
  FormConfig: TFormConfig;

implementation

{$R *.dfm}
uses About, LibraryEvents, LibCML;

////////////////////////////////////////////////////////////////////////////////

procedure TFormConfig.b_ScanBrdClick(Sender: TObject);
var
  i,cnt: Integer;
begin
 cb_mtbName.Enabled := false;

 cb_mtbName.Clear;
 cnt := MTBdrv.GetDeviceCount();
 if (cnt > 0) then
   for i := 0 to cnt-1 do
     cb_mtbName.Items.Add(MTBdrv.GetDeviceSerial(i));
 cb_mtbName.ItemIndex := -1;

 cb_mtbName.Enabled := true;
end;

procedure TFormConfig.CB_LogLevelChange(Sender: TObject);
begin
 MTBdrv.LogLevel := TLogLevel(Self.CB_LogLevel.ItemIndex);
end;

procedure TFormConfig.OnScanned(Sender:TObject);
var
  i: Integer;
  LI: TListItem;
  cfg:TModulConfigGet;
begin
 lv_modules.Color := clWindow;
 lv_modules.Clear;
 l_modcount.Caption := IntToStr(MTBdrv.ModuleCount);
 TS_Device.Enabled := true;

 for i := 0 to _MTB_MAX_ADDR do
  begin
   if (not MTBdrv.IsModule(i)) then continue;

   LI := lv_modules.Items.Add;
   cfg := MTBdrv.GetModuleCfg(i);
   LI.Caption := cfg.CFGpopis;

   LI.SubItems.Add(IntToStr(i));
   LI.SubItems.Add(MTBdrv.GetModuleTypeName(i));
   LI.SubItems.Add(IntToHex(MTBdrv.GetCfg(i), 8));
 end;
end;

procedure TFormConfig.PM_AboutClick(Sender: TObject);
begin
 F_About.Show();
end;

procedure TFormConfig.PM_CloseClick(Sender: TObject);
begin
 Self.Hide();
end;

procedure TFormConfig.lv_modulesDblClick(Sender: TObject);
begin
 if (lv_modules.Selected <> nil) then
   FormModule.OpenForm(StrToInt(lv_modules.Items.Item[lv_modules.ItemIndex].SubItems.Strings[0]));
end;

procedure TFormConfig.cb_speedChange(Sender: TObject);
begin
 MTBdrv.MtbSpeed := TMtbSpeed(Self.cb_speed.ItemIndex+2);
end;

procedure TFormConfig.cb_mtbNameChange(Sender: TObject);
begin
 MTBdrv.UsbSerial := cb_mtbName.text;
end;

procedure TFormConfig.pm_mod_nastaveniClick(Sender: TObject);
begin
 FormModule.OpenForm(StrToInt(lv_modules.Items.Item[lv_modules.ItemIndex].SubItems.Strings[0]));
end;

procedure TFormConfig.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Self.cb_speed.ItemIndex := Integer(MTBdrv.MtbSpeed)-2;

  case (Mtbdrv.ScanInterval) of
   ti50 :Self.RG_TimerInterval.ItemIndex := 0;
   ti100:Self.RG_TimerInterval.ItemIndex := 1;
   ti200:Self.RG_TimerInterval.ItemIndex := 2;
   ti250:Self.RG_TimerInterval.ItemIndex := 3;
  end;

  //zjisteni pripojenych MTB zarizeni
  b_ScanBrdClick(self);

  for i := 0 to cb_mtbName.Items.Count-1 do
   begin
    if (cb_mtbName.Items.Strings[i] = MTBdrv.UsbSerial) then
     begin
      cb_mtbName.ItemIndex := i;
      Break;
     end;
   end;

 Self.Caption := Self.Caption+'        v'+_VERSION;
 Self.PC_Main.ActivePageIndex := 0;
 Self.CB_LogLevel.ItemIndex := Integer(MTBdrv.LogLevel);
end;

procedure TFormConfig.LV_LogCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var str:string;
begin
 (Sender as TCustomListView).Canvas.Brush.Color := clWhite;

 if (Item.SubItems.Count > 0) then
  begin
   str := LeftStr(Item.SubItems.Strings[0],3);

   if (str = 'ERR') then (Sender as TCustomListView).Canvas.Brush.Color := $AAFFFF;

   str := RightStr(Item.SubItems.Strings[0],Length(Item.SubItems.Strings[0])-13);
   str := LeftStr(str,7);
   if (str = 'Data in') then (Sender as TCustomListView).Canvas.Brush.Color := $FFD0D0;
   if (str = 'Data ou') then (Sender as TCustomListView).Canvas.Brush.Color := $A0FFD0;
  end;
end;

procedure TFormConfig.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
 CanClose := false;
 Hide;
end;

procedure TFormConfig.RG_TimerIntervalClick(Sender: TObject);
begin
 MTBdrv.ScanInterval := TTimerInterval(StrToIntDef(RG_TimerInterval.Items.Strings[RG_TimerInterval.ItemIndex],50));
end;

procedure TFormConfig.pm_modPopup(Sender: TObject);
var cyklus:Integer;
begin
 if (lv_modules.Selected <> nil) then
  begin
   for cyklus := 0 to pm_mod.Items.Count-1 do pm_mod.Items.Items[cyklus].Enabled := true;
  end else begin
   for cyklus := 0 to pm_mod.Items.Count-1 do pm_mod.Items.Items[cyklus].Enabled := false;
 end;//else LV_HV.Selected <> nil
end;

procedure TFormConfig.BeforeOpen(Sender:TObject);
begin
 Self.L_Openned.Caption := 'otevírám...';
 Self.L_Openned.Font.Color := clSilver;

 Self.cb_speed.Enabled   := false;
 Self.cb_mtbName.Enabled := false;
 Self.b_ScanBrd.Enabled  := false;
end;

procedure TFormConfig.AfterOpen(Sender:TObject);
begin
 Self.L_Openned.Caption := 'otevøeno';
 Self.L_Openned.Font.Color := clGreen;
 Self.OnScanned(Self);
end;

procedure TFormConfig.BeforeClose(Sender:TObject);
begin
 Self.L_Openned.Caption := 'zavírám...';
 Self.L_Openned.Font.Color := clSilver;
end;

procedure TFormConfig.AfterClose(Sender:TObject);
begin
 Self.L_Openned.Caption := 'uzavøeno';
 Self.L_Openned.Font.Color := clRed;
 Self.cb_speed.Enabled   := true;
 Self.cb_mtbName.Enabled := true;
 Self.b_ScanBrd.Enabled  := true
end;

procedure TFormConfig.BeforeStart(Sender:TObject);
begin
 Self.L_Started.Caption := 'spouštím...';
 Self.L_Started.Font.Color := clSilver;
 Self.b_ScanBrd.Enabled := false;
end;

procedure TFormConfig.AfterStart(Sender:TObject);
begin
 Self.L_Started.Caption := 'spuštìna';
 Self.L_Started.Font.Color := clGreen;
end;

procedure TFormConfig.BeforeStop(Sender:TObject);
begin
 Self.L_Started.Caption := 'zastavuji...';
 Self.L_Started.Font.Color := clSilver;
 Self.b_ScanBrd.Enabled  := true;
 Self.cb_mtbName.Enabled := true;
end;

// vyresetovat konfiguraci vsech modulu, ktere nebyly nalezeny
// to je dobre napriklad pri prechodu na jine kolejiste
procedure TFormConfig.B_DeleteNonExistClick(Sender: TObject);
var i, j:Integer;
begin
 if (Application.MessageBox('Opravdu vyresetovat konfiguraci všech MTB modulù, které se nenachází v tabulce níže?', 'Opravdu', MB_YESNO OR MB_ICONQUESTION) <> mrYes) then Exit(); 

 for i := 1 to _MTB_MAX_ADDR do
  begin
   if (not MTBdrv.IsModule(i)) then
    begin
     for j := 0 to 3 do
       MTBdrv.WrCfgData.CFGdata[j] := 0;
     MTBdrv.WrCfgData.CFGpopis := '';
     MTBdrv.SetModuleCfg(i);
    end;
  end;//for i

 Application.MessageBox('Konfigurace pøíslušných MTB modulù smazána', 'Hotovo', MB_OK OR MB_ICONINFORMATION);
end;//procedure

procedure TFormConfig.AfterStop(Sender:TObject);
begin
 Self.L_Started.Caption      := 'zastavena';
 Self.L_Started.Font.Color   := clRed;
 if (Assigned(LibEvents.AfterStop.event)) then LibEvents.AfterStop.event(Self, LibEvents.AfterStop.data);
end;

procedure TFormConfig.OnLog(Sender: TObject; logLevel:TLogLevel; logValue: string);
var LI:TListItem;
    timeStr:string;
begin
 if (Self.LV_Log.Items.Count > 1000) then
  Self.LV_Log.Clear();

 DateTimeToString(timeStr, 'hh:mm:ss.zzz', time);
 LI := Self.LV_Log.Items.Insert(0);
 LI.Caption := timeStr;
 case (logLevel) of
  llError  : LI.SubItems.Add('Error');
  llChange : LI.SubItems.Add('Zmìna');
  llCmd    : LI.SubItems.Add('Pøíkaz');
  llRawCmd : LI.SubItems.Add('RAW');
  llDebug  : LI.SubItems.Add('Debug');
 else
  LI.SubItems.Add('');
 end;
 LI.SubItems.Add(logValue);
end;

procedure TFormConfig.OnError(Sender: TObject; errValue: word; errAddr: byte);
var str:string;
begin

 if (errAddr = 255) then
  begin
   //errors on main board (MTB-USB)
   case (errValue) of
    21,22,25:begin
        //start error
       Self.L_Started.Caption := 'zastavena';
       Self.L_Started.Font.Color   := clRed;
     end;
    31:begin
      //stop error
    end;
    1,2:begin
      //open error
     Self.L_Openned.Caption := 'uzavøeno';
     Self.L_Openned.Font.Color := clRed;

     Self.b_ScanBrd.Enabled  := true;
     Self.cb_mtbName.Enabled := true;
     Self.cb_speed.Enabled   := true;
    end;
    11:begin
      //close error
    end;
   end;//case
  end;//if errAddr = 255
end;//procedure

initialization

finalization
  FreeAndNil(FormConfig);

end.//unit
