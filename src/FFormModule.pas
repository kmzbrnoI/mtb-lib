////////////////////////////////////////////////////////////////////////////////
// FFormModule.pas
//  MTB communication library
//  MTB module config form
//  (c) Petr Travnik (petr.travnik@kmz-brno.cz),
//      Jan Horacek (jan.horacek@kmz-brno.cz),
//      Michal Petrilak (engineercz@gmail.com)
////////////////////////////////////////////////////////////////////////////////

{
   LICENSE:

   Copyright 2015-2018 Petr Travnik, Michal Petrilak, Jan Horacek

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

  TFormModules is a class representing MTB module configuration form.
}

unit FFormModule;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TFormModule = class(TForm)
    gb_1: TGroupBox;
    gb_2: TGroupBox;
    gb_3: TGroupBox;
    rg_delay: TRadioGroup;
    cbIR0: TCheckBox;
    cbIR1: TCheckBox;
    cbIR2: TCheckBox;
    cbIR3: TCheckBox;
    cbSCOM0: TCheckBox;
    cbSCOM1: TCheckBox;
    cbSCOM2: TCheckBox;
    cbSCOM3: TCheckBox;
    B_Save: TButton;
    B_Cancel: TButton;
    E_nazev_desky: TEdit;
    L_nazev_desky: TLabel;
    Label1: TLabel;
    L_FWVersion: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure B_CancelClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
  private
    ledIN: Array[0..15] of TShape;
    ledOUT: Array[0..15] of TShape;
    textIN: Array[0..15] of TLabel;
    textOUT: Array[0..15] of TLabel;

    OpenModule:Integer;
  public
    procedure OpenForm(module: Integer);
    procedure RefreshStates();

    procedure OnChange(Sender:TObject);
  end;

var
  FormModule: TFormModule;

implementation

uses FFormConfig, MTBusb, LibCML;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////

procedure TFormModule.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 15 do begin
    ledIN[i] := TShape.Create(Self);
    with ledIN[i] do begin
      Parent := gb_1;
      Left   := 15 + (i div 8) * 130;
      Top    := 20 + (i mod 8) * 20;
      Width  := 15;
      Height := 10;
      Hint   := 'Vstup '+IntToStr(i);
      Brush.Color := clGray;
    end;

    textIN[i] := TLabel.Create(Self);
    with textIN[i] do begin
      Parent  := gb_1;
      Left    := ledIN[i].Left + 20;
      Top     := ledIN[i].Top;
      Caption := IntToStr(i);
    end;

    ledOUT[i] := TShape.Create(Self);
    with ledOUT[i] do begin
      Parent := gb_2;
      Left   := 15 + (i div 8) * 130;
      Top    := 20 + (i mod 8) * 20;
      Width  := 15;
      Height := 10;
      Hint   := 'Výstup '+IntToStr(i);
      Brush.Color := clGray;
    end;

    textOUT[i] := TLabel.Create(Self);
    with textOUT[i] do begin
      Parent  := gb_2;
      Left    := ledOUT[i].Left + 20;
      Top     := ledOUT[i].Top;
      Caption := IntToStr(i);
    end;
  end;
end;//procedure

procedure TFormModule.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 15 do begin
    ledIN[i].Free;
    ledOUT[i].Free;
    textIN[i].Free;
    textOUT[i].Free;
  end;
end;

procedure TFormModule.OpenForm(module: Integer);
var
  i: Cardinal;
  conf:TModulConfigGet;
  typ:TModulType;
begin
  Self.OpenModule := module;

  conf := MTBdrv.GetModuleCfg(module);
  typ  := MTBdrv.GetModuleType(module);

  i := conf.CFGdata[0] AND $0F;
  cbSCOM0.Checked := (i AND 1) > 0;
  cbSCOM1.Checked := (i AND 2) > 0;
  cbSCOM2.Checked := (i AND 4) > 0;
  cbSCOM3.Checked := (i AND 8) > 0;

  rg_delay.ItemIndex := (conf.CFGdata[1] AND $0F);

  i := (conf.CFGdata[2] AND $0F);
  cbIR0.Checked := (i AND 1) > 0;
  cbIR1.Checked := (i AND 2) > 0;
  cbIR2.Checked := (i AND 4) > 0;
  cbIR3.Checked := (i AND 8) > 0;

  E_nazev_desky.Text  := conf.CFGpopis;
  L_FWVersion.Caption := conf.CFGfw;

  cbSCOM0.Visible := ((typ = TModulType.idMTB_UNI_ID) or (typ = TModulType.idMTB_UNIOUT_ID) or
                      (typ = TModulType.idMTB_TTL_ID) or (typ = TModulType.idMTB_TTLOUT_ID) or
                      (typ = TModulType.idNone));
  cbSCOM1.Visible := cbSCOM0.Visible;
  cbSCOM2.Visible := cbSCOM0.Visible;
  cbSCOM3.Visible := cbSCOM0.Visible;

  cbIR0.Visible := ((typ = TModulType.idMTB_UNI_ID) or (typ = TModulType.idNone));
  cbIR1.Visible := cbIR0.Visible;
  cbIR2.Visible := cbIR0.Visible;
  cbIR3.Visible := cbIR0.Visible;

  FormModule.Caption := 'Konfigurace modulu '+IntToStr(Module);

  Self.RefreshStates();
  Self.Show();
end;

procedure TFormModule.RefreshStates();
var
  data:Word;
  i: Integer;
  cl: TColor;
begin
  if (Self.OpenModule = 0) then Exit();

  cbSCOM0.Enabled := (not MTBdrv.Scanning);
  cbSCOM1.Enabled := (not MTBdrv.Scanning);
  cbSCOM2.Enabled := (not MTBdrv.Scanning);
  cbSCOM3.Enabled := (not MTBdrv.Scanning);

  cbIR0.Enabled := (not MTBdrv.Scanning);
  cbIR1.Enabled := (not MTBdrv.Scanning);
  cbIR2.Enabled := (not MTBdrv.Scanning);
  cbIR3.Enabled := (not MTBdrv.Scanning);

  rg_delay.Enabled := (not MTBdrv.Scanning);

  data := MTBdrv.GetIModule(Self.OpenModule);
  for i := 0 to 15 do
   begin
    if (MTBdrv.Scanning) then
      if ((data shr i) and $01 = 1) then cl := clLime else cl := clRed
    else
      cl := clGray;

    ledIN[i].Brush.Color := cl;
   end;//for i

  data := MTBdrv.GetOModule(Self.OpenModule);
  for i := 0 to 15 do begin
    if (MTBdrv.Scanning) then
      if ((data shr i) and $01 = 1) then cl := clLime else cl := clRed
    else
      cl := clGray;

    ledOUT[i].Brush.Color := cl;
  end;
end;

procedure TFormModule.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  Self.OpenModule := 0;
  CanClose := false;
  Hide;
end;

procedure TFormModule.B_CancelClick(Sender: TObject);
begin
 Self.Close;
end;

procedure TFormModule.B_SaveClick(Sender: TObject);
var
  B0: integer;
  B1: integer;
  B2: integer;
begin
  Screen.Cursor := crHourGlass;

  B0 := 0;
  B2 := 0;

  if (cbSCOM0.Checked) then B0 := B0 OR 1;
  if (cbSCOM1.Checked) then B0 := B0 OR 2;
  if (cbSCOM2.Checked) then B0 := B0 OR 4;
  if (cbSCOM3.Checked) then B0 := B0 OR 8;
  MTBdrv.WrCfgData.CFGdata[0] := B0;

  B1 := rg_delay.ItemIndex;
  MTBdrv.WrCfgData.CFGdata[1] := B1;

  if (cbIR0.Checked) then B2 := B2 OR 1;
  if (cbIR1.Checked) then B2 := B2 OR 2;
  if (cbIR2.Checked) then B2 := B2 OR 4;
  if (cbIR3.Checked) then B2 := B2 OR 8;
  MTBdrv.WrCfgData.CFGdata[2] := B2;
  MTBdrv.WrCfgData.CFGpopis := Self.E_nazev_desky.Text;

  MTBdrv.SetModuleCfg(OpenModule);
  FormConfig.UpdateModulesList();

  Screen.Cursor := crDefault;
  Self.Close;
end;

//vola se pri zmene MTB portu - zajistuje aktualni stav vstupu a vystupu
procedure TFormModule.OnChange(Sender:TObject);
begin
 if (Self.Showing) then Self.RefreshStates;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
  FreeAndNil(FormModule);


end.
