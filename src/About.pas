////////////////////////////////////////////////////////////////////////////////
// About.pas
//  MTB communication library
//  ABout dialog
//  (c) Petr Travnik (petr.travnik@kmz-brno.cz),
//      Jan Horacek (jan.horacek@kmz-brno.cz),
//      Michal Petrilak (engineercz@gmail.com)
// 30.05.2015
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

  TF_About is a class representing simple about form.
}

unit About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TF_About = class(TForm)
    L_About1: TLabel;
    L_About2: TLabel;
    L_About3: TLabel;
    ST_about4: TStaticText;
    ST_about5: TStaticText;
    ST_about6: TStaticText;
    GB_Versions: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    L_VersionLib: TLabel;
    L_VersionDrv: TLabel;
    L_VersionUSB: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  F_About: TF_About;

const
 _VERSION = '2.2.10.0';

implementation

uses FFormConfig, MTBusb, LibCML;

{$R *.dfm}

procedure TF_About.FormShow(Sender: TObject);
begin
 Self.L_VersionLib.Caption := _VERSION;
 Self.L_VersionDrv.Caption := SW_VERSION;
 Self.L_VersionUSB.Caption := MTBdrv.HWVersion;
end;

end.
