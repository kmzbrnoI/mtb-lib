////////////////////////////////////////////////////////////////////////////////
// LibCML.pas
//  MTB communication library
//  Main library class.
//  (c) Jan Horacek (jan.horacek@kmz-brno.cz)
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

  TCML is main library class.
  It covers all the high-level library principles.
}

unit LibCML;

interface

uses MTBusb;

type
  TCML = class
    private
      procedure MTBOnChange(Sender:TObject);
      procedure MTBOnInputChanged(Sender: TObject; module: byte);
      procedure MTBOnOutputChanged(Sender: TObject; module: byte);

      procedure MTBBeforeOpen(Sender:TObject);
      procedure MTBAfterOpen(Sender:TObject);
      procedure MTBBeforeClose(Sender:TObject);
      procedure MTBAfterClose(Sender:TObject);

      procedure MTBBeforeStart(Sender:TObject);
      procedure MTBAfterStart(Sender:TObject);
      procedure MTBBeforeStop(Sender:TObject);
      procedure MTBAfterStop(Sender:TObject);

    public
      constructor Create();
      destructor Destroy(); override;

      procedure OnError(Sender: TObject; errValue: word; errAddr: byte);
      procedure OnLog(Sender: TObject; ll:TLogLevel; logValue: string);

  end;

var
  CML:TCML;
  MTBdrv: TMTBusb;

implementation

uses FFormConfig, FFormModule, LibraryEvents;

////////////////////////////////////////////////////////////////////////////////

constructor TCML.Create();
begin
 inherited;

 MTBdrv.OnError         := Self.OnError;
 MTBdrv.OnLog           := Self.OnLog;
 MTBdrv.OnChange        := self.MTBOnChange;
 MTBdrv.OnInputChange   := Self.MTBOnInputChanged;
 MTBdrv.OnOutputChange  := Self.MTBOnOutputChanged;

 MTBdrv.BeforeOpen      := Self.MTBBeforeOpen;
 MTBdrv.AfterOpen       := Self.MTBAfterOpen;

 MTBdrv.BeforeStart     := Self.MTBBeforeStart;
 MTBdrv.AfterStart      := Self.MTBAfterStart;

 MTBdrv.BeforeStop      := Self.MTBBeforeStop;
 MTBdrv.AfterStop       := Self.MTBAfterStop;

 MTBdrv.BeforeClose     := Self.MTBBeforeClose;
 MTBdrv.AfterClose      := Self.MTBAfterClose;
end;

destructor TCML.Destroy();
begin
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TCML.MTBBeforeOpen(Sender:TObject);
begin
 if (Assigned(FormConfig)) then FormConfig.BeforeOpen(Sender);
 if (Assigned(LibEvents.BeforeOpen.event)) then LibEvents.BeforeOpen.event(Self, LibEvents.BeforeOpen.data);
end;

procedure TCML.MTBAfterOpen(Sender:TObject);
begin
 if (Assigned(FormConfig)) then FormConfig.AfterOpen(Sender);
 if (Assigned(LibEvents.AfterOpen.event)) then LibEvents.AfterOpen.event(Self, LibEvents.AfterOpen.data);
end;

procedure TCML.MTBBeforeClose(Sender:TObject);
begin
 if (Assigned(FormConfig)) then FormConfig.BeforeClose(Sender);
 if (Assigned(LibEvents.BeforeClose.event)) then LibEvents.BeforeClose.event(Self, LibEvents.BeforeClose.data);
end;

procedure TCML.MTBAfterClose(Sender:TObject);
begin
 if (Assigned(FormConfig)) then FormConfig.AfterClose(Sender);
 if (Assigned(LibEvents.AfterClose.event)) then LibEvents.AfterClose.event(Self, LibEvents.AfterClose.data);
end;

procedure TCML.MTBBeforeStart(Sender:TObject);
begin
 if (Assigned(FormConfig)) then FormConfig.BeforeStart(Sender);
 if (Assigned(LibEvents.BeforeStart.event)) then LibEvents.BeforeStart.event(Self, LibEvents.BeforeStart.data);
end;

procedure TCML.MTBAfterStart(Sender:TObject);
begin
 if (Assigned(FormConfig)) then FormConfig.AfterStart(Sender);
 if (Assigned(LibEvents.AfterStart.event)) then LibEvents.AfterStart.event(Self, LibEvents.AfterStart.data);
 if (Assigned(FormModule)) then FormModule.RefreshStates();
end;

procedure TCML.MTBBeforeStop(Sender:TObject);
begin
 if (Assigned(FormConfig)) then FormConfig.BeforeStop(Sender);
 if (Assigned(LibEvents.BeforeStop.event)) then LibEvents.BeforeStop.event(Self, LibEvents.BeforeStop.data);
 if (Assigned(FormModule)) then FormModule.RefreshStates();
end;

procedure TCML.MTBAfterStop(Sender:TObject);
begin
 if (Assigned(FormConfig)) then FormConfig.AfterStop(Sender);
 if (Assigned(LibEvents.AfterStop.event)) then LibEvents.AfterStop.event(Self, LibEvents.AfterStop.data);
end;

procedure TCML.MTBOnChange(Sender:TObject);
begin
 if (Assigned(FormModule)) then FormModule.OnChange(Sender);
end;

procedure TCML.OnLog(Sender: TObject; ll:TLogLevel; logValue: string);
begin
 if (Assigned(FormConfig)) then FormConfig.OnLog(Sender, ll, logValue);
 if (Assigned(LibEvents.OnLog.event)) then LibEvents.OnLog.event(Self, LibEvents.OnLog.data, Integer(ll), PChar(logValue));
end;

procedure TCML.OnError(Sender: TObject; errValue: word; errAddr: byte);
begin
 if (Assigned(FormConfig)) then FormConfig.OnError(Sender, errValue, errAddr);
 if (Assigned(LibEvents.OnError.event)) then
   LibEvents.OnError.event(Self, LibEvents.OnError.data, errValue, errAddr, PChar(MTBdrv.GetErrString(errValue)));
end;

procedure TCML.MTBOnInputChanged(Sender: TObject; module: byte);
begin
 if (Assigned(LibEvents.OnInputChanged.event)) then LibEvents.OnInputChanged.event(Self, LibEvents.OnInputChanged.data, module);
end;

procedure TCML.MTBOnOutputChanged(Sender: TObject; module: byte);
begin
 if (Assigned(LibEvents.OnOutputChanged.event)) then LibEvents.OnOutputChanged.event(Self, LibEvents.OnOutputChanged.data, module);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  MTBDrv := TMTBusb.Create(nil, 'mtb');
  CML := TCML.Create();

finalization
  MTBdrv.Free();
  CML.Free();

end.
