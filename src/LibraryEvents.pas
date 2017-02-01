////////////////////////////////////////////////////////////////////////////////
// LibraryEvents.pas
//  MTB communication library
//  Library event definition
//   (c) Jan Horacek (jan.horacek@kmz-brno.cz),
////////////////////////////////////////////////////////////////////////////////

{
   LICENSE:

   Copyright 2015-2016 Jan Horacek

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

  This file defines library callback events. Class TLibEvents holds library
  events.
}

unit LibraryEvents;

interface

type
  TStdNotifyEvent = procedure (Sender: TObject; data:Pointer); stdcall;
  TStdLogEvent = procedure (Sender: TObject; data:Pointer; logLevel:Integer; msg:PChar); stdcall;
  TStdErrorEvent = procedure (Sender: TObject; data:Pointer; errValue: word; errAddr: byte; errMsg:PChar); stdcall;
  TStdModuleChangeEvent = procedure (Sender: TObject; data:Pointer; module: byte); stdcall;

  TMyErrorEvent = record
    event: TStdErrorEvent;
    data: Pointer;
  end;
  TMyNotifyEvent = record
    event: TStdNotifyEvent;
    data: Pointer;
  end;
  TMyModuleChangeEvent = record
    event: TStdModuleChangeEvent;
    data:Pointer
  end;
  TMyLogEvent = record
    event: TStdLogEvent;
    data: Pointer;
  end;

  TLibEvents = record
    BeforeOpen:TMyNotifyEvent;
    AfterOpen:TMyNotifyEvent;
    BeforeClose:TMyNotifyEvent;
    AfterClose:TMyNotifyEvent;

    BeforeStart:TMyNotifyEvent;
    AfterStart:TMyNotifyEvent;
    BeforeStop:TMyNotifyEvent;
    AfterStop:TMyNotifyEvent;

    OnScanned:TMyNotifyEvent;

    OnError:TMyErrorEvent;
    OnLog:TMyLogEvent;
    OnInputChanged:TMyModuleChangeEvent;
    OnOutputChanged:TMyModuleChangeEvent;
  end;

var
   LibEvents:TLibEvents;

implementation

end.
