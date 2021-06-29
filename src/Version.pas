unit Version;

interface

uses Windows, SysUtils;


// returns version of filename \FileName
function GetVersion(const FileName: string): string;
function GetLibVersion(): string;

implementation

function GetLibVersion(): string;
var fn:string;
begin
  fn := GetModuleName(HInstance);
  Result := GetVersion(fn);
end;

function GetVersion(const FileName: string): string;
var
  size, len: longword;
  handle: Cardinal;
  buffer: pchar;
  pinfo: ^VS_FIXEDFILEINFO;
  Major, Minor, Release: word;
begin
  Result := 'Není dostupná';
  size := GetFileVersionInfoSize(Pointer(FileName), handle);
  if size > 0 then begin
    GetMem(buffer, size);
    if GetFileVersionInfo(Pointer(FileName), 0, size, buffer)
    then
      if VerQueryValue(buffer, '\', pointer(pinfo), len) then begin
        Major := HiWord(pinfo.dwFileVersionMS);
        Minor := LoWord(pinfo.dwFileVersionMS);
        Release := HiWord(pinfo.dwFileVersionLS);
        Result := Format('%d.%d.%d',[Major, Minor, Release]);
      end;
    FreeMem(buffer);
  end;
end;

end.//unit
