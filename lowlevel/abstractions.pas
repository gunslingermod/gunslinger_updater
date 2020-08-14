unit abstractions;

{$mode delphi}

interface

type
  FZ_GAME_VERSION = (FZ_VER_UNKNOWN, FZ_VER_SOC_10006, FZ_VER_SOC_10006_V2, FZ_VER_CS_1510, FZ_VER_COP_1602);

  FZAbstractGameVersion = class
  public
    function ThreadSpawn(proc:uintptr; args:uintptr; name:PAnsiChar = nil; stack:cardinal = 0):boolean; virtual; abstract;
    procedure Log(txt:PAnsiChar); virtual; abstract;
  end;

  FZNoGameVersion = class(FZAbstractGameVersion)
  protected
    _log_file:textfile;
  public
    constructor Create();
    destructor Destroy(); override;
    function ThreadSpawn(proc:uintptr; args:uintptr; {%H-}name:PAnsiChar = nil; {%H-}stack:cardinal = 0):boolean; override;
    procedure Log(txt:PAnsiChar); override;
  end;

  function Init():boolean; stdcall;
  procedure Free(); stdcall;
  function VersionAbstraction():FZAbstractGameVersion;

implementation
uses CommonHelper, Windows;

constructor FZNoGameVersion.Create();
begin
  assignfile(_log_file, 'update.log');
  rewrite(_log_file);
end;

destructor FZNoGameVersion.Destroy();
begin
  closefile(_log_file);
end;

procedure FZNoGameVersion.Log(txt: PAnsiChar);
var
  s:string;
begin
  s:=FZCommonHelper.GetCurTime()+' : '+txt;
  writeln(_log_file, s);
  flush(_log_file);
end;

function FZNoGameVersion.ThreadSpawn(proc: uintptr; args: uintptr; name:PAnsiChar = nil; stack:cardinal = 0):boolean;
var
  thId:cardinal;
begin
  result:=true;
  thId:=0;
  CreateThread(nil, 0, FZCommonHelper.UintToPtr(proc), FZCommonHelper.UintToPtr(args), 0, thId);
end;


////////////////////////////////////////////////////////////////////////////////////
{Global area}
////////////////////////////////////////////////////////////////////////////////////

var
  _abstraction: FZAbstractGameVersion;

function VersionAbstraction: FZAbstractGameVersion;
begin
  result:=_abstraction;
end;

function Init():boolean; stdcall;
begin
  _abstraction:=FZNoGameVersion.Create();
  result:=true;
end;

procedure Free(); stdcall;
begin
  _abstraction.Free();
end;

end.

