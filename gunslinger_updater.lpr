program gunslinger_updater;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, abstractions, LogMgr, userltxdumper, Decompressor, Localizer
  { you can add units after this };

{$R *.res}

procedure DecompressLogger(txt:PAnsiChar); stdcall;
begin
  FZLogMgr.Get.Write('[Decompressor]'+txt, FZ_LOG_DBG);
end;

begin
  Application.Title:='GUNSLINGER Mod Updater';
  abstractions.Init();
  LogMgr.Init();
  Decompressor.Init(@DecompressLogger);
  FZLogMgr.Get.SetSeverity(FZ_LOG_DBG);

  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

  Decompressor.Free();
  LogMgr.Free();
  abstractions.Free();
end.

