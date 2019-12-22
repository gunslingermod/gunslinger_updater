program gunslinger_updater;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, abstractions, LogMgr, userltxdumper
  { you can add units after this };

{$R *.res}

begin
  abstractions.Init();
  LogMgr.Init();
  FZLogMgr.Get.SetSeverity(FZ_LOG_DBG);

  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

  LogMgr.Free();
  abstractions.Free();
end.

