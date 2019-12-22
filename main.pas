unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls,

  HttpDownloader, FileManager, Windows;

type

  MasterListParseResult = (MASTERLIST_PARSE_OK, MASTERLIST_PARSE_ERROR, MASTERLIST_MAINTENANCE, MASTERLIST_CANTPARSE);
  DownloaderState = (DL_STATE_INIT, DL_STATE_MASTERLIST_LOADING, DL_STATE_UPDATE_DOWNLOADER, DL_STATE_MASTERLIST_PARSE, DL_STATE_DATA_LOADING, DL_STATE_DATA_LOADING_COMPLETED, DL_STATE_TERMINAL);
  FZMasterLinkListAddr = array of string;

  DownloaderUpdateParams = record
    url:string;
    md5:string;
    size:cardinal;
    filename:string;
  end;

  CurrentDownloadInfo = record
    lock:TCriticalSection;
    info:FZFileActualizingProgressInfo;
  end;
  pCurrentDownloadInfo = ^CurrentDownloadInfo;

  { TForm1 }

  TForm1 = class(TForm)
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Timer1: TTimer;
    update_status: TLabel;
    update_progress: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    procedure SetStatus(status:string);
    procedure ChangeState(state:DownloaderState);
    function StartDownloadFileAsync(link:string; filename:string):boolean;
    function EndDownloadFileAsync():boolean;
  private
    _state:DownloaderState;
    _master_links:FZMasterLinkListAddr;
    _master_url_index:integer;
    _master_list_path:string;
    _downloader_update_params:DownloaderUpdateParams;
    _dl_info:CurrentDownloadInfo;

    _dlThread:FZDownloaderThread;
    _dl:FZFileDownloader;
    _th_handle:THandle;
    _filelist:FZFiles;

  public

  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}
uses LogMgr, FastMd5, IniFile, Registry, userltxdumper;

procedure PushToArray(var a:FZMasterLinkListAddr; s:string);
var
  i:integer;
begin
  i:=length(a);
  setlength(a, i+1);
  a[i]:=s;
end;

function GetFileMD5(filename:string; var md5:string):boolean;
var
  ctx:TMD5Context;
  hfile, hmap:handle;
  data:pointer;
begin
  hfile:=CreateFile(PAnsiChar(filename), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hfile = INVALID_HANDLE_VALUE then begin
    exit;
  end;

  hmap:=CreateFileMapping(hFile, nil, PAGE_READONLY, 0, 0, nil);
  if hmap = 0 then begin
    CloseHandle(hfile);
    exit;
  end;

  data:=MapViewOfFile(hmap, FILE_MAP_READ, 0, 0, 0);
  if data = nil then begin
    CloseHandle(hmap);
    CloseHandle(hfile);
    exit;
  end;

  ctx:=MD5Start();
  md5:=MD5End(ctx, data, GetFileSize(hfile, nil));
  result:=true;

  UnmapViewOfFile(data);
  CloseHandle(hmap);
  CloseHandle(hfile);
end;

function GetDownloaderUpdateParams(list_name:string; var params:DownloaderUpdateParams):boolean;
var
  cur_md5, target_md5, target_url:string;
  target_size:cardinal;
  ini:FZIniFile;
const
  UPDATER_SECTION:string ='updater';
begin
  result:=false;
  if not GetFileMD5(Application.ExeName, cur_md5) then exit;
  cur_md5:=LowerCase(cur_md5);

  ini:=FZIniFile.Create(list_name);
  target_md5:=LowerCase(ini.GetStringDef(UPDATER_SECTION, 'md5', cur_md5));
  target_url:=ini.GetStringDef(UPDATER_SECTION, 'url', '');
  target_size:=ini.GetIntDef(UPDATER_SECTION, 'size', 0);
  ini.Free();

  if target_size=0 then exit;

  if target_md5<>cur_md5 then begin
    result:=true;
    params.url:=target_url;
    params.md5:=target_md5;
    params.size:=target_size;
  end;
end;

function ParseFileList(list_name:string; filelist:FZFiles):MasterListParseResult;
var
  cfg:FZIniFile;
  section, filename, fileurl:string;
  files_count, i, compression:integer;
  fileCheckParams:FZCheckParams;
begin
  result:=MASTERLIST_PARSE_ERROR;

  cfg:=FZIniFile.Create(list_name);
  try
    if cfg.GetBoolDef('main', 'maintenance', false) then begin
      result:=MASTERLIST_MAINTENANCE;
      exit;
    end;

    files_count:=cfg.GetIntDef('main', 'files_count', 0);
    if files_count = 0 then begin
      FZLogMgr.Get.Write('No files in file list', FZ_LOG_ERROR);
      exit;
    end;

    for i:=0 to files_count-1 do begin
      section:='file_'+inttostr(i);
      FZLogMgr.Get.Write('Parsing section '+section, FZ_LOG_INFO);
      filename:=cfg.GetStringDef(section, 'path', '' );
      if (length(filename)=0) then begin
        FZLogMgr.Get.Write('Invalid name for file #'+inttostr(i), FZ_LOG_ERROR);
        exit;
      end;

      if cfg.GetBoolDef(section,'ignore', false) then begin
        if not fileList.AddIgnoredFile(filename) then begin
          FZLogMgr.Get.Write('Cannot add to ignored file #'+inttostr(i)+' ('+filename+')', FZ_LOG_ERROR);
          exit;
        end;
      end else begin
        fileurl:=cfg.GetStringDef(section, 'url', '' );
        if (length(fileurl)=0) then begin
          FZLogMgr.Get.Write('Invalid url for file #'+inttostr(i), FZ_LOG_ERROR);
          exit;
        end;

        compression:=cfg.GetIntDef(section, 'compression', 0);

        fileCheckParams.crc32:=0;
        if not cfg.GetHex(section, 'crc32', fileCheckParams.crc32) then begin
          FZLogMgr.Get.Write('Invalid crc32 for file #'+inttostr(i), FZ_LOG_ERROR);
          exit;
        end;

        fileCheckParams.size:=cfg.GetIntDef(section, 'size', 0);
        if fileCheckParams.size=0 then begin
          FZLogMgr.Get.Write('Invalid size for file #'+inttostr(i), FZ_LOG_ERROR);
          exit;
        end;
        fileCheckParams.md5:=LowerCase(cfg.GetStringDef(section, 'md5', ''));

        if not fileList.UpdateFileInfo(filename, fileurl, compression, fileCheckParams) then begin
          FZLogMgr.Get.Write('Cannot update file info #'+inttostr(i)+' ('+filename+')', FZ_LOG_ERROR);
          exit;
        end;
      end;

      result:=MASTERLIST_PARSE_OK;
    end;
  finally
    cfg.Free();
  end;

end;

function DownloadCallback(info:FZFileActualizingProgressInfo; userdata:pointer):boolean;
var
  pinfo:pCurrentDownloadInfo;
begin
  pinfo:=userdata;
  EnterCriticalSection(pinfo^.lock);
  pinfo^.info:=info;
  LeaveCriticalSection(pinfo^.lock);
  result:=true;
end;

procedure StartActualization(filelist:FZFiles); stdcall;
begin
  filelist.ActualizeFiles();
end;

function ExtractParentFromFsGame():string;
var
  f:textfile;
  line:string;
begin
  result:='';

  assignfile(f, 'fsgame.ltx');
  try
    reset(f);

    while not eof(f) do begin
      readln(f, line);
      line:=trim(line);
      if leftstr(line, length('$game_root$')) = '$game_root$' then begin
        result:=line;
        break;
      end;
    end;
    closefile(f);
  except
    result:='';
  end;
end;

function SelectGuessedGameInstallDir():string;
const
  REG_PATH:string = 'SOFTWARE\GSC Game World\STALKER-COP';
  REG_PARAM:string = 'InstallPath';
var
  reg:TRegistry;
begin
  try
    Reg:=TRegistry.Create(KEY_READ);
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(REG_PATH,false);
    result:=Reg.ReadString(REG_PARAM);
    Reg.Free;
  except
    result:='';
  end;
end;

function IsGameInstalledInDir(dir:string):boolean;
var
  files:FZMasterLinkListAddr; //I'm too lazy to rename the type
  i:integer;
  md5:string;
const
  CONFIGS_MD5 = 'F2D38A8D462E459044CC7C296FD0A168';
begin
  result:=false;

  PushToArray(files, 'resources\configs.db');
  PushToArray(files, 'resources\resources.db0');
  PushToArray(files, 'resources\resources.db1');
  PushToArray(files, 'resources\resources.db2');
  PushToArray(files, 'resources\resources.db3');
  PushToArray(files, 'resources\resources.db4');
  PushToArray(files, 'levels\levels.db0');
  PushToArray(files, 'levels\levels.db1');

  if (length(dir)>0) and (dir[length(dir)]<>'\') and (dir[length(dir)]<>'/') then begin
    dir:=dir+'\';
  end;

  for i:=0 to length(files)-1 do begin
    if not FileExists(dir+files[i]) then begin
      exit;
    end;
  end;

  if not GetFileMD5(dir+files[0], md5) then exit;
  md5:=uppercase(md5);
  if md5<>CONFIGS_MD5 then exit;

  result:=true;
end;

function CreateFsgame(parent_root:string):boolean;
var
  f:textfile;
begin
  result:=false;
  assignfile(f, 'fsgame.ltx');
  try
    rewrite(f);
    writeln(f, parent_root);
    writeln(f, '$app_data_root$ = false | false | $fs_root$| userdata\');
    writeln(f, '$arch_dir$ = false| false| $game_root$');
    writeln(f, '$arch_dir_levels$ = false| false| $game_root$| levels\');
    writeln(f, '$arch_dir_resources$ = false| false| $game_root$| resources\');
    writeln(f, '$arch_dir_localization$ = false| false| $game_root$| localization\');
    writeln(f, '$arch_dir_patches$ = false| true| $fs_root$| patches\');
    writeln(f, '$game_arch_mp$ = false| false| $game_root$| mp\');
    writeln(f, '$game_data$ = false| true| $fs_root$| gamedata\');
    writeln(f, '$game_ai$ = true| false| $game_data$| ai\');
    writeln(f, '$game_spawn$ = true| false| $game_data$| spawns\');
    writeln(f, '$game_levels$ = true| false| $game_data$| levels\');
    writeln(f, '$game_meshes$ = true| true| $game_data$| meshes\');
    writeln(f, '$game_anims$ = true| true| $game_data$| anims\');
    writeln(f, '$game_dm$ = true| true| $game_data$| meshes\');
    writeln(f, '$game_shaders$ = true| true| $game_data$| shaders\');
    writeln(f, '$game_sounds$ = true| true| $game_data$| sounds\');
    writeln(f, '$game_textures$ = true| true| $game_data$| textures\');
    writeln(f, '$game_config$ = true| false| $game_data$| configs\');
    writeln(f, '$game_weathers$ = true| false| $game_config$| environment\weathers');
    writeln(f, '$game_weather_effects$ = true| false| $game_config$| environment\weather_effects');
    writeln(f, '$textures$ = true| true| $game_data$| textures\');
    writeln(f, '$level$ = false| false| $game_levels$');
    writeln(f, '$game_scripts$ = true| false| $game_data$| scripts\');
    writeln(f, '$logs$ = true| false| $app_data_root$| logs\');
    writeln(f, '$screenshots$ = true| false| $app_data_root$| screenshots\');
    writeln(f, '$game_saves$ = true| false| $app_data_root$| savedgames\');
    writeln(f, '$downloads$ = false| false| $app_data_root$');
    closefile(f);
    result:=true;
  except
    result:=false;
  end;
end;

function CheckAndCorrectUserltx():boolean;
const
  path:string = 'userdata\user.ltx';
var
  f:textfile;
begin
  result:=FileExists(path);
  if not result then begin
    ForceDirectories('userdata');
    assignfile(f, path);
    try
      rewrite(f);
      DumpUserLtx(f);
      closefile(f);
      result:=true;
    except
      result:=false;
    end;
  end;
end;

{ TForm1 }

function TForm1.StartDownloadFileAsync(link:string; filename:string):boolean;
begin
  if (_dlThread<>nil) or (_dl <> nil) then begin
    result:=false;
    exit;
  end;

  _dlThread:=FZCurlDownloaderThread.Create();
  _dl:=_dlThread.CreateDownloader(link, filename, 0);
  result:=_dl.StartAsyncDownload();
end;

function TForm1.EndDownloadFileAsync():boolean;
begin
  result:=(_dl<>nil) and (_dlThread<>nil);

  FreeAndNil(_dl);
  FreeAndNil(_dlThread);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  error_msg:string;
  si:TStartupInfo;
  pi:TProcessInformation;
  master_parse_res:MasterListParseResult;
  hfile:handle;
  tid:cardinal;
  curdir:string;
  i:integer;
  itm_data:FZFileItemData;
  progress:FZFileActualizingProgressInfo;
  parent_root:string;
begin
  timer1.Enabled:=false;
  timer1.Interval:=0;
  error_msg:='';

  case _state of
  DL_STATE_INIT:
    begin
      SetStatus('Downloading master-list...');
      if not StartDownloadFileAsync(_master_links[_master_url_index], _master_list_path) then begin
        error_msg := 'Can''t start master-list download';
        ChangeState(DL_STATE_TERMINAL);
      end else begin
        ChangeState(DL_STATE_MASTERLIST_LOADING);
      end;
    end;

  DL_STATE_MASTERLIST_LOADING:
    begin
      if not _dl.IsDownloading() then begin
        if not _dl.IsSuccessful() then begin
          if length(_master_links)-1 > _master_url_index then begin
            _master_url_index:=_master_url_index+1;
            ChangeState(DL_STATE_INIT);
          end else begin
            error_msg := 'Error while downloading master-list';
            ChangeState(DL_STATE_TERMINAL);
          end;
        end else begin
          SetStatus('Parsing master-list...');
          ChangeState(DL_STATE_MASTERLIST_PARSE);
        end;
        EndDownloadFileAsync();
      end;
    end;

  DL_STATE_MASTERLIST_PARSE:
    begin
      if GetDownloaderUpdateParams(_master_list_path, _downloader_update_params) and (length(_downloader_update_params.url) > 0) then begin
        SetStatus('Updating downloader...');
        if not StartDownloadFileAsync(_downloader_update_params.url, _downloader_update_params.filename) then begin
          error_msg := 'Can''t downloader update for downloader';
          ChangeState(DL_STATE_TERMINAL);
        end else begin
          ChangeState(DL_STATE_UPDATE_DOWNLOADER);
        end;
      end else begin
        master_parse_res := MASTERLIST_PARSE_ERROR;
        curdir:=GetCurrentDir();
        if (length(curdir)>0) and (curdir[length(curdir)]<>'\') and (curdir[length(curdir)]<>'/') then begin
          curdir:=curdir+'\';
        end;

        _filelist:=FZFiles.Create();
        _filelist.SetDlMode(FZ_DL_MODE_CURL);
        InitializeCriticalSection(_dl_info.lock);
        _filelist.SetCallback(@DownloadCallback, @_dl_info);

        if _filelist.ScanPath(GetCurrentDir()) then begin
          master_parse_res:=ParseFileList(_master_list_path, _filelist);
        end;
        DeleteFile(PAnsiChar(_master_list_path));

        //iterate over all records and delete the ones which shouldn't be deleted
        if master_parse_res = MASTERLIST_PARSE_OK then begin
          // we won't delete files at all for safety
          for i:=_filelist.EntriesCount() downto 0 do begin
            itm_data:=_filelist.GetEntry(i);
            if (itm_data.required_action = FZ_FILE_ACTION_UNDEFINED) then begin
              _filelist.DeleteEntry(i);
            end;
          end;
        end;

        if master_parse_res = MASTERLIST_PARSE_ERROR then begin
          error_msg:='Invalid masterlist content';
        end else if master_parse_res = MASTERLIST_MAINTENANCE then begin
          error_msg:='Maintenance is in progress, please try again later';
        end else if master_parse_res = MASTERLIST_CANTPARSE then begin
          error_msg:='Downloaded masterlist file is not available. Please check access rights and add the directory to the antivirus exclusions';
        end;

        if master_parse_res = MASTERLIST_PARSE_OK then begin
          _filelist.SortBySize();
          _filelist.Dump(FZ_LOG_INFO);
          progress.status:=FZ_ACTUALIZING_BEGIN;
          _th_handle:=CreateThread(nil, 0, @StartActualization, _filelist, 0, tid);
          if _th_handle <> 0 then begin
            SetStatus('Downloading content...');
            ChangeState(DL_STATE_DATA_LOADING);
          end else begin
            error_msg:='Problems while creating downloader thread';
            ChangeState(DL_STATE_TERMINAL);
          end;
        end else begin
          DeleteCriticalSection(_dl_info.lock);
          FreeAndNil(_filelist);
          ChangeState(DL_STATE_TERMINAL);
        end;
      end;
    end;

  DL_STATE_UPDATE_DOWNLOADER:
    begin
      if not _dl.IsDownloading() then begin
        if not _dl.IsSuccessful() then begin
          error_msg := 'Error while downloading update';
        end else begin
          SetStatus('Running update...');

          //hack - helps to avoid mis-restarting
          hfile:=CreateFile(PAnsiChar(_downloader_update_params.filename), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
          CloseHandle(hfile);

          FillMemory(@si, sizeof(si),0);
          FillMemory(@pi, sizeof(pi),0);
          si.cb:=sizeof(si);
          if not CreateProcess(PAnsiChar(_downloader_update_params.filename), '', nil, nil, false, 0, nil, nil, si, pi) then begin
            error_msg := 'Can''t run update, please try again';
          end;
        end;
        ChangeState(DL_STATE_TERMINAL);
        EndDownloadFileAsync();
      end else begin
        if _downloader_update_params.size > 0 then begin
          update_progress.Min:=0;
          update_progress.Max:=_downloader_update_params.size;
          update_progress.Position:=_dl.DownloadedBytes();
        end else begin
          update_progress.Visible:=false;
        end;
      end;
    end;

  DL_STATE_DATA_LOADING:
    begin
      EnterCriticalSection(_dl_info.lock);
      progress:=_dl_info.info;
      LeaveCriticalSection(_dl_info.lock);

      if (progress.status = FZ_ACTUALIZING_FINISHED) or (progress.status = FZ_ACTUALIZING_FAILED ) then begin
        TerminateThread(_th_handle, 0);
        DeleteCriticalSection(_dl_info.lock);
        FreeAndNil(_filelist);

        if (progress.status = FZ_ACTUALIZING_FINISHED) then begin
          SetStatus('Finalizing...');
          update_progress.Min:=0;
          update_progress.Max:=1;
          update_progress.Position:=1;
          self.Repaint;
          ChangeState(DL_STATE_DATA_LOADING_COMPLETED);
        end else begin
          error_msg := 'Downloading is not successful, please try again';
          ChangeState(DL_STATE_TERMINAL);
        end;
      end else if progress.status = FZ_ACTUALIZING_VERIFYING then begin
        SetStatus('Verifying resources...');
        update_progress.Min:=0;
        update_progress.Max:=1;
        update_progress.Position:=1;
      end else begin
        update_progress.Min:=0;
        update_progress.Max:=progress.estimated_dl_size;
        update_progress.Position:=progress.total_downloaded;
      end;
    end;

  DL_STATE_DATA_LOADING_COMPLETED:
    begin
      parent_root:=ExtractParentFromFsGame();

      if (length(parent_root) = 0) then begin
        SelectDirectoryDialog1.InitialDir:=SelectGuessedGameInstallDir();
        if SelectDirectoryDialog1.Execute() then begin
          i := IDYES;
          if not IsGameInstalledInDir(SelectDirectoryDialog1.FileName) then begin
            i:=MessageBox(self.Handle, PAnsiChar('Looks like the game is NOT installed in the selected directory "'+SelectDirectoryDialog1.FileName+'".'+chr($0d)+chr($0a)+'Continue anyway?'), 'Please confirm', MB_YESNO or MB_ICONQUESTION);
          end;
          if i = IDYES then begin
            parent_root:=SelectDirectoryDialog1.FileName;
            if (length(parent_root)>0) and (parent_root[length(parent_root)]<>'\') and (parent_root[length(parent_root)]<>'/') then begin
              parent_root:=parent_root+'\';
            end;
            parent_root:= '$game_root$ = false| false| '+ parent_root;
          end;
        end else begin
          i:=MessageBox(self.Handle, 'Stop updating?', 'Please confirm', MB_YESNO or MB_ICONQUESTION);
          if i = IDYES then begin
            SetStatus('Exiting updater');
            ChangeState(DL_STATE_TERMINAL);
          end;
        end;
      end;

      if (length(parent_root) <> 0) then begin
        if CreateFsgame(parent_root) and CheckAndCorrectUserltx() then begin
          i:=MessageBox(self.Handle, PAnsiChar('The mod has been successfully updated! Do you want to run the game?'), 'Congratulations!', MB_YESNO or MB_ICONINFORMATION);
          if i = IDYES then begin
            FillMemory(@si, sizeof(si),0);
            FillMemory(@pi, sizeof(pi),0);
            si.cb:=sizeof(si);
            CreateProcess('bin\xrEngine.exe', '', nil, nil, false, 0, nil, nil, @si, @pi);
            SetStatus('Exiting updater');
          end;
        end else begin
          error_msg:='Can''t update configs';
        end;
        ChangeState(DL_STATE_TERMINAL);
      end;
    end;

  DL_STATE_TERMINAL:
    begin
      Application.Terminate;
    end;
  end;

  if length(error_msg) > 0 then begin
    MessageBox(self.Handle, PAnsiChar(error_msg), 'Error!', MB_OK or MB_ICONERROR);
    Application.Terminate;
  end;

  timer1.Enabled:=true;
  timer1.Interval:=200;
end;

procedure TForm1.SetStatus(status: string);
begin
  FZLogMgr.Get.Write('Change visual status: '+status, FZ_LOG_INFO);
  update_status.Caption:=status;
end;

procedure TForm1.ChangeState(state:DownloaderState);
begin
  FZLogMgr.Get.Write('Global downloader state changed to '+inttostr(cardinal(state)), FZ_LOG_INFO);
  _state:=state;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  path:string;
  si:TStartupInfo;
  pi:TProcessInformation;

  cnt:integer;
  status:boolean;
  hfile:handle;
const
  update_suffix:string = '.update.exe';
begin
  _downloader_update_params.filename:=Application.ExeName+update_suffix;

  if rightstr(Application.ExeName, length(update_suffix)) = update_suffix then begin
    // It's an update for downloader, let's copy it instead of the original one
    path:=leftstr(Application.ExeName, length(Application.ExeName)-length(update_suffix));

    status:=false;
    for cnt:=0 to 10 do begin
      DeleteFile(PAnsiChar(path));
      status:=CopyFile(PAnsiChar(Application.ExeName), PAnsiChar(path), false);
      if status then break;
      // Wait while updater terminates
      Sleep(1000);
    end;

    if status then begin
      //hack - helps to avoid mis-restarting
      hfile:=CreateFile(PAnsiChar(path), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      CloseHandle(hfile);

      FillMemory(@si, sizeof(si),0);
      FillMemory(@pi, sizeof(pi),0);
      si.cb:=sizeof(si);
      if not CreateProcess(PAnsiChar(path), '', nil, nil, false, 0, nil, nil, si, pi) then begin
        MessageBox(self.Handle, 'Can''t restart updater executable module. Please do it manually', 'Error!', MB_ICONERROR or MB_OK);
      end;
    end else begin
      MessageBox(self.Handle, 'Can''t replace updater executable module. Please try again or replace it manually', 'Error!', MB_ICONERROR or MB_OK);
    end;
    Application.Terminate;
  end;

  if FileExists(_downloader_update_params.filename) then begin
    status:=false;
    for cnt:=0 to 10 do begin
      status:=DeleteFile(PAnsiChar(_downloader_update_params.filename));
      if status then break;
      Sleep(1000);
    end;
  end;

  path:=GetCurrentDir();
  if (length(path)>0) and (path[length(path)]<>'\') and (path[length(path)]<>'/') then begin
    path:=path+'\';
  end;
  path:=path+'files.list';
  // TODO: Randomize array
  PushToArray(_master_links, 'http://192.168.1.2/guns_test_update.txt');
  _master_url_index:=0;
  _master_list_path:=path;
  ChangeState(DL_STATE_INIT);
end;

end.

