unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls,

  HttpDownloader, FileManager, Windows, LazUTF8, Localizer;

type

  MasterListParseResult = (MASTERLIST_PARSE_OK, MASTERLIST_PARSE_ERROR, MASTERLIST_MAINTENANCE, MASTERLIST_CANTPARSE);
  DownloaderState = (DL_STATE_INIT, DL_STATE_MASTERLIST_START_DOWNLOADING, DL_STATE_MASTERLIST_LOADING, DL_STATE_UPDATE_DOWNLOADER, DL_STATE_MASTERLIST_PARSE, DL_STATE_DATA_LOADING, DL_STATE_DATA_LOADING_COMPLETED, DL_STATE_RUN_GAME, DL_STATE_TERMINAL);
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
    Image1: TImage;
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
    _download_dir:string;
    _downloader_update_params:DownloaderUpdateParams;
    _dl_info:CurrentDownloadInfo;
    _ignore_maintenance:boolean;
    _uninstall_list:string;

    _dlThread:FZDownloaderThread;
    _dl:FZFileDownloader;
    _th_handle:THandle;
    _filelist:FZFiles;
    _mod_initially_actual:boolean;

  public

  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}
uses LogMgr, FastMd5, IniFile, Registry, userltxdumper;

// We need to use 'Native' WinApi functions to avoid problems with conversion of codepages
// The functions return results using current locale codepeages
function GetWorkingDirectory():string;
var
  arr:array of char;
  val:cardinal;
begin
  result:='';
  val:=GetCurrentDirectory(0, nil);
  if val <= 0 then exit;
  setlength(arr, val);

  val:=GetCurrentDirectory(val, @arr[0]);
  if (val <=0) or (val >= cardinal(length(arr))) then exit;

  result:=PAnsiChar(@arr[0]);
end;

function GetExecutableName():string;
var
  buf:array of char;
  res:cardinal;
  dir:string;
begin
  result:='';

  setlength(buf, 260);
  repeat
    res:=GetModuleFileName(0, @buf[0], length(buf));
    if res >= cardinal(length(buf)) then begin
      setlength(buf, length(buf) * 2);
    end else begin
      result:=PAnsiChar(@buf[0]);
      break;
    end;
  until res = 0;

  if length(result) > 0 then begin
    dir:=GetWorkingDirectory();
    if (length(result) > length(dir)) and (leftstr(result, length(dir)) = dir) then begin
      result:=rightstr(result, length(result)-length(dir));
      if (result[1]<>'/') and (result[1]<>'\') then begin
        result:='.\'+result;
      end else begin
        result:='.'+result;
      end;
    end;
  end;
end;

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
  fname:string;
begin
  result:=false;
  fname:=UTF8ToWinCP(filename);
  FZLogMgr.Get.Write('Opening "'+fname+'" for MD5 calculation', FZ_LOG_DBG);
  hfile:=CreateFile(PAnsiChar(fname), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
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
  fname:string;
const
  UPDATER_SECTION:string ='updater';
begin
  result:=false;
  cur_md5:='';
  fname:=GetExecutableName();
  if not GetFileMD5(fname, cur_md5) then begin
    FZLogMgr.Get.Write('Cannot calculate MD5 of "'+fname+'", cancelling update', FZ_LOG_ERROR);
    exit;
  end;
  cur_md5:=LowerCase(cur_md5);

  ini:=FZIniFile.Create(UTF8ToWinCP(list_name));
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

function IsMaintenance(list_name:string; cfg:FZIniFile=nil):boolean;
var
  owncfg:boolean;
begin
  result:=true;
  owncfg:=false;
  if cfg = nil then begin
    cfg:=FZIniFile.Create(UTF8ToWinCP(list_name));
    owncfg:=true;
  end;

  result:=cfg.GetBoolDef('main', 'maintenance', false);

  if owncfg then begin
    cfg.Free;
  end;
end;

function ParseFileList(list_name:string; filelist:FZFiles; ignore_maintenance:boolean; var items_to_install:string):MasterListParseResult;
var
  cfg:FZIniFile;
  section, filename, fileurl:string;
  files_count, i, compression:integer;
  fileCheckParams:FZCheckParams;
begin
  result:=MASTERLIST_PARSE_ERROR;
  items_to_install:='';

  cfg:=FZIniFile.Create(UTF8ToWinCP(list_name));
  try
    if not ignore_maintenance and IsMaintenance(list_name, cfg) then begin
      result:=MASTERLIST_MAINTENANCE;
      exit;
    end;

    files_count:=cfg.GetIntDef('main', 'files_count', 0);
    if files_count = 0 then begin
      FZLogMgr.Get.Write('No files in file list', FZ_LOG_ERROR);
      result:=MASTERLIST_PARSE_ERROR;
      exit;
    end;

    for i:=0 to files_count-1 do begin
      section:='file_'+inttostr(i);
      FZLogMgr.Get.Write('Parsing section '+section, FZ_LOG_INFO);
      filename:=cfg.GetStringDef(section, 'path', '' );
      if (length(filename)=0) then begin
        FZLogMgr.Get.Write('Invalid name for file #'+inttostr(i), FZ_LOG_ERROR);
        result:=MASTERLIST_PARSE_ERROR;
        exit;
      end;

      if cfg.GetBoolDef(section,'delete', false) then begin
        // The file will be processed later while checking the list
        FZLogMgr.Get.Write('File #'+inttostr(i)+' ('+filename+') needs removing', FZ_LOG_INFO);
      end else if cfg.GetBoolDef(section,'ignore', false) then begin
        if not fileList.AddIgnoredFile(filename) then begin
          FZLogMgr.Get.Write('Cannot add to ignored file #'+inttostr(i)+' ('+filename+')', FZ_LOG_ERROR);
          result:=MASTERLIST_PARSE_ERROR;
          exit;
        end;
      end else begin
        fileurl:=cfg.GetStringDef(section, 'url', '' );
        if (length(fileurl)=0) then begin
          FZLogMgr.Get.Write('Invalid url for file #'+inttostr(i), FZ_LOG_ERROR);
          result:=MASTERLIST_PARSE_ERROR;
          exit;
        end;

        compression:=cfg.GetIntDef(section, 'compression', 0);

        fileCheckParams.crc32:=0;
        if not cfg.GetHex(section, 'crc32', fileCheckParams.crc32) then begin
          FZLogMgr.Get.Write('Invalid crc32 for file #'+inttostr(i), FZ_LOG_ERROR);
          result:=MASTERLIST_PARSE_ERROR;
          exit;
        end;

        fileCheckParams.size:=cfg.GetIntDef(section, 'size', 0);
        if fileCheckParams.size=0 then begin
          FZLogMgr.Get.Write('Invalid size for file #'+inttostr(i), FZ_LOG_ERROR);
          result:=MASTERLIST_PARSE_ERROR;
          exit;
        end;
        fileCheckParams.md5:=LowerCase(cfg.GetStringDef(section, 'md5', ''));

        if not fileList.UpdateFileInfo(filename, fileurl, compression, fileCheckParams) then begin
          FZLogMgr.Get.Write('Cannot update file info #'+inttostr(i)+' ('+filename+')', FZ_LOG_ERROR);
          result:=MASTERLIST_PARSE_ERROR;
          exit;
        end;

        items_to_install:=items_to_install+filename+chr($0d)+chr($0a);
      end;

      result:=MASTERLIST_PARSE_OK;
    end;
  finally
    cfg.Free();
  end;
end;

procedure FilterDeletionItems(list_name:string; filelist:FZFiles);
var
  cfg:FZIniFile;
  i, j:integer;
  section, filename, itmname:string;
  itm_data:FZFileItemData;
  delflag:boolean;
begin
  cfg:=FZIniFile.Create(UTF8ToWinCP(list_name));
  try
    // we won't delete files for safety (except explicitely defined ones)
    for i:=filelist.EntriesCount() downto 0 do begin
      itm_data:=filelist.GetEntry(i);
      if (itm_data.required_action = FZ_FILE_ACTION_UNDEFINED) then begin
        delflag:=false;

        // Check if the file is marked as 'deleted' in the config
        for j:=0 to cfg.GetIntDef('main', 'files_count', 0)-1 do begin
          section:='file_'+inttostr(j);
          filename:=stringreplace(cfg.GetStringDef(section, 'path', '' ), '/', '\',[rfReplaceAll]);
          itmname:=stringreplace(itm_data.name, '/', '\', [rfReplaceAll]);
          if filename=itmname then begin
            delflag:=cfg.GetBoolDef(section,'delete', false);
            break;
          end;
        end;

        if not delflag then begin
          filelist.DeleteEntry(i);
          FZLogMgr.Get.Write('Skipping file #'+inttostr(i)+' ('+itm_data.name+')', FZ_LOG_ERROR);
        end else begin
          FZLogMgr.Get.Write('Leave file #'+inttostr(i)+' ('+itm_data.name+') in the list for removing', FZ_LOG_ERROR);
        end;

      end;
    end;
  finally
    cfg.Free();
  end;
end;

function IsAllFilesActual(filelist:FZFiles):boolean;
var
  i:integer;
begin
  result:=true;
  for i:=0 to filelist.EntriesCount()-1 do begin
    if (filelist.GetEntry(i).required_action <> FZ_FILE_ACTION_NO) and (filelist.GetEntry(i).required_action<>FZ_FILE_ACTION_IGNORE) then begin
      result:=false;
      break;
    end;
  end;
end;

procedure DumpUninstallList(list:string);
var
  f:textfile;
const
  UNINSTALL_DATA_PATH:string='uninstall.dat';
begin
  assignfile(f, UNINSTALL_DATA_PATH);
  try
    rewrite(f);
    writeln(f, list);
    writeln(f, 'update.log');
    writeln(f, GetExecutableName());
    closefile(f)
  except
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

procedure StartActualization(frm:TForm1); stdcall;
begin
  if not frm._filelist.ActualizeFiles() then begin
    EnterCriticalSection(frm._dl_info.lock);
    frm._dl_info.info.status:=FZ_ACTUALIZING_FAILED;
    LeaveCriticalSection(frm._dl_info.lock);
  end;
end;

function ExtractParentFromFsGame():string;
var
  f:textfile;
  line:string;
  i:integer;
begin
  result:='';

  assignfile(f, 'fsgame.ltx');
  try
    reset(f);

    while not eof(f) do begin
      readln(f, line);
      line:=trim(line);
      if leftstr(line, length('$game_root$')) = '$game_root$' then begin
        result:='';
        for i:= length(line) downto 1 do begin
          if line[i] = '|' then begin
            if length(result) > 0 then break else continue;
          end;
          result:=line[i]+result;
        end;
        break;
      end;
    end;
    closefile(f);
  except
    result:='';
  end;

  result:=WinCPToUTF8(trim(result));
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

  result:=WinCPToUTF8(trim(result));
end;

function IsGameInstalledInDir(dir:string):boolean;
var
  files:FZMasterLinkListAddr; //I'm too lazy to rename the type
  i:integer;
  md5:string;
  filename:string;
const
  CONFIGS_MD5 = 'F2D38A8D462E459044CC7C296FD0A168';
begin
  result:=false;

  setlength(files, 0);
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
    filename:=dir+files[i];
    if not FileExists(filename) then begin
      exit;
    end;
  end;

  md5:='';
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
    parent_root:= '$game_root$ = false| false| '+ UTF8ToWinCP(parent_root);
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
      DumpUserLtx(f, screen.Width, screen.Height);
      closefile(f);
      result:=true;
    except
      result:=false;
    end;
  end;
end;

function DropBatFile(batname:string; oldname:string; newname:string):boolean;
var
  f:textfile;
  i:integer;
  params:string;
begin
  result:=false;

  params:='';
  for i:=1 to ParamCount do begin
    params:= params + ParamStr(i)+' ';
  end;

  assignfile(f, batname);
  try
    batname:=UTF8ToWinCP(batname);
    oldname:=UTF8ToWinCP(oldname);
    newname:=UTF8ToWinCP(newname);
    rewrite(f);
    writeln(f, 'chcp '+inttostr(GetACP())+' >nul');
    writeln(f, ':1');
    writeln(f, 'del "'+oldname+'"');
    writeln(f, 'if exist "'+oldname+'" goto 1');
    writeln(f, 'move "'+newname+'" "'+oldname+'"');
    writeln(f, '@start "" "'+oldname+'" '+params);
    writeln(f, 'del "'+batname+'"');
    closefile(f);
    result:=true;
  except
    result:=false;
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
  _dl:=_dlThread.CreateDownloader(link, UTF8ToWinCP(filename), 0);
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
  error_msg, error_caption:string;
  error_icon_style:DWORD;
  si:TStartupInfo;
  pi:TProcessInformation;
  master_parse_res:MasterListParseResult;
  tid:cardinal;
  i:integer;
  progress:FZFileActualizingProgressInfo;
  confirmed:boolean;
  parent_root:string;
  bat:string;
  md5:string;
  retry_allowed:boolean;

  next_state:DownloaderState;
begin
  timer1.Enabled:=false;
  timer1.Interval:=0;
  error_msg:='';
  error_caption:='err_caption';
  error_icon_style:=MB_ICONERROR;
  retry_allowed:=true;

  case _state of
  DL_STATE_INIT:
    begin
      _mod_initially_actual:=false;
      _master_url_index:=0;
      self.update_progress.Position:=0;
      ChangeState(DL_STATE_MASTERLIST_START_DOWNLOADING);
    end;

  DL_STATE_MASTERLIST_START_DOWNLOADING:
    begin
      SetStatus('stage_dl_masterlist');
      if not StartDownloadFileAsync(_master_links[_master_url_index], _master_list_path) then begin
        error_msg := 'err_master_start_dl';
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
            FZLogMgr.Get.Write('Switching to masterlist #: '+inttostr(_master_url_index)+': '+_master_links[_master_url_index], FZ_LOG_IMPORTANT_INFO);
            ChangeState(DL_STATE_MASTERLIST_START_DOWNLOADING);
          end else begin
            error_msg := 'err_master_dl';
          end;
        end else begin
          SetStatus('stage_parse_masterlist');
          ChangeState(DL_STATE_MASTERLIST_PARSE);
        end;
        EndDownloadFileAsync();
      end;
    end;

  DL_STATE_MASTERLIST_PARSE:
    begin
      retry_allowed:=false;

      if not _ignore_maintenance and IsMaintenance(_master_list_path, nil) then begin
        error_msg:='err_maintenance';
        error_caption:='err_warning';
        error_icon_style:=MB_ICONWARNING;
      end else if GetDownloaderUpdateParams(_master_list_path, _downloader_update_params) and (length(_downloader_update_params.url) > 0) then begin
        SetStatus('stage_update_downloader');
        if not StartDownloadFileAsync(_downloader_update_params.url, _downloader_update_params.filename) then begin
          error_msg := 'err_updater_update_dl';
        end else begin
          ChangeState(DL_STATE_UPDATE_DOWNLOADER);
        end;
      end else begin
        master_parse_res := MASTERLIST_PARSE_ERROR;

        _filelist:=FZFiles.Create();
        _filelist.SetDlMode(FZ_DL_MODE_CURL);
        InitializeCriticalSection(_dl_info.lock);
        _filelist.SetCallback(@DownloadCallback, @_dl_info);
        FZLogMgr.Get.Write('Scanning path "'+_download_dir+'"', FZ_LOG_INFO);
        if _filelist.ScanPath(_download_dir) then begin
          FZLogMgr.Get.Write('Parsing master list "'+_master_list_path+'"', FZ_LOG_INFO);
          master_parse_res:=ParseFileList(_master_list_path, _filelist, _ignore_maintenance, _uninstall_list);
        end;

        if master_parse_res = MASTERLIST_PARSE_ERROR then begin
          error_msg:='err_invalid_masterlist';
        end else if master_parse_res = MASTERLIST_MAINTENANCE then begin
          error_msg:='err_maintenance';
          error_caption:='err_warning';
          error_icon_style:=MB_ICONWARNING;
        end else if master_parse_res = MASTERLIST_CANTPARSE then begin
          error_msg:='err_masterlist_open';
        end else if master_parse_res = MASTERLIST_PARSE_OK then begin
          FilterDeletionItems(_master_list_path, _filelist);
          _filelist.SortBySize();
          _filelist.Dump(FZ_LOG_INFO);
          if IsAllFilesActual(_filelist) then begin
            _mod_initially_actual:=true;
            FZLogMgr.Get.Write('All files are in actual state', FZ_LOG_IMPORTANT_INFO);
            self.update_progress.Min:=0;
            self.update_progress.Max:=100;
            self.update_progress.Position:=100;
            FreeAndNil(_filelist);
            DeleteCriticalSection(_dl_info.lock);
            SetStatus('stage_finalizing');
            ChangeState(DL_STATE_DATA_LOADING_COMPLETED);
          end else begin
            progress.status:=FZ_ACTUALIZING_BEGIN;
            tid:=0;
            _th_handle:=CreateThread(nil, 0, @StartActualization, self, 0, tid);
            if _th_handle <> 0 then begin
              SetStatus('stage_dl_content');
              ChangeState(DL_STATE_DATA_LOADING);
            end else begin
              error_msg:='err_cant_start_dl_thread';
            end;
          end;
        end else begin
          if length(error_msg)=0 then begin
            error_msg:='err_invalid_masterlist';
          end;
        end;

        if length(error_msg) > 0 then begin
          DeleteCriticalSection(_dl_info.lock);
          FreeAndNil(_filelist);
        end;
      end;

      DeleteFile(PAnsiChar(UTF8ToWinCP(_master_list_path)));
    end;

  DL_STATE_UPDATE_DOWNLOADER:
    begin
      md5:='';
      if not _dl.IsDownloading() then begin
        if not _dl.IsSuccessful() then begin
          error_msg := 'err_updater_update_dl';
        end else if not GetFileMD5(_downloader_update_params.filename, md5) or (lowercase(_downloader_update_params.md5)<>lowercase(md5))  then begin
          error_msg:='err_integrity_check_failure';
        end else begin
          SetStatus('stage_run_downloader_update');

          bat:=GetExecutableName()+'.update.bat';
          if not DropBatFile(bat, GetExecutableName(), _downloader_update_params.filename) then begin
            error_msg := 'err_bat_copy_fail';
          end else begin
            FillMemory(@si, sizeof(si),0);
            FillMemory(@pi, sizeof(pi),0);
            si.cb:=sizeof(si);
            if not CreateProcess(nil, PAnsiChar('cmd.exe /C @start "" /B "'+UTF8ToWinCP(bat)+'"'), nil, nil, false, CREATE_NO_WINDOW, nil, nil, si, pi) then begin
              error_msg := 'err_cant_run_update';
            end else begin
              CloseHandle(pi.hProcess);
              CloseHandle(pi.hThread);

              // Downloader updated normally. Finish work of current instance.
              ChangeState(DL_STATE_TERMINAL);
            end;
          end;
        end;
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
          SetStatus('stage_finalizing');
          update_progress.Min:=0;
          update_progress.Max:=1;
          update_progress.Position:=1;
          self.Repaint;
          ChangeState(DL_STATE_DATA_LOADING_COMPLETED);
        end else begin
          error_msg := 'err_dl_not_successful';
        end;
      end else if progress.status = FZ_ACTUALIZING_VERIFYING then begin
        SetStatus('stage_verifying');
        update_progress.Min:=0;
        update_progress.Max:=1;
        update_progress.Position:=1;
      end else begin
        if (progress.estimated_dl_size > 0) and (progress.total_downloaded <= progress.estimated_dl_size) then begin
          update_progress.Min:=0;
          update_progress.Max:=progress.estimated_dl_size;
          update_progress.Position:=progress.total_downloaded;
        end;
      end;
    end;

  DL_STATE_DATA_LOADING_COMPLETED:
    begin
      retry_allowed:=false;
      confirmed:=true;
      parent_root:=ExtractParentFromFsGame();

      if (length(parent_root) = 0) or not IsGameInstalledInDir(parent_root) then begin
        confirmed:=false;
        SelectDirectoryDialog1.FileName:='';
        if length(parent_root) > 0 then begin
          SelectDirectoryDialog1.InitialDir:=parent_root;
        end else begin
          SelectDirectoryDialog1.InitialDir:=SelectGuessedGameInstallDir();
        end;
        SelectDirectoryDialog1.Title:=LocalizeString('msg_select_game_dir');

        Application.MessageBox(PAnsiChar(LocalizeString('msg_select_game_dir')), PAnsiChar(LocalizeString('msg_confirm')), MB_OK or MB_ICONINFORMATION);
        if SelectDirectoryDialog1.Execute() then begin
          i := IDYES;
          if not IsGameInstalledInDir(SelectDirectoryDialog1.FileName) then begin
            i:=Application.MessageBox(PAnsiChar(LocalizeString('msg_no_game_in_dir') + ' "'+UTF8ToWinCP(SelectDirectoryDialog1.FileName)+'".'+chr($0d)+chr($0a)+LocalizeString('msg_continue_anyway')), PAnsiChar(LocalizeString('msg_confirm')), MB_YESNO or MB_ICONQUESTION);
          end;
          if i = IDYES then begin
            parent_root:=SelectDirectoryDialog1.FileName;
            if (length(parent_root)>0) and (parent_root[length(parent_root)]<>'\') and (parent_root[length(parent_root)]<>'/') then begin
              parent_root:=parent_root+'\';
            end;
            confirmed:=true;
          end;
        end else begin
          i:=Application.MessageBox(PAnsiChar(LocalizeString('msg_cancel_install')), PAnsiChar(LocalizeString('msg_confirm')), MB_YESNO or MB_ICONQUESTION);
          if i = IDYES then begin
            SetStatus('stage_exiting');
            ChangeState(DL_STATE_TERMINAL);
          end;
        end;
      end;

      if (length(parent_root) <> 0) and confirmed then begin
        next_state:=DL_STATE_TERMINAL;
        DumpUninstallList(_uninstall_list);
        if CreateFsgame(parent_root) and CheckAndCorrectUserltx() then begin
          if _mod_initially_actual then begin
            i:=Application.MessageBox(PAnsiChar(LocalizeString('msg_noactions_run_game')), PAnsiChar(LocalizeString('msg_congrats')), MB_YESNO or MB_ICONINFORMATION);
          end else begin
            i:=Application.MessageBox(PAnsiChar(LocalizeString('msg_success_run_game')), PAnsiChar(LocalizeString('msg_congrats')), MB_YESNO or MB_ICONINFORMATION);
          end;
          if i = IDYES then begin
            next_state:=DL_STATE_RUN_GAME;
          end;
        end else begin
          error_msg:='err_cant_update_configs';
        end;
        ChangeState(next_state);
      end;
    end;

  DL_STATE_RUN_GAME:
    begin
      retry_allowed:=false;
      FillMemory(@si, sizeof(si),0);
      FillMemory(@pi, sizeof(pi),0);
      si.cb:=sizeof(si);
      CreateProcess('bin\xrEngine.exe', '', nil, nil, false, 0, nil, nil, @si, @pi);
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
      ChangeState(DL_STATE_TERMINAL);
    end;

  DL_STATE_TERMINAL:
    begin
      Application.Terminate;
    end;
  end;

  if length(error_msg) > 0 then begin
    FZLogMgr.Get.Write('Visual Error: '+error_msg, FZ_LOG_ERROR);
    if retry_allowed then begin
      error_msg:=LocalizeString(error_msg);
      if error_msg[length(error_msg)] <> '.' then begin
        error_msg:=error_msg+'.';
      end;
      error_msg:=error_msg+' '+LocalizeString('retry_question');
      i:=Application.MessageBox(PAnsiChar(LocalizeString(error_msg)), PAnsiChar(LocalizeString(error_caption)), MB_YESNO or error_icon_style);
      if i = IDYES then begin
        FZLogMgr.Get.Write('Retry selected', FZ_LOG_IMPORTANT_INFO);
        ChangeState(DL_STATE_INIT);
      end else begin
        ChangeState(DL_STATE_TERMINAL);
      end;
    end else begin
      Application.MessageBox(PAnsiChar(LocalizeString(error_msg)), PAnsiChar(LocalizeString(error_caption)), MB_OK or error_icon_style);
      ChangeState(DL_STATE_TERMINAL);
    end;
  end;

  timer1.Enabled:=true;
  timer1.Interval:=200;
end;

procedure TForm1.SetStatus(status: string);
begin
  FZLogMgr.Get.Write('Change visual status: '+status, FZ_LOG_IMPORTANT_INFO);
  update_status.Caption:=LocalizeString(status);
end;

procedure TForm1.ChangeState(state:DownloaderState);
begin
  FZLogMgr.Get.Write('Global downloader state changed to '+inttostr(cardinal(state)), FZ_LOG_INFO);
  _state:=state;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  update_suffix:string = '.upd.exe';
  border_size:integer = 10;
  between_label_and_progress:integer=2;
begin
  self.Caption:=self.Caption+' (Build ' + {$INCLUDE %DATE} + ')';
  FZLogMgr.Get.Write(self.Caption, FZ_LOG_IMPORTANT_INFO);

  self.Image1.Top:=0;
  self.Image1.Left:=0;
  self.Image1.Width:=self.Image1.Picture.Width;
  self.Image1.Height:=self.Image1.Picture.Height;
  self.Width:=self.Image1.Width;
  self.Height:=self.Image1.Height;

  self.update_progress.Left:=border_size;
  self.update_progress.Width:=self.Width-2*border_size;
  self.update_progress.Top:=self.Height-self.update_progress.Height-border_size;
  self.update_progress.Position:=0;

  self.update_status.Left:=border_size;
  self.update_status.Width:=self.Width-2*border_size;
  self.update_status.Top:=self.Height-self.update_progress.Height-border_size-self.update_status.Height-between_label_and_progress;
  self.update_status.Caption:='';

  _downloader_update_params.filename:=GetExecutableName()+update_suffix;

  if FileExists(_downloader_update_params.filename) then begin
     DeleteFile(PAnsiChar(UTF8ToWinCP(_downloader_update_params.filename)));
  end;

  _download_dir:='.\';
  _master_list_path:=_download_dir+'files.list';

  if ParamCount = 0 then begin
    // TODO: Randomize array
    PushToArray(_master_links, 'https://raw.githubusercontent.com/gunslingermod/updater_links/master/guns.list');
  end else begin
    FZLogMgr.Get.Write('Set master link to "'+ParamStr(1)+'"', FZ_LOG_INFO);
    PushToArray(_master_links, ParamStr(1));
  end;

  if (paramcount >= 2) and (trim(ParamStr(2))='true') then begin
    FZLogMgr.Get.Write('Ignoring maintenance mode', FZ_LOG_INFO);
    _ignore_maintenance:=true;
  end else begin
    _ignore_maintenance:=false;
  end;

  ChangeState(DL_STATE_INIT);
  Timer1.Enabled:=true;
end;

end.

