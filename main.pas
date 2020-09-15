unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, CheckLst,

  HttpDownloader, FileManager, Windows, LazUTF8, Localizer;

type

  MasterListParseResult = (MASTERLIST_PARSE_OK, MASTERLIST_PARSE_ERROR, MASTERLIST_MAINTENANCE, MASTERLIST_CANTPARSE);
  DownloaderState = (DL_STATE_INIT, DL_STATE_MASTERLIST_START_DOWNLOADING, DL_STATE_MASTERLIST_LOADING, DL_STATE_UPDATE_DOWNLOADER, DL_STATE_MASTERLIST_PREPARSE, DL_STATE_SELECT_CONFIG, DL_STATE_MASTERLIST_FILESPARSE, DL_STATE_DATA_LOADING, DL_STATE_DATA_LOADING_COMPLETED, DL_STATE_RUN_GAME, DL_STATE_TERMINAL);
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

  DownloadOption = record
    key:string;
    enabled:boolean;
    name_ru:string;
    name_en:string;
    groups:string;
  end;

  DownloadOptionsList = array of DownloadOption;

  { TForm1 }

  TForm1 = class(TForm)
    btn_next: TButton;
    options_list: TCheckListBox;
    Image1: TImage;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Timer1: TTimer;
    update_status: TLabel;
    update_progress: TProgressBar;
    lbl_select_options: TLabel;
    procedure btn_nextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure options_listClickCheck(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    procedure SetStatus(status:string);
    procedure ChangeState(state:DownloaderState);
    function StartDownloadFileAsync(link:string; filename:string):boolean;
    function EndDownloadFileAsync():boolean;
    procedure RedrawOptionsChecks();
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
    _options:DownloadOptionsList;

    _dlThread:FZDownloaderThread;
    _dl:FZFileDownloader;
    _th_handle:THandle;
    _filelist:FZFiles;
    _mod_initially_actual:boolean;
    _silent_mode:boolean;
    _fast_mode:boolean;

  public

  end;

var
  Form1: TForm1;

const
  OPTIONS_CONFIG_NAME:string = 'update_configuration.ini';
  FILELIST_CONFIG_NAME:string = 'files.list';

implementation
{$R *.lfm}
uses LogMgr, FastMd5, IniFile, Registry, userltxdumper, IniFiles, CommonHelper;

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

procedure DeleteFolder(FolderName: string);
var
  SR: TSearchRec;
  Len: Integer;
begin
  Len := Length(FolderName);
  if FolderName[Len] = '\' then FolderName := Copy(FolderName, 1, Len-1);
  if sysutils.FindFirst(FolderName + '\*.*', faAnyFile, SR) = 0 then begin
    repeat
      if SR.Name = '.' then Continue;
      if SR.Name = '..' then Continue;
      if SR.Attr and faDirectory <> 0 then begin
        DeleteFolder(FolderName + '\' + SR.Name)
      end else begin
        DeleteFile(PAnsiChar(FolderName + '\' + SR.Name))
      end;
    until sysutils.FindNext(SR) <> 0;
    sysutils.FindClose(SR);
  end;
  RemoveDir(FolderName);
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
{$IFDEF SKIP_DOWNLOADER_UPDATE}
  result:=false;
  exit;
{$ENDIF}

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

function FindInstallationOptionWithSameGroup(var options:DownloadOptionsList; group:string; search_only_active:boolean; exclude_id:integer; start_id:integer = 0):integer;
var
  i:integer;
  groups, cur_group:string;
begin
  result:=-1;
  if length(group) = 0 then exit;

  for i:=start_id to length(options)-1 do begin
    if search_only_active and not options[i].enabled then continue;
    if exclude_id = i then continue;

    groups:=options[i].groups+',';
    cur_group:='';
    while FZCommonHelper.GetNextParam(groups, cur_group, ',') do begin
      cur_group:=trim(cur_group);
      if cur_group = group then begin
        result:=i;
        break;
      end;
    end;

    if result>=0 then break;
  end;
end;

function CanEnableInstallationOption(var options:DownloadOptionsList; id:integer):boolean;
var
  groups, group:string;
begin
  result:=false;
  if id >= length(options) then exit;
  if options[id].enabled then exit;

  result:=true;
  groups:=options[id].groups+',';
  group:='';
  while FZCommonHelper.GetNextParam(groups, group, ',') do begin
    group:=trim(group);
    if FindInstallationOptionWithSameGroup(options, group, true, id) >= 0 then begin
      result:=false;
      break;
    end;
  end;
end;

function ChangeInstallationOptionStatus(var options:DownloadOptionsList; id:integer; new_status:boolean):boolean;
begin
  result:=false;
  if id >= length(options) then exit;
  if options[id].enabled = new_status then begin
    result:=true;
    exit;
  end;

  if new_status then begin
    //need check possibility before activating new item
    if CanEnableInstallationOption(options, id) then begin
      options[id].enabled:=true;
      result:=true;
    end;
  end else begin
    //no need to check - disablig option can't lead to conflicts and always possible
    options[id].enabled:=false;
    result:=true;
  end;
end;

function ValidateSelectedOptionsList(var options:DownloadOptionsList) :boolean;
var
   i:integer;
   groups, group:string;
begin
  result:=true;

  // Проверяем, не прописано ли в двух опциях одинаковых групп!
  for i:=0 to length(options)-1 do begin
    if not options[i].enabled then continue;
    group:='';
    groups:=options[i].groups+',';
    while (FZCommonHelper.GetNextParam(groups, group, ',')) do begin
      group:=trim(group);
      if FindInstallationOptionWithSameGroup(options, group, true, i) >= 0 then begin
        result:=false;
        break;
      end;
    end;
    if not result then break;
  end;
end;

function ParseInstallationOptions(list_name:string; var options:DownloadOptionsList; var fast_mode_allowed:boolean):boolean;
var
  cfg:FZIniFile;
  options_cfg:TIniFile;
  options_count, i:integer;
  section:string;
begin
  result:=false;
  fast_mode_allowed:=false;

  cfg:=FZIniFile.Create(UTF8ToWinCP(list_name));
  options_cfg:=TIniFile.Create(OPTIONS_CONFIG_NAME);
  try
    options_count:=cfg.GetIntDef('main', 'options_count', 0);
    FZLogMgr.Get.Write('Master list declares '+inttostr(options_count)+' installation options', FZ_LOG_IMPORTANT_INFO);

    if options_count = 0 then begin
      result:=true;
      exit;
    end;

    setlength(options, options_count);

    for i:=0 to options_count-1 do begin
      section:='option_'+inttostr(i);
      FZLogMgr.Get.Write('Parsing options section '+section, FZ_LOG_INFO);
      options[i].key:=cfg.GetStringDef(section, 'key', '' );

      // TODO: handle situation when multiple options have the same key
      if (length(options[i].key)=0) then begin
        FZLogMgr.Get.Write('Invalid key for option #'+inttostr(i), FZ_LOG_ERROR);
        exit;
      end;

      options[i].name_en:=cfg.GetStringDef(section, 'name_en', '' );
      options[i].name_ru:=cfg.GetStringDef(section, 'name_ru', '' );
      options[i].groups:=cfg.GetStringDef(section, 'groups', '' );

      if (length(options[i].name_en)>0) and (length(options[i].name_ru) = 0) then begin
        options[i].name_ru:=options[i].name_en;
      end else if (length(options[i].name_ru)>0) and (length(options[i].name_en) = 0) then begin
        options[i].name_en:=options[i].name_ru;
      end else if (length(options[i].name_en) = 0) and (length(options[i].name_ru) = 0) then begin
        options[i].name_ru := options[i].key;
        options[i].name_en := options[i].key;
      end;

      options[i].enabled:=strtointdef(options_cfg.ReadString('options', options[i].key, '0'), 0) <> 0;
      FZLogMgr.Get.Write('Option '+inttostr(i)+' is '+options[i].key+' ('+options[i].name_ru+' | '+options[i].name_en+'), status = '+booltostr(options[i].enabled, true), FZ_LOG_IMPORTANT_INFO);
    end;

    if not ValidateSelectedOptionsList(options) then begin
      FZLogMgr.Get.Write('Invalid options combination, RESET ALL options', FZ_LOG_IMPORTANT_INFO);
      for i:=0 to options_count-1 do begin
        options[i].enabled:=false;
      end;
    end else begin
      fast_mode_allowed:=true;
    end;

    result:=true;
  finally
    if not result then begin
      setlength(options, 0);
    end;

    cfg.Free();
    options_cfg.Free();
  end;
end;

procedure SaveInstallationOptions(options:DownloadOptionsList);
var
  options_cfg:TIniFile;
  i:integer;
  val:string;
begin
  options_cfg:=TIniFile.Create(OPTIONS_CONFIG_NAME);
  try
    for i:=0 to length(options)-1 do begin
      if options[i].enabled then begin
        val:='1';
      end else begin
        val:='0';
      end;
      options_cfg.WriteString('options', options[i].key, val);
    end;

  finally
    options_cfg.Free();
  end;
end;

procedure MarkInstallationAsValid(valid:boolean);
var
  options_cfg:TIniFile;
  val:string;
begin
  options_cfg:=TIniFile.Create(OPTIONS_CONFIG_NAME);
  try
    if valid then begin
      val:='1';
    end else begin
      val:='0';
    end;
    options_cfg.WriteString('main', 'installation_valid', val);
  finally
    options_cfg.Free();
  end;
end;

procedure SaveLastUpdateTime(time:TDateTime; updates_present:boolean);
var
  options_cfg:TIniFile;
  val:string;
begin
  options_cfg:=TIniFile.Create(OPTIONS_CONFIG_NAME);
  try
    if updates_present then begin
      val:='1';
    end else begin
      val:='0';
    end;
    options_cfg.WriteString('main', 'updates_present', val);
    options_cfg.WriteString('main', 'last_update_check', DateTimeToStr(time));
  finally
    options_cfg.Free();
  end;
end;


function CheckFileConditionString(condstr:string; options:DownloadOptionsList):boolean;
var
  inverse:boolean;
  i, j:integer;
  opt_found:boolean;
  condstr_mod:string;
  cur_cond:string;
const
  OP_STOP:AnsiChar='#';
  OP_NEG:AnsiChar='!';
  OP_OR:AnsiChar='|';
begin
  result:=false;
  inverse:=false;
  cur_cond:='';
  condstr_mod:=condstr+OP_OR+OP_STOP;
  for i:=1 to length(condstr_mod) do begin
    if condstr_mod[i]=OP_STOP then begin
        break;
    end else if condstr_mod[i]=OP_NEG then begin
      inverse:=not inverse;
    end else if condstr_mod[i]=OP_OR then begin
      cur_cond:=trim(cur_cond);

      // Пустой кондишн считаем всегда истинным
      if length(cur_cond) = 0 then begin
        result:=true;
        break;
      end;

      opt_found:=false;
      for j:=0 to length(options)-1 do begin
        if options[j].key = cur_cond then begin
          opt_found:=true;
          result:=options[j].enabled;
          break;
        end;
      end;

      if not opt_found then begin
        result:=inverse;
      end else if inverse then begin
        result:=not result;
      end;

      if result then begin
        break;
      end else begin
        inverse:=false;
        cur_cond:='';
      end;
    end else begin
      cur_cond:=cur_cond+condstr_mod[i];
    end;
  end;
end;

function ParseFileList(list_name:string; filelist:FZFiles; ignore_maintenance:boolean; var items_to_install:string; options:DownloadOptionsList):MasterListParseResult;
var
  cfg:FZIniFile;
  section, filename, fileurl, condstr:string;
  meet_condition:boolean;
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

      condstr:=cfg.GetStringDef(section, 'condition', '');
      if length(condstr) > 0 then begin
        meet_condition:=CheckFileConditionString(condstr, options);
        if meet_condition then begin
          FZLogMgr.Get.Write('File #'+inttostr(i)+' ('+filename+') meets condition '+condstr, FZ_LOG_IMPORTANT_INFO);
        end else begin
          FZLogMgr.Get.Write('File #'+inttostr(i)+' ('+filename+') doesn''t meet condition '+condstr, FZ_LOG_IMPORTANT_INFO);
        end;
      end else begin
        meet_condition:=true;
      end;

      if cfg.GetBoolDef(section,'delete', false) or not meet_condition then begin
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

procedure FilterDeletionItems(list_name:string; filelist:FZFiles; options:DownloadOptionsList);
var
  cfg:FZIniFile;
  i, j:integer;
  section, filename, itmname:string;
  itm_data:FZFileItemData;
  delflag:boolean;
  condstr:string;
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
            condstr:=cfg.GetStringDef(section, 'condition', '');
            delflag:=cfg.GetBoolDef(section,'delete', false) or not CheckFileConditionString(condstr, options);
            break;
          end;
        end;

        if not delflag then begin
          filelist.DeleteEntry(i);
          FZLogMgr.Get.Write('Skipping file #'+inttostr(i)+' ('+itm_data.name+')', FZ_LOG_IMPORTANT_INFO);
        end else begin
          FZLogMgr.Get.Write('Leave file #'+inttostr(i)+' ('+itm_data.name+') in the list for removing', FZ_LOG_IMPORTANT_INFO);
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
      FZLogMgr.Get.Write('File "'+ filelist.GetEntry(i).name+'" NOT in actual state ('+inttostr(integer(filelist.GetEntry(i).required_action))+')', FZ_LOG_INFO);
      result:=false;
      break;
    end;
  end;
end;

procedure DumpUninstallList(list:string; full:boolean);
var
  f:textfile;
  str:string;
  updatelog_present:boolean;
  optionsconfig_present:boolean;
  filelistconfig_present:boolean;
  updaterexecutable_present:boolean;
  updatername:string;
const
  UNINSTALL_DATA_PATH:string='uninstall.dat';
  UPDATER_LOG_NAME:string='update.log';
begin
  updatername:=lowercase(GetExecutableName());
  updatelog_present:=false;
  optionsconfig_present:=false;
  filelistconfig_present:=false;
  updaterexecutable_present:=false;

  assignfile(f, UNINSTALL_DATA_PATH);
  try
    if not full then begin
      reset(f);
      while not eof(f) do begin
        readln(f, str);
        str:=lowercase(trim(str));
        if length(str) = 0 then continue;

        if str = UPDATER_LOG_NAME then updatelog_present:=true;
        if str = OPTIONS_CONFIG_NAME then optionsconfig_present:=true;
        if str = FILELIST_CONFIG_NAME then filelistconfig_present:=true;
        if str = updatername then updaterexecutable_present:=true;
      end;
      closefile(f);

      append(f);
    end else if length(list) > 0 then begin
      rewrite(f);
      writeln(f, list);
    end;

    if not updatelog_present then writeln(f, UPDATER_LOG_NAME);
    if not optionsconfig_present then writeln(f, OPTIONS_CONFIG_NAME);
    if not filelistconfig_present then writeln(f, FILELIST_CONFIG_NAME);
    if not updaterexecutable_present then writeln(f, updatername);
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
{$IFDEF CHANGES_LOCKED}
    writeln(f, '$app_data_root$ = false | false | $fs_root$| userdata\');
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
    writeln(f, '$arch_dir$ = false| false| $game_root$');
    writeln(f, '$arch_dir_levels$ = false| false| $game_root$| levels\');
    writeln(f, '$arch_dir_resources$ = false| false| $game_root$| resources\');
    writeln(f, '$arch_dir_localization$ = false| false| $game_root$| localization\');
    writeln(f, '$arch_dir_patches$ = false| true| $fs_root$| patches\');
    writeln(f, '$game_arch_mp$ = false| false| $game_root$| mp\');
    writeln(f, '$logs$ = true| false| $app_data_root$| logs\');
    writeln(f, '$screenshots$ = true| false| $app_data_root$| screenshots\');
    writeln(f, '$game_saves$ = true| false| $app_data_root$| savedgames\');
    writeln(f, '$downloads$ = false| false| $app_data_root$');
{$ELSE}
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
{$ENDIF}
    closefile(f);
    result:=true;
  except
    result:=false;
  end;
end;

function FindLogFile():string;
var
  SR: TSearchRec;
const
  LOG_DIR: string = 'userdata\logs\';
  LOG_NAME: string = 'xray_*.log';
begin
  result:='';
  if sysutils.FindFirst(LOG_DIR+LOG_NAME, faAnyFile, SR) = 0 then begin
    repeat
      if SR.Attr and faDirectory = 0 then begin
        result:=LOG_DIR+SR.Name;
        break;
      end;
    until sysutils.FindNext(SR) <> 0;
    sysutils.FindClose(SR);
  end;
end;

function CheckForCrashInLog(logname:string):boolean;
var
  f:textfile;
  ln:string;
begin
  result:=false;
  assignfile(f, logname);
  try
    reset(f);
    while not eof(f) do begin
      readln(f, ln);
      if ln = 'stack trace:' then begin
        result:=true;
        break;
      end;
    end;
    closefile(f);
  except
  end;

end;

function CheckAndCorrectUserltx(var changes_made:boolean):boolean;
const
  path:string = 'userdata\user.ltx';
  path_cp:string= 'userdata\user.ltx.backup';
var
  f, f_cp:textfile;
  logname, ln:string;
begin
  changes_made:=false;
  result:=FileExists(path);
  if not result then begin
    ForceDirectories('userdata');
    assignfile(f, path);
    try
      rewrite(f);
      DumpUserLtx(f, screen.Width, screen.Height, true);
      closefile(f);
      result:=true;
      changes_made:=true;
    except
      result:=false;
    end;
  end else begin
    logname:=FindLogFile();
    FZLogMgr.Get.Write('Log file is '+logname, FZ_LOG_IMPORTANT_INFO);
    if (length(logname) > 0) and CheckForCrashInLog(logname) then begin
      FZLogMgr.Get.Write('Crash detected, rollback to fail-safe settings', FZ_LOG_IMPORTANT_INFO);

      if FileExists(path_cp) then begin
        DeleteFile(PAnsiChar(path_cp));
      end;

      if not MoveFile(PAnsiChar(path), PAnsiChar(path_cp)) then begin
        FZLogMgr.Get.Write('Can''t backup old user.ltx', FZ_LOG_ERROR);
        result:=false;
      end else begin
        assignfile(f, path);
        assignfile(f_cp, path_cp);
        try
          rewrite(f);

          // Copy user's bindings
          reset(f_cp);
          while not eof(f_cp) do begin
            readln(f_cp, ln);
            if leftstr(ln, length('bind')) = 'bind' then begin
              writeln(f, ln);
            end;
          end;
          closefile(f_cp);

          //Dump the other settings
          DumpUserLtx(f, screen.Width, screen.Height, false);
          closefile(f);
          result:=true;
          changes_made:=true;

          DeleteFile(PAnsiChar(logname));
        except
          result:=false;
        end;
      end;
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
    writeln(f, 'chcp '+inttostr(GetACP())+' > nul');
    writeln(f, ':1');
    writeln(f, 'del "'+oldname+'"');
    writeln(f, 'if exist "'+oldname+'" goto 1');
    writeln(f, 'move "'+newname+'" "'+oldname+'"');
    writeln(f, '@start "" "'+oldname+'" '+params);
    writeln(f, 'del "'+batname+'" && exit');
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
  need_update:boolean;
  configs_were_changed:boolean;
  next_state:DownloaderState;
  fastmode_allowed:boolean;
begin
  timer1.Enabled:=false;
  timer1.Interval:=0;
  error_msg:='';
  error_caption:='err_caption';
  error_icon_style:=MB_ICONERROR;
  retry_allowed:=not _silent_mode and not _fast_mode;
  fastmode_allowed:=true;

  case _state of
  DL_STATE_INIT:
    begin
      _mod_initially_actual:=false;
      _master_url_index:=0;
      self.update_progress.Position:=0;
      setlength(_options, 0);

      if _silent_mode then begin
        // To prevent start multiple updater instances in background
        SaveLastUpdateTime(Now(), false);
      end;

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
          ChangeState(DL_STATE_MASTERLIST_PREPARSE);
        end;
        EndDownloadFileAsync();
      end;
    end;

  DL_STATE_MASTERLIST_PREPARSE:
    begin
      retry_allowed:=false;

      if not _ignore_maintenance and IsMaintenance(_master_list_path, nil) then begin
        error_msg:='err_maintenance';
        error_caption:='err_warning';
        error_icon_style:=MB_ICONWARNING;
      end else if GetDownloaderUpdateParams(_master_list_path, _downloader_update_params) and (length(_downloader_update_params.url) > 0) then begin
        SetStatus('stage_update_downloader');
        if _silent_mode then begin
          FZLogMgr.Get.Write('Downloader needs update', FZ_LOG_INFO);
          SaveLastUpdateTime(Now(), true);
          ChangeState(DL_STATE_TERMINAL);
        end else if not StartDownloadFileAsync(_downloader_update_params.url, _downloader_update_params.filename) then begin
          error_msg := 'err_updater_update_dl';
        end else begin
          ChangeState(DL_STATE_UPDATE_DOWNLOADER);
        end;
      end else begin
        //Пробуем распарсить все доступные опции
        if ParseInstallationOptions(_master_list_path, _options, fastmode_allowed) then begin
          //Рисуем на форме доступные опции
          if length(_options) > 0 then begin
            self.options_list.Items.Clear();
            for i:=0 to length(_options)-1 do begin
              self.options_list.AddItem(SelectLocalized(_options[i].name_ru, _options[i].name_en), nil);
              self.options_list.Checked[i]:=_options[i].enabled;
            end;

            if fastmode_allowed and (_fast_mode or _silent_mode) then begin
              // Выбранный список опций валиден и непротиворечив, режимы fast и silent доступны
              // Не показываем никаких контролов пользователю, чтобы стейт-машина автоматом приняла его
              FZLogMgr.Get.Write('Applying previous-saved options and skip selection in GUI', FZ_LOG_IMPORTANT_INFO);
              self.options_list.Visible:=false;
              self.btn_next.Visible:=false;
              self.lbl_select_options.Visible:=false;
              RedrawOptionsChecks();
              ChangeState(DL_STATE_SELECT_CONFIG);
            end else if _silent_mode then begin
              // Опции из конфига противоречат друг другу, но мы в silent-режиме
              // Однозначно надо не показывать контролы и сохранять инфу о том, что требуется обновление
              FZLogMgr.Get.Write('Mutually exclusive options in silent mode - terminating', FZ_LOG_IMPORTANT_INFO);
              SaveLastUpdateTime(Now(), true);
              ChangeState(DL_STATE_TERMINAL);
            end else begin
              // Используем обычный графический режим с правом пользователя на самостоятельный выбор опций
              FZLogMgr.Get.Write('Now allow user to select options', FZ_LOG_IMPORTANT_INFO);
              self.options_list.Visible:=true;
              self.btn_next.Visible:=true;
              self.lbl_select_options.Visible:=true;
              RedrawOptionsChecks();
              ChangeState(DL_STATE_SELECT_CONFIG);
            end;
          end else begin
            RedrawOptionsChecks();
            ChangeState(DL_STATE_SELECT_CONFIG);
          end;
        end else begin
          error_msg:='err_invalid_masterlist';
        end;
      end;

      if length(error_msg) > 0 then begin
        DeleteFile(PAnsiChar(UTF8ToWinCP(_master_list_path)));
      end;
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
        DeleteFile(PAnsiChar(UTF8ToWinCP(_master_list_path)));
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

  DL_STATE_SELECT_CONFIG:
    begin
      retry_allowed:=false;
      if not self.options_list.Visible and not self.btn_next.Visible then begin
        for i:=0 to length(_options)-1 do begin
          _options[i].enabled:=self.options_list.Checked[i];
          FZLogMgr.Get.Write('Option "'+_options[i].key+'" status is "'+booltostr(_options[i].enabled, true)+'"', FZ_LOG_INFO);
        end;
        ChangeState(DL_STATE_MASTERLIST_FILESPARSE)
      end;
    end;

  DL_STATE_MASTERLIST_FILESPARSE:
    begin
      retry_allowed:=false;

      master_parse_res := MASTERLIST_PARSE_ERROR;

      _filelist:=FZFiles.Create();
      _filelist.SetDlMode(FZ_DL_MODE_CURL);
      InitializeCriticalSection(_dl_info.lock);
      _filelist.SetCallback(@DownloadCallback, @_dl_info);
      FZLogMgr.Get.Write('Scanning path "'+_download_dir+'"', FZ_LOG_INFO);
      if _filelist.ScanPath(_download_dir) then begin
        FZLogMgr.Get.Write('Parsing master list "'+_master_list_path+'"', FZ_LOG_INFO);
        master_parse_res:=ParseFileList(_master_list_path, _filelist, _ignore_maintenance, _uninstall_list, _options);
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
        if _silent_mode then begin
          FilterDeletionItems(_master_list_path, _filelist, _options);
          _filelist.SortBySize();
          _filelist.Dump(FZ_LOG_INFO);
          need_update:=not IsAllFilesActual(_filelist);
          SaveLastUpdateTime(Now(), need_update);
          FZLogMgr.Get.Write('Need update: '+booltostr(need_update, true), FZ_LOG_IMPORTANT_INFO);
          ChangeState(DL_STATE_TERMINAL);
        end else begin
          MarkInstallationAsValid(false);
          SaveInstallationOptions(_options);
          FilterDeletionItems(_master_list_path, _filelist, _options);
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

      DeleteFile(PAnsiChar(UTF8ToWinCP(_master_list_path)));
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
        DumpUninstallList(_uninstall_list, true);
        configs_were_changed:=false;
        if CreateFsgame(parent_root) and CheckAndCorrectUserltx(configs_were_changed) then begin
          if not _mod_initially_actual then begin
            DeleteFolder('userdata\shaders_cache');
          end;
          MarkInstallationAsValid(true);
          SaveLastUpdateTime(Now(), false);
          if _mod_initially_actual and not configs_were_changed then begin
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
      DumpUninstallList('', false);
      Application.Terminate;
    end;
  end;

  if length(error_msg) > 0 then begin
    FZLogMgr.Get.Write('Visual Error: '+error_msg, FZ_LOG_ERROR);
    if _silent_mode or _fast_mode then begin
      ChangeState(DL_STATE_TERMINAL);
    end else if retry_allowed then begin
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
  MAIN_MASTER_LINK:string = 'https://raw.githubusercontent.com/gunslingermod/updater_links/master/guns.list';
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

  self.options_list.Left:=self.update_progress.Left;
  self.options_list.Width:=self.update_progress.Width;
  self.options_list.Top:=30;
  self.options_list.Height:=self.update_progress.Top - self.options_list.Top - 50;
  self.options_list.Visible:=false;

  self.lbl_select_options.Top:=self.options_list.Top - 25;
  self.lbl_select_options.Left:=self.update_status.Left;
  self.lbl_select_options.Visible:=false;
  self.lbl_select_options.Caption:=LocalizeString('caption_select_options');

  self.btn_next.Caption:=LocalizeString('next');
  self.btn_next.Width:=80;
  self.btn_next.Left:= (self.Width-btn_next.Width) div 2;
  self.btn_next.Top:=options_list.Top+self.options_list.Height+10;
  self.btn_next.Visible:=false;

  self._silent_mode:=false;
  self._fast_mode:=false;

  _downloader_update_params.filename:=GetExecutableName()+update_suffix;

  if FileExists(_downloader_update_params.filename) then begin
    DeleteFile(PAnsiChar(UTF8ToWinCP(_downloader_update_params.filename)));
  end;

  _download_dir:='.\';
  _master_list_path:=_download_dir+FILELIST_CONFIG_NAME;

  if ParamCount = 0 then begin
    // TODO: Randomize array
    PushToArray(_master_links, MAIN_MASTER_LINK);
  end else if ParamStr(1) = 'silent' then begin
    _silent_mode:=true;
    FZLogMgr.Get.Write('Using SILENT mode', FZ_LOG_IMPORTANT_INFO);
    PushToArray(_master_links, MAIN_MASTER_LINK);
  end else if ParamStr(1) = 'fast' then begin
    _fast_mode:=true;
    FZLogMgr.Get.Write('Using FAST mode', FZ_LOG_IMPORTANT_INFO);
    PushToArray(_master_links, MAIN_MASTER_LINK);
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

procedure TForm1.FormShow(Sender: TObject);
begin
  if _silent_mode then begin
    self.Hide();
  end;
end;

procedure TForm1.RedrawOptionsChecks();
var
  i:integer;
begin
  for i:=0 to self.options_list.Count-1 do begin
    if self._options[i].enabled then begin
      self.options_list.Checked[i]:=true;
      self.options_list.ItemEnabled[i]:=true;
    end else if CanEnableInstallationOption(self._options, i) then begin
      self.options_list.Checked[i]:=false;
      self.options_list.ItemEnabled[i]:=true;
    end else begin
      self.options_list.Checked[i]:=false;
      self.options_list.ItemEnabled[i]:=false;
    end;
  end;
end;

procedure TForm1.options_listClickCheck(Sender: TObject);
var
  i, idx:integer;
begin
  if self.options_list.Count<>length(self._options) then begin
    FZLogMgr.Get.Write('Options count mismatch! Visual '+inttostr(self.options_list.Count)+', real '+inttostr(length(self._options)), FZ_LOG_ERROR);
    exit;
  end;

  idx:=-1;
  // Пробежимся по всем опциям и найдем индекс той, состояние которой поменялось
  for i:=0 to self.options_list.Count-1 do begin
    if self.options_list.Checked[i]<>self._options[i].enabled then begin
      idx:=i;
      break;
    end;
  end;
  if idx < 0 then exit;

  // Сообщим о том, что опция выбрана - должны пересчитаться ограничения
  if not ChangeInstallationOptionStatus(self._options, idx, self.options_list.Checked[i]) then begin
    FZLogMgr.Get.Write('Cannot change status to '+booltostr(self.options_list.Checked[i])+' for '+self._options[i].key, FZ_LOG_ERROR);
  end;

  // Перерисуем с учетом новых ограничений
  RedrawOptionsChecks();
end;

procedure TForm1.btn_nextClick(Sender: TObject);
begin
  self.btn_next.Visible:=false;
  self.options_list.Visible:=false;
  self.lbl_select_options.Visible:=false;
end;

end.

