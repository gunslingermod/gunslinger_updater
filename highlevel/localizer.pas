unit Localizer;

{$mode objfpc}{$H+}

interface

function LocalizeString(str:string):string;

implementation
uses windows;

function LocalizeString(str: string): string;
var
  locale:cardinal;
const
  RUS_ID:cardinal=1049;
begin
  result:=str;
  locale:=GetSystemDefaultLangID();
  if str = 'stage_dl_masterlist' then begin
    if locale = RUS_ID then result:='Загрука мастер-списка...' else result:='Downloading master-list...';
  end else if str = 'stage_parse_masterlist' then begin
    if locale = RUS_ID then result:='Обработка мастер-списка...' else result:='Parsing master-list...';
  end else if str = 'stage_update_downloader' then begin
    if locale = RUS_ID then result:='Обновление загрузчика...' else result:='Updating downloader...';
  end else if str = 'stage_dl_content'  then begin
    if locale = RUS_ID then result:='Загрузка файлов мода...' else result:='Downloading content...';
  end else if str = 'stage_run_downloader_update'  then begin
    if locale = RUS_ID then result:='Установка обновления загрузчика...' else result:='Running update...';
  end else if str = 'stage_finalizing'  then begin
    if locale = RUS_ID then result:='Настройка мода...' else result:='Finalizing...';
  end else if str = 'stage_verifying'  then begin
    if locale = RUS_ID then result:='Верификация скачанных файлов...' else result:='Verifying resources...';
  end else if str = 'stage_exiting' then begin
    if locale = RUS_ID then result:='Завершение работы загрузчика.' else result:='Exiting updater.';
  end else if str = 'err_master_start_dl' then begin
    if locale = RUS_ID then result:='Не удалось начать загрузку мастер-списка!' else result:='Can''t start master-list download!';
  end else if str = 'err_master_dl'  then begin
    if locale = RUS_ID then result:='Не удалось загрузить мастер-список!' else result:='Error while downloading master-list!';
  end else if str = 'err_maintenance' then begin
    if locale = RUS_ID then result:='Ведутся технические работы. Пожалуйста, попробуйте позже' else result:='Maintenance is in progress, please try again later';
  end else if str = 'err_updater_update_dl' then begin
    if locale = RUS_ID then result:='Не удалось скачать обновление для загрузчика!' else result:='Can''t download update for downloader!';
  end else if str = 'err_invalid_masterlist' then begin
    if locale = RUS_ID then result:='Содержимое мастер-списка повреждено' else result:='Invalid masterlist content';
  end else if str = 'err_masterlist_open' then begin
    if locale = RUS_ID then result:='Не удалось получить доступ к данным скачанного мастер-списка, проверьте наличие прав и настройки антивируса' else result:='Downloaded masterlist file is not available. Please check access rights and add the directory to the antivirus exclusions';
  end else if str = 'err_cant_start_dl_thread' then begin
    if locale = RUS_ID then result:='Не удалось начать загрузку' else result:='Problems while creating downloader thread';
  end else if str = 'err_integrity_check_failure' then begin
    if locale = RUS_ID then result:='Нарушена целостность скачанных файлов, попробуйте начать заново' else result:='Checking update integrity failed, please try again';
  end else if str = 'err_bat_copy_fail' then begin
    if locale = RUS_ID then result:='Не удалось записать BAT-файл для обновления, проверьте настройки антивируса или скопируйте файл с обновлением вручную' else result:='Can''t write BAT file, please check anti-virus settings or copy the update manually';
  end else if str = 'err_cant_run_update' then begin
    if locale = RUS_ID then result:='Не удалось запустить обновление, проверьте настройки антивируса или запустите обновление вручную' else result:='Can''t run update, please check anti-virus settings or copy the update manually';
  end else if str = 'err_dl_not_successful' then begin
    if locale = RUS_ID then result:='Загрузка не удалась, попробуйте снова' else result:='Downloading is not successful, please try again';
  end else if str = 'err_cant_update_configs' then begin
    if locale = RUS_ID then result:='Не удалось обновить конфигурационные файлы мода' else result:='Can''t update configs';
  end else if str = 'err_caption' then begin
    if locale = RUS_ID then result:='Ошибка!' else result:='Error!';
  end else if str = 'err_warning' then begin
    if locale = RUS_ID then result:='Внимание!' else result:='Warning!';
  end else if str = 'msg_confirm' then begin
    if locale = RUS_ID then result:='Требуется подтверждение' else result:='Please confirm';
  end else if str = 'msg_congrats' then begin
    if locale = RUS_ID then result:='Успех' else result:='Congratulations!';
  end else if str = 'msg_select_game_dir' then begin
    if locale = RUS_ID then result:='Пожалуйста, укажите директорию, в которой установлена оригинальная игра' else result:='Now please select a directory where the original game is installed';
  end else if str = 'msg_no_game_in_dir' then begin
    if locale = RUS_ID then result:='Похоже, что оригинальная игра НЕ установлена в директории' else result:='Looks like the game is NOT installed in the selected directory';
  end else if str ='msg_continue_anyway'  then begin
    if locale = RUS_ID then result:='Продолжить в любом случае?' else result:='Continue anyway?';
  end else if str ='msg_cancel_install' then begin
    if locale = RUS_ID then result:='Прекратить установку?' else result:='Stop updating?';
  end else if str ='msg_success_run_game'  then begin
    if locale = RUS_ID then result:='Мод был успешно обновлен. Желаете сыграть прямо сейчас?' else result:='The mod has been successfully updated! Do you want to run the game?';
{  end else if str =  then begin
    if locale = RUS_ID then result:= else result:=;
}  end;

end;

end.

