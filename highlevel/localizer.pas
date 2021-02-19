unit Localizer;

{$mode objfpc}{$H+}

interface

function LocalizeString(str:string):string;
function SelectLocalized(rus:string; eng:string):string;

implementation
uses windows;

function LocalizeString(str: string): string;
begin
  result:=str;

  if str = 'stage_dl_masterlist' then begin
    result:=SelectLocalized('Загрузка мастер-списка...', 'Downloading master-list...');
  end else if str = 'stage_parse_masterlist' then begin
    result:=SelectLocalized('Обработка мастер-списка...', 'Parsing master-list...');
  end else if str = 'stage_calc_files' then begin
    result:=SelectLocalized('Анализ конфигурации...', 'Analyzing current configuration...');
  end else if str = 'stage_update_downloader' then begin
    result:=SelectLocalized('Обновление загрузчика...', 'Updating downloader...');
  end else if str = 'stage_dl_content'  then begin
    result:=SelectLocalized('Загрузка файлов мода...', 'Downloading content...');
  end else if str = 'stage_run_downloader_update'  then begin
    result:=SelectLocalized('Установка обновления загрузчика...', 'Running update...');
  end else if str = 'stage_finalizing'  then begin
    result:=SelectLocalized('Настройка мода...', 'Finalizing...');
  end else if str = 'stage_verifying'  then begin
    result:=SelectLocalized('Верификация скачанных файлов...', 'Verifying resources...');
  end else if str = 'stage_exiting' then begin
    result:=SelectLocalized('Завершение работы загрузчика.', 'Exiting updater.');
  end else if str = 'err_master_start_dl' then begin
    result:=SelectLocalized('Не удалось начать загрузку мастер-списка.', 'Can''t start master-list download.');
  end else if str = 'err_master_dl'  then begin
    result:=SelectLocalized('Не удалось загрузить мастер-список.', 'Error while downloading master-list.');
  end else if str = 'err_maintenance' then begin
    result:=SelectLocalized('Ведутся технические работы. Пожалуйста, попробуйте позже.', 'Maintenance is in progress, please try again later');
  end else if str = 'err_updater_update_dl' then begin
    result:=SelectLocalized('Не удалось скачать обновление загрузчика.', 'Can''t download update for downloader.');
  end else if str = 'err_invalid_masterlist' then begin
    result:=SelectLocalized('Содержимое мастер-списка повреждено', 'Invalid masterlist content');
  end else if str = 'err_masterlist_open' then begin
    result:=SelectLocalized('Не удалось получить доступ к данным скачанного мастер-списка, проверьте наличие прав и настройки антивируса', 'Downloaded masterlist file is not available. Please check access rights and add the directory to the antivirus exclusions');
  end else if str = 'err_cant_start_dl_thread' then begin
    result:=SelectLocalized('Не удалось начать загрузку', 'Problems while creating downloader thread');
  end else if str = 'err_cant_start_calc_thread' then begin
    result:=SelectLocalized('Не удалось начать анализ конфигурации', 'Problems while creating checker thread');
  end else if str = 'err_integrity_check_failure' then begin
    result:=SelectLocalized('Нарушена целостность скачанных файлов', 'Update integrity check failed.');
  end else if str = 'err_bat_copy_fail' then begin
    result:=SelectLocalized('Не удалось записать BAT-файл для обновления, проверьте настройки антивируса или скопируйте файл с обновлением вручную', 'Can''t write BAT file, please check anti-virus settings or copy the update manually');
  end else if str = 'err_cant_run_update' then begin
    result:=SelectLocalized('Не удалось запустить обновление, проверьте настройки антивируса или запустите обновление вручную', 'Can''t run update, please check anti-virus settings or copy the update manually');
  end else if str = 'err_dl_not_successful' then begin
    result:=SelectLocalized('Загрузка не удалась.', 'Downloading is not successful.');
  end else if str = 'err_cant_update_configs' then begin
    result:=SelectLocalized('Не удалось обновить конфигурационные файлы мода', 'Can''t update configs');
  end else if str = 'err_caption' then begin
    result:=SelectLocalized('Ошибка!', 'Error!');
  end else if str = 'err_warning' then begin
    result:=SelectLocalized('Внимание!', 'Warning!');
  end else if str = 'msg_confirm' then begin
    result:=SelectLocalized('Требуется подтверждение', 'Please confirm');
  end else if str = 'msg_congrats' then begin
    result:=SelectLocalized('Успех', 'Congratulations!');
  end else if str = 'msg_select_game_dir' then begin
    result:=SelectLocalized('Пожалуйста, укажите директорию, в которой установлена оригинальная игра', 'Now please select a directory where the original game is installed');
  end else if str = 'msg_no_game_in_dir' then begin
    result:=SelectLocalized('Похоже, что оригинальная игра НЕ установлена в директории', 'Looks like the game is NOT installed in the selected directory');
  end else if str ='msg_continue_anyway'  then begin
    result:=SelectLocalized('Продолжить в любом случае?', 'Continue anyway?');
  end else if str ='msg_cancel_install' then begin
    result:=SelectLocalized('Прекратить установку?', 'Stop updating?');
  end else if str ='msg_success_run_game'  then begin
    result:=SelectLocalized('Мод был успешно обновлен. Желаете сыграть прямо сейчас?', 'The mod has been successfully updated. Do you want to run the game?');
  end else if str ='msg_noactions_run_game'  then begin
    result:=SelectLocalized('Файлы мода в актуальном состоянии, обновление не требуется. Желаете сыграть прямо сейчас?', 'The mod is in actual state, update is not needed. Do you want to run the game?');
  end else if str ='retry_question'  then begin
    result:=SelectLocalized('Попробовать еще раз?', 'Retry?');
  end else if str ='next' then begin
    result:=SelectLocalized('Далее', 'Next');
  end else if str ='caption_select_options' then begin
    result:=SelectLocalized('Выберите желаемые опции:', 'Please select options to install:');
  end;
end;

function SelectLocalized(rus: string; eng: string): string;
var
  locale:cardinal;
const
  RUS_ID:cardinal=1049;
begin
  locale:=GetSystemDefaultLangID();
  if locale = RUS_ID then begin
    result:=rus;
  end else begin
    result:=eng;
  end;
end;

end.

