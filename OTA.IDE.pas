unit OTA.IDE;

interface

uses Windows, ToolsAPI;

type
  TSetActiveProjectModule = class(TNotifierObject, IOTAIDENotifier)
  private
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  protected
    class var NotifierIndex: Integer;
  public
    class procedure Setup;
    class procedure TearDown;
  end;

procedure Register;

implementation

uses SysUtils, Classes, Registry, Dialogs;

resourcestring
  StrActiveProjectModule = 'ActiveProjectModule';

procedure Register;
begin
  TSetActiveProjectModule.Setup;
end;

procedure TSetActiveProjectModule.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TSetActiveProjectModule.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
var F: string;
    R: TRegistry;
    sModuleName: string;
    S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Delimiter := '\';
    S.DelimitedText := (BorlandIDEServices as IOTAModuleServices).GetActiveProject.FileName;
    if S.Count < 4 then
      MessageDlg('Unable to determine project module name', mtError, [mbOK], 0);
    sModuleName := S[S.Count - 4];
  finally
    S.Free;
  end;

  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    F := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\Environment Variables';
    if R.OpenKey(F, True) then begin
      if R.ReadString(StrActiveProjectModule) <> sModuleName then
        R.WriteString(StrActiveProjectModule, sModuleName);
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
end;

procedure TSetActiveProjectModule.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
end;

class procedure TSetActiveProjectModule.Setup;
var N: IOTAIDENotifier;
begin
  N := TSetActiveProjectModule.Create;
  NotifierIndex := (BorlandIDEServices as IOTAServices).AddNotifier(N);
end;

class procedure TSetActiveProjectModule.TearDown;
begin
  if TSetActiveProjectModule.NotifierIndex <> -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(NotifierIndex);
end;

initialization
  TSetActiveProjectModule.NotifierIndex := -1;
finalization
  TSetActiveProjectModule.TearDown;
end.
