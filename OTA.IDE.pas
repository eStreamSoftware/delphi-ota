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

uses SysUtils, Classes, Registry;

resourcestring
  StrActiveProjectModule = 'ActiveProjectModule';
  StrActiveHostApplication = 'ActiveHostApplication';

procedure Register;
begin
  TSetActiveProjectModule.Setup;
  SplashScreenServices.AddProductBitmap('E Stream Software IDE Expert', 0);
end;

procedure TSetActiveProjectModule.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TSetActiveProjectModule.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
var F: string;
    R: TRegistry;
    sModuleName, sHostApplication: string;
    S: TStringList;
    i: integer;
    M: IOTAModuleServices;
begin
  sHostApplication := '';
  sModuleName := '';

  M := BorlandIDEServices as IOTAModuleServices;

  // Calculate Active Project Module
  S := TStringList.Create;
  try
    S.Delimiter := '\';
    S.DelimitedText := M.GetActiveProject.FileName;
    if S.Count >= 4 then
      sModuleName := S[S.Count - 4];
  finally
    S.Free;
  end;

  // Calculate Active Host Application
  for i := M.MainProjectGroup.ProjectCount - 1 downto 0 do begin
    F := M.MainProjectGroup.Projects[i].ProjectOptions.TargetName;
    if SameText(ExtractFileExt(F), '.EXE') then
      sHostApplication := F;
  end;

  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    F := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\Environment Variables';
    if R.OpenKey(F, True) then begin
      if R.ReadString(StrActiveProjectModule) <> sModuleName then begin
        R.WriteString(StrActiveProjectModule, sModuleName);
        SetEnvironmentVariable(PAnsiChar(StrActiveProjectModule), PAnsiChar(sModuleName));
      end;

      if R.ReadString(StrActiveHostApplication) <> sHostApplication then begin
        R.WriteString(StrActiveHostApplication, sHostApplication);
        SetEnvironmentVariable(PAnsiChar(StrActiveHostApplication), PAnsiChar(sHostApplication));
      end;

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
