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
    function GetModuleName(const aFileName: string): string;
    procedure SetVariable(const aName, aValue: string);
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
  SplashScreenServices.AddPluginBitmap('E Stream Software IDE Expert', 0);
end;

procedure TSetActiveProjectModule.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TSetActiveProjectModule.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
var F: string;
    sHostApplication: string;
    i: integer;
    M: IOTAModuleServices;
begin
  sHostApplication := '';

  M := BorlandIDEServices as IOTAModuleServices;

  // Calculate Active Host Application
  for i := M.MainProjectGroup.ProjectCount - 1 downto 0 do begin
    F := M.MainProjectGroup.Projects[i].ProjectOptions.TargetName;
    if SameText(ExtractFileExt(F), '.EXE') then
      sHostApplication := F;
  end;

  SetVariable(StrActiveHostApplication, sHostApplication);
end;

procedure TSetActiveProjectModule.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var sExt: string;
    sGroup, sProj: string;
begin
  if NotifyCode = ofnFileOpening then begin
    sExt := ExtractFileExt(FileName);
    sGroup := {$if CompilerVersion<=18} '.bdsgroup' {$else} '.groupproj' {$ifend};
    sProj := {$if CompilerVersion<=18} '.bdsproj' {$else} '.dproj' {$ifend};
    if SameText(sExt, sGroup) or SameText(sExt, sProj) then
      SetVariable(StrActiveProjectModule, GetModuleName(FileName));
  end;
  Cancel := False;
end;

class procedure TSetActiveProjectModule.Setup;
var N: IOTAIDENotifier;
begin
  N := TSetActiveProjectModule.Create;
  NotifierIndex := (BorlandIDEServices as IOTAServices).AddNotifier(N);
end;

procedure TSetActiveProjectModule.SetVariable(const aName, aValue: string);
var R: TRegistry;
    F: string;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    F := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\Environment Variables';
    if R.OpenKey(F, True) then begin
      if R.ReadString(aName) <> aValue then begin
        R.WriteString(aName, aValue);
        SetEnvironmentVariable(PAnsiChar(aName), PAnsiChar(aValue));
      end;
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
end;

function TSetActiveProjectModule.GetModuleName(const aFileName: string): string;
var S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Delimiter := '\';
    S.DelimitedText := aFileName;
    if S.Count >= 4 then
      Result := S[S.Count - 4];
  finally
    S.Free;
  end;
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
