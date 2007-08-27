unit OTA.IDE;

interface

uses Windows, ToolsAPI;

type
  TOTAUtil = class abstract
  public
    class procedure SetVariable(const aName, aValue: string);
  end;

  TSetActiveProjectModule = class(TNotifierObject, IOTAIDENotifier)
  private
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  protected
    class var NotifierIndex: Integer;
    function GetModuleName(const aFileName: string): string;
  public
    class procedure Setup;
    class procedure TearDown;
  end;

  TSetOEMDir = class(TNotifierObject, IOTAIDENotifier)
  private
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  protected
    class var NotifierIndex: Integer;
    function GetOEMDir(const aFileName: string; out aOEMDir: string): Boolean;
  public
    procedure AfterConstruction; override;
    class procedure Setup;
    class procedure TearDown;
  end;

procedure Register;

implementation

uses SysUtils, Classes, Registry, Dialogs, FileCtrl;

resourcestring
  StrActiveProjectModule   = 'ActiveProjectModule';
  StrActiveHostApplication = 'ActiveHostApplication';
  StrFactoryDir            = 'FactoryDir';
  StrOEMDir                = 'OEMDir';

procedure Register;
begin
  TSetActiveProjectModule.Setup;
  TSetOEMDir.Setup;
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
  TOTAUtil.SetVariable(StrActiveHostApplication, sHostApplication);
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
      TOTAUtil.SetVariable(StrActiveProjectModule, GetModuleName(FileName));
  end;
  Cancel := False;
end;

class procedure TSetActiveProjectModule.Setup;
var N: IOTAIDENotifier;
begin
  N := TSetActiveProjectModule.Create;
  NotifierIndex := (BorlandIDEServices as IOTAServices).AddNotifier(N);
end;

function TSetActiveProjectModule.GetModuleName(const aFileName: string): string;
var S: TStringList;
    i: integer;
begin
  Result := '';
  S := TStringList.Create;
  try
    S.CaseSensitive := False;
    S.Delimiter := '\';
    S.DelimitedText := aFileName;
    i := S.IndexOf('Project');
    if (i <> -1) and (i < S.Count) then 
      Result := S[i + 2];
  finally
    S.Free;
  end;
end;

class procedure TSetActiveProjectModule.TearDown;
begin
  if NotifierIndex <> -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(NotifierIndex);
end;

{ TSetOEMDir }

procedure TSetOEMDir.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TSetOEMDir.AfterConstruction;
var s: string;
begin
  inherited;
  s := GetEnvironmentVariable(StrFactoryDir);
  if (s = '') or not DirectoryExists(s) then begin
    if SelectDirectory(Format('Choose Factory Folder (%s)', [StrFactoryDir]), '', s) then
      TOTAUtil.SetVariable(StrFactoryDir, s);
  end;
end;

procedure TSetOEMDir.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
var sOEMDir, sEnvOEMDir: string;
    M: IOTAModuleServices;
begin
  M := BorlandIDEServices as IOTAModuleServices;
  if GetOEMDir(M.MainProjectGroup.FileName, sOEMDir) then begin
    sEnvOEMDir := GetEnvironmentVariable(StrOEMDir);
    if sEnvOEMDir <> sOEMDir then
      TOTAUtil.SetVariable(StrOEMDir, sOEMDir);
  end;
end;

procedure TSetOEMDir.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var sExt: string;
    sGroup, sProj: string;
    sOEMDir: string;
begin
  if NotifyCode = ofnFileOpening then begin
    sExt := ExtractFileExt(FileName);
    sGroup := {$if CompilerVersion<=18} '.bdsgroup' {$else} '.groupproj' {$ifend};
    sProj := {$if CompilerVersion<=18} '.bdsproj' {$else} '.dproj' {$ifend};
    if SameText(sExt, sGroup) or SameText(sExt, sProj) then begin
      if GetOEMDir(FileName, sOEMDir) then
        TOTAUtil.SetVariable(StrOEMDir, sOEMDir);
    end;
  end;
  Cancel := False;
end;

function TSetOEMDir.GetOEMDir(const aFileName: string; out aOEMDir: string):
    Boolean;
var N: TStringList;
    sIniFile: string;
begin
  aOEMDir := '';
  sIniFile := Format('%s\%s', [ExtractFilePath(aFileName), 'setup.ini']);
  Result := FileExists(sIniFile);
  if not Result then Exit;

  N := TStringList.Create;
  try
    N.CaseSensitive := False;
    N.LoadFromFile(sIniFile);
    aOEMDir := Format('%s\%s\oem\%s', [GetEnvironmentVariable(StrFactoryDir), N.Values['Factory'], N.Values['OEM']]);
    Result := True;
  finally
    N.Free;
  end;
end;

class procedure TSetOEMDir.Setup;
var N: IOTAIDENotifier;
begin
  N := TSetOEMDir.Create;
  NotifierIndex := (BorlandIDEServices as IOTAServices).AddNotifier(N);
end;

class procedure TSetOEMDir.TearDown;
begin
  if NotifierIndex <> -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(NotifierIndex);
end;

class procedure TOTAUtil.SetVariable(const aName, aValue: string);
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

initialization
  TSetActiveProjectModule.NotifierIndex := -1;
  TSetOEMDir.NotifierIndex := -1;
finalization
  TSetActiveProjectModule.TearDown;
  TSetOEMDir.TearDown;
end.
