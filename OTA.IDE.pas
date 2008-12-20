unit OTA.IDE;

interface

uses Windows, ToolsAPI, Menus, Messages;

type
  TOTAUtil = class abstract
  public
    class function GetSourceDir(const aProject: string; out aDir: string): boolean;
    class function GetSetupIni(const aProject: string; out aFile: string): boolean;
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

  TSearchMissingFile = class(TNotifierObject, INTAProjectMenuCreatorNotifier)
  private
    FText: string;
    procedure OnMenuClick(Sender: TObject);
  protected
    class var NotifierIndex: Integer;
    function AddMenu(const Ident: string): TMenuItem;
    function CanHandle(const Ident: string): Boolean;
  public
    procedure AfterConstruction; override;
    class procedure Setup;
    class procedure TearDown;
  end;

  TSearchProject = class(TNotifierObject, INTAProjectMenuCreatorNotifier)
  private
    FText: string;
    function FindActiveProjectIndex: Integer;
    procedure OnMenuClick(Sender: TObject);
  protected
    class var NotifierIndex: Integer;
    function AddMenu(const Ident: string): TMenuItem;
    function CanHandle(const Ident: string): Boolean;
  public
    class procedure Setup;
    class procedure TearDown;
  end;

procedure Register;

implementation

uses SysUtils, Classes, Registry, FileCtrl, Dialogs;

resourcestring
  StrActiveProjectModule   = 'ActiveProjectModule';
  StrActiveHostApplication = 'ActiveHostApplication';
  StrFactoryDir            = 'FactoryDir';
  StrOEMDir                = 'OEMDir';
  StrSourceDir             = 'SourceDir';

procedure Register;
begin
  TSetActiveProjectModule.Setup;
  TSetOEMDir.Setup;
  TSearchProject.Setup;
  TSearchMissingFile.Setup;
  SplashScreenServices.AddPluginBitmap('E Stream Software IDE Expert', 0);
end;

procedure TSetActiveProjectModule.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TSetActiveProjectModule.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
var sHostApplication: string;
    sFile: string;
    S: TStringList;
begin
  sHostApplication := '';

  if TOTAUtil.GetSetupIni(Project.FileName, sFile) then begin
    S := TStringList.Create;
    try
      S.CaseSensitive := False;
      S.LoadFromFile(sFile);
      if S.Values['commonname'] <> '' then
        sHostApplication := Format('%s\%s.exe', [ExtractFilePath(Project.ProjectOptions.TargetName), S.Values['commonname']]);
    finally
      S.Free;
    end;
  end;

  // Set Host Application in environment variable
  TOTAUtil.SetVariable(StrActiveHostApplication, sHostApplication);
end;

procedure TSetActiveProjectModule.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var sExt: string;
    sGroup, sProj: string;
    sDir: string;
begin
  if NotifyCode = ofnFileOpening then begin
    sExt := ExtractFileExt(FileName);
    sGroup := {$if CompilerVersion<=18} '.bdsgroup' {$else} '.groupproj' {$ifend};
    sProj := {$if CompilerVersion<=18} '.bdsproj' {$else} '.dproj' {$ifend};
    if SameText(sExt, sGroup) or SameText(sExt, sProj) then begin
      TOTAUtil.SetVariable(StrActiveProjectModule, GetModuleName(FileName));
      if TOTAUtil.GetSourceDir(FileName, sDir) then
        TOTAUtil.SetVariable(StrSourceDir, sDir);
    end;
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
    o: string;
begin
  Result := False;
  if TOTAUtil.GetSetupIni(aFileName, sIniFile) then begin
    N := TStringList.Create;
    try
      N.CaseSensitive := False;
      N.LoadFromFile(sIniFile);
      o := N.Values['OEM'];
      if o = '' then o := 'developer';
      aOEMDir := Format('%s\%s\oem\%s', [GetEnvironmentVariable(StrFactoryDir), N.Values['name'], o]);
      Result := True;
    finally
      N.Free;
    end;
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

class function TOTAUtil.GetSetupIni(const aProject: string; out aFile: string):
    boolean;
var sFile: string;
    F: TSearchRec;
    sPath: string;
begin
  Result := False;

  // Find Project root path
  if not GetSourceDir(aProject, sPath) then Exit;

  // Find Host Application File Name in setup.ini
  if FindFirst(Format('%s\*.*', [sPath]), faDirectory, F) = 0 then begin
    while FindNext(F) = 0 do begin
      sFile := Format(
                 '%s\%s\%s\setup.ini',
                 [sPath, F.Name, {$if CompilerVersion<=18.5}'project'{$else}'project.d12'{$ifend}]
               );
      if FileExists(sFile) then begin
        aFile := sFile;
        Result := True;
        Break;
      end;
    end;
    FindClose(F);
  end;
end;

class function TOTAUtil.GetSourceDir(const aProject: string; out aDir: string):
    boolean;
var S: TStringList;
    i: integer;
begin
  Result := False;
  S := TStringList.Create;
  try
    S.Delimiter := '\';
    S.DelimitedText := aProject;
    if S.Count > 3 then begin
      for i := 1 to 3 do
        S.Delete(S.Count - 1);
      aDir := S.DelimitedText;
      Result := True;
    end;
  finally
    S.Free;
  end;
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
        SetEnvironmentVariable(PChar(aName), PChar(aValue));
      end;
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
end;

function TSearchMissingFile.AddMenu(const Ident: string): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := 'Search Missing File';
  Result.OnClick := OnMenuClick;
end;

procedure TSearchMissingFile.AfterConstruction;
begin
  inherited;
  FText := 'Library.rc';  //by default search Library.rc
end;

function TSearchMissingFile.CanHandle(const Ident: string): Boolean;
begin
  Result := SameText(Ident, sProjectContainer);
end;

procedure TSearchMissingFile.OnMenuClick(Sender: TObject);
var i, j, lCount: integer;
    bFound: boolean;
    G: IOTAProjectGroup;
    M: IOTAMessageGroup;
begin
  if InputQuery('Search Missing File', 'Please enter file name', FText) then begin
    lCount := 0;
    M := (BorlandIDEServices as IOTAMessageServices80).AddMessageGroup(Format('Missing "%s" Projects', [FText]));
    (BorlandIDEServices as IOTAMessageServices80).ClearMessageGroup(M);
    G := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
    for i := 0 to G.ProjectCount - 1 do begin
      if not SameText(ExtractFileExt(G.Projects[i].ProjectOptions.TargetName), '.BPL') then Continue;
      bFound := False;
      for j := 0 to G.Projects[i].GetModuleCount - 1 do begin
        bFound := SameText(G.Projects[i].GetModule(j).Name, FText);
        if bFound then
          Break;
      end;
      if not bFound then begin
        (BorlandIDEServices as IOTAMessageServices80).AddTitleMessage(ExtractFileName(G.Projects[i].ProjectOptions.TargetName), M);
        Inc(lCount);
      end;
    end;
    ShowMessageFmt('%d projects found.', [lCount]);
    (BorlandIDEServices as IOTAMessageServices80).ShowMessageView(M);
  end;
end;

class procedure TSearchMissingFile.Setup;
var N: INTAProjectMenuCreatorNotifier;
begin
  N := TSearchMissingFile.Create;
  NotifierIndex := (BorlandIDEServices as IOTAProjectManager).AddMenuCreatorNotifier(N);
end;

class procedure TSearchMissingFile.TearDown;
begin
  if NotifierIndex <> -1 then
    (BorlandIDEServices as IOTAProjectManager).RemoveMenuCreatorNotifier(NotifierIndex);
end;

function TSearchProject.AddMenu(const Ident: string): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := 'Search Project';
  Result.ShortCut := TextToShortCut('F3');
  Result.OnClick := OnMenuClick;
end;
                                                       
function TSearchProject.CanHandle(const Ident: string): Boolean;
begin
  Result := SameText(Ident, sProjectContainer);
end;

function TSearchProject.FindActiveProjectIndex: Integer;
var G: IOTAProjectGroup;
    i: integer;
begin
  Result := -1;
  G := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
  for i := 0 to G.ProjectCount - 1 do begin
    if G.Projects[i] = G.ActiveProject then begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TSearchProject.OnMenuClick(Sender: TObject);
var i, lActiveIndex: integer;
    G: IOTAProjectGroup;
begin
  if InputQuery('Find Project', 'Project to find', FText) then begin
    G := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
    lActiveIndex := FindActiveProjectIndex + 1;
    if lActiveIndex >= G.ProjectCount then
      lActiveIndex := 0; //start from first
    for i := lActiveIndex to G.ProjectCount - 1 do begin
      if Pos(FText, G.Projects[i].ProjectOptions.TargetName) > 0 then begin
        G.ActiveProject := G.Projects[i];
        Break;
      end;
    end;
  end;
end;

class procedure TSearchProject.Setup;
var N: INTAProjectMenuCreatorNotifier;
begin
  N := TSearchProject.Create;
  NotifierIndex := (BorlandIDEServices as IOTAProjectManager).AddMenuCreatorNotifier(N);
end;

class procedure TSearchProject.TearDown;
begin
  if NotifierIndex <> -1 then
    (BorlandIDEServices as IOTAProjectManager).RemoveMenuCreatorNotifier(NotifierIndex);
end;

initialization
  TSetActiveProjectModule.NotifierIndex := -1;
  TSetOEMDir.NotifierIndex := -1;
  TSearchProject.NotifierIndex := -1;
  TSearchMissingFile.NotifierIndex := -1;
finalization
  TSetActiveProjectModule.TearDown;
  TSetOEMDir.TearDown;
  TSearchProject.TearDown;
  TSearchMissingFile.TearDown;
end.
