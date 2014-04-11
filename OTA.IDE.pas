unit OTA.IDE;

interface

uses Windows, Classes, ToolsAPI;

type
  TNotifierOTA = class abstract
  protected
    FClass: TInterfacedClass;
    FNotifier: IInterface;
    FNotifierIndex: Integer;
  public
    constructor Create(const aClass: TInterfacedClass);
    procedure Setup; virtual; abstract;
  end;

  TNotifierOTA_Services = class(TNotifierOTA)
  public
    procedure Setup; override;
    procedure BeforeDestruction; override;
  end;

  TOTAFactoryClass = class of TOTAFactory;

  TOTAFactory = class abstract
  private
    FList: TList;
    class var FInstance: TOTAFactory;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function GetList: TList;
    class function Register(const aNotifier: TNotifierOTA): TOTAFactoryClass;
    class procedure Setup;
    class procedure SetupAll;
    class procedure TearDown;
  end;

  TOTAUtil = class abstract
  public
    class function GetSourceDir(const aProject: string; out aDir: string): boolean;
    class function GetSetupIni(const aProject: string; out aFile: string): boolean;
    class procedure SetVariable(const aName, aValue: string);
  end;

procedure Register;

implementation

uses SysUtils, Registry, DesignIntf;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  TOTAFactory.SetupAll;
  SplashScreenServices.AddPluginBitmap('E Stream Software IDE Expert', 0);
end;

procedure TOTAFactory.AfterConstruction;
begin
  inherited;
  FList := TList.Create;
end;

procedure TOTAFactory.BeforeDestruction;
var i: integer;
begin
  inherited;
  for i := FList.Count - 1 downto 0 do
    TObject(FList[i]).Free;
  FList.Free;
end;

function TOTAFactory.GetList: TList;
begin
  Result := FList;
end;

class function TOTAFactory.Register(const aNotifier: TNotifierOTA): TOTAFactoryClass;
begin
  FInstance.GetList.Add(aNotifier);
  Result := Self;
end;

class procedure TOTAFactory.Setup;
begin
  FInstance := TOTAFactory.Create;
end;

class procedure TOTAFactory.SetupAll;
var i: integer;
begin
  for i := 0 to FInstance.GetList.Count - 1 do
    TNotifierOTA(FInstance.GetList[i]).Setup;
end;

class procedure TOTAFactory.TearDown;
begin
  FreeAndNil(FInstance);
end;

constructor TNotifierOTA.Create(const aClass: TInterfacedClass);
begin
  inherited Create;
  FClass := aClass;
  FNotifierIndex := -1;
end;

procedure TNotifierOTA_Services.Setup;
begin
  FNotifier := FClass.Create;
  FNotifierIndex := (BorlandIDEServices as IOTAServices).AddNotifier(FNotifier as IOTAIDENotifier);
end;

procedure TNotifierOTA_Services.BeforeDestruction;
begin
  inherited;
  if FNotifierIndex <> -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(FNotifierIndex);
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
                 [sPath, F.Name, {$ifdef VER185}'project.d11'
                                 {$else}'project'{$endif}
                 ]
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
    S.StrictDelimiter := True;
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
      if not R.KeyExists(aName) or (R.ReadString(aName) <> aValue) then begin
        R.WriteString(aName, aValue);
        SetEnvironmentVariable(PChar(aName), PChar(aValue));
      end;
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
end;

initialization
  TOTAFactory.Setup;
finalization
  TOTAFactory.TearDown;
end.
