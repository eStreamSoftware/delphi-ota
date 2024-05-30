unit OTA.IDE;

interface

uses
  Winapi.Windows, System.Classes, System.Generics.Collections, System.SysUtils,
  ToolsAPI;

type
  TNotifierOTA<T> = class(TInterfacedObject, TProc)
  type
    TFactory = TFunc<T>;
    TAdd = reference to function(const aNotifier: T): Integer;
    TRemove = reference to procedure(aIndex: Integer);
  protected
    FNotifierIndex: Integer;
    FFactory: TFactory;
    FAdd: TAdd;
    FRemove: TRemove;
    procedure Invoke;
  public
    constructor Create(aAdd: TAdd; aRemove: TRemove; const aFactory: TFactory);
    procedure BeforeDestruction; override;
  end;

  TNotifier_Services = class(
      TNotifierOTA<IOTAIDENotifier>)
  public
    constructor Create(const aFactory: TFunc<IOTAIDENotifier>); reintroduce;
  end;

  TNotifier_DebuggerServices = class(TNotifierOTA<IOTADebuggerNotifier>)
  public
    constructor Create(const aFactory: TFunc<IOTADebuggerNotifier>); reintroduce;
  end;

  TNotifier_ProjectManager = class(
      TNotifierOTA<IOTAProjectMenuItemCreatorNotifier>)
  public
    constructor Create(const aFactory: TFunc<IOTAProjectMenuItemCreatorNotifier>); reintroduce;
  end;

  TNotifier_KeyboardServices = class(TNotifierOTA<IOTAKeyboardBinding>)
  public
    constructor Create(const aFactory: TFunc<IOTAKeyboardBinding>); reintroduce;
  end;

  TOTAFactory = class abstract
  private
    FSetups: TArray<TProc>;
    class var FInstance: TOTAFactory;
  public
    class constructor Create;
    class destructor Destroy;
    procedure BeforeDestruction; override;
    class procedure RegisterProc(const aSetup: TProc);
    class procedure SetupAll;
  end;

  TOTAUtil = class abstract
  public
    class function GetSourceDir(const aProject: string; out aDir: string): boolean;
    class function GetSetupIni(const aProject: string; out aFile: string): boolean;
    class procedure SetVariable(const aName, aValue: string);
  end;

procedure Register;

implementation

uses
  System.Win.Registry, DesignIntf;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  TOTAFactory.SetupAll;
  SplashScreenServices.AddPluginBitmap('E Stream Software IDE Expert', 0);
end;

class constructor TOTAFactory.Create;
begin
  FInstance := TOTAFactory.Create;
end;

class destructor TOTAFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

procedure TOTAFactory.BeforeDestruction;
begin
  FSetups := [];
  inherited;
end;

class procedure TOTAFactory.RegisterProc(const aSetup: TProc);
begin
  FInstance.FSetups := FInstance.FSetups + [aSetup];
end;

class procedure TOTAFactory.SetupAll;
begin
  for var o in FInstance.FSetups do o();
end;

procedure TNotifierOTA<T>.BeforeDestruction;
begin
  if FNotifierIndex <> -1 then
    FRemove(FNotifierIndex);
  inherited;
end;

constructor TNotifierOTA<T>.Create(aAdd: TAdd; aRemove: TRemove; const
    aFactory: TFactory);
begin
  inherited Create;
  FAdd := aAdd;
  FRemove := aRemove;
  FFactory := aFactory;

  FNotifierIndex := -1;
end;

procedure TNotifierOTA<T>.Invoke;
begin
  FNotifierIndex := FAdd(FFactory());
end;

constructor TNotifier_Services.Create(
  const aFactory: TFunc<IOTAIDENotifier>);
begin
  inherited Create(
    function(const aNotifier: IOTAIDENotifier): Integer begin
      Result := (BorlandIDEServices as IOTAServices).AddNotifier(aNotifier);
    end
  , procedure(aIndex: Integer) begin
      (BorlandIDEServices as IOTAServices).RemoveNotifier(aIndex);;
    end
  , aFactory
  );
end;

constructor TNotifier_DebuggerServices.Create(
  const aFactory: TFunc<IOTADebuggerNotifier>);
begin
  inherited Create(
    function(const aNotifier: IOTADebuggerNotifier): Integer begin
      Result := (BorlandIDEServices as IOTADebuggerServices).AddNotifier(aNotifier);
    end
  , procedure(aIndex: Integer) begin
      (BorlandIDEServices as IOTADebuggerServices).RemoveNotifier(aIndex);;
    end
  , aFactory
  );
end;

constructor TNotifier_ProjectManager.Create(const aFactory: TFunc<IOTAProjectMenuItemCreatorNotifier>);
begin
  inherited Create(
    function(const aNotifier: IOTAProjectMenuItemCreatorNotifier): Integer begin
      Result := (BorlandIDEServices as IOTAProjectManager).AddMenuItemCreatorNotifier(aNotifier);
    end
  , procedure(aIndex: Integer) begin
      (BorlandIDEServices as IOTAProjectManager).RemoveMenuItemCreatorNotifier(aIndex);;
    end
  , aFactory
  );
end;

constructor TNotifier_KeyboardServices.Create(
  const aFactory: TFunc<IOTAKeyboardBinding>);
begin
  inherited Create(
    function(const aNotifier: IOTAKeyboardBinding): Integer begin
      Result := (BorlandIDEServices as IOTAKeyboardServices).AddKeyboardBinding(aNotifier);
    end
  , procedure(aIndex: Integer) begin
      (BorlandIDEServices as IOTAKeyboardServices).RemoveKeyboardBinding(aIndex);;
    end
  , aFactory
  );
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

end.
