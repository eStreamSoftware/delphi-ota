unit OTA.FormatUses;

interface

uses
  System.Classes, System.Generics.Defaults, Vcl.Menus, ToolsAPI,
  DockForm;

type
  TDelphiLibrary = class abstract
  strict private
    class function GetFiles(aPaths: TArray<string>; aComparer:
        IComparer<string>): TArray<string>;
  public
    class function Get3rdPartyLibrary: TArray<string>;
    class function GetBuiltInLibrary: TArray<string>;
  end;

  TUsesClause = record
  strict private
    FStartPos: Integer;  // zero based
    FEndPos: Integer;    // zero based
  private
    FClause: string;
    class procedure InjectNewLine(var aUsesItems: TArray<string>; aLibraries:
        TArray<TArray<string>>); static;
  public
    constructor Create(aStartPos, aEndPos: Integer; aClause: string);
    class operator Implicit(aClause: TUsesClause): string;
    property StartPos: Integer read FStartPos;
    property EndPos: Integer read FEndPos;
  end;

  TPascalUnit = record
  strict private
    FUseses: TArray<TUsesClause>;
    FSource: string;
    procedure AddUses(aUses: TUsesClause);
  public
    constructor Create(aSource: string);
    class operator Implicit(aSource: string): TPascalUnit;
    class operator Implicit(aUnit: TPascalUnit): string;
    property Useses: TArray<TUsesClause> read FUseses;
  end;

  TProject_FormatUses = class(TNotifierObject, IOTAProjectMenuItemCreatorNotifier)
  protected
    procedure AddMenu(const Project: IOTAProject; const IdentList: TStrings; const
        ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
  end;

  TEditor_FormatUses = class(TNotifierObject, IOTANotifier, INTAEditServicesNotifier)
  private
    FMenuItem: TMenuItem;
    procedure DoFormatUses(Sender: TObject);
  protected
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    class procedure FormatUses(E: IOTASourceEditor);
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  Winapi.Windows, System.AnsiStrings, System.Generics.Collections, System.IOUtils,
  System.RegularExpressions, System.SysUtils, System.Types, System.Win.Registry,
  OTA.IDE140;

type
  TComparer_UnitScopeName = class(TInterfacedObject, IComparer<string>)
    FScopeNames: TDictionary<string,Integer>;
    function Compare(const Left, Right: string): Integer;
    function GetScopeName(aUnitName: string): string;
  public
    constructor Create(aUnitScopes: array of string);
    procedure BeforeDestruction; override;
  end;

  TComparer_UnitName = class(TInterfacedObject, IComparer<string>)
    FUnitNames: TDictionary<string,Integer>;
    function Compare(const Left, Right: string): Integer;
  public
    constructor Create(aUnitNames: array of string);
    procedure BeforeDestruction; override;
  end;

class function TDelphiLibrary.Get3rdPartyLibrary: TArray<string>;
var R: TRegistry;
    D: TDictionary<string,string>;
    L: TStrings;
    o, s, t, P: string;
    C: TArray<string>;
    Ms: TMatchCollection;
    M: TMatch;
begin
  R := TRegistry.Create;
  D := TDictionary<string,string>.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;

    // Read Environment Variables
    R.OpenKey((BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\Environment Variables', False);
    L := TStringList.Create;
    try
      R.GetValueNames(L);
      for s in L do begin
        D.Add(Format('$(%s)', [s]).ToUpper, R.ReadString(s));
      end;
    finally
      R.CloseKey;
      L.Free;
    end;
    D.Add('$(PLATFORM)', 'win32');
    D.Add('$(CONFIG)', 'debug');

    // Read and parse search path
    R.OpenKey((BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\Library\Win32', False);
    try
      P := R.ReadString('Search Path');

      C := [];
      for s in P.Split([';']) do begin
        if s.ToUpper.Contains('$(BDSLIB)') then Continue;
        t := s.ToUpper;
        Ms := TRegEx.Matches(t, '(?sUi)(\$\(.+\))');
        for M in Ms do begin
          if D.TryGetValue(M.Groups[1].Value, o) then
            t := t.Replace(M.Groups[1].Value, o);
        end;
        C := C + [t];
      end;
      Result := GetFiles(C, nil);
    finally
      R.CloseKey;
    end;
  finally
    D.Free;
    R.Free;
  end;
end;

class function TDelphiLibrary.GetBuiltInLibrary: TArray<string>;
begin
  Result := GetFiles(
              [(BorlandIDEServices as IOTAServices).GetRootDirectory + '\lib\win32\debug',
               (BorlandIDEServices as IOTAServices).GetRootDirectory + '\source'],
              TComparer_UnitScopeName.Create([
                'Winapi', 'System', 'Data', 'Datasnap', 'EMS', 'FireDAC', 'IBX', 'REST', 'Soap', 'Xml',
                'Web', 'DUnitX', 'FMX', 'Vcl', 'VclTee'
              ]) as IComparer<string>
            );
end;

class function TDelphiLibrary.GetFiles(aPaths: TArray<string>; aComparer:
    IComparer<string>): TArray<string>;
var i: Integer;
    A: TStringDynArray;
    s: string;
begin
  // Enumerate all dcu files
  A := [];
  for s in aPaths do begin
    A := A + TDirectory.GetFiles(s, '*.dcu', TSearchOption.soAllDirectories);
    A := A + TDirectory.GetFiles(s, '*.pas', TSearchOption.soAllDirectories);
  end;
  for i := Low(A) to High(A) do
    A[i] := TPath.GetFileNameWithoutExtension(A[i]);

  // Sort dcu file names by aComparer
  if aComparer = nil then
    TArray.Sort<string>(A)
  else
    TArray.Sort<string>(A, aComparer);

  // Return the dcu file name array
  Result := [];
  for s in A do
    Result := Result + [s];
end;

constructor TUsesClause.Create(aStartPos, aEndPos: Integer; aClause:
    string);
begin
  FStartPos := aStartPos;
  FEndPos := aEndPos;
  FClause := aClause;
end;

class procedure TUsesClause.InjectNewLine(var aUsesItems: TArray<string>;
    aLibraries: TArray<TArray<string>>);
var s: string;
    A: TArray<string>;
    D: TDictionary<string,Integer>;
    LastLibType, CurLibType: Integer;
    i, iLen: Integer;
begin
  // Traverse aUsesItems and inject Line Break indicator (#) to last item of built in or third party item
  D := TDictionary<string,Integer>.Create;
  try
    i := 1;
    for A in aLibraries do begin
      for s in A do
        D.AddOrSetValue(s.ToUpper, i);
      Inc(i);
    end;

    LastLibType := -1;
    for i := Low(aUsesItems) to High(aUsesItems) do begin
      if not D.TryGetValue(aUsesItems[i].ToUpper, CurLibType) then CurLibType := -1;
      if LastLibType <> CurLibType then begin
        if i > Low(aUsesItems) then
          aUsesItems[i-1] := aUsesItems[i-1] + '#';
        LastLibType := CurLibType;
      end;
    end;
  finally
    D.Free;
  end;

  iLen := 0;
  for i := Low(aUsesItems) to High(aUsesItems) do begin
    if aUsesItems[i].EndsWith('#') then
      iLen := 0
    else if iLen + aUsesItems[i].Length < 80 then
      Inc(iLen, aUsesItems[i].Length + Length(', '))
    else begin
      if i > Low(aUsesItems) then
        aUsesItems[i - 1] := aUsesItems[i - 1] + '#';
      iLen := aUsesItems[i].Length;
    end;
  end;
end;

class operator TUsesClause.Implicit(aClause: TUsesClause): string;
var s: string;
    A: TArray<string>;
    i: Integer;
begin
  s := aClause.FClause;
  if not s.StartsWith('uses', True) then Exit(s);
  if s.EndsWith(';') then
    s := s.Remove(s.Length - 1);

  A := s.Remove(0, 4).Split([',']);
  for i := Low(A) to High(A) do
    A[i] := A[i].TrimLeft.TrimRight;

  TArray.Sort<string>(A, TComparer_UnitName.Create(TDelphiLibrary.GetBuiltInLibrary + TDelphiLibrary.Get3rdPartyLibrary) as IComparer<string>);

  InjectNewLine(A, [TDelphiLibrary.GetBuiltInLibrary, TDelphiLibrary.Get3rdPartyLibrary]);

  Result := s.Remove(4) + #13#10
          + '  ' + string.Join(', ', A).Replace('#,', ','#13#10' ')
          + ';';
end;

constructor TPascalUnit.Create(aSource: string);
var M: TMatch;
    U: TUsesClause;
    s: string;
begin
  FUseses := [];
  FSource := aSource;

  for s in TArray<string>.Create('(?si)interface\s+(uses\s+)([^''\[\]=;]+;)', '(?si)implementation\s+(uses\s+)([^''\[\]=;]+;)') do begin
    M := TRegEx.Match(aSource, s);
    if M.Success then begin
      U := TUsesClause.Create(M.Groups[1].Index - 1, M.Groups[1].Index - 1 + M.Groups[1].Length + M.Groups[2].Length, M.Groups[1].Value + M.Groups[2].Value);
      AddUses(U);
    end;
  end;
end;

procedure TPascalUnit.AddUses(aUses: TUsesClause);
begin
  FUseses := FUseses + [aUses];
end;

class operator TPascalUnit.Implicit(aSource: string): TPascalUnit;
begin
  Result := TPascalUnit.Create(aSource);
end;

class operator TPascalUnit.Implicit(aUnit: TPascalUnit): string;
var i: Integer;
    u: TUsesClause;
begin
  Result := aUnit.FSource;
  for i := High(aUnit.Useses) downto Low(aUnit.Useses) do begin
    u := aUnit.Useses[i];
    Result := Result
              .Remove(u.StartPos, u.EndPos - u.StartPos)
              .Insert(u.StartPos, u);
  end;
end;

procedure TComparer_UnitScopeName.BeforeDestruction;
begin
  inherited;
  FScopeNames.Free;
end;

function TComparer_UnitScopeName.Compare(const Left, Right: string): Integer;
var iLeft, iRight: Integer;
begin
  if not FScopeNames.TryGetValue(GetScopeName(Left), iLeft) then iLeft := -1;
  if not FScopeNames.TryGetValue(GetScopeName(Right), iRight) then iRight := -1;
  if (iLeft <> -1) and (iRight <> -1) then begin
    if iLeft <> iRight then
      Result := iLeft - iRight
    else
      Result := string.Compare(Left, Right);
  end else if (iLeft = -1) and (iRight = -1) then
    Result := string.Compare(Left, Right)
  else if iLeft = -1 then
    Result := 1
  else
    Result := -1;
end;

constructor TComparer_UnitScopeName.Create(aUnitScopes: array of string);
var i: Integer;
begin
  inherited Create;
  FScopeNames := TDictionary<string,Integer>.Create;

  for i := Low(aUnitScopes) to High(aUnitScopes) do
    FScopeNames.Add(aUnitScopes[i].ToUpper, i);
end;

function TComparer_UnitScopeName.GetScopeName(aUnitName: string): string;
var A: TArray<string>;
begin
  A := aUnitName.Split(['.']);
  if Length(A) > 1 then
    Result := A[0].ToUpper
  else
    Result := '';
end;

procedure TComparer_UnitName.BeforeDestruction;
begin
  inherited;
  FUnitNames.Free;
end;

function TComparer_UnitName.Compare(const Left, Right: string): Integer;
var iLeft, iRight: Integer;
begin
  if not FUnitNames.TryGetValue(Left.ToUpper, iLeft) then iLeft := -1;
  if not FUnitNames.TryGetValue(Right.ToUpper, iRight) then iRight := -1;
  if (iLeft <> -1) and (iRight <> -1) then
    Result := iLeft - iRight
  else if (iLeft = -1) and (iRight = -1) then
    Result := string.Compare(Left.ToUpper, Right.ToUpper)
  else if iLeft = -1 then
    Result := 1
  else
    Result := -1;
end;

constructor TComparer_UnitName.Create(aUnitNames: array of string);
var i: Integer;
begin
  inherited Create;
  FUnitNames := TDictionary<string,Integer>.Create;
  for i := Low(aUnitNames) to High(aUnitNames) do
    FUnitNames.AddOrSetValue(aUnitNames[i].ToUpper, i);
end;

procedure TProject_FormatUses.AddMenu(const Project: IOTAProject; const IdentList:
    TStrings; const ProjectManagerMenuList: IInterfaceList; IsMultiSelect:
    Boolean);
var S: string;
    bIsPasFile: boolean;
    m: IOTAProjectManagerMenu;
begin
  if IdentList.IndexOf(sFileContainer) = -1 then Exit;
  if IdentList.IndexOf(sOptionSet) <> -1 then Exit;
  if IdentList.IndexOf(sDirectoryContainer) <> -1 then Exit;

  bIsPasFile := False;
  for S in IdentList do begin
    if not FileExists(S) then Continue;
    if SameText(ExtractFileExt(S), '.pas') then begin
      bIsPasFile := True;
      Break;
    end;
  end;

  if not bIsPasFile then Exit;

  m := TNotifierOTA_ProjectManagerMenu.Create;
  m.IsMultiSelectable := True;
  m.Caption := 'Format uses clause';

  (m as IOTAProjectManagerMenuExecute).Execute :=
    procedure (aContext: IOTAProjectMenuContext)
    var M: IOTAModule;
        U: TPascalUnit;
    begin
      M := (BorlandIDEServices as IOTAModuleServices).FindModule(aContext.Ident);
      if Assigned(M) then
        TEditor_FormatUses.FormatUses(M.CurrentEditor as IOTASourceEditor)
      else begin
        U := TFile.ReadAllText(aContext.Ident);
        TFile.WriteAllText(aContext.Ident, U);
      end;
    end;

  ProjectManagerMenuList.Add(m);
end;

procedure TEditor_FormatUses.AfterConstruction;
begin
  inherited;
  FMenuItem := nil;
end;

procedure TEditor_FormatUses.DockFormRefresh(const EditWindow: INTAEditWindow;
  DockForm: TDockableForm);
begin

end;

procedure TEditor_FormatUses.DockFormUpdated(const EditWindow: INTAEditWindow;
  DockForm: TDockableForm);
begin

end;

procedure TEditor_FormatUses.DockFormVisibleChanged(const EditWindow: INTAEditWindow;
  DockForm: TDockableForm);
begin

end;

procedure TEditor_FormatUses.DoFormatUses(Sender: TObject);
begin
  FormatUses((BorlandIDEServices as IOTAEditorServices).GetTopBuffer);
end;

procedure TEditor_FormatUses.EditorViewActivated(const EditWindow: INTAEditWindow;
  const EditView: IOTAEditView);
var P: TPopupMenu;
begin
  if not Assigned(FMenuItem) then begin
    P := EditView.GetEditWindow.Form.FindComponent('EditorLocalMenu') as TPopupMenu;
    FMenuItem := TMenuItem.Create(nil);
    FMenuItem.Caption := 'Format Uses Clause';
    FMenuItem.ShortCut := TextToShortCut('Ctrl+Alt+Shift+U');
    FMenuItem.OnClick := DoFormatUses;
    P.Items.Add(FMenuItem);
  end;
end;

procedure TEditor_FormatUses.EditorViewModified(const EditWindow: INTAEditWindow;
  const EditView: IOTAEditView);
begin

end;

class procedure TEditor_FormatUses.FormatUses(E: IOTASourceEditor);
const BufSize = 16 * 1024;
var P: PAnsiChar;
    M: TStream;
    i, c: Integer;
    U: TPascalUnit;
    W: IOTAEditWriter;
    B: TBytes;
    R: IOTAEditReader;
begin
  M := TMemoryStream.Create;
  P := System.AnsiStrings.AnsiStrAlloc(BufSize);
  try
    i := 0;
    R := E.CreateReader;
    repeat
      c := R.GetText(i, P, BufSize);
      if c > 0 then begin
        M.Write(P[0], c);
        Inc(i, c);
      end;
    until c < BufSize;
    M.Position := 0;
    SetLength(B, M.Size);
    M.Read(B, MaxInt);
    U := TEncoding.UTF8.GetString(B);
  finally
    System.AnsiStrings.StrDispose(P);
    M.Free;
  end;

  for i := High(U.Useses) downto Low(U.Useses) do begin
    M := TStringStream.Create(U.Useses[i], TEncoding.UTF8);
    P := System.AnsiStrings.AnsiStrAlloc(M.Size + 1);
    W := E.CreateUndoableWriter;
    try
      M.Read(P[0], M.Size);
      P[M.Size] := #0;
      W.CopyTo(U.Useses[i].StartPos);
      W.DeleteTo(U.Useses[i].EndPos);
      W.Insert(P);
    finally
      W := nil;
      System.AnsiStrings.StrDispose(P);
      M.Free;
    end;
  end;
end;

procedure TEditor_FormatUses.WindowActivated(const EditWindow: INTAEditWindow);
begin

end;

procedure TEditor_FormatUses.WindowCommand(const EditWindow: INTAEditWindow; Command,
  Param: Integer; var Handled: Boolean);
begin

end;

procedure TEditor_FormatUses.WindowNotification(const EditWindow: INTAEditWindow;
  Operation: TOperation);
begin

end;

procedure TEditor_FormatUses.WindowShow(const EditWindow: INTAEditWindow; Show,
  LoadedFromDesktop: Boolean);
begin

end;

end.
