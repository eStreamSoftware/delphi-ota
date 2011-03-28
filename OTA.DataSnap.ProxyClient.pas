unit OTA.DataSnap.ProxyClient;

interface

uses
  SysUtils, Classes, ToolsAPI, Generics.Collections, uPSUtils;

type
  TToken = record
    Token: TPSPasToken;
    TokenPos: integer;
    OriginalToken: string;
    class function Create(aToken: TPSPasToken; aTokenPos: integer; aOriginalToken: string):
        TToken; static;
    function TokenText: string;
    const
      Map: array [TPSPasToken] of string = ('', '', '', '', ';', ',', '.', ':',
        '(', ')', '[', ']', ':=', '=', '<>', '>', '>=', '<', '<=', '+', '-', '/', '*', '',
        '', '', '', '', '', '', '', 'and', 'array', 'begin', 'case', 'const', 'div', 'do', 'downto', 'else',
        'end', 'for', 'function', 'if', 'in', 'mod', 'not', 'of', 'or', 'procedure', 'program', 'repeat',
        'record', 'set', 'shl', 'shr', 'then', 'to', 'type', 'until', 'uses', 'var', 'while', 'with', 'xor', 'exit',
        'class', 'constructor', 'destructor', 'inherited', 'private', 'public',
        'published', 'protected', 'property', 'virtual', 'override', 'as', 'is',
        'unit', 'try', 'except', 'finally', 'external', 'forward', 'export', 'label', 'goto', 'chr', 'ord', 'interface',
        'implementation', 'initialization', 'finalization', 'out', 'nil');
  end;

  TTokenList = TList<TToken>;

  TTokenMethod = reference to procedure(T: TToken);

  TPascalParser_Patchable = class
  private
    FTokens: TTokenList;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Parse(const aScript: string);
    function Patch(PatchMethod: TProc<TTokenList, TProc<TToken, Boolean>>): string;
    procedure Iterate(aMethod: TTokenMethod);
    property Tokens: TTokenList read FTokens;
  end;

  TAddInterface_DataSnap_ProxyClient = class(TNotifierObject, IOTAProjectMenuItemCreatorNotifier)
  private
    const c_Token_NextLineIndent2 = #$D#$A'  ';
    const c_Token_NextLineIndent4 = #$D#$A'    ';
    procedure DataSnapPatch(L: TTokenList; AddPatch: TProc<TToken, Boolean>);
    procedure VisitClass(const E: TEnumerator<TToken>; const aClassToken: TToken;
        AddPatch: TProc<TToken, Boolean>);
  protected
    procedure AddMenu(const Project: IOTAProject; const IdentList: TStrings;
      const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
  end;

implementation

uses IOUtils, OTA.IDE140;

class function TToken.Create(aToken: TPSPasToken; aTokenPos: integer; aOriginalToken:
    string): TToken;
begin
  Result.Token := aToken;
  Result.TokenPos := aTokenPos;
  Result.OriginalToken := aOriginalToken;
end;

function TToken.TokenText: string;
begin
  Result := Map[Token];
  if Result = '' then
    Result := OriginalToken;
end;

{ TPascalParser_Patchable }

procedure TPascalParser_Patchable.AfterConstruction;
begin
  inherited;
  FTokens := TTokenList.Create;
end;

procedure TPascalParser_Patchable.BeforeDestruction;
begin
  inherited;
  FTokens.Free;
end;

procedure TPascalParser_Patchable.Iterate(aMethod: TTokenMethod);
var
  T: TToken;
begin
  for T in FTokens do
    aMethod(T);
end;

procedure TPascalParser_Patchable.Parse(const aScript: string);
var
  C: TPSPascalParser;
  T: TToken;
begin
  C := TPSPascalParser.Create;
  try
    C.EnableComments := True;
    C.EnableWhitespaces := True;

    C.SetText(AnsiString(aScript));
    while C.CurrTokenID <> CSTI_EOF do
    begin
      T.Token := C.CurrTokenID;
      T.TokenPos := C.CurrTokenPos;
      T.OriginalToken := string(C.OriginalToken);

      FTokens.Add(T);
      C.Next;
    end;
  finally
    C.Free;
  end;
end;

function TPascalParser_Patchable.Patch(PatchMethod: TProc<TTokenList, TProc<TToken, Boolean>>):
    string;
var B: TStringBuilder;
    lList_Before, lList_After: TTokenList;
    E_Before, E_After: TEnumerator<TToken>;
begin
  lList_Before := TTokenList.Create;
  lList_After := TTokenList.Create;

  PatchMethod(FTokens,
    procedure (T: TToken; aBefore: Boolean)
    begin
      if aBefore then
        lList_Before.Add(T)
      else
        lList_After.Add(T);
    end
  );

  B := TStringBuilder.Create;
  E_Before := lList_Before.GetEnumerator;
  E_After := lList_After.GetEnumerator;
  try
    if not E_Before.MoveNext then FreeAndNil(E_Before);
    if not E_After.MoveNext then FreeAndNil(E_After);
    Iterate(
      procedure(T: TToken)
      begin
        {$region 'Append to Before Token'}
        if Assigned(E_Before) and (E_Before.Current.TokenPos = T.TokenPos) then begin
          B.Append(E_Before.Current.TokenText);
          if not E_Before.MoveNext then
            FreeAndNil(E_Before);
        end;
        {$endregion}
        B.Append(T.TokenText); //current token
        {$region 'Append to After Token'}
        if Assigned(E_After) and (E_After.Current.TokenPos = T.TokenPos) then begin
          B.Append(E_After.Current.TokenText);
          if not E_After.MoveNext then
            FreeAndNil(E_After);
        end;
        {$endregion}
      end
    );
    Result := B.ToString;
  finally
    FreeAndNil(E_Before);
    lList_Before.Free;
    lList_After.Free;
    B.Free;
  end;
end;

procedure TAddInterface_DataSnap_ProxyClient.AddMenu(
  const Project: IOTAProject; const IdentList: TStrings;
  const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
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
  m.Caption := 'Add DataSnap Proxy Client Interface';

  (m as IOTAProjectManagerMenuExecute).Execute :=
    procedure (aContext: IOTAProjectMenuContext)
    var sCode: string;
        C: TPascalParser_Patchable;
        M: IOTAModule;
        E: IOTASourceEditor;
        B: TBytes;
        W: IOTAEditWriter;
    begin
      M := (BorlandIDEServices as IOTAModuleServices).OpenModule(aContext.Ident);     //Open file editor
      M.Show;
      E := M.CurrentEditor as IOTASourceEditor;

      C := TPascalParser_Patchable.Create;
      try
        sCode := TFile.ReadAllText(aContext.Ident);              //Read text from file
        C.Parse(sCode);                                          //convert class to interface
        B := TEncoding.UTF8.GetBytes(C.Patch(DataSnapPatch));    //String to TBytes
        SetLength(B, Length(B) + 1);                             //Reserve a place for null terminated
        B[Length(B) - 1] := 0;                                   //Set Last position as null terminated
        W := E.CreateWriter;
        W.DeleteTo(Length(sCode));                               //delete all code in the editor
        W.Insert(PAnsiChar(B));                                  //insert the new code to the editor
        M.CurrentEditor.MarkModified;                            //make editor in modified mode
      finally
        C.Free;
      end;
    end;

  ProjectManagerMenuList.Add(m);
end;

procedure TAddInterface_DataSnap_ProxyClient.DataSnapPatch(L: TTokenList;
    AddPatch: TProc<TToken, Boolean>);
var
  T: TToken;
  E: TEnumerator<TToken>;
  S: TStringBuilder;
  lClassList: TStringList;
  i: integer;
begin
  E := L.GetEnumerator;
  S := TStringBuilder.Create;
  lClassList := TStringList.Create;
  try
    {$region 'Uses - Disable SqlExpr'}
    while E.MoveNext and (E.Current.Token <> CSTII_uses) do;
    while E.MoveNext and (E.Current.Token <> CSTI_SemiColon) do begin
      if SameText(E.Current.TokenText, 'SqlExpr') then begin
        AddPatch(TToken.Create(CSTI_Identifier, E.Current.TokenPos, '{'), True);
        E.MoveNext;
        AddPatch(TToken.Create(CSTI_Identifier, E.Current.TokenPos, '}'), False);
      end;
    end;
    {$endregion}
    {$region 'Section: Class'}
    while E.MoveNext and (E.Current.Token <> CSTII_Implementation) do begin
      case E.Current.Token of
        CSTI_Identifier: T := E.Current;
        CSTII_Class    : begin
                           lClassList.Add(T.TokenText); //keep class name
                           VisitClass(E, T, AddPatch);
                         end;
      end;
    end;
    {$endregion}
    {$region 'Implementation'}
    {$region 'Uses'}
    E.MoveNext;
    AddPatch(TToken.Create(CSTI_Identifier, E.Current.TokenPos, #13#10#13#10'uses Rtti {$ifdef estream_core}, DataSnap.ProxyClient.Helper{$endif};'), True);
    {$endregion}
    {$region 'Add Constructor Implementation'}
    while E.MoveNext and (E.Current.Token <> CSTII_constructor) do;
    S.Clear;
    for i := 0 to lClassList.Count - 1 do begin
      S.Append(Format('constructor %s.Create(NewConnection: TFunc<string, IInterface>; AInstanceOwner: Boolean);', [lClassList[i]]));
      S.Append(#13#10'begin');
      S.Append(c_Token_NextLineIndent2).Append('FDataSnapConnection := NewConnection(GetServerMethodClassName(ClassName));');
      S.Append(c_Token_NextLineIndent2).Append('Create(GetDBXConnection(FDataSnapConnection), AInstanceOwner)');
      S.Append(#13#10'end;'#13#10#13#10);
    end;
    AddPatch(TToken.Create(CSTI_Identifier, E.Current.TokenPos, S.ToString), True);
    {$endregion}
    {$endregion}
  finally
    E.Free;
    S.Free;
    lClassList.Free;
  end;
end;

procedure TAddInterface_DataSnap_ProxyClient.VisitClass(const E:
    TEnumerator<TToken>; const aClassToken: TToken; AddPatch: TProc<TToken,
    Boolean>);
var lToken_Interface, lToken_Class, lToken_Private, lToken_Constructor: TToken;
    S: TStringBuilder;
    G: TGUID;
    lInterface: string;
begin
  lInterface := Format('I%s', [Copy(aClassToken.TokenText, 2, Length(aClassToken.TokenText) - 1)]);
  S := TStringBuilder.Create;
  try
    {$region 'Add Interface'}
    S.Append(lInterface + ' = interface(IInterface)');
    CreateGUID(G);
    S.Append(c_Token_NextLineIndent2).AppendFormat('  [%s]', [QuotedStr(GUIDToString(G))]);
    lToken_Interface := TToken.Create(CSTI_Identifier, aClassToken.TokenPos, S.ToString);
    {$endregion}

    {$region 'Convert Class to TInterfacedObject'}
    lToken_Class := TToken.Create(CSTI_Identifier, E.Current.TokenPos, Format('(TInterfacedObject, %s)', [lInterface]));
    {$endregion}

    {$region 'Private'}
    while E.MoveNext and (E.Current.Token <> CSTII_private) do;
    {$region 'Add Variable: FDataSnapConnection'}
    E.MoveNext;
    lToken_Private := TToken.Create(CSTI_Identifier, E.Current.TokenPos, 'FDataSnapConnection: IInterface;' + c_Token_NextLineIndent4);
    {$endregion}

    {$endregion}
    {$endregion}
    {$region 'Add Constructor'}
    while E.MoveNext and (E.Current.Token <> CSTII_constructor) do;
    lToken_Constructor := TToken.Create(CSTI_Identifier, E.Current.TokenPos, 'constructor Create(NewConnection: TFunc<string, IInterface>; AInstanceOwner: Boolean = True); overload;' + c_Token_NextLineIndent4);
    {$endregion}

    {$region 'Add Interface'}
    while E.MoveNext and ((E.Current.Token <> CSTII_function) and (E.Current.Token <> CSTII_procedure)) do; //move to function and procedure
    S.Clear;
    S.Append(c_Token_NextLineIndent4);
    while E.Current.Token <> CSTII_end do begin
      S.Append(E.Current.TokenText);
      E.MoveNext;
    end;
    S.Append('end;').AppendLine.Append(c_Token_NextLineIndent2);
    lToken_Interface.OriginalToken := lToken_Interface.OriginalToken + S.ToString;
    {$endregion}

    AddPatch(lToken_Interface,       True);
    AddPatch(lToken_Class,           False);
    AddPatch(lToken_Private,         False);
    AddPatch(lToken_Constructor,     True);
  finally
    S.Free;
  end;
end;

end.
