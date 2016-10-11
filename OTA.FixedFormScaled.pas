unit OTA.FixedFormScaled;

interface

uses Classes, Windows, Messages, ToolsAPI;

type
  TStringIntList = class
  strict private
    FLocked: boolean;
  private
    FList: TStringList;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Add(aStr: string; aInt: integer);
    function Delete(aStr: string; out aInt: Integer): Boolean;
    function Exists(aStr: string): boolean;
    function GetInt(aStr: string; out aInt: Integer): boolean;
    procedure Lock;
    procedure Unlock;
  end;

  TUnscaleFiles = class abstract
  strict private
    class var FInstance: TStringIntList;
  public
    class function Instance: TStringIntList;
  end;

  TDelphiFormFiles = class
  private
    FSourceFile: string;
    FFormFile: string;
    FContents: TStringList;
    procedure Save;
  public
    constructor Create(const aFormFile, aSourceFile: string);
    procedure BeforeDestruction; override;
    function GetPixelsPerInch(const aDefaultPPI: Integer): integer;
    class procedure ReopenModule(const aFileName: string);
    procedure Unscaled;
    procedure ResetScaled;
    property FormFile: string read FFormFile;
    property SourceFile: string read FSourceFile;
  end;

  TForm_PPI_Controller = class(TNotifierObject, IOTANotifier, IOTAModuleNotifier, IOTAFormNotifier)
  const
    WM_Reopen_Module = WM_APP;
    WM_Revert_PPI = WM_APP + 1;
    WM_Revert_Scaled = WM_APP + 2;
  private
    FEditor: IOTAFormEditor;
    FForm: IOTAComponent;
    FModuleFileName: string;
    FHandle: THandle;
    FSaveToStorage: boolean;
    procedure WndProc(var Message: TMessage);
  protected // IOTANotifier
    procedure BeforeSave;
    procedure Destroyed;
  protected // IOTAModuleNotifier
    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);
  protected // IOTAFormNotifier
    procedure FormActivated;
    procedure FormSaving;
    procedure ComponentRenamed(ComponentHandle: TOTAHandle;
      const OldName, NewName: string);
  public
    constructor Create(const aEditor: IOTAFormEditor; const aForm: IOTAComponent;
        aModuleFileName: string);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TFixedFormScaled = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  const
    WM_Reopen_Module = WM_APP;
  private
    FHandle: THandle;
    FIgnorePrompt: boolean;
    FModuleNotifiers: TStringIntList;
    FViewAsTexts: TStringList;
    FFormFiles: TStringList;
    FProjectDesktopLoaded: Boolean;
    function GetModule(aFileName: string): IOTAModule;
    procedure ResetFormFileScaled(const aDFMFile, aPasFile: string);
    procedure WndProc(var Message: TMessage);
  protected
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName:
        string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

uses SysUtils, ActiveX, Controls, Dialogs, Forms;

{$if CompilerVersion = 18.5}
procedure CheckOSError(LastError: Integer);
begin
  if LastError <> 0 then
    RaiseLastOSError(LastError);
end;
{$ifend}

procedure Log(aMsg: string);
begin
  (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(Format('[%s] %s', [ExtractFileName(GetModuleName(HInstance)), aMsg]));
end;

procedure ShowContent(S: IStream);
var B: TBytes;
    T: tagSTATSTG;
    iRead: Integer;
    iPos: UInt64;
begin
  if (S.Stat(T, STATFLAG_NONAME) = S_OK) then begin
    SetLength(B, T.cbSize);
    CheckOSError(S.Seek(0, STREAM_SEEK_SET, iPos));
    CheckOSError(S.Read(@B[0], T.cbSize, @iRead));
    if iRead = T.cbSize then
      {$if CompilerVersion > 18.5}(StringOf(B));{$ifend}
  end;
end;

procedure LogModules;
var M: IOTAModuleServices;
    i: Integer;
    sExt: string;
begin
  M := BorlandIDEServices as IOTAModuleServices;
  for i := 0 to M.ModuleCount - 1 do begin
    sExt := ExtractFileExt(M.Modules[i].FileName);
    if SameText(sExt, '.pas') or SameText(sExt, '.dfm') then
      Log('Module: ' + M.Modules[i].FileName);
  end;
end;

procedure TDelphiFormFiles.BeforeDestruction;
begin
  inherited;
  FContents.Free;
end;

constructor TDelphiFormFiles.Create(const aFormFile, aSourceFile: string);
begin
  inherited Create;
  FFormFile := aFormFile;
  FSourceFile := aSourceFile;

  FContents := TStringList.Create;
  FContents.LoadFromFile(FFormFile);
end;

function TDelphiFormFiles.GetPixelsPerInch(const aDefaultPPI: Integer): integer;
var L, S: string;
begin
  S := 'PixelsPerInch =';
  for L in FContents do begin
    if Pos(S, L) > 0 then begin
      if not TryStrToInt(Copy(L, Pos(S, L) + Length(S), MaxInt), Result) then
        Result := aDefaultPPI;
    end;
  end;
end;

class procedure TDelphiFormFiles.ReopenModule(const aFileName: string);
var S: IOTAModuleServices;
    M: IOTAModule;
begin
  S := BorlandIDEServices as IOTAModuleServices;
  M := S.OpenModule(aFileName);
  if Assigned(M) then begin
    TUnscaleFiles.Instance.Lock;
    try
      if M.CloseModule(True) then begin
        M := S.OpenModule(aFileName);
        if Assigned(M) then
          M.Show;
      end;
    finally
      TUnscaleFiles.Instance.Unlock;
    end;
  end;
end;

procedure TDelphiFormFiles.ResetScaled;
begin
  FContents.Delete(5);
  Save;
end;

procedure TDelphiFormFiles.Save;
var D: TDateTime;
begin
  Assert(FileAge(FFormFile, D));
  FContents.SaveToFile(FFormFile);
  FileSetDate(FFormFile, DateTimeToFileDate(D));
end;

procedure TDelphiFormFiles.Unscaled;
begin
  FContents.Insert(5, 'Scaled = False');
  Save;
end;

procedure TForm_PPI_Controller.AfterConstruction;
begin
  inherited;
  FHandle := AllocateHWnd(WndProc);
end;

procedure TForm_PPI_Controller.BeforeDestruction;
begin
  inherited;
  DeallocateHWnd(FHandle);
end;

procedure TForm_PPI_Controller.BeforeSave;
begin
  FSaveToStorage := True;
end;

function TForm_PPI_Controller.CheckOverwrite: Boolean;
begin
  Result := True;
end;

procedure TForm_PPI_Controller.ComponentRenamed(ComponentHandle: TOTAHandle;
  const OldName, NewName: string);
begin

end;

constructor TForm_PPI_Controller.Create(const aEditor: IOTAFormEditor; const
    aForm: IOTAComponent; aModuleFileName: string);
begin
  inherited Create;
  FEditor := aEditor;
  FForm := aForm;
  FModuleFileName := aModuleFileName;
  FSaveToStorage := False;
end;

procedure TForm_PPI_Controller.Destroyed;
begin

end;

procedure TForm_PPI_Controller.FormActivated;
begin

end;

procedure TForm_PPI_Controller.FormSaving;
var oPPi, nPPI, oScaled, nScaled: Integer;
begin
  if TUnscaleFiles.Instance.GetInt(FModuleFileName, oPPI) then begin
    FForm.GetPropValueByName('PixelsPerInch', nPPI);
    FForm.GetPropValueByName('Scaled', oScaled);

    if oPPI <> nPPI then begin
      FForm.SetPropByName('PixelsPerInch', oPPI);
      if not FSaveToStorage then PostMessage(FHandle, WM_Revert_PPI, nPPI, 0);
    end;

    nScaled := 1;
    if oScaled <> nScaled then begin
      FForm.SetPropByName('Scaled', nScaled);
      if not FSaveToStorage then PostMessage(FHandle, WM_Revert_Scaled, oScaled, 0);
    end;

    if FSaveToStorage then begin
      PostMessage(FHandle, WM_Reopen_Module, 0, 0);
      FSaveToStorage := False;
    end;
  end;
end;

procedure TForm_PPI_Controller.ModuleRenamed(const NewName: string);
begin

end;

procedure TForm_PPI_Controller.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_Reopen_Module then
    TDelphiFormFiles.ReopenModule(FModuleFileName)
  else if Message.Msg = WM_Revert_PPI then begin
    FForm.SetPropByName('PixelsPerInch', Message.WParam);
  end else if Message.Msg = WM_Revert_Scaled then begin
    FForm.SetPropByName('Scaled', Message.WParam);
  end else
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.wParam, Message.lParam);
end;

procedure TFixedFormScaled.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TFixedFormScaled.AfterConstruction;
begin
  inherited;
  FHandle := AllocateHWnd(WndProc);
  FIgnorePrompt := False;
  FModuleNotifiers := TStringIntList.Create;
  FViewAsTexts := TStringList.Create;
  FFormFiles := TStringList.Create;
  FProjectDesktopLoaded := False;
end;

procedure TFixedFormScaled.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin

end;

procedure TFixedFormScaled.BeforeDestruction;
begin
  inherited;
  DeallocateHWnd(FHandle);
  FModuleNotifiers.Free;
  FViewAsTexts.Free;
  FFormFiles.Free;
end;

procedure TFixedFormScaled.FileNotification(NotifyCode: TOTAFileNotification; const
    FileName: string; var Cancel: Boolean);
var M: IOTAModule;
    i, j: Integer;
    Editor: IOTAFormEditor;
    C: IOTAComponent;
    iScaled: Integer;
    iPPI: Integer;
    o: IOTANotifier;
    iNotifier: integer;
    sPasFile: string;
begin
  case NotifyCode of
    ofnProjectDesktopLoad: begin
      FProjectDesktopLoaded := True;
      while FFormFiles.Count > 0 do begin
        ResetFormFileScaled(FFormFiles.Names[FFormFiles.Count - 1], FFormFiles.ValueFromIndex[FFormFiles.Count - 1]);
        FFormFiles.Delete(FFormFiles.Count - 1);
      end;
    end;

    ofnFileOpened:
    begin
      if not FileExists(FileName){it is a new file}then Exit;

      M := GetModule(FileName);

      if Assigned(M) and (M.ModuleFileCount = 2){module has 2 editors} then begin
        for i := 0 to M.ModuleFileCount - 1 do begin
          if Supports(M.ModuleFileEditors[i], IOTAFormEditor, Editor) then begin
            C := Editor.GetRootComponent;
            if C.IsTControl
               and C.GetPropValueByName('PixelsPerInch', iPPI)
               and C.GetPropValueByName('Scaled', iScaled)
               and (FViewAsTexts.IndexOf(Editor.FileName) = -1)
            then begin
              if (iScaled = 1){Form.Scaled = True} then begin
                if FProjectDesktopLoaded then
                  ResetFormFileScaled(Editor.FileName, FileName) {IDE has fully started, reset scale directly}
                else
                  FFormFiles.Values[Editor.FileName] := FileName; {IDE hasn't fully started, collect dfm and pas files pair}
              end else if TUnscaleFiles.Instance.Exists(FileName) {Add controller to re-scaled form editor} then begin
                Log('Add controller to form: ' + ExtractFileName(Editor.FileName));
                o := TForm_PPI_Controller.Create(Editor, C, FileName);
                FModuleNotifiers.Add(FileName, M.AddNotifier(o as IOTAModuleNotifier));
              end;
            end;
          end;
        end;
      end else if SameText(ExtractFileExt(FileName), '.dfm'){Switch form to text} then begin
        sPasFile := ChangeFileExt(FileName, '.pas');
        if TUnscaleFiles.Instance.Exists(sPasFile) then begin
          if TUnscaleFiles.Instance.GetInt(sPasFile, iPPI) and (iPPI <> Screen.PixelsPerInch) then begin
            FViewAsTexts.Add(FileName);
            ShowMessageFmt('PixelsPerInch in %s is %d.'#13'It will re-scale to %d when switch to form', [ExtractFileName(FileName), iPPI, Screen.PixelsPerInch]);
          end;
        end;
      end;
    end;

    ofnFileClosing:
    begin
      if FViewAsTexts.Find(FileName, i) then
        FViewAsTexts.Delete(i);
      TUnscaleFiles.Instance.Delete(FileName, i);
      M := GetModule(FileName);
      if Assigned(M) then begin
        for i := 0 to M.ModuleFileCount - 1 do begin
          if Supports(M.ModuleFileEditors[i], IOTAFormEditor, Editor) then begin
            j := FFormFiles.IndexOfName(Editor.FileName);
            if j <> -1 then
              FFormFiles.Delete(j);
          end;
          if FModuleNotifiers.Delete(FileName, iNotifier) then begin
            M.RemoveNotifier(iNotifier);
            Log('Remove controller from form: ' + ExtractFileName(FileName));
          end;
        end;
      end;
    end;
  end;
end;

function TFixedFormScaled.GetModule(aFileName: string): IOTAModule;
var i: integer;
begin
  Result := nil;
  with (BorlandIDEServices as IOTAModuleServices) do begin
    for i := ModuleCount - 1 downto 0 do begin
     if SameText(Modules[i].FileName, aFileName) then begin
        Result := Modules[i];
        Break;
      end;
    end;
  end;
end;

procedure TFixedFormScaled.ResetFormFileScaled(const aDFMFile,
  aPasFile: string);
var F: TDelphiFormFiles;
    iPPI, oPPI: Integer;
    sMsg: string;
    iMsg: TModalResult;
    M: IOTAModule;
    i: Integer;
    Editor: IOTAFormEditor;
    C: IOTAComponent;
    bFree: boolean;
begin
  bFree := True;
  F := TDelphiFormFiles.Create(aDFMFile, aPasFile);
  try
    iPPI := 0;
    M := GetModule(aPasFile);
    if Assigned(M) and (M.ModuleFileCount = 2){module has 2 editors} then begin
      for i := 0 to M.ModuleFileCount - 1 do begin
        if Supports(M.ModuleFileEditors[i], IOTAFormEditor, Editor) then begin
          C := Editor.GetRootComponent;
          C.GetPropValueByName('PixelsPerInch', iPPI);
        end;
      end;
    end;

    oPPI := F.GetPixelsPerInch(iPPI);
    if (iPPI <> oPPI) {IDE has re-scaled form} then begin
      Log('Re-scaled form detected: ' + ExtractFileName(aDFMFile));
      sMsg := Format(
                'Form %s has been re-scale from PixelsPerInch (PPI) %d to %d.'#13'Do you want to keep original PPI (%d)?' +
                #13#13'Note: Press Ignore button will not prompt this dialog anymore and will always keep original PPI.',
                [ExtractFileName(aDFMFile), oPPI, iPPI, oPPI]
              );
      iMsg := TModalResult(0);
      if not FIgnorePrompt then begin
        iMsg := MessageDlg(sMsg, mtConfirmation, [mbYes, mbNo, mbIgnore], 0);
        FIgnorePrompt := iMsg = mrIgnore;
      end;
      if FIgnorePrompt or (iMsg = mrYes) then begin
        F.Unscaled;
        TUnscaleFiles.Instance.Add(aPasFile, oPPI);
        PostMessage(FHandle, WM_Reopen_Module, WParam(F), 0);
        bFree := False;
      end;
    end;
  finally
    if bFree then F.Free;
  end;
end;

procedure TFixedFormScaled.WndProc(var Message: TMessage);
var F: TDelphiFormFiles;
begin
  if Message.Msg = WM_Reopen_Module then begin
    F := TDelphiFormFiles(Message.WParam);
    try
      // Reopen module
      F.ReopenModule(F.SourceFile);

      // Reset form file scaled
      F.ResetScaled;
    finally
      F.Free;
    end;
  end else
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.wParam, Message.lParam);
end;

procedure TStringIntList.Add(aStr: string; aInt: integer);
begin
  if FLocked then Exit;
  FList.Values[aStr] := IntToStr(aInt);
end;

procedure TStringIntList.AfterConstruction;
begin
  inherited;
  FList := TStringList.Create;
end;

procedure TStringIntList.BeforeDestruction;
begin
  inherited;
  FList.Free;
end;

function TStringIntList.Delete(aStr: string; out aInt: Integer):
    Boolean;
var i: integer;
begin
  Result := False;
  if FLocked then Exit;

  i := FList.IndexOfName(aStr);
  if i <> -1 then begin
    Result := TryStrToInt(FList.Values[aStr], aInt);
    if Result then FList.Delete(i);
  end;
end;

function TStringIntList.Exists(aStr: string): boolean;
begin
  Result := FList.IndexOfName(aStr) <> -1;
end;

function TStringIntList.GetInt(aStr: string; out aInt: Integer): boolean;
begin
  Result := TryStrToInt(FList.Values[aStr], aInt);
end;

procedure TStringIntList.Lock;
begin
  FLocked := True;
end;

procedure TStringIntList.Unlock;
begin
  FLocked := False;
end;

class function TUnscaleFiles.Instance: TStringIntList;
begin
  if FInstance = nil then
    FInstance := TStringIntList.Create;
  Result := FInstance;
end;

end.
