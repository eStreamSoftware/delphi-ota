unit OTA.FixedFormScaled;

interface

uses Classes, Windows, Messages, ToolsAPI;

type
  TUnscaleFiles = class abstract
  strict private
    class var FInstance: TUnscaleFiles;
  strict private
    FList: TStringList;
    FLocked: boolean;
  public
    procedure AfterConstruction; override;
    procedure Add(aFile: string; const aOriginalPPI: Integer);
    procedure Delete(aFile: string);
    function Exists(aFile: string): boolean;
    function GetOriginalPPI(aFile: string; out aOriginalPPI: Integer): boolean;
    procedure Lock;
    procedure Unlock;
  public
    class function Instance: TUnscaleFiles;
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

  TForm_PPI_Controller = class(TNotifierObject, IOTANotifier, IOTAFormNotifier)
  private
    FForm: IOTAComponent;
    FModuleFileName: string;
    FHandle: THandle;
    procedure WndProc(var Message: TMessage);
  protected
    procedure FormActivated;
    procedure FormSaving;
    procedure ComponentRenamed(ComponentHandle: TOTAHandle;
      const OldName, NewName: string);
  public
    constructor Create(const aForm: IOTAComponent; aModuleFileName: string);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TFixedFormScaled = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  private
    FHandle: THandle;
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

uses SysUtils;

procedure Log(aMsg: string);
begin
  (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(Format('[%s] %s', [ExtractFileName(GetModuleName(HInstance)), aMsg]));
end;

procedure TUnscaleFiles.Add(aFile: string; const aOriginalPPI: Integer);
begin
  if FLocked then Exit;
  FList.Values[aFile] := IntToStr(aOriginalPPI);
end;

procedure TUnscaleFiles.AfterConstruction;
begin
  inherited;
  FList := TStringList.Create;
  FLocked := False;
end;

procedure TUnscaleFiles.Delete(aFile: string);
var i: integer;
begin
  if FLocked then Exit;

  i := FList.IndexOfName(aFile);
  if i <> -1 then
    FList.Delete(i);

  if FList.Count = 0 then
    FreeAndNil(FInstance);
end;

function TUnscaleFiles.Exists(aFile: string): boolean;
begin
  Result := FList.IndexOfName(aFile) <> -1;
end;

function TUnscaleFiles.GetOriginalPPI(aFile: string;
  out aOriginalPPI: Integer): boolean;
begin
  Result := TryStrToInt(FList.Values[aFile], aOriginalPPI);
end;

class function TUnscaleFiles.Instance: TUnscaleFiles;
begin
  if FInstance = nil then
    FInstance := TUnscaleFiles.Create;
  Result := FInstance;
end;

procedure TUnscaleFiles.Lock;
begin
  FLocked := True;
end;

procedure TUnscaleFiles.Unlock;
begin
  FLocked := False;
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

procedure TForm_PPI_Controller.ComponentRenamed(ComponentHandle: TOTAHandle;
  const OldName, NewName: string);
begin

end;

constructor TForm_PPI_Controller.Create(const aForm: IOTAComponent; aModuleFileName:
    string);
begin
  inherited Create;
  FForm := aForm;
  FModuleFileName := aModuleFileName;
end;

procedure TForm_PPI_Controller.FormActivated;
begin

end;

procedure TForm_PPI_Controller.FormSaving;
var i: Integer;
begin
  if TUnscaleFiles.Instance.GetOriginalPPI(FModuleFileName, i) then
    FForm.SetPropByName('PixelsPerInch', i);

  i := 1;
  FForm.SetPropByName('Scaled', i);

  PostMessage(FHandle, WM_APP, 0, 0);
end;

procedure TForm_PPI_Controller.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_APP then
    TDelphiFormFiles.ReopenModule(FModuleFileName)
  else
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.wParam, Message.lParam);
end;

procedure TFixedFormScaled.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TFixedFormScaled.AfterConstruction;
begin
  inherited;
  FHandle := AllocateHWnd(WndProc);
  Log(ClassName + ' installed');
end;

procedure TFixedFormScaled.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin

end;

procedure TFixedFormScaled.BeforeDestruction;
begin
  inherited;
  DeallocateHWnd(FHandle);
end;

procedure TFixedFormScaled.FileNotification(NotifyCode: TOTAFileNotification; const
    FileName: string; var Cancel: Boolean);
var Services: IOTAModuleServices;
    M: IOTAModule;
    i: Integer;
    Editor: IOTAFormEditor;
    C: IOTAComponent;
    iScaled: Integer;
    iPPI, oPPI: Integer;
    F: TDelphiFormFiles;
begin
  case NotifyCode of
    ofnFileOpened:
    begin
      if not FileExists(FileName){it is a new file}then Exit;

      Services := BorlandIDEServices as IOTAModuleServices;
      M := Services.FindModule(FileName);

      if Assigned(M) and (M.ModuleFileCount = 2){module has 2 editors} then begin
        for i := 0 to M.ModuleFileCount - 1 do begin
          if Supports(M.ModuleFileEditors[i], IOTAFormEditor, Editor) then begin
            C := Editor.GetRootComponent;
            if C.IsTControl
               and C.GetPropValueByName('PixelsPerInch', iPPI)
               and C.GetPropValueByName('Scaled', iScaled)
            then begin
              if (iScaled = 1){Form.Scaled = True} then begin
                F := TDelphiFormFiles.Create(Editor.FileName, FileName);
                oPPI := F.GetPixelsPerInch(iPPI);
                if (iPPI <> oPPI) {IDE has re-scaled form} then begin
                  Log('Re-scaled form detected: ' + ExtractFileName(Editor.FileName));
                  F.Unscaled;
                  TUnscaleFiles.Instance.Add(FileName, oPPI);
                  PostMessage(FHandle, WM_APP, WParam(F), 0);
                end;
              end else if (TUnscaleFiles.Instance.Exists(FileName)) {Add controller to re-scaled form editor} then begin
                Log('Add controller to form: ' + ExtractFileName(Editor.FileName));
                Editor.AddNotifier(TForm_PPI_Controller.Create(C, FileName));
              end;
            end;
          end;
        end;
      end;
    end;

    ofnFileClosing:
    begin
      TUnscaleFiles.Instance.Delete(FileName);
    end;
  end;
end;

procedure TFixedFormScaled.WndProc(var Message: TMessage);
var F: TDelphiFormFiles;
begin
  if Message.Msg = WM_APP then begin
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

end.
