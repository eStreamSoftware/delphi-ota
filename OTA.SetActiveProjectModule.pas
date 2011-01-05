unit OTA.SetActiveProjectModule;

interface

uses
  ToolsAPI;

type
  TSetActiveProjectModule = class(TNotifierObject, IOTAIDENotifier, IOTAIDENotifier50)
  protected
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
  protected
    function GetModuleName(const aFileName: string): string;
  end;

implementation

uses SysUtils, Classes, OTA.IDE;    

resourcestring
  StrActiveProjectModule   = 'ActiveProjectModule';
  StrActiveHostApplication = 'ActiveHostApplication';
  StrSourceDir             = 'SourceDir';

procedure TSetActiveProjectModule.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TSetActiveProjectModule.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin

end;

procedure TSetActiveProjectModule.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
var sHostApplication: string;
    sFile: string;
    S: TStringList;
begin
  if IsCodeInsight then Exit;
  
  sHostApplication := '';

  if TOTAUtil.GetSetupIni(Project.FileName, sFile) then begin
    S := TStringList.Create;
    try
      S.CaseSensitive := False;
      S.LoadFromFile(sFile);
      if S.Values['commonname'] <> '' then
        sHostApplication := Format('%s\%s.exe', [ExcludeTrailingPathDelimiter(ExtractFilePath(Project.ProjectOptions.TargetName)), S.Values['commonname']]);
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

function TSetActiveProjectModule.GetModuleName(const aFileName: string): string;
var S: TStringList;
    i: integer;
begin
  Result := '';
  S := TStringList.Create;
  try
    S.CaseSensitive := False;
    S.Delimiter := '\';
    S.StrictDelimiter := True;
    S.DelimitedText := aFileName;
    i := S.IndexOf('Project');
    if (i <> -1) and (i < S.Count) then
      Result := S[i + 2];
  finally
    S.Free;
  end;
end;

procedure TSetActiveProjectModule.AfterCompile(Succeeded,
  IsCodeInsight: Boolean);
begin

end;

end.
