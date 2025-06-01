unit OTA.SetActiveProjectModule;

interface

uses
  System.SysUtils, ToolsAPI;

type
  TSetActiveProjectModule = class(TNotifierObject, IOTAIDENotifier, IOTAIDENotifier50, TFunc<IOTAIDENotifier>)
  protected
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
    function Invoke: IOTAIDENotifier;
  protected
    function GetModuleName(const aFileName: string): string;
  end;

implementation

uses
  System.Classes, System.IOUtils,
  OTA.IDE;

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
begin
  if IsCodeInsight then Exit;

  var sHostApplication := '';
  var sFile: string;
  if TOTAUtil.GetSetupIni(Project.FileName, sFile) then begin
    var S := TStringList.Create;
    try
      S.CaseSensitive := False;
      S.LoadFromFile(sFile);
      if S.Values['commonname'] <> '' then
        sHostApplication := TPath.ChangeExtension(
          TPath.Combine(TPath.GetDirectoryName(Project.ProjectOptions.TargetName), S.Values['commonname'])
        , 'exe'
        );
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

      sDir := '';
      if TOTAUtil.GetSourceDir(FileName, sDir) then
        TOTAUtil.SetVariable(StrSourceDir, sDir);
    end;
  end;
  Cancel := False;
end;

function TSetActiveProjectModule.GetModuleName(const aFileName: string): string;
begin
  var P := TPath.GetDirectoryName(aFileName);
  while not TFile.Exists(TPath.Combine(P, '.gitmodules')) do begin
    P := TPath.GetDirectoryName(P);
    if P.IsEmpty then Exit('');
  end;
  Result := TPath.GetFileName(P);
end;

function TSetActiveProjectModule.Invoke: IOTAIDENotifier;
begin
  Result := Self as IOTAIDENotifier;
end;

procedure TSetActiveProjectModule.AfterCompile(Succeeded,
  IsCodeInsight: Boolean);
begin

end;

end.
