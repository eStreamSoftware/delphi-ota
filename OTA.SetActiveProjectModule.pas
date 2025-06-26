unit OTA.SetActiveProjectModule;

interface

uses
  System.SysUtils, ToolsAPI;

type
  TSetActiveProjectModule = class(TNotifierObject, IOTAIDENotifier, IOTAIDENotifier50, TFunc<IOTAIDENotifier>)
  const
    StrActiveHostApplication = 'ActiveHostApplication';
    StrActiveProjectModule   = 'ActiveProjectModule';
  private
    procedure SetupVariables(aProject: IOTAProject);
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
    function GetModuleName(aFileName: string): string;
    function GetModuleRoot(aFileName: string): string;
  end;

implementation

uses
  System.Classes, System.IOUtils,
  OTA.IDE;

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
  if not IsCodeInsight then SetupVariables(Project);
end;

procedure TSetActiveProjectModule.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  if NotifyCode = ofnEndProjectGroupOpen then
    SetupVariables((BorlandIDEServices as IOTAModuleServices).MainProjectGroup.ActiveProject);
end;

function TSetActiveProjectModule.GetModuleName(aFileName: string): string;
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

procedure TSetActiveProjectModule.SetupVariables(aProject: IOTAProject);
begin
  if aProject = nil then Exit;

  TOTAUtil.SetVariable(StrActiveProjectModule, GetModuleName(aProject.FileName));

  var sHostApplication := '';
  var sFile: string;

  if TOTAUtil.GetSetupIni(GetModuleRoot(aProject.FileName), sFile) then begin
    var S := TStringList.Create;
    try
      S.CaseSensitive := False;
      S.LoadFromFile(sFile);
      if S.Values['commonname'] <> '' then
        sHostApplication := TPath.ChangeExtension(
          TPath.Combine(TPath.GetDirectoryName(aProject.ProjectOptions.TargetName), S.Values['commonname'])
        , 'exe'
        );
    finally
      S.Free;
    end;
  end;
  // Set Host Application in environment variable
  TOTAUtil.SetVariable(StrActiveHostApplication, sHostApplication);
end;

procedure TSetActiveProjectModule.AfterCompile(Succeeded,
  IsCodeInsight: Boolean);
begin

end;

function TSetActiveProjectModule.GetModuleRoot(aFileName: string): string;
begin
  var P := TPath.GetDirectoryName(aFileName);
  while not TFile.Exists(TPath.Combine(P, '.gitmodules')) do begin
    P := TPath.GetDirectoryName(P);
    if P.IsEmpty then Exit('');
  end;
  Result := P;
end;

end.
