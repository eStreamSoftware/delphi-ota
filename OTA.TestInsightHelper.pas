unit OTA.TestInsightHelper;

interface

uses
  System.Classes, System.SysUtils, ToolsAPI;

type
  TTestInsightHelper = class(TNotifierObject, IOTADebuggerNotifier,
      IOTADebuggerNotifier90, TFunc<IOTADebuggerNotifier>,
      IOTAProjectMenuItemCreatorNotifier, TFunc<IOTAProjectMenuItemCreatorNotifier>,
      IOTAIDENotifier, TFunc<IOTAIDENotifier>)
  const
    StrTestRunCount = 'TestRunCount';
    StrTestPackage  = 'TestPackage';
  type
    TActionVerb = (vActiveHostApp, vTestInsight, vSequential, vParallel);
    TActionVerbHelper = record helper for TActionVerb
      class function From(Value: string): TActionVerb; static;
      function ToString: string;
    end;
  private
    class var FLastActiveProject: IOTAProject;
  protected
    procedure ProcessCreated(const Process: IOTAProcess);
    procedure ProcessDestroyed(const Process: IOTAProcess);
    procedure BreakpointAdded(const Breakpoint: IOTABreakpoint);
    procedure BreakpointDeleted(const Breakpoint: IOTABreakpoint);

    procedure BreakpointChanged(const Breakpoint: IOTABreakpoint);
    procedure CurrentProcessChanged(const Process: IOTAProcess);
    procedure ProcessStateChanged(const Process: IOTAProcess);
    function BeforeProgramLaunch(const Project: IOTAProject): Boolean;
    procedure ProcessMemoryChanged;
  protected
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  protected
    function Invoke: IOTADebuggerNotifier;
  protected
    procedure DoExecute(const aMenuContextList: IInterfaceList);
    procedure AddMenu(const Project: IOTAProject; const IdentList: TStrings; const
        ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
    function Invoke_IOTAProjectMenuItemCreatorNotifier: IOTAProjectMenuItemCreatorNotifier;
    function Invoke_IOTAIDENotifier: IOTAIDENotifier;
    function TFunc<IOTAProjectMenuItemCreatorNotifier>.Invoke = Invoke_IOTAProjectMenuItemCreatorNotifier;
    function TFunc<IOTAIDENotifier>.Invoke = Invoke_IOTAIDENotifier;
  end;

implementation

uses
  System.IOUtils, System.StrUtils, DCCStrs,
  OTA.IDE, OTA.ProjectManagerMenu;

class function TTestInsightHelper.TActionVerbHelper.From(
  Value: string): TActionVerb;
begin
  REsult := TActionVerb(Value.ToInteger);
end;

function TTestInsightHelper.TActionVerbHelper.ToString: string;
begin
  Result := Integer(Self).ToString;
end;

procedure TTestInsightHelper.AddMenu(const Project: IOTAProject;
  const IdentList: TStrings; const ProjectManagerMenuList: IInterfaceList;
  IsMultiSelect: Boolean);
var A: array[Boolean] of string;
begin
  if Assigned(Project) and (IdentList.Contains(sProjectContainer)) then begin
    // Menu Item: Parallel
    A[False] := vParallel.ToString;
    A[True] := vSequential.ToString;
    var bIsParallel := GetEnvironmentVariable(StrTestRunCount).EndsWith('P', True);
    ProjectManagerMenuList.Add(TNotifierObject_ProjectManagerMenu.Create('TestInsight Helper - Run Parallel', A[bIsParallel], pmmpUserBuild, DoExecute, ClassName, bIsParallel) as IOTAProjectManagerMenu);
  end;
end;

procedure TTestInsightHelper.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TTestInsightHelper.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin

end;

function TTestInsightHelper.BeforeProgramLaunch(const Project: IOTAProject): Boolean;
begin
  TOTAUtil.SetVariableIfEmpty(StrTestRunCount, 1.ToString);

  var DefinesStr := (Project.ProjectOptions as IOTAProjectOptionsConfigurations).ActiveConfiguration.GetValue(sDefine, True);

  if MatchText('TestInsight', DefinesStr.Split([';'])) then begin
    if Assigned(FLastActiveProject) then
      TOTAUtil.SetVariable(StrTestPackage, FLastActiveProject.ProjectOptions.TargetName);
  end else begin
    var sTestPackage := '';
    if Project.ApplicationType = sPackage then begin
      for var i := 0 to Project.GetModulecount - 1 do begin
        var m := Project.GetModule(i);
        if (m.ModuleType = omtPackageImport) and SameText(m.Name, 'dunit') then begin
          sTestPackage := Project.ProjectOptions.TargetName;
          Break;
        end;
      end;
    end;
    TOTAUtil.SetVariable(StrTestPackage, sTestPackage);
  end;

  Result := True;
end;

procedure TTestInsightHelper.BreakpointAdded(const Breakpoint: IOTABreakpoint);
begin

end;

procedure TTestInsightHelper.BreakpointChanged(const Breakpoint: IOTABreakpoint);
begin

end;

procedure TTestInsightHelper.BreakpointDeleted(const Breakpoint: IOTABreakpoint);
begin

end;

procedure TTestInsightHelper.CurrentProcessChanged(const Process: IOTAProcess);
begin

end;

procedure TTestInsightHelper.DoExecute(const aMenuContextList: IInterfaceList);
begin
  if aMenuContextList.Count = 0 then Exit;

  case TActionVerb.From((aMenuContextList.Items[0] as IOTAProjectMenuContext).Verb) of
    vSequential: begin
      var a := GetEnvironmentVariable(StrTestRunCount);
      if a.EndsWith('P') then a := a.Remove(a.Length - 1, 1);
      TOTAUtil.SetVariable(StrTestRunCount, a);
    end;
    vParallel: begin
      var a := GetEnvironmentVariable(StrTestRunCount);
      if not a.EndsWith('P') then a := a + 'P';
      TOTAUtil.SetVariable(StrTestRunCount, a);
    end;
  end;
end;

procedure TTestInsightHelper.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  if NotifyCode = ofnActiveProjectChanged then begin
    if not GetActiveProject.Filename.EndsWith('TestInsight.dproj', True) then
      FLastActiveProject := GetActiveProject;
  end else if NotifyCode = ofnBeginProjectGroupClose then
    FLastActiveProject := nil;
end;

function TTestInsightHelper.Invoke: IOTADebuggerNotifier;
begin
  Result := Self as IOTADebuggerNotifier;
end;

function TTestInsightHelper.Invoke_IOTAIDENotifier: IOTAIDENotifier;
begin
  Result := Self as IOTAIDENotifier;
end;

function TTestInsightHelper.Invoke_IOTAProjectMenuItemCreatorNotifier: IOTAProjectMenuItemCreatorNotifier;
begin
  Result := Self as IOTAProjectMenuItemCreatorNotifier;
end;

procedure TTestInsightHelper.ProcessCreated(const Process: IOTAProcess);
begin

end;

procedure TTestInsightHelper.ProcessDestroyed(const Process: IOTAProcess);
begin

end;

procedure TTestInsightHelper.ProcessMemoryChanged;
begin

end;

procedure TTestInsightHelper.ProcessStateChanged(const Process: IOTAProcess);
begin

end;

end.
