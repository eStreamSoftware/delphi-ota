unit OTA.ConfigurationBuild;

interface

uses Winapi.Windows, System.Classes, ToolsAPI;

type
  T_ProjectGroup_ConfigurationBuild = class(TNotifierObject, IOTAProjectMenuItemCreatorNotifier)
  private
    function NewBuildMenu(aConfiguration: string): IOTAProjectManagerMenu;
  protected
    procedure AddMenu(const Project: IOTAProject; const IdentList: TStrings; const
        ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
  end;

  T_Project_ConfigurationBuild = class(TNotifierObject, IOTAProjectMenuItemCreatorNotifier)
  private
    function NewBuildMenu(aConfiguration: string): IOTAProjectManagerMenu;
  protected
    procedure AddMenu(const Project: IOTAProject; const IdentList: TStrings; const
        ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
  end;

implementation

uses System.SysUtils, OTA.IDE140;

procedure T_ProjectGroup_ConfigurationBuild.AddMenu(const Project: IOTAProject; const
    IdentList: TStrings; const ProjectManagerMenuList: IInterfaceList;
    IsMultiSelect: Boolean);
var m: IOTAProjectManagerMenu;
begin
  if IdentList.IndexOf(sProjectGroupContainer) = -1 then Exit;
  if (BorlandIDEServices as IOTAModuleServices).MainProjectGroup = nil then Exit;

  ProjectManagerMenuList.Add(NewBuildMenu('Debug'));
  ProjectManagerMenuList.Add(NewBuildMenu('Profile'));
  ProjectManagerMenuList.Add(NewBuildMenu('Release'));

  // Add separator
  m := TNotifierOTA_ProjectManagerMenu.Create;
  m.Position := 3;
  m.Caption := '-';
  ProjectManagerMenuList.Add(m);
end;

function T_ProjectGroup_ConfigurationBuild.NewBuildMenu(
  aConfiguration: string): IOTAProjectManagerMenu;
begin
  Result := TNotifierOTA_ProjectManagerMenu.Create;
  Result.Caption := 'Build - ' + aConfiguration;

  (Result as IOTAProjectManagerMenuExecute).Execute :=
    procedure (aContext: IOTAProjectMenuContext)
    var G: IOTAProjectGroup;
        C: IOTAProjectOptionsConfigurations;
        A: IOTABuildConfiguration;
        i, j: integer;
    begin
      G := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;

      for i := 0 to G.ProjectCount - 1 do begin
        C := G.Projects[i].ProjectOptions as IOTAProjectOptionsConfigurations;
        A := C.ActiveConfiguration;
        try
          for j := 0 to C.ConfigurationCount - 1 do begin
            if not SameText(C.Configurations[j].Name, aConfiguration) then Continue;
            C.ActiveConfiguration := C.Configurations[j];
            G.Projects[i].ProjectBuilder.BuildProject(cmOTABuild, False, i = 0);
          end;
        finally
          C.ActiveConfiguration := A;
        end;
      end;
    end;
end;

procedure T_Project_ConfigurationBuild.AddMenu(const Project: IOTAProject;
  const IdentList: TStrings; const ProjectManagerMenuList: IInterfaceList;
  IsMultiSelect: Boolean);
var m: IOTAProjectManagerMenu;
begin
  if IdentList.IndexOf(sProjectContainer) = -1 then Exit;

  ProjectManagerMenuList.Add(NewBuildMenu('Debug'));
  ProjectManagerMenuList.Add(NewBuildMenu('Profile'));
  ProjectManagerMenuList.Add(NewBuildMenu('Release'));

  // Add separator
  m := TNotifierOTA_ProjectManagerMenu.Create;
  m.Position := 3;
  m.Caption := '-';
  ProjectManagerMenuList.Add(m);
end;

function T_Project_ConfigurationBuild.NewBuildMenu(
  aConfiguration: string): IOTAProjectManagerMenu;
begin
  Result := TNotifierOTA_ProjectManagerMenu.Create;
  Result.IsMultiSelectable := True;
  Result.Caption := 'Build - ' + aConfiguration;

  (Result as IOTAProjectManagerMenuExecute).Execute :=
    procedure (aContext: IOTAProjectMenuContext)
    var C: IOTAProjectOptionsConfigurations;
        A: IOTABuildConfiguration;
        i: integer;
    begin
      C := aContext.Project.ProjectOptions as IOTAProjectOptionsConfigurations;
      A := C.ActiveConfiguration;
      try
        for i := 0 to C.ConfigurationCount - 1 do begin
          if not SameText(C.Configurations[i].Name, aConfiguration) then Continue;
          C.ActiveConfiguration := C.Configurations[i];
          aContext.Project.ProjectBuilder.BuildProject(cmOTABuild, False, i = 0);
        end;
      finally
        C.ActiveConfiguration := A;
      end;
    end;
end;

end.
