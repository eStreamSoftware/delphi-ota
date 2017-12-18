{$Message Fatal 'Deprecated'}

unit OTA.QC92507;

interface

uses
  System.Classes, ToolsAPI;

type
  T_QC92507_BaseConfigurationContainer = class(TNotifierObject, IOTAProjectMenuItemCreatorNotifier)
  protected
    procedure AddMenu(const Project: IOTAProject; const IdentList: TStrings;
      const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
  end;

  T_QC92507_BuildConfigContainer = class(TNotifierObject, IOTAProjectMenuItemCreatorNotifier)
  protected
    procedure AddMenu(const Project: IOTAProject; const IdentList: TStrings;
      const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
  end;

implementation

uses System.SysUtils, OTA.IDE140;

procedure T_QC92507_BaseConfigurationContainer.AddMenu(const Project: IOTAProject;
  const IdentList: TStrings; const ProjectManagerMenuList: IInterfaceList;
  IsMultiSelect: Boolean);
var S: string;
    m: IOTAProjectManagerMenu;
begin
  if IdentList.IndexOf(sBuildConfigContainer) = -1 then Exit;

  for S in IdentList do begin
    if Pos('TBaseConfigurationContainer', S) = 0 then Continue;

    m := TNotifierOTA_ProjectManagerMenu.Create;
    m.Caption := 'Build - QC92507';

    (m as IOTAProjectManagerMenuExecute).Execute :=
      procedure (aContext: IOTAProjectMenuContext)
      var i, iCount: integer;
          C: IOTAProjectOptionsConfigurations;
          A: IOTABuildConfiguration;
      begin
        iCount := 0;
        C := aContext.Project.ProjectOptions as IOTAProjectOptionsConfigurations;
        A := C.ActiveConfiguration;
        try
          for i := 0 to C.ConfigurationCount - 1 do begin
            // ignore base configuration
            if C.Configurations[i].Name = sBaseConfigurationKey then Continue;
            // Activate configuration
            C.ActiveConfiguration := C.Configurations[i];
            // Execute Build
            aContext.Project.ProjectBuilder.BuildProject(cmOTABuild, False, iCount = 0);
            Inc(iCount);
          end;
        finally
          C.ActiveConfiguration := A;
        end;
      end;

    ProjectManagerMenuList.Add(m);

    Break;
  end;
end;

procedure T_QC92507_BuildConfigContainer.AddMenu(const Project: IOTAProject;
  const IdentList: TStrings; const ProjectManagerMenuList: IInterfaceList;
  IsMultiSelect: Boolean);
var S: string;
    m: IOTAProjectManagerMenu;
begin
  if IdentList.IndexOf(sBuildConfigContainer) = -1 then Exit;

  for S in IdentList do begin
    if Pos('TBuildConfigContainer', S) = 0 then Continue;

    m := TNotifierOTA_ProjectManagerMenu.Create;
    m.IsMultiSelectable := True;
    m.Caption := 'Build - QC92507';

    (m as IOTAProjectManagerMenuExecute).Execute :=
      procedure (aContext: IOTAProjectMenuContext)
      var i: integer;
          C: IOTAProjectOptionsConfigurations;
          A: IOTABuildConfiguration;
      begin
        C := aContext.Project.ProjectOptions as IOTAProjectOptionsConfigurations;
        A := C.ActiveConfiguration;
        try
          for i := 0 to C.ConfigurationCount - 1 do begin
            if C.Configurations[i].Key = aContext.Ident then begin
              // Activate configuration
              C.ActiveConfiguration := C.Configurations[i];
              // Execute Build
              aContext.Project.ProjectBuilder.BuildProject(cmOTABuild, False, True);
              Break;
            end;
          end;
        finally
          C.ActiveConfiguration := A;
        end;
      end;

    ProjectManagerMenuList.Add(m);

    Break;
  end;
end;

end.
