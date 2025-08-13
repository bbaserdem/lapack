# Python workspace applications
{
  pkgs,
  uvBoilerplate,
  pythonProject,
  outputs,
  ...
}: let
  inherit (pkgs) lib;

  # Create apps for workspace packages that have executable outputs
  createAppForPackage = packageName: {
    ${packageName} = {
      type = "app";
      program = "${outputs.packages.${pkgs.system}.${packageName}}/bin/${packageName}";
    };
  };

  # Create apps for all workspace packages that exist in outputs
  workspaceApps = lib.foldl' (acc: ws:
    if outputs.packages.${pkgs.system} ? ${ws.projectName}
    then acc // (createAppForPackage ws.projectName)
    else acc
  ) {} pythonProject.workspaces;

  # If not emptyRoot, also check for main project
  mainProjectApp = 
    if !pythonProject.emptyRoot && (outputs.packages.${pkgs.system} ? ${pythonProject.projectName})
    then createAppForPackage pythonProject.projectName
    else {};

in mainProjectApp // workspaceApps
