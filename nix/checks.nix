{
  uvBoilerplate,
  pythonProject,
  ...
}: let
  inherit (uvBoilerplate.pkgs) lib;
  
  # Create checks for all workspaces that have tests
  pythonChecks = lib.listToAttrs (
    lib.filter (x: x.value != null) (
      map (ws: {
        name = "${ws.name}-pytest";
        value = if (uvBoilerplate.pythonSet ? ${ws.name}) && 
                  (uvBoilerplate.pythonSet.${ws.name}.passthru ? tests) &&
                  (uvBoilerplate.pythonSet.${ws.name}.passthru.tests ? pytest) 
               then uvBoilerplate.pythonSet.${ws.name}.passthru.tests.pytest
               else null;
      }) uvBoilerplate.allWorkspaces
    )
  );
in pythonChecks