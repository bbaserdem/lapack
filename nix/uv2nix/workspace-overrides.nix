# Workspace package overrides for editable installs and testing
{
  lib,
  stdenv,
  pythonProject,
  ...
}: {
  # Create workspace package overrides for testing
  mkWorkspaceOverrides = final: prev: let
    # Apply overrides to each workspace package
    overridePackage = name:
      prev.${name}.overrideAttrs (old: {
        passthru =
          old.passthru
          // {
            tests = let 
              # Include the package with core testing tools
              testEnv = {
                ${name} = [];
              } 
              // (lib.optionalAttrs (final ? pytest) { pytest = []; })
              // (lib.optionalAttrs (final ? pytest-cov) { pytest-cov = []; });
              
              virtualenv = final.mkVirtualEnv "${name}-pytest-env" testEnv;
            in
              (old.tests or {})
              // {
                pytest = stdenv.mkDerivation {
                  name = "${old.name}-pytest";
                  inherit (old) src;
                  nativeBuildInputs = [
                    virtualenv
                  ];
                  dontConfigure = true;
                  buildPhase = ''
                    runHook preBuild
                    pytest --cov tests --cov-report html
                    runHook postBuild
                  '';
                  installPhase = ''
                    runHook preInstall
                    mv htmlcov $out
                    runHook postInstall
                  '';
                };
              };
          };
      });

    # Create overrides for all workspace packages (including root if not empty)
    allWorkspacePackages = 
      (if pythonProject.emptyRoot then [] else [pythonProject.projectName])
      ++ (map (ws: ws.projectName) pythonProject.workspaces);

    # Apply overrides to all workspace packages that exist in the package set
    workspaceOverrides = lib.listToAttrs (
      lib.filter (x: x.value != null) 
        (map (name: {
          inherit name;
          value = if prev ? ${name} then overridePackage name else null;
        }) allWorkspacePackages)
    );
  in
    workspaceOverrides;

  # Create editable package overrides for workspace packages
  mkEditableOverrides = final: prev: let
    # Helper function to sanitize project names for directory names
    sanitizeName = name: builtins.replaceStrings ["-"] ["_"] name;

    # Create editable override for a workspace package
    makeWorkspaceEditable = workspaceSpec:
      if prev ? ${workspaceSpec.projectName}
      then {
        name = workspaceSpec.projectName;
        value = prev.${workspaceSpec.projectName}.overrideAttrs (old: {
          src = lib.fileset.toSource {
            root = workspaceSpec.projectRoot;
            fileset = lib.fileset.unions [
              (workspaceSpec.projectRoot + "/pyproject.toml")
              (workspaceSpec.projectRoot + "/README.md")
              (workspaceSpec.projectDir or (workspaceSpec.projectRoot + "/src/" + sanitizeName workspaceSpec.projectName))
            ];
          };

          # Hatchling (build system) has a dependency on the editables package when building editables
          # In normal python flows this dependency is dynamically handled, in PEP660
          # With Nix, the dependency needs to be explicitly declared
          nativeBuildInputs =
            old.nativeBuildInputs
            ++ final.resolveBuildSystem {
              editables = [];
            };
        });
      }
      else {
        name = workspaceSpec.projectName;
        value = null;
      };

    # Handle the root package
    rootPackageOverride =
      if prev ? ${pythonProject.projectName} && !pythonProject.emptyRoot
      then {
        ${pythonProject.projectName} = prev.${pythonProject.projectName}.overrideAttrs (old: {
          src = lib.fileset.toSource {
            root = pythonProject.projectRoot;
            fileset = lib.fileset.unions [
              (pythonProject.projectRoot + "/pyproject.toml")
              (pythonProject.projectRoot + "/README.md")
              (pythonProject.projectDir or (pythonProject.projectRoot + "/src/" + sanitizeName pythonProject.projectName))
            ];
          };

          nativeBuildInputs =
            old.nativeBuildInputs
            ++ final.resolveBuildSystem {
              editables = [];
            };
        });
      }
      else {};

    # Create overrides for all workspace packages
    workspaceOverrides = lib.listToAttrs (
      lib.filter (x: x.value != null) 
        (map makeWorkspaceEditable pythonProject.workspaces)
    );
  in
    workspaceOverrides // rootPackageOverride;
}
