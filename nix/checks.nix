{
  uvBoilerplate,
  projectName,
  ...
}: {
  inherit (uvBoilerplate.pythonSet.${projectName}.passthru.tests) pytest;
}
