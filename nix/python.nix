# Python project spec
{
  # Single package configuration
  projectName = "lapack-workspace";
  projectRoot = ".";
  # projectDir is optional - if not defined, defaults to "src/<sanitizedName>"

  # No workspaces for simple single package template
  workspaces = [
    {
      projectName = "fortran-mapper";
      projectRoot = "fortran-mapper";
      projectDir = "fortran-mapper/src/fortran_mapper";
    }
    {
      projectName = "fortran-mapper-hooks-lapack";
      projectRoot = "fortran-mapper/hooks/lapack";
      projectDir = "fortran-mapper/hooks/lapack/src/fortran_mapper_hooks_lapack";
    }
  ];
  emptyRoot = true;
}
