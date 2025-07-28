{
  name = "lapack-workspace";
  directory = ./.;
  workspaces = [
    {
      name = "connectome";
      directory = ./connectome;
      workspaces = [
        {
          name = "connectome-hooks-fortran";
          directory = ./connectome/hooks/fortran;
        }
        {
          name = "connectome-hooks-lapack";
          directory = ./connectome/hooks/lapack;
        }
      ];
    }
    {
      name = "fortran-mapper";
      directory = ./fortran-mapper;
      workspaces = [
        {
          name = "fortran-mapper-hooks-lapack";
          directory = ./fortran-mapper/hooks/lapack;
        }
      ];
    }
  ];
}
