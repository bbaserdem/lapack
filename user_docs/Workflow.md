# Workflow

Main workflow chart, with each feature in a given stage.

```mermaid
flowchart TD
  %% Root repo
  main(["main"])

  %% Milestones
  stage0{{"0"}}
  stage1{{"1"}}
  stage2{{"2"}}

  %% Main branches
  devops["devops"]
  master["master"]
  organize["Organize"]
  feature["feature"]

  %% Sub-branches (circles)
  docker2nix(["devops/docker2nix"])
  map(["map"])
  docs1(["docs"])
  reorg(["reorg"])
  tests(["tests"])

  %% Feature sub-branches
  docs2(["docs"])
  sparse(["sparse"])
  rustbinding(["rust-binding"])
  partialcomp(["partial-comp"])

  %% Structure
  main --> stage0
  stage0 --> devops --> docker2nix
  docker2nix --> master

  main --> master

  master --> stage1
  stage1 --> organize
  organize --> map --> docs1 --> reorg --> tests
  organize --> docs1

  tests --> master

  master --> stage2
  stage2 --> feature --> docs2 --> sparse --> rustbinding --> partialcomp

  %% Show docs2 links to all
  docs2 --> rustbinding
  docs2 --> partialcomp
```
