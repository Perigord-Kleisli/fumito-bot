{
  description = "srid/haskell-template: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];
      perSystem =
        { self'
        , config
        , pkgs
        , ...
        }: {
          # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
          haskellProjects.default = {
            # The base package set (this value is the default)
            # basePackages = pkgs.haskellPackages;

            # Packages to add on top of `basePackages`
            packages = {
              # Add source or Hackage overrides here
              # (Local packages are added automatically)
              /*
            aeson.source = "1.5.0.0" # Hackage version
            shower.source = inputs.shower; # Flake input
              */
            };

            # Add your package overrides here
            settings = {
              /*
            haskell-template = {
              haddock = false;
            };
            aeson = {
              check = false;
            };
              */
            };

            # Development shell configuration
            devShell = {
              # TODO: Remove this after https://github.com/numtide/treefmt-nix/issues/65
              tools = _:
                {
                  treefmt = config.treefmt.build.wrapper;
                }
                // config.treefmt.build.programs;
              hlsCheck.enable = false;
            };

            # What should haskell-flake add to flake outputs?
            autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
          };

          # Auto formatters. This also adds a flake check to ensure that the
          # source tree was auto formatted.
          treefmt.config = {
            inherit (config.flake-root) projectRootFile;
            package = pkgs.treefmt;
            programs.nixpkgs-fmt.enable = true;

            programs.ormolu.enable = true;
            programs.ormolu.package = pkgs.haskellPackages.fourmolu;
            settings.formatter.ormolu = {
              options = [
                "--ghc-opt"
                "-XImportQualifiedPost"
              ];
            };
          };

          # Dev shell scripts.
          mission-control.scripts = {
            docs = {
              description = "Start Hoogle server for project dependencies";
              exec = ''
                echo http://127.0.0.1:8888
                hoogle serve -p 8888 --local
              '';
              category = "Dev Tools";
            };
            repl = {
              description = "Start the cabal repl";
              exec = ''
                cabal repl "$@"
              '';
              category = "Dev Tools";
            };
            fmt = {
              description = "Format the source tree";
              exec = config.treefmt.build.wrapper;
              category = "Dev Tools";
            };
            run = {
              description = "Run the project with ghcid auto-recompile";
              exec = ''
                ghcid -c "cabal repl fumito-minor:exe:fumito" --warnings -T :main
              '';
              category = "Primary";
            };
          };

          # Default package & app.
          packages.default = self'.packages.fumito-minor;
          apps.default = self'.apps.fumito;

          # Default shell.
          devShells.default = pkgs.mkShell {
            name = "fumito-minor";
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
              config.flake-root.devShell
              config.mission-control.devShell
            ];
          };
        };
    };
}
