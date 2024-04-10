{
  inputs = {
    flakes.url = "github:deemp/flakes";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    devshell.url = "github:deemp/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.devshell.flakeModule
      ];
      perSystem =
        {
          self',
          system,
          lib,
          config,
          pkgs,
          ...
        }:
        {
          # Our only Haskell project. You can have multiple projects, but this template
          # has only one.
          # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
          haskellProjects.default = {
            # To avoid unnecessary rebuilds, we filter projectRoot:
            # https://community.flake.parts/haskell-flake/local#rebuild
            projectRoot = builtins.toString (
              lib.fileset.toSource {
                root = ./.;
                fileset = lib.fileset.unions [
                  ./eo-phi-normalizer
                  ./scripts/transform-eo-tests
                  ./cabal.project
                  ./README.md
                ];
              }
            );

            # Development shell configuration
            devShell = {
              hlsCheck.enable = false;
            };

            # What should haskell-flake add to flake outputs?
            autoWire = [
              "packages"
              "apps"
              "checks"
            ]; # Wire all but the devShell
          };

          # Auto formatters. This also adds a flake check to ensure that the
          # source tree was auto formatted.
          treefmt.config = {
            projectRootFile = "flake.nix";
            programs.nixfmt-rfc-style.enable = true;
            programs.cabal-fmt.enable = true;
            programs.fourmolu = {
              enable = true;
              ghcOpts = [
                "NoPatternSynonyms"
                "CPP"
              ];
            };
            settings.formatter.fourmolu.excludes = [ "eo" ];
            settings.global.excludes = [ "eo" ];
          };

          # Default package & app.
          apps.default = self'.apps.haskell-template;

          packages =
            let
              inherit (inputs.flakes.lib.${system}.drv-tools) mkShellApps;
            in
            mkShellApps {
              default = self'.packages.haskell-template;
              pipeline = {
                runtimeInputs = [
                  pkgs.stack
                  pkgs.jdk21
                  pkgs.maven
                  pkgs.perl
                ];
                text =
                  let
                    mkProgram = n: ''
                      export PROGRAM="${builtins.toString n}"
                      ${builtins.readFile ./scripts/pipeline.sh}
                    '';
                  in
                  ''
                    export JAVA_HOME="${pkgs.jdk21.home}"
                    ${lib.concatMapStringsSep "\n\n" mkProgram [ 2 ]}
                  '';
                description = "Run pipeline";
                excludeShellChecks = [ "SC2139" ];
              };

              update-markdown = {
                runtimeInputs = [
                  pkgs.mdsh
                  pkgs.mdbook-linkcheck
                  pkgs.stack
                ];
                text =
                  let
                    text = ''
                      mdsh

                      ${lib.concatMapStringsSep "\n" (x: "mdsh -i site/docs/src/${x} --work_dir .") [
                        "common/sample-program.md"
                        "common/celsius.md"
                        "normalizer.md"
                        "normalizer/transform.md"
                        "normalizer/metrics.md"
                        "normalizer/dataize.md"
                        "normalizer/report.md"
                        "contributing.md"
                      ]}

                      rm program.phi celsius.phi

                      npm i
                      npx prettier -w "**/*.md"'';
                  in
                  ''
                    export LC_ALL=C.UTF-8
                    stack install

                    cat << EOF > scripts/run-mdsh.sh
                    ${text}
                    EOF

                    chmod +x scripts/run-mdsh.sh

                    ${text}
                  '';
              };
            };

          # Default shell.
          devshells.default =
            let
              tools = [
                pkgs.ghcid
                pkgs.hpack
                pkgs.stack
                pkgs.gh
                pkgs.mdsh
                pkgs.mdbook
              ];
            in
            {
              packagesFrom = [
                config.haskellProjects.default.outputs.devShell
                config.treefmt.build.devShell
              ];
              bash.extra = "export LANG=C.utf8";
              commands = {
                inherit tools;
                scripts = [
                  self'.packages.pipeline
                  self'.packages.update-markdown
                ];
              };
            };
        };
    };

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
