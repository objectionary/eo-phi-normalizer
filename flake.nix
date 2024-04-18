{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    devshell = {
      url = "github:deemp/devshell";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    mdsh = {
      url = "github:zimbatm/mdsh";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
      flake = false;
    };
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
        let
          ghcVersion = "964";
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack"; # will be available as the usual `stack` in terminal
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --system-ghc \
                  --no-install-ghc \
                  --nix \
                  --nix-shell-file stack.nix \
                  --nix-path nixpkgs=${inputs.nixpkgs} \
                "
            '';
          };
          mkShellApps = lib.mapAttrs (
            name: value:
            if !(lib.isDerivation value) && lib.isAttrs value then
              pkgs.writeShellApplication (value // { inherit name; })
            else
              value
          );
        in
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

            basePackages = pkgs.haskell.packages."ghc${ghcVersion}";

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
            programs.fourmolu = {
              enable = true;
              ghcOpts = [
                "NoPatternSynonyms"
                "CPP"
              ];
            };
            settings.formatter.fourmolu.excludes = [
              "eo"
              "Setup.hs"
              "Abs.hs"
              "Print.hs"
              "*.cabal"
            ];
            settings.global.excludes = [ "eo" ];
          };

          # Default package & app.
          apps.default = self'.packages.default;

          packages = mkShellApps {
            default = self'.packages.eo-phi-normalizer;
            pipeline = {
              runtimeInputs = [
                stack-wrapped
                pkgs.jdk21
                pkgs.maven
                pkgs.perl
              ];
              text = ''
                export JAVA_HOME="${pkgs.jdk21.home}"
                ${builtins.readFile ./scripts/pipeline.sh}
              '';
              meta.description = "Run pipeline";
              excludeShellChecks = [ "SC2139" ];
            };

            update-markdown = {
              runtimeInputs = [
                inputs.mdsh.packages.${system}.default
                pkgs.mdbook-linkcheck
                stack-wrapped
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


                  cat << EOF > scripts/update-markdown.sh
                  ${text}
                  EOF

                  chmod +x scripts/update-markdown.sh

                  ${text}
                '';
            };

            # buildStackProject arguments: https://github.com/NixOS/nixpkgs/blob/c7089236291045a523429e681bdaecb49bb501f3/pkgs/development/haskell-modules/generic-stack-builder.nix#L4-L11
            stack-shell = pkgs.haskell.lib.buildStackProject {
              name = "stack-shell";
              ghc = pkgs.haskell.compiler."ghc${ghcVersion}";
            };
          };

          # Default shell.
          devshells.default =
            let
              tools = [
                stack-wrapped
                pkgs.ghcid
                pkgs.hpack
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
                  {
                    prefix = "nix run .#";
                    packages = {
                      inherit (self'.packages) pipeline update-markdown;
                    };
                  }
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
