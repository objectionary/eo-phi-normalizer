{
  inputs = {
    flakes.url = "github:deemp/flakes";
    eoc = {
      url = "github:deemp/eoc";
      flake = false;
    };
    # should be synchronized with
    # https://github.com/objectionary/eoc/blob/116286a11aa538705c0f2b794abbdbcc6dec33ef/mvnw/.mvn/wrapper/maven-wrapper.properties#L18
    maven-wrapper-jar = {
      flake = false;
      url = "https://repo.maven.apache.org/maven2/org/apache/maven/wrapper/maven-wrapper/3.2.0/maven-wrapper-3.2.0.jar";
    };
  };
  outputs = inputs: inputs.flakes.makeFlake {
    inputs = {
      inherit (inputs.flakes.all)
        haskell-tools drv-tools devshell
        flakes-tools nixpkgs formatter
        slimlock;
      inherit (inputs) eoc maven-wrapper-jar;
    };
    perSystem = { inputs, system }:
      let
        # We're going to make some dev tools for our Haskell package
        # The NixOS wiki has more info - https://nixos.wiki/wiki/Haskell

        # --- Imports ---

        pkgs = inputs.nixpkgs.legacyPackages.${system};
        inherit (pkgs) lib;
        inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
        inherit (inputs.drv-tools.lib.${system}) mkShellApps;
        inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
        inherit (inputs.haskell-tools.lib.${system}) toolsGHC;

        # --- Parameters ---

        # The desired GHC version
        ghcVersion = "963";

        packageName = "eo-phi-normalizer";

        # --- Override ---

        # We need to prepare an attrset of Haskell packages and include our packages into it,
        # so we define an override - https://nixos.wiki/wiki/Haskell#Overrides.
        # We'll supply the necessary dependencies to our packages.
        # Sometimes, we need to fix the broken packages - https://gutier.io/post/development-fixing-broken-haskell-packages-nixpkgs/.
        # For doing that, we use several helper functions.
        # Overriding the packages may trigger multiple rebuilds,
        # so we override as few packages as possible.

        inherit (pkgs.haskell.lib)
          # override deps of a package
          overrideCabal
          ;

        eo-test-transformer = "eo-test-transformer";

        # Here's our override
        # Haskell overrides are described here: https://nixos.org/manual/nixpkgs/unstable/#haskell
        override.overrides = self: super: {
          "${packageName}" = super.callCabal2nix packageName ./${packageName} { };
          "${eo-test-transformer}" = super.callCabal2nix eo-test-transformer ./eo/test-transformer { };
        };

        # --- Haskell tools ---

        # We call a helper function that will give us tools for Haskell
        inherit (toolsGHC {
          version = ghcVersion;
          inherit override;

          # If we work on multiple packages, we need to supply all of them
          # so that their dependencies can be correctrly filtered.

          # Suppose we develop packages A and B, where B is in dependencies of A.
          # GHC will be given dependencies of both A and B.
          # However, we don't want B to be in the list of dependencies of GHC
          # because build of GHC may fail due to errors in B.
          packages = ps: [ ps.${packageName} ps.${eo-test-transformer} ];
        })
          hls cabal fourmolu justStaticExecutable
          ghcid ghc haskellPackages hpack stack;

        # --- Tools ---

        # We list the tools that we'd like to use
        tools = [
          ghcid
          hpack
          fourmolu
          cabal
          stack
          pkgs.gh
          # `cabal` already has a `ghc` on its `PATH`,
          # so you may remove `ghc` from this list.
          # Then, you can access `ghc` like `cabal exec -- ghc --version`.

          # However, sometimes, HLS wants a GHC.
          # In this case, write it before `HLS` - see https://github.com/NixOS/nixpkgs/issues/225895
          # ghc

          hls
        ];

        # --- Packages ---

        packages = mkShellApps {
          eo = pkgs.buildNpmPackage rec {
            name = "";
            version = "0.15.1";
            src = inputs.eoc;
            npmDepsHash = "sha256-j6lfte6RhxRY5cRHcrtIHfZDe0lP1ovEukgHbHsGPb0=";
            npmInstallFlags = [ "--omit=dev" ];
            dontNpmBuild = true;

            postPatch =
              let path = "mvnw/.mvn/wrapper/maven-wrapper.jar"; in
              ''
                cp ${inputs.maven-wrapper-jar} ${path}
                chmod +x ${path}
              '';
            meta = with pkgs.lib; {
              description = "EO compiler";
              homepage = "https://github.com/objectionary/eoc";
              license = licenses.mit;
              mainProgram = "eoc";
            };
          };

          # --- Haskell package ---

          # This is a static executable with given runtime dependencies.
          # In this case, its name is the same as the package name.
          default = justStaticExecutable {
            package = haskellPackages.${packageName};
            description = "A Haskell `hello-world` script";
          };

          "${packageName}" = haskellPackages."${packageName}";

          pipeline = {
            runtimeInputs = [
              stack
              pkgs.jdk21
              packages.eo
              pkgs.maven
              pkgs.perl
            ];
            text =
              let mkProgram = n: ''
                export PROGRAM="${builtins.toString n}"
                ${builtins.readFile ./pipeline.sh}
              ''; in

              ''
                export JAVA_HOME="${pkgs.jdk21.home}"
                ${lib.concatMapStringsSep "\n\n" mkProgram [ 2 ]}
              '';
            description = "Run pipeline";
            excludeShellChecks = [ "SC2139" ];
          };

          genPhi = {
            text = ''
              export JAVA_HOME="${pkgs.jdk21.home}"
              mkdir -p .pipeline/"$PROGRAM"
              mkdir -p test/phi

              cp eo-runtime/src/test/eo/org/eolang-subset/"$PROGRAM".eo .pipeline/"$PROGRAM"/app.eo

              cd .pipeline/"$PROGRAM"
              ${lib.getExe packages.eo} -- clean
              ${lib.getExe packages.eo} -- phi
              cp .eoc/phi/app.phi ../../test/phi/"$PROGRAM".phi
            '';
          };

          genPhis = {
            text = ''
              cd eo
              LANG=en_US.UTF-8
              ${lib.getExe stack} run eo-test-transformer
              ${lib.getExe pkgs.yq} -r .[].set test/config.yaml \
                | xargs -I {} bash -c 'x={}; export PROGRAM=''${x%.eo}; ${lib.getExe packages.genPhi}'
            '';
            excludeShellChecks = [ "SC2016" ];
          };
        };

        # --- Devshells ---

        devShells = {
          default = mkShell {
            packages = tools;
            # sometimes necessary for programs that work with files
            bash.extra = "export LANG=C.utf8";
            commands =
              mkCommands "tools" tools
              ++ mkRunCommands "packages" { inherit (packages) default pipeline; }
            ;
          };
        };

      in
      {
        inherit packages devShells;
        formatter = inputs.formatter.${system};
      };
  };

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
