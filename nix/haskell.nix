############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ haskell-nix
, # Version info (git revision)
  gitrev
, inputMap
}:
let

  inherit (haskell-nix) haskellLib;

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  project = haskell-nix.cabalProject' ({ pkgs
                            , lib
                            , config
                            , buildProject
                            , ...
                            }: {
    inherit inputMap;
    name = "cardano-node";
    # We clean-up src to avoid rebuild for unrelated changes:
    src = haskellLib.cleanSourceWith {
      src = ../.;
      name = "cardano-node-src";
      filter = path: type: let
        relPath = lib.removePrefix "${../.}/" path;
      in
        # excludes top-level directories not part of cabal project (or used by tests):
        (type
          != "directory"
          || (builtins.match ".*/.*" relPath != null)
          || (!(lib.elem relPath [
              "nix"
              "doc"
              "ci"
            ])
            && !(lib.hasPrefix "." relPath)))
        # exclude ".gitignore" files
        && !(lib.hasSuffix ".gitignore" relPath)
        # only keep cabal.project from files at root:
        && (type == "directory" || builtins.match ".*/.*" relPath != null || (relPath == "cabal.project"));
    };
    compiler-nix-name = "ghc8107";
    cabalProjectLocal = ''
      allow-newer: terminfo:base
    '' + lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
      -- When cross compiling we don't have a `ghc` package
      package plutus-tx-plugin
        flags: +use-ghc-stub
    '';
    shell = {
      name = "cabal-dev-shell";

      # These programs will be available inside the nix-shell.
      nativeBuildInputs = with pkgs.pkgsBuildBuild; [
        nix-prefetch-git
        pkg-config
        hlint
        ghcid
        haskell-language-server
        cabal
      ];

      withHoogle = true;
    };
    modules =
      let
        inherit (config) src;
        # deduce package names and exes from the cabal project to avoid hard-coding them:
        projectPackagesExes =
            let
              project = haskell-nix.cabalProject'  (builtins.removeAttrs config ["modules"]);
              packages = haskellLib.selectProjectPackages project.hsPkgs;
            in
            lib.genAttrs
              (lib.attrNames packages)
              (name: lib.attrNames packages.${name}.components.exes);
        projectPackageNames = builtins.attrNames projectPackagesExes;
      in
      [
        ({ pkgs, ... }: {
          packages.cardano-tracer.package.buildable = with pkgs.stdenv.hostPlatform; lib.mkForce (!isMusl);
          packages.cardano-node-chairman.components.tests.chairman-tests.buildable = lib.mkForce pkgs.stdenv.hostPlatform.isUnix;
          packages.plutus-tx-plugin.components.library.platforms = with lib.platforms; [ linux darwin ];
          packages.tx-generator.package.buildable = with pkgs.stdenv.hostPlatform; !isMusl;
        })
        ({ pkgs, ... }: {
          # Needed for the CLI tests.
          # Coreutils because we need 'paste'.
          packages.cardano-cli.components.tests.cardano-cli-test.build-tools =
            lib.mkForce (with pkgs.buildPackages; [ jq coreutils shellcheck ]);
          packages.cardano-cli.components.tests.cardano-cli-golden.build-tools =
            lib.mkForce (with pkgs.buildPackages; [ jq coreutils shellcheck ]);
          packages.cardano-testnet.components.tests.cardano-testnet-tests.build-tools =
            lib.mkForce (with pkgs.buildPackages; [ jq coreutils shellcheck lsof ]);
        })
        ({ pkgs, ... }: {
          # Use the VRF fork of libsodium
          packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
          packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
        })
        ({ pkgs, options, ... }: {
          # add shell completion:
          packages = lib.mapAttrs
            (name: exes: {
              components.exes = lib.genAttrs exes (exe: {
                postInstall = lib.optionalString (!pkgs.stdenv.hostPlatform.isWindows
                  && lib.elem exe ["cardano-node" "cardano-cli" "cardano-topology" "locli"]) ''
                  BASH_COMPLETIONS=$out/share/bash-completion/completions
                  ZSH_COMPLETIONS=$out/share/zsh/site-functions
                  mkdir -p $BASH_COMPLETIONS $ZSH_COMPLETIONS
                  $out/bin/${exe} --bash-completion-script ${exe} > $BASH_COMPLETIONS/${exe}
                  $out/bin/${exe} --zsh-completion-script ${exe} > $ZSH_COMPLETIONS/_${exe}
                '';
              });
            })
            projectPackagesExes;
        })
        ({ pkgs, config, ... }: {
          # Packages we wish to ignore version bounds of.
          # This is similar to jailbreakCabal, however it
          # does not require any messing with cabal files.
          packages.katip.doExactConfig = true;
          # split data output for ekg to reduce closure size
          packages.ekg.components.library.enableSeparateDataOutput = true;
          # cardano-cli-test depends on cardano-cli
          packages.cardano-cli.preCheck = "
          export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
          export CARDANO_NODE_SRC=${src}
        ";
          packages.cardano-node-chairman.components.tests.chairman-tests.build-tools =
            lib.mkForce [
              pkgs.lsof
              config.hsPkgs.cardano-node.components.exes.cardano-node
              config.hsPkgs.cardano-cli.components.exes.cardano-cli
              config.hsPkgs.cardano-node-chairman.components.exes.cardano-node-chairman
            ];
          # cardano-node-chairman depends on cardano-node and cardano-cli
          packages.cardano-node-chairman.preCheck = "
          export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
          export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
          export CARDANO_NODE_CHAIRMAN=${config.hsPkgs.cardano-node-chairman.components.exes.cardano-node-chairman}/bin/cardano-node-chairman${pkgs.stdenv.hostPlatform.extensions.executable}
          export CARDANO_NODE_SRC=${src}
        ";
          # cardano-testnet needs access to the git repository source
          packages.cardano-testnet.preCheck = "
          export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
          export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
          export CARDANO_SUBMIT_API=${config.hsPkgs.cardano-submit-api.components.exes.cardano-submit-api}/bin/cardano-submit-api${pkgs.stdenv.hostPlatform.extensions.executable}
          ${lib.optionalString (!pkgs.stdenv.hostPlatform.isWindows) ''
          ''}
          export CARDANO_NODE_SRC=${src}
        ";
        })
        ({ pkgs, ... }: lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
          # Needed for profiled builds to fix an issue loading recursion-schemes part of makeBaseFunctor
          # that is missing from the `_p` output.  See https://gitlab.haskell.org/ghc/ghc/-/issues/18320
          # This work around currently breaks regular builds on macOS with:
          # <no location info>: error: ghc: ghc-iserv terminated (-11)
          packages.plutus-core.components.library.ghcOptions = [ "-fexternal-interpreter" ];
        })
        {
          packages = lib.genAttrs projectPackageNames
            (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
        }
        ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
          # systemd can't be statically linked
          packages.cardano-git-rev.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
          packages.cardano-node.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
          packages.cardano-tracer.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
        })
        # Musl libc fully static build
        ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isMusl (
          let
            # Module options which adds GHC flags and libraries for a fully static build
            fullyStaticOptions = {
              enableShared = false;
              enableStatic = true;
            };
          in
          {
            packages = lib.genAttrs projectPackageNames (name: fullyStaticOptions);
            # Haddock not working and not needed for cross builds
            doHaddock = false;
          }
        ))
        ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
          # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
          packages.Win32.components.library.build-tools = lib.mkForce [ ];
          packages.terminal-size.components.library.build-tools = lib.mkForce [ ];
          packages.network.components.library.build-tools = lib.mkForce [ ];
        })
        # TODO add flags to packages (like cs-ledger) so we can turn off tests that will
        # not build for windows on a per package bases (rather than using --disable-tests).
        # configureArgs = lib.optionalString stdenv.hostPlatform.isWindows "--disable-tests";
      ];
  });
in project.appendOverlays (with haskellLib.projectOverlays; [
  projectComponents
  (final: prev: let inherit (final.pkgs) lib; in {
    profiled = final.appendModule {
      modules = [{
        enableLibraryProfiling = true;
        packages.cardano-node.components.exes.cardano-node.enableProfiling = true;
        packages.tx-generator.components.exes.tx-generator.enableProfiling = true;
        packages.locli.components.exes.locli.enableProfiling = true;
      }];
    };
    asserted = final.appendModule {
      modules = [{
        packages = lib.genAttrs [
          "ouroboros-consensus"
          "ouroboros-consensus-cardano"
          "ouroboros-consensus-cardano-tools"
          "ouroboros-consensus-byron"
          "ouroboros-consensus-shelley"
          "ouroboros-network"
          "network-mux"
        ]
          (name: { flags.asserts = true; });
      }];
    };
    eventlogged = final.appendModule
      {
        modules = [{
          packages = final.pkgs.lib.genAttrs [ "cardano-node" ]
            (name: { configureFlags = [ "--ghc-option=-eventlog" ]; });
        }];
      };
    # add passthru and gitrev to hsPkgs:
    hsPkgs = lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
      (path: value:
        if (lib.isAttrs value) then
          lib.recursiveUpdate
            (if lib.elemAt path 2 == "exes" && lib.elem (lib.elemAt path 3) [ "cardano-node" "cardano-cli" ] then
              # Stamp executables with version info.
              # Done outside the haskell.nix derivation to avoid compilation and tests depending on rev.
              final.pkgs.buildPackages.runCommand value.name
                {
                  inherit (value) exeName exePath meta passthru;
                } ''
                mkdir -p $out
                cp --no-preserve=timestamps --recursive ${value}/* $out/
                chmod -R +w $out/bin
                ${final.pkgs.pkgsBuildBuild.haskellBuildUtils}/bin/set-git-rev "${gitrev}" $out/bin/*
              ''
            else value)
            {
              # Also add convenient passthru to some alternative compilation configurations:
              passthru = {
                profiled = lib.getAttrFromPath path final.profiled.hsPkgs;
                asserted = lib.getAttrFromPath path final.asserted.hsPkgs;
                eventlogged = lib.getAttrFromPath path final.eventlogged.hsPkgs;
              };
            } else value)
      prev.hsPkgs;
  })
])
