{ system ? builtins.currentSystem or "x86_64-linux"
, ghc ? "ghc8102"
}:

let
  lib = pkgs.lib;
  hlib = pkgs.haskell.lib;
  nix = import ./nix;

  disableOptionalHaskellBuildSteps = super: args: super.mkDerivation (args // {
    doCheck = false;
    doBenchmark = false;
    doHoogle = false;
    doHaddock = false;
    enableLibraryProfiling = false;
    enableExecutableProfiling = false;
  });

  haskellPkgSetOverlay = pkgs.callPackage ./nix/haskell/overlay.nix {
    inherit (nix) sources;
  };

  # Borrowed from
  # https://github.com/dhess/dhess-lib-nix/blob/f3f9660c4d86eabee40fd587b15c3535936857a3/overlays/haskell/lib.nix#L11
  # As long as you have nix-expressions available, you should
  # generally prefer using properExtend over manually extending or
  # overriding haskellPackages. Extending has problems and overriding
  # falls apart as soon as you have to do it more than once because
  # the package set will only take the latest set of overrides, wiping
  # away any of the previous overrides. The examples in this project
  # show a worked example but generally the usage is:
  #
  # hpkgs = extendHaskellPackages (pkgs.haskell.pkgs.${ghc}) (self: super: {
  #   my-package = ....
  # });
  extendHaskellPackages = haskellPackages: fun:
    haskellPackages.override (oldArgs: {
      overrides = lib.composeExtensions (oldArgs.overrides or (_: _: {}))
        fun;
    });

  pkgs = nix.pkgSetForSystem system {
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };

  #TODO: use gitignore.nix?
  #TODO: disable tests?
  mxnetOverlay = hfinal: hprev: {
    mxnet = hfinal.callCabal2nix "mxnet" (lib.sourceByRegex ./. [
      "^mxnet.hs$"
      "^css.*$"
      "^pages.*$"
      "^posts.*$"
      "^javascripts.*$"
      "^templates.*$"
      "mxnet.cabal"
    ]) {};
  };

  ############################################################################
  # Construct a 'base' Haskell package set, disabling the test
  # and benchmark suites for all dependencies by default.
  baseHaskellPkgs =
    extendHaskellPackages pkgs.haskell.packages.${ghc} (
      _hfinal: hprev: {
        mkDerivation = disableOptionalHaskellBuildSteps hprev;
      }
    );

  haskellPkgs = baseHaskellPkgs.override (
    old: {
      overrides = builtins.foldl' pkgs.lib.composeExtensions
        (old.overrides or (_: _: {})) [
        haskellPkgSetOverlay
        mxnetOverlay
      ];
    }
  );

  shell = haskellPkgs.shellFor {
    packages = p: [
      pkgs.cabal-install
      p.mxnet
    ];

    withHoogle = true;

    # include any recommended tools
    nativeBuildInputs = [
      haskellPkgs.ghcid
      haskellPkgs.hpack
      haskellPkgs.cabal-install
    ];
  };

in {
 inherit shell;
}
