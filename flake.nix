{
  description = "git-identity.el";

  inputs = {
    gnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
    melpa = {
      url = "github:melpa/melpa";
      flake = false;
    };

    nomake = {
      url = "github:emacs-twist/nomake";
      inputs.gnu-elpa.follows = "gnu-elpa";
      inputs.melpa.follows = "melpa";
    };
  };

  outputs =
    { self
    , nomake
    , ...
    } @ inputs:
    nomake.lib.mkFlake {
      src = ./.;

      localPackages = [
        "git-identity"
      ];

      extraPackages = [
        "buttercup"
      ];

      scripts = {
        test = {
          compile = false;
          text = ''
            emacs -batch -l buttercup -f buttercup-run-discover "$PWD"
          '';
        };
      };
    };
}
