let rev = "1849e695b00a54cda86cb75202240d949c10c7ce";
in import (builtins.fetchTarball "github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {}
