import (builtins.fetchTarball {
  name = "gzc-nixpkgs-pinned";
  url = "https://github.com/nixos/nixpkgs/archive/540dccb2aeaffa9dc69bfdc41c55abd7ccc6baa3.tar.gz";
  # Use `nix-prefetch-url --unpack <url>`
  sha256 = "1j58m811w7xxjncf36hqcjqsfj979hkfcwx9wcrm3g3zbayavapg";
}) {}
