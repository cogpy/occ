#!/bin/sh
# Unpack Guile 3.0 without root (sandboxes where sudo/apt install are broken)
# and print the directory holding a `guile` wrapper; prepend it to PATH.
#
#   export PATH="$(sh .claude/skills/guix-ci-workflow/scripts/local-guile.sh /tmp/guile):$PATH"
#   guile --version
set -e
dir=${1:?usage: local-guile.sh <dir>}
root="$dir/root"

if [ ! -x "$root/usr/bin/guile-3.0" ]; then
  mkdir -p "$dir"
  (cd "$dir" && apt-get download guile-3.0 guile-3.0-libs libgc1 libffi8 libunistring5 >&2)
  for deb in "$dir"/*.deb; do dpkg -x "$deb" "$root"; done
fi

lib="$root/usr/lib/x86_64-linux-gnu"
mkdir -p "$dir/bin"
cat > "$dir/bin/guile" <<EOF
#!/bin/sh
export LD_LIBRARY_PATH="$lib"
export GUILE_LOAD_PATH="$root/usr/share/guile/3.0"
export GUILE_LOAD_COMPILED_PATH="$lib/guile/3.0/ccache"
export GUILE_SYSTEM_EXTENSIONS_PATH="$lib/guile/3.0/extensions"
exec "$root/usr/bin/guile-3.0" "\$@"
EOF
chmod +x "$dir/bin/guile"
echo "$dir/bin"
