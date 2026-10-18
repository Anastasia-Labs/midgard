# Toolchain

There are two independent ways to get everything that builds and runs Midgard:
Docker or Nix. Pick one. Neither depends on the other, and Nix builds no
container images.

Both take their versions from the same places:

- `demo/toolchain.json`: Node, Go, Rust, the PostgreSQL major, cardano-node,
  Ogmios, Kupo and mithril-client.
- `packageManager` in `demo/package.json`: pnpm, with its tarball digest.
- `demo/flake.lock`: the Aiken compiler (both routes run it through Nix, see
  `onchain/aiken/AGENTS.md`).

`demo/flake.nix` reads the manifest directly. The Dockerfiles, the compose
files and CI cannot, so `node scripts/release/toolchain.mjs` checks every
version pin they carry against it (the `toolchain-pins` preflight check, which
repo-tools CI runs). To move a version, edit the manifest and each pin the
check names.

## Docker

The Dockerfiles in `demo/midgard-node`, `demo/midgard-watcher` and
`demo/da-committee-node` build the Go transport, the Rust MPF owner and the
pnpm workspace in their own stages. The compose files run PostgreSQL and the
Cardano services from upstream images pinned by digest. See
`demo/midgard-node/README.md` for the compose stacks and
`demo/midgard-node-tools/devnet` for the local devnet.

## Nix

From `demo`:

- `nix develop` opens a shell with Node, pnpm, Go, Rust, Aiken, PostgreSQL,
  cardano-node, cardano-cli, Ogmios, Kupo and mithril-client.
- `nix build .#<package>` builds one of them on its own:

| Package                     | What it is                                                  |
| --------------------------- | ----------------------------------------------------------- |
| `midgard-node`              | The operator node, wrapped with the pinned Node.            |
| `midgard-watcher`           | The watcher CLI.                                            |
| `da-committee-node`         | The DA committee node and `midgard-public-retained-da`.     |
| `midgard-l1-node-transport` | The Go node-transport sidecar, built as the Dockerfiles do. |
| `architecture-g-owner`      | The Rust MPF owner the node runs as a child process.        |
| `postgresql`                | PostgreSQL at the manifest's major version.                 |
| `cardano-node`              | cardano-node and cardano-cli, plus each network's config.   |
| `ogmios`, `kupo`            | The chain-index services, from their release binaries.      |
| `mithril-client`            | The snapshot client, from its release binary.               |
| `aiken`                     | The pinned compiler.                                        |

The three Midgard apps default `L1_NODE_TRANSPORT_BINARY_PATH` and
`MPF_NATIVE_OWNER_BINARY_PATH` to the Nix builds of the native binaries. Like
the images, they take the contract blueprint as an input: run them from the
repository root after building it, or set `MIDGARD_REAL_BLUEPRINT_PATH`.

Ogmios, Kupo and mithril-client publish no Intel macOS build, so those
packages are missing on `x86_64-darwin`.

Two fixed-output hashes in `demo/flake.nix` follow lockfiles: `vendorHash`
follows `demo/l1-node-transport/native/go.sum` and `pnpmDepsHash` follows
`demo/pnpm-lock.yaml`. After changing either lockfile, a Nix build fails and
prints the new hash. Nix CI (`.github/workflows/nix-ci.yml`) builds every
package whenever a lockfile, the manifest or a flake file changes.
