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

`demo/flake.nix` reads the manifest directly. Everything else that names a
version cannot: the Dockerfiles, the compose files and `.env.example`, CI's
Go setup, `go.mod` and `.nvmrc`. `node scripts/release/toolchain.mjs` (the
`toolchain-pins` preflight check, which repo-tools CI runs) checks each of
them still pins its tools at the manifest's versions. To move a version, edit
the manifest and each pin the check names.

## Docker

The Dockerfiles in `demo/midgard-node`, `demo/midgard-watcher` and
`demo/da-committee-node` build the Go transport, the Rust MPF owner and the
pnpm workspace in their own stages. The compose files run PostgreSQL and the
Cardano services from upstream images. See
`demo/midgard-node/README.md` for the compose stacks and
`demo/midgard-node-tools/devnet` for the local devnet; the devnet and the
observability stack run only under Docker.

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

As in the images, each Midgard app's output carries its native binaries, each
with a `.sha256`, under `native/`, and its wrapper sets only the environment
its Dockerfile sets. Name those paths in the app's own configuration:
`L1_NODE_TRANSPORT_BINARY_PATH`, `MPF_NATIVE_OWNER_BINARY_PATH` and
`MPF_NATIVE_OWNER_BINARY_SHA256` for the node,
`CARDANO_L1_NODE_TRANSPORT_BINARY_PATH` for the DA committee, and
`l1NodeTransportBinaryPath` in the watcher's process file. Like the images,
the apps take the contract blueprint as an input: run them from the
repository root after building it, or set `MIDGARD_REAL_BLUEPRINT_PATH`.

Nix CI builds and starts everything on `x86_64-linux`; other platforms are
untested. Aiken, Ogmios, Kupo and mithril-client have no Intel macOS build,
so the development shell does not evaluate on `x86_64-darwin`.

Two fixed-output hashes in `demo/flake.nix` follow lockfiles: `vendorHash`
follows `demo/l1-node-transport/native/go.sum` and `pnpmDepsHash` follows
`demo/pnpm-lock.yaml`. After changing either lockfile, a Nix build fails and
prints the new hash. Nix CI (`.github/workflows/nix-ci.yml`) runs on a pull
request that changes either lockfile, the manifest or a flake file.
