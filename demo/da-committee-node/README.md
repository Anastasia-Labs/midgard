# DA committee node

`da-committee-node` retains authenticated block payloads, participates in the
configured DA committee, and serves retained data. It is separate from the
independent verifier/challenger in `demo/midgard-watcher`.

From this directory, build with `pnpm run build`, then run the committee CLI
with `node dist/index.js`. The separate public retained-reader entrypoint is
`node dist/public-retained-da.js`. Follow the
[committee setup guide](../../docs-site/content/docs/watchers/da-committee-node.mdx)
for complete deployment, libp2p, database, and reader configuration, and the
[availability responder guide](docs/availability-responder.md) for submission
and responder prerequisites.

## Local credential file

Component CLI bootstrap loads an optional `./config.yaml` relative to the
process working directory. The file is a **flat mapping of existing environment
variable names to nonempty string values**, not a separate runtime-manifest
schema. Quote all values, including numbers and booleans. For example,
`DA_SIGNER_INDEX: "0"` is a string; `DA_SIGNER_INDEX: 0` is not accepted.

| Control                                          | Behavior                                                                                                                                       |
| ------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| No override                                      | Load `./config.yaml` when present; an absent default file is allowed.                                                                          |
| `MIDGARD_CONFIG_FILE=/absolute/path/config.yaml` | Load that file; a missing explicit file is fatal.                                                                                              |
| `MIDGARD_CONFIG_MODE=disabled`                   | Disable YAML loading, including an explicit file.                                                                                              |
| `MIDGARD_DOTENV_MODE=disabled`                   | Disable implicit default YAML loading for isolated harnesses; an explicit `MIDGARD_CONFIG_FILE` still applies unless YAML loading is disabled. |

Set these controls in the launching process environment. Existing process
environment values win over YAML values. The node CLI applies the same YAML
bootstrap before dotenv; library imports do not implicitly load this file.
YAML errors redact contents. Keep credential files gitignored with mode `0600`
(`chmod 600 config.yaml`), and never put real secrets in tracked examples.

## Credential formats and authority

Use the existing credential format for each variable; their prefixes differ:

- `DA_SIGNER_KEY_SOURCE_0`, `DA_SIGNER_KEY_SOURCE_1`, etc. store the indexed
  signer credentials together. Each accepts `cardano-seed:` followed by a mnemonic,
  `cardano-seed-file:` followed by its file path, or the other supported Cardano
  and raw Ed25519 sources in [src/signer.ts](src/signer.ts).
- `L1_SUBMITTER_KEY_SOURCE` uses `seed:` for a mnemonic or `private-key:` for
  a Bech32 private key; see [src/l1/submitter.ts](src/l1/submitter.ts) for its
  complete source formats.
- Libp2p identity sources authenticate transport peers. They are separate from
  committee signing credentials and must match the appropriate runtime
  manifest identities.

Set `DA_SIGNER_INDEX` to choose one signer per process. The shared file may
contain a default; `DA_SIGNER_INDEX=1 node dist/index.js` overrides it and uses
`DA_SIGNER_KEY_SOURCE_1`. Member-specific transport and database settings must
still match that process. Missing selected keys and noncanonical index suffixes
fail startup. A process may instead supply one `DA_SIGNER_KEY_SOURCE` together
with its index, but single and indexed source forms cannot be mixed.

YAML loading does not create credentials, supply a missing committee signer,
or alter the governed threshold. Before starting a committee, verify that each
configured signer derives the exact deployed member verification key and that
enough available signers satisfy the deployed threshold. A public verification
key alone cannot recover its signing key.

The public retained reader needs its own non-signer libp2p identity and
SELECT-only database role. Give it a separate credential file containing only
its required reader configuration, rather than the committee's signer,
database-writer, or L1 submitter credentials. YAML does not replace the bound
deployment/runtime manifests, database privileges, or readiness checks.
