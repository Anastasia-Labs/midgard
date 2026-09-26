# Component configuration and wallet storage

Each executable service reads `config.yaml` in its working directory. Use
`MIDGARD_CONFIG_FILE=/absolute/path/config.yaml` for service managers, containers
or an alternate configuration location. An explicitly selected missing file is a startup error.
An absent default file leaves environment-only launches available.

The YAML is a flat mapping of existing configuration names to nonempty strings.
Explicit process environment values take precedence over YAML; for the node,
YAML takes precedence over `.env`. Quote numbers and booleans. Duplicate keys,
aliases, nested values and invalid YAML fail before applying any settings.
Parser errors do not include the file contents. Component validators still
validate the resulting settings and wallet identities.

Set `MIDGARD_CONFIG_MODE=disabled` to prevent YAML loading. Existing isolated
harnesses using `MIDGARD_DOTENV_MODE=disabled` also bypass implicit checkout YAML;
they may select an explicit file with `MIDGARD_CONFIG_FILE`. These loader controls
must be supplied externally, not inside YAML.

| Component | Local credential file | Wallet roles |
| --- | --- | --- |
| Operator node and node tools | `demo/midgard-node/config.yaml` | Operator, merge, reference publisher, user |
| DA committee members | `demo/da-committee-node/config.yaml` | Indexed committee signers, L1 submission, availability responder |
| Fault-proof watcher | `demo/midgard-watcher/config.yaml` | Prover and availability challenger |
| Public retained DA reader | Its own directory's `config.yaml` | No wallet; dedicated libp2p identity |

Runtime genesis is empty on every network, matching atomic initialization.
Fund L2 wallets through authenticated deposits. Legacy testnet genesis wallet
seeds do not create runtime balances; isolated fixtures supply their own inputs.

Templates are in each package as `config.example.yaml`; the public reader has
`public-retained-da.config.example.yaml`. Copy a template to the private path,
remove the empty `{}` mapping when adding settings, and use mode `0600`.
Private `demo/**/config.yaml` files and `.env.bak-*` backups are git-ignored.
Do not put real credentials in examples, tracked files, command arguments or logs.

`midgard-node-tools` uses the node's loader: launch from the node directory or
select its file explicitly. Committee signer mnemonic sources use
`cardano-seed:`, committee L1 submitter sources use `seed:`, and watcher wallet
environment sources contain the raw mnemonic. These prefixes are not interchangeable.
Each DA process selects one indexed source with `DA_SIGNER_INDEX`; an explicit
process value overrides the file's default. For example, from the DA package,
`DA_SIGNER_INDEX=1 node dist/index.js` selects member 1 from the shared file.
Do not mix indexed sources with the single-process `DA_SIGNER_KEY_SOURCE` form.
Missing selected sources fail startup; membership is still validated against
the deployment. Sharing the file does not start multiple members or supply their
individual transport/database configuration.

The watcher's `--config watcher-process.json` still describes protocol, storage
and authority configuration. Its wallet `keySource` entries must use environment
references to the variable names populated from YAML. Existing file references
continue to read their named files. Library config parsers do not implicitly
read local files; the executable entrypoints load YAML.

Docker does not automatically mount host YAML files. A service deployment must
mount its private file read-only and set `MIDGARD_CONFIG_FILE` to the container
path. The existing watcher Compose file uses Docker secret files; changing to
YAML also requires matching environment references in its process JSON. Do not
assume a host file changes a running container or service.

## Local migration status, 2026-09-24

Existing node wallet seeds and producer transport identity have been copied to
the node YAML. Member 0's saved signer and separate L1 submitter seed have been
copied to its YAML. Their identities are unchanged. Original `.env`, backup and
standalone seed files remain available to existing shell scripts and acceptance
tooling; this change does not delete them or restart services.

A replacement member 1 wallet was subsequently created with explicit user
authorization. Both signer seeds are now stored together in
`demo/da-committee-node/config.yaml` as `DA_SIGNER_KEY_SOURCE_0` and
`DA_SIGNER_KEY_SOURCE_1`; the redundant member directory has been removed.
Its saved mnemonic reproduces the public identity recorded in the checkpoint
progress log. It is not yet installed in the deployed committee; the original
member's private key was never persisted.

The fresh campaign subsequently sorted the replacement signer at index 0 and
the existing operator signer at index 1. Both seeds remain in the shared file.
A distinct availability responder is now saved and funded; the dedicated
public reader has transport and read-only SQL credentials. The watcher still
has incomplete configuration. Fresh reference publication and service startup
are recorded in the checkpoint progress log; configuration alone does not close
live acceptance.
