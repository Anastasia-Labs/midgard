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

## Store ownership

The committee store is PostgreSQL, named by `DA_COMMITTEE_DATABASE_URL`. The
L1 follower's tables live in the same database. One member may run
active/passive across hosts against one store: the store's instance lock, a
Postgres session advisory lock, admits one writer. The same session also
holds the follower's writer lease, so the store and the follower are held,
lost and retaken together, and `midgard-l1-follower reset` is refused while a
committee process holds the store.

A process that starts while another live process holds the lock is refused
with `starting:store_instance_lock_held`; startup holds unready and retries
without a deadline. Other startup failures are classified. A dependency that
is down or still starting (a refused, reset or timed-out connection, a
Postgres connection-class error, a Cardano node socket not created yet) is
retried with backoff for at most 15 minutes in a row, under
`starting:<detail>`. Past that budget the process exits non-zero with
`committee_startup_dependency_unavailable`, and the supervisor's restart is
the backoff. Any other failure, such as a store holding another deployment's
state (`stale_deployment_state_requires_fresh_redeploy`), bad credentials, a
configuration or key-material refusal or an error it does not recognise, is
one no restart is known to repair: the process logs `committee_startup_held`
and stays up, `/readyz` naming `committee_startup_failed` with the failure's
detail and `/healthz` live (`status: "held"`), until an operator restarts it.
Only a configuration that does not load, before any port is known, and a
`--once` run exit non-zero on such a failure.
A process whose lock session ends (a lost connection, a restarted database)
refuses every decision effect, logs `committee_store_instance_lock_suspended`
and reconnects with backoff from 1 s, doubling to 30 s. If another process
took the lock meanwhile, this one becomes the passive member: it logs
`committee_store_instance_lock_passive`, keeps refusing every effect and keeps
retrying, and takes over once the holder's session ends, logging
`committee_store_instance_lock_restored`. Neither process exits: `/readyz`
names `store_instance_lock_reacquiring` or
`store_instance_lock_held_elsewhere`, and `/healthz` stays live. The wait on
another holder has no deadline. A reconnect that fails is classified: a
transient failure (Postgres unreachable, a dropped connection) is retried
for at most 15 minutes in a row; any other failure (bad credentials, a
missing database, an unclassified error), or the 15 minutes running out,
stops the retries. The process then logs
`committee_store_instance_lock_failed`. When the 15 minutes ran out it then
logs `committee_transient_budget_exhausted` (source `store_instance_lock`)
and exits non-zero, the supervisor's restart being the backoff; on any other
failure it keeps refusing every effect and names `store_instance_lock_failed`
on `/readyz` until it is restarted. The committee's L1 follower bounds its
transient store failures the same way: past 15 minutes with no event applied
it stops `l1_follower_transient_exhausted`, and the process logs
`committee_transient_budget_exhausted` (source `l1_follower`) and exits
non-zero. A Cardano node outage has no bound: `/readyz` names it while it
lasts.

A decision effect the old holder began and never completed is redone by the
new holder as the next attempt; a late completion of the old attempt is
refused, so no effect is lost or run twice. Process death, including a crash
or a container restart, ends its session and releases the lock: no operator
ever removes a lock by hand.
