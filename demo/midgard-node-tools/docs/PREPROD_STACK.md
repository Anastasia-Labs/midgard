# Persistent Preprod stack

From the repository root, one command builds the tooling and sets up or resumes
Cardano Preprod with local Docker providers, the node, DA committee, public
retained DA and watcher, then runs wallet journeys:

```sh
pnpm --dir demo/midgard-node-tools run e2e-stack --config /absolute/path/preprod-stack.json
```

`--config` must be an absolute path: the package script runs from the package
directory, so a relative path would not resolve against the caller's directory.
An already built tool runs the same command as
`node demo/midgard-node-tools/dist/index.js e2e-stack --config ...`.

This command is the only live acceptance flow. The operating runbook (run
modes, the step-by-step reference, evidence and stop-message recovery) is the
[midgard-e2e-acceptance skill](../../../.agents/skills/midgard-e2e-acceptance/SKILL.md);
this document specifies the configuration and behavior it relies on.

Use Node 22.16 or newer, the repository's pnpm, its pinned Aiken compiler, Docker
Compose 2.21 or newer (setup refuses older versions), Go (setup selects
1.26.5), Rust/Cargo and Linux `flock`. Install the workspace dependencies first. Setup builds
the Preprod contract profile and the workspace runtimes. Provider startup may
need several hours to download and synchronize the Preprod snapshots.

Copy `config/preprod-stack.example.json` and replace its absolute paths. This
file contains names of secret environment variables, never their values. It
uses the existing operator stack's `.env`, provider data, deployment directory
and Docker volumes. Set `NETWORK=Preprod`, `MIDGARD_DEPLOYMENT_PROFILE=preprod-testing`
(or `preprod-public`, below),
`RUN_GENESIS_ON_STARTUP=false`, and the exact L2
`MIN_FEE_A`/`MIN_FEE_B`. Keep the operator deployment profile settings from
`demo/midgard-node/.env.example`. Define all wallet and DA secret variables named
by the stack configuration in that `.env`. The stack-only secrets (user,
recipient, prover, availability, DA submitter, DA members, transports and DA
passwords) must use distinct `STACK_` names, so they never name a node, DA or
watcher setting; the generated node environment blanks them. Each wallet role
must be distinct.
Set `DA_THRESHOLD` to the intended threshold, between the SDK governed floor
`ceil(2 * memberCount / 3)` and the member count (the example uses two members
and threshold two). Setup derives and sorts the Cardano verification keys and
uses the same signer indexes in every generated service. Duplicate keys are
rejected. `DA_OWNERS_HEX`, when supplied, must contain sorted, unique 28-byte
payment key hashes; otherwise the node derives its owners from its local
signers. The node's existing initialization checks validate governance before
any deployment spending. Multi-owner governance is the standing configuration.
Transport identities use persistent `seed:<32-byte-hex>` or `hex:` sources.
The retained DA identity is separate from every signer and the producer.

Wallet budgets are explicit minimum balances for fresh setup; choose them for
your contract publication costs and measured funding requirements. The prover
and availability addresses use the existing watcher Enterprise address
derivation; the other wallet roles use the node's normal seed addresses. Each
wallet also needs a plain ADA output of at least 5 ADA for fees/collateral.
The DA submitter wallet needs at least 2 plain ADA outputs holding 80 ADA
together, the floor its committee checks when it starts.
The configured budgets apply only before the node records the signed hub-oracle
nonce, its first write to the deployment run state. A resumed or attached deployment, before or after initialization, requires only
that working capital: 5 ADA per wallet plus that plain output. The user budget
must cover every deposit plus fee headroom at fresh setup. Before each deposit
is first submitted, the journey also checks that the user's L1 balance covers
that deposit plus 5 ADA of fee headroom; a resent deposit reuses its saved
intent without repeating the check.

The Compose wrapper derives operator host ports in linked worktrees. Configure
the node endpoint and local provider URLs to match those ports (`scripts/operator-compose.sh
--print-env --env-file .env`). DA ports are explicit and must not collide with
another stack. Host database commands such as `db:migrate` use loopback
Postgres at `MIDGARD_POSTGRES_HOST_PORT`; containers use `postgres:5432`. The
journey's deposits and withdrawals use no database: it runs the user's own
`midgard-user` tool against the stack's Kupo and Ogmios. Set `MIDGARD_POSTGRES_HOST_PORT` explicitly in
the `.env`; 5433 and 55433 are refused because they belong to test databases.
Before each host database command, setup compares the Postgres cluster
identity seen inside Compose with the one reached at `127.0.0.1` on that port
and refuses to run when they differ.
Setup builds and pins the host native owner, exports the local Cardano config,
and configures the native ledger query helper for host commands and containers.
Node and committee reward-account queries use that same local Cardano socket.

Watcher inputs use the existing watcher process template. Replace the process
template's illustrative historical script provider endpoints with the release's
operational provider quorum. Those external history providers are release
infrastructure inputs. Setup replaces the node genesis identity and the
retained DA peer with deployment-specific values; the watcher follows the local
node directly. `watcher-compose.env` must name the existing regular secret
files with `WATCHER_ROLLBACK_KEY_FILE`, `WATCHER_PROVER_KEY_FILE`,
`WATCHER_AVAILABILITY_KEY_FILE`, and a value for `MIDGARD_L1_CONFIG_DIR`
(replaced with the exported local node config at setup). The rollback key is
32-byte lowercase hex; wallet secrets must be canonical, without a trailing
newline. The prover and availability files must hold the seeds of the
corresponding funded wallets. The stack's watcher keeps its SQLite stores in
the project-scoped volume `<node project>_watcher-state`, separate from any
standalone `midgard-watcher` deployment.

For an existing deployment, `releaseDirectory` holds the signed watcher release
artifacts named by the process template; set `releaseInput` to `null` when no
signing is required. Fresh deployment authoring requires a JSON file with:

```json
{
  "signingKeyFile": "/absolute/path/persistent-ed25519-private-key.pem",
  "programCommitments": { "<program-name>": "<release-commitment>" },
  "fundingProfiles": []
}
```

The key must already exist. `fundingProfiles` may stay empty: the watcher
prices its reservations from the runtime funding policy and never needs one. A
profile you do supply must be a genuine measurement for the exact blueprint,
Cardano parameter snapshot, applied reference scripts, economics and prover
wallet. Setup signs their canonical bundle and the rule
bundle against the confirmed deployment; it never fabricates measurements or
silently replaces an existing release. A profile mismatch stops setup and
preserves all deployed state. Before the authority is signed, replace the
measurements in the same release input file and rerun to resume against that
deployment. The signing key and program commitments remain fixed. Once signed,
the saved authority binds the measured bundle digest and rejects replacements. The example above describes the input shape;
replace its illustrative values with release artifacts before running.

With `MIDGARD_DEPLOYMENT_PROFILE=preprod-public`, first build the checkout for
that profile (`node demo/scripts/deployment-profiles.mjs generate preprod-public && pnpm --dir demo/midgard-node-tools run build`;
the stack refuses a `.env` naming another profile than the compiled one), then
copy `config/preprod-public-stack.example.json` instead. Its `endpoint` is
the `https://` loopback address of the TLS ingress, the node's only public
listener (the node itself serves only a Unix socket), and its `public` section names the private node configuration file
(the six node secrets, mode 0600), the TLS certificate, key and CA for
`MIDGARD_PUBLIC_API_ORIGIN`, the release trust roots, the blueprint
classification, the release directory, an optional extra ingress port and
`retainedDaHost`, the public DNS name the stack announces its retained DA at
(`/dns4`, on `da.ports.retainedTransport`, listening on every interface);
`--check` refuses it missing, local, private or an IP address. A
preprod-testing stack instead lets its own watcher dial the retained DA on
loopback through the generated watcher setting `da.stackLoopbackPeers`, which
a production build's watcher refuses by name. The `.env` then holds the release revision and
the digest-pinned images, but none of the node secrets and none of the public
file paths; the stack derives those. `--check` refuses a missing or wrong
public input before anything is built or spent. The node runs from
`MIDGARD_PUBLIC_NODE_IMAGE` over HTTPS. The release input also names a
`provenanceFile`, the release provenance without
`evidence.parameterRegistryDigest`; `--check` checks its other fields against
the `.env` and the compiled profile. Once the deployment is final, the stack
completes the provenance with the digest of the parameter registry the
deployment wrote, once, into the run directory, and signs the public release
for it into the release directory with the release input's signing key, by
the same code as `sign-deployment-release`; the watcher release uses the same
completed provenance. Rerunning verifies that release and never signs again. The operator procedure, including wallet
funding, is
[preprod-public-deploy](../../../docs-site/content/docs/operators/node/preprod-public-deploy.mdx).

`--check` runs every check that needs no build, network or service, and
starts nothing and spends nothing: the configuration fields and absolute paths,
the node environment (network, profile, genesis, exact fees,
the Postgres host port and the operator and DA port collisions), the presence
of every named secret, wallet mnemonics and derived addresses, the committee
keys and threshold, distinct persistent transport identities, readable watcher
secret files and the release input shape. It loads the built workspace
packages, so build them once first. The Compose version, `compose config`, the
watcher rollback key decoding and the prover/availability seed match run
later, after the builds, on a full run. `--setup-only` completes setup
and readiness checks without the wallet journeys; Compose keeps the services
running after the command exits. Run the same command again to attach or
resume. `docker compose restart` on the configured project preserves the
running services' generated settings. The generated override is saved in
`<runDirectory>/services/compose.json`; use the operator Compose wrapper with
that override when managing this stack. Do not start this stack with the node
README's plain `docker compose ... up`: that reads the `.env` holding the stack
secrets without the generated settings.

The file journal is written atomically and fsynced, with a stable run identity.
Reference, deposit and withdrawal journals preserve submitted intents. The
hub-oracle nonce transaction is recorded in the node's run state, with its
signed bytes, before it is first submitted, and the node refuses to build a
fresh nonce once that run state exists. A rerun therefore completes a nonce that
landed and resubmits exactly the recorded bytes while their inputs are unspent;
it never builds a second nonce. It stops, naming the transaction, if another
transaction spent those inputs (`SignedNonceConflictError`, with the spent
inputs) or if the ledger rejects the recorded bytes while their inputs are
unspent (`SignedNonceRejectedError`, with the ledger's reason, for example a
fee that a protocol-parameter change made too small). Those bytes can never
land, and rerunning the stack stops the same way. Either start again from a
separate linked worktree (see fresh setup below), or, deliberately replacing the
deployment identity, run the node's `prepare-hub-oracle-one-shot-nonce
--run-state <runDirectory>/deployment-run-state.json --fresh-redeploy
--fresh-redeploy-reason <reason>` yourself and then rerun the stack, which
adopts the confirmed replacement nonce. Before repeating a transaction step,
setup queries Cardano. It can reconstruct a deployment manifest when initialization confirmed
before the success record was written, and it waits, preserving the data, when
a finalized initialization is no longer at the Cardano tip. An ambiguous
submission stops without constructing a new one. Transfer bytes are saved
before their first send and reused after a lost response.

A controller lock prevents two runs against one node directory, and a command
lock allows one stack command at a time. Interrupting the controller (Ctrl-C or
kill) releases the controller lock; child commands do not inherit its
descriptor. An in-flight child command ends at its next output line, which
releases the command lock. A child blocked without output survives the
controller and keeps the command lock. A rerun then starts, but its first stack
command does not run: it stops with `CommandNotStartedError` ("another stack
command holds" the lock) and leaves the journal record as it was, until that
child exits. The rerun's Cardano and journal reconciliation then prevents
duplicate transactions.

A run is bound to its identity: network, deployment profile, node and run
directories, wallet seeds, DA members, transports, threshold, owners and
cosigner, the watcher rollback, prover and availability keys, and the
release signer and program commitments (or the existing signed release
artifacts). Changing any of these stops a resume. Timeouts, journey size,
budgets, ports and templates may change between runs. When
a change reaches the generated service configuration, the rerun regenerates it,
rewrites the node environment and runs Compose `up` again, which recreates the
containers whose configuration changed. The deployment and local storage
identities are checked on restart. Corrupt records, changed identity or
mismatched storage stop without resetting anything. No stack command uses
volume deletion, database wipes or fresh-redeploy flags. Preserve the run
directory, signing keys, `.env` and service volumes.

Fresh setup requires fresh local storage: a Postgres volume that was never used
or was only migrated, with no deployment rows (the migrations' own tables and
the unchanged calibration seed are allowed), and an empty or absent node `db`
directory. To get one without deleting existing volumes, run from a separate
linked worktree: the Compose wrapper gives it its own project, volumes, host
ports and node directory.

A second operator's node attaches to a running stack before the stack commits
its first block. A node with an empty database builds its ledger only from the
deployment's genesis, so it cannot join after the first block, or rejoin after
losing its database (#868). Run the stack with `--setup-only`, attach the second
instance, then run the stack again for its wallet journeys. The stack's committee accepts payloads only
from the producers it admits: list each further producer in the stack
configuration's `da.admittedProducers`, by the multiaddr it listens on, ending
in its peer id (never its key). Copy
`config/preprod-testing-twin-operator2.example.json` into a second linked
worktree built for `preprod-testing`. It names the stack configuration it
attaches to (`attachTo`), its own node directory, `.env`, run directory and
endpoint, the four node wallets by the stack's variable names (operator, merge,
reference, settlement; the merge wallet is treated exactly as the first
operator's is), and `producerTransportEnv`, the `STACK_` variable in its `.env`
holding its persistent DA transport identity. `--check` on that configuration
refuses, naming the address to add, a producer the stack does not admit: add
that address to the stack's `da.admittedProducers` and rerun the stack, which
regenerates its committee's configuration. It also refuses the stack's node or
run directory, any of the stack's node, metrics, DA or Postgres host ports, the
stack's Compose project, any of the stack's wallets (by payment key, per
role), and wallets other than the four node wallets. A run starts only its own
Postgres and node: it skips the nonce,
reference scripts and initialization, copies the stack's deployment and L1
origin once the stack has initialized, registers and activates its operator,
and runs the node on the stack's Cardano node socket, publishing to the stack's
committee. It never runs wallet journeys. Rerun the same command to resume or confirm it.

To let each operator commit the other's L2 transactions on a `preprod-testing`
stack, set `L2_TX_PEERS` in both nodes' `.env` to the other node's API as its
container reaches it, for example `L2_TX_PEERS=http://host.docker.internal:3020`
in the stack's `.env` and `L2_TX_PEERS=http://host.docker.internal:3010` in the
second instance's (the endpoints of the example configurations). Rerun each
command to apply it. Empty, the default, leaves sharing off.

A public stack wires this itself. Its nodes have no HTTP port, and both TLS
ingresses serve the stack's one certificate, so each node dials the other's
ingress at the certificate's host name (the host of
`MIDGARD_PUBLIC_API_ORIGIN`), `https://<host>:8443`, and trusts the stack's
`public.tlsCaFile` through `NODE_EXTRA_CA_CERTS`. Two Docker networks carry it,
one per direction, each holding one ingress under that name and the other
instance's node. The stack sets this up when its `da.admittedProducers` names a
producer on `/ip4/127.0.0.1/` (the second instance on its host), and refuses
more than one such producer. Run the stack again after admitting the second
instance, before attaching it: the stack's Compose project creates both
networks, `midgard-l2-peers-<id>-to-stack` and `-to-attached`, and the second
instance's project joins them. An `L2_TX_PEERS` already in either `.env` is
kept beside the generated peer.

A second instance of a public stack (`preprod-rehearsal` or `preprod-public`)
runs from a linked worktree built for the stack's profile, with
`MIDGARD_DEPLOYMENT_PROFILE` naming it and `RUN_GENESIS_ON_STARTUP=false`; copy
`config/preprod-rehearsal-operator2.example.json`. Its configuration also names
`nodeConfigFile`, its own private node configuration (mode 0600: its admin API
key, Postgres password and four node seeds, none of them in its `.env`, which
also holds none of the settings the stack derives), and an `https://` loopback
`endpoint`. It shares the stack's release, never builds one: the release
revision, the public origin and the node, Postgres and ingress images come from
the stack's `.env`, and its own `.env` may only repeat them; its node runs from
the stack's node image behind a TLS ingress of its own that serves the stack's
TLS certificate, and mounts the stack's signed release and trust roots
read-only. Every host port, the ingress's API port included, listens on
loopback only. `--check` refuses an operator or merge wallet not on the compiled
profile's allow-list, a validation worker pool below 2 or disabled HTTP rate
limits, and a node directory, run directory or node configuration that would
lie in the stack's release directory (or hold it). A run refuses to start its
node before the stack has signed its public release.

Forced orders and Plutus L2 transactions for the rehearsal journey are
submitted by hand with `midgard-user forced-order --shape <shape>` and
`midgard-user plutus-tx`, from a wallet holding L2 outputs on the stack's
node; see [the midgard-user README](../../midgard-user-cli/README.md#operator-rehearsal-commands)
for the shapes and their options. For example, after
`node scripts/contrib.mjs build --package @al-ft/midgard-user-cli`:

```sh
node demo/midgard-user-cli/dist/cli.js forced-order --shape plain \
  --node-endpoint https://<host>:8443 --manifest <stack manifest> \
  --l1 kupmios --kupo-url <kupo> --ogmios-url <ogmios> \
  --wallet-seed-phrase-file /abs/path/seed --json
```

When the stack's certificate is signed by its own CA, give the command that CA
with `NODE_EXTRA_CA_CERTS=<public.tlsCaFile>`.

The rehearsal's throughput drill is two runs of `e2e-stress-l2-throughput`,
each labelled in its `summary.json` and `summary.md` with the one kind of
evidence it gives. Both run from `demo/midgard-node` (the open-loop engine is
`scripts/throughput-valid-stress.mjs` there) with the node's environment,
including any `HTTP_RATE_LIMIT_*` setting: the drill reads the node's rate
limits through the node's own configuration reader and refuses to run with
`HTTP_RATE_LIMIT_ENABLED=false`. Neither raises nor disables a limit.

The functional check goes through the public TLS ingress as one client:
closed-loop, at most the node's per-client number of requests in flight, and
each request class paced to half the slowest bucket the node charges one
client for it. Its label is "Functional E2E through the public ingress from
one client"; it is not a throughput figure.

```sh
cd demo/midgard-node
MIDGARD_STACK_NODE_TLS_CA_FILE=<public.tlsCaFile> \
MIDGARD_STACK_NODE_TLS_SERVERNAME=<host of MIDGARD_PUBLIC_API_ORIGIN> \
node ../midgard-node-tools/dist/index.js e2e-stress-l2-throughput \
  --route public-ingress --endpoint https://127.0.0.1:<ingress port> \
  --load-model closed-loop-smoke --mode serial-chain --count 25 \
  --wallet-seed-phrase-env <variable naming an independent, L2-funded wallet> \
  --out-dir <directory beside runDirectory> --json
```

The capacity run goes straight at the node's Unix socket, open-loop, with the
node's rate limiter on. The load generator spreads its requests over enough
synthetic clients, each in its own /24 of the benchmarking range
198.18.0.0/15 and named in `x-midgard-client-ip`, that no client carries more
than half the write class's per-client or per-prefix rate or more requests at
once than the node allows one client. The global buckets still apply. Its
label is "Class A node capacity, load generator cohosted on the node socket":
relative tracking of durable admission, not a production-capacity claim. Build
the corpus first with `stress-corpus-generate` (see the
[benchmark reference](../../../.agents/skills/midgard-e2e-acceptance/references/benchmark.md#bounded-e2e-stress)).

```sh
cd demo/midgard-node
node ../midgard-node-tools/dist/index.js e2e-stress-l2-throughput \
  --route node --endpoint http://localhost \
  --node-socket <runDirectory>/services/http-socket/node.sock \
  --load-model open-loop-upper-bound --tx-corpus <tx-corpus.ndjson> \
  --corpus-slice-id <slice> --target-rate-tps <rate> \
  --open-loop-max-in-flight <count> \
  --out-dir <directory beside runDirectory> --json
```

`--endpoint` only names the socket; the node serves plain HTTP there. The
socket's directory is mode 0700, so run as the stack's user. A spread above
512 synthetic clients is refused when the engine starts.

The generated node environment explicitly sets `MIN_QUEUE_LENGTH_FOR_MERGING=1`
for these small Preprod journeys. This uses the existing automatic merge worker
and leaves the operator's general configuration unchanged. Each cycle waits for automatic deposit absorption, transfers value to the
recipient, retrieves the committed payload through public libp2p DA, waits for
automatic merge/finality, submits the recipient's withdrawal and observes the
node's automatic payment. Checks compare exact L2 balance changes including
fees and the exact L1 payout destination and assets, using an independently
admitted local Cardano observation. An already-spent payout remains verifiable
through its historical transaction. A second cycle checks that activity
continues after the first payment. No merge or payout repair command is used.

Private evidence lives in `attempts/`, `stack-journal.json`, per-cycle receipt
files and `setup-summary.json`/`journey-summary.json`. The journey summary proves
this functional scope; it is not the repository's full fault-proof, rollback
and release-readiness acceptance suite. The finalizer, `e2e-finalize-summary`
with `--stack-config`, re-derives this evidence from the run's records, node and
database into the release-readiness dashboard, where the state-correction gates
the stack does not produce stay blocked as not run; see
[release-readiness.md](../../../.agents/skills/midgard-e2e-acceptance/references/release-readiness.md).

Focused recovery checks:

```sh
MIDGARD_SKIP_DB_TESTS=1 pnpm --dir demo/midgard-node-tools exec vitest run tests/full-stack
```

Build the Preprod blueprint first (`pnpm --dir demo deployment:build preprod-testing`).
Without `MIDGARD_SKIP_DB_TESTS=1`, the run also executes the storage queries
against a migrated test Postgres. These tests cover unrecorded confirmations,
ambiguous submissions, durable intents, configuration drift, corruption, exact
balances and payouts, transfer resends and service readiness. The lock tests
drive the controller and command locks with real processes, including a holder
killed with SIGKILL. They do not replace the live wallet journey against funded
Preprod wallets.
