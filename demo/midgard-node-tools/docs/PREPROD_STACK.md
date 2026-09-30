# Persistent Preprod stack

From the repository root, one command builds the tooling and sets up or resumes
Cardano Preprod with local Docker providers, the node, DA committee, public
retained DA, watcher and trusted-head authority, then runs wallet journeys:

```sh
pnpm --dir demo/midgard-node-tools run e2e-stack --config /absolute/path/preprod-stack.json
```

`--config` must be an absolute path: the package script runs from the package
directory, so a relative path would not resolve against the caller's directory.
An already built tool runs the same command as
`node demo/midgard-node-tools/dist/index.js e2e-stack --config ...`.

Use Node 22.16 or newer, the repository's pnpm, its pinned Aiken compiler, Docker
Compose 2.21 or newer (setup refuses older versions), Go (setup selects
1.25.7), Rust/Cargo and Linux `flock`. Install the workspace dependencies first. Setup builds
the Preprod contract profile and the workspace runtimes. Provider startup may
need several hours to download and synchronize the Preprod snapshots.

Copy `config/preprod-stack.example.json` and replace its absolute paths. This
file contains names of secret environment variables, never their values. It
uses the existing operator stack's `.env`, provider data, deployment directory
and Docker volumes. Set `NETWORK=Preprod`, `MIDGARD_DEPLOYMENT_PROFILE=preprod-testing`, `L1_PROVIDER=Kupmios`, loopback Kupo
and Ogmios URLs, no failover, `RUN_GENESIS_ON_STARTUP=false`, and the exact L2
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
another stack. Host operational commands (`db:migrate`, `submit-deposit`,
`submit-withdrawal`) use loopback Postgres at `MIDGARD_POSTGRES_HOST_PORT`;
containers use `postgres:5432`. Set `MIDGARD_POSTGRES_HOST_PORT` explicitly in
the `.env`; 5433 and 55433 are refused because they belong to test databases.
Before each host database command, setup compares the Postgres cluster
identity seen inside Compose with the one reached at `127.0.0.1` on that port
and refuses to run when they differ.
Setup builds and pins the host native owner, exports the local Cardano config,
and configures the native ledger query helper for host commands and containers.
Node and committee reward-account queries use that same local Cardano socket.

Watcher inputs use the existing watcher process and authority templates. Replace
the process template's illustrative historical script provider endpoints with
the release's operational provider quorum. Those external history providers are
release infrastructure inputs. Setup
replaces the node genesis identity, local provider URLs, retained DA peer and
finality policy with deployment-specific values. `watcher-compose.env` must
name the existing regular secret files with `WATCHER_RECORD_KEY_FILE`,
`WATCHER_ROLLBACK_KEY_FILE`, `WATCHER_PROVER_KEY_FILE`,
`WATCHER_AVAILABILITY_KEY_FILE`, `WATCHER_BEARER_FILE`, and a value for
`MIDGARD_L1_CONFIG_DIR` (replaced with the exported local node config at setup).
Authentication keys are 32-byte lowercase hex; bearer text and wallet secrets
must be canonical, without a trailing newline. The prover and availability
files must hold the seeds of the corresponding funded wallets. The stack's
watcher keeps its SQLite stores and authority records in the project-scoped
volumes `<node project>_watcher-state` and
`<node project>_watcher-authority-records`. It starts its own trusted-head
chain and state, separate from any standalone `midgard-watcher` deployment.
Use a record key file that no standalone watcher uses, or retire the
standalone watcher first.

For an existing deployment, `releaseDirectory` holds the signed watcher release
artifacts named by the process template; set `releaseInput` to `null` when no
signing is required. Fresh deployment authoring requires a JSON file with:

```json
{
  "signingKeyFile": "/absolute/path/persistent-ed25519-private-key.pem",
  "programCommitments": { "<program-name>": "<release-commitment>" },
  "fundingProfiles": [
    "<measured WatcherWorkflowFundingProfileBody per launch category>"
  ]
}
```

The key must already exist. Profiles must be genuine measurements for the
exact blueprint, Cardano parameter snapshot, applied reference scripts,
economics and prover wallet. Setup signs their canonical bundle and the rule
bundle against the confirmed deployment; it never fabricates measurements or
silently replaces an existing release. A profile mismatch stops setup and
preserves all deployed state. Before the authority is signed, replace the
measurements in the same release input file and rerun to resume against that
deployment. The signing key and program commitments remain fixed. Once signed,
the saved authority binds the measured bundle digest and rejects replacements. The example above describes the input shape;
replace its illustrative values with release artifacts before running.

`--check` runs every check that needs no build, network or service, and
starts nothing and spends nothing: the configuration fields and absolute paths,
the node environment (network, profile, Kupmios, failover, genesis, exact fees,
the Postgres host port and the operator and DA port collisions), the presence
of every named secret, wallet mnemonics and derived addresses, the committee
keys and threshold, distinct persistent transport identities, readable watcher
secret files and the release input shape. It loads the built workspace
packages, so build them once first. The Compose version, `compose config`, the
watcher key and bearer decoding and the prover/availability seed match run
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
cosigner, the watcher record, rollback, prover and availability keys, and the
release signer and program commitments (or the existing signed release
artifacts). Changing any of these stops a resume. Timeouts, journey size,
budgets, ports, templates and the watcher bearer may change between runs. When
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
and release-readiness acceptance suite.

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
