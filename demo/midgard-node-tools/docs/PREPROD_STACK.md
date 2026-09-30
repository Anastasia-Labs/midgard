# Persistent Preprod stack

From the repository root, one command builds the tooling and sets up or resumes
Cardano Preprod with local Docker providers, the node, DA committee, public
retained DA, watcher and trusted-head authority, then runs wallet journeys:

```sh
pnpm --dir demo/midgard-node-tools e2e-stack --config /absolute/path/preprod-stack.json
```

Use Node 22.16 or newer, the repository's pnpm, its pinned Aiken compiler, Docker
Compose, Go (setup selects 1.25.7), Rust/Cargo and Linux `flock`. Install the workspace dependencies first. Setup builds
the Preprod contract profile and the workspace runtimes. Provider startup may
need several hours to download and synchronize the Preprod snapshots.

Copy `config/preprod-stack.example.json` and replace its absolute paths. This
file contains names of secret environment variables, never their values. It
uses the existing operator stack's `.env`, provider data, deployment directory
and Docker volumes. Set `NETWORK=Preprod`, `MIDGARD_DEPLOYMENT_PROFILE=preprod-testing`, `L1_PROVIDER=Kupmios`, loopback Kupo
and Ogmios URLs, no failover, `RUN_GENESIS_ON_STARTUP=false`, and the exact L2
`MIN_FEE_A`/`MIN_FEE_B`. Keep the operator deployment profile settings from
`demo/midgard-node/.env.example`. Define all wallet and DA secret variables named
by the stack configuration in that `.env`. Each wallet role must be distinct.
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
your contract publication costs and measured funding requirements. The prover and availability addresses use the existing watcher Enterprise
address derivation; the other wallet roles use the node's normal seed addresses.
Each wallet also needs a plain ADA output of at least 5 ADA for fees/collateral. The user
budget must cover every deposit plus fee headroom. On attachment, funding
checks require working capital rather than the original deployment budget.

The Compose wrapper derives operator host ports in linked worktrees. Configure
the node endpoint and local provider URLs to match those ports (`scripts/operator-compose.sh
--print-env --env-file .env`). DA ports are explicit and must not collide with
another stack. Host operational commands use loopback Postgres at the operator
stack's host port; containers use `postgres:5432`.
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
files must hold the seeds of the corresponding funded wallets. The watcher
retains its own SQLite stores and authority records in named volumes.

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

`--check` validates paths, environment, budgets and local endpoint bindings
without starting services or spending funds. `--setup-only` completes setup
and readiness checks without the wallet journeys; Compose keeps the services
running after the command exits. Run the same command again to attach or
resume. `docker compose restart` on the configured project preserves the
running services' generated settings. The generated override is saved in
`<runDirectory>/services/compose.json`; use the operator Compose wrapper with
that override when managing this stack.

The file journal is written atomically and fsynced, with a stable run identity.
Native nonce/reference/deposit/withdrawal journals preserve submitted intents.
Before repeating a transaction step, setup queries Cardano. It can reconstruct
a deployment manifest when initialization confirmed before the success record
was written. An ambiguous submission stops without constructing a new one.
Transfer bytes are saved before their first send and reused after a lost
response. A controller lock prevents two runs against one node directory; a
separate command lock remains held while an orphaned child command finishes.
The deployment and local storage identities are checked on restart. Corrupt
records, changed configuration or mismatched storage stop without resetting
anything. No command uses volume deletion, database wipes or fresh-redeploy
flags. Preserve the run directory, signing keys, `.env` and service volumes.

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
pnpm --dir demo/midgard-node-tools exec vitest run --config vitest.full-stack.config.mjs
```

These tests cover unrecorded confirmations, ambiguous submissions, durable
intents, configuration drift, corruption, exact payouts and service readiness.
They also exercise the actual kernel lock through process death. They do not
replace the live wallet journey against funded Preprod wallets.
