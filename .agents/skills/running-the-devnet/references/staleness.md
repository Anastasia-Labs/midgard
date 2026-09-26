# What makes a running deployment stale

Verified against the tree as of 2026-09-25. A deployment's identity is its
finalized contract deployment manifest (`contract-deployment-info.json`). The
node, SDK and watcher check parts of that identity against the code they were
built from. Knowing which parts are checked, and which are not, tells you
whether a change needs a relaunch or a redeploy.

## Where the node gets its contracts

`makeMidgardContractRuntime` in
`demo/midgard-node/src/services/midgard-contracts.ts` picks one source:

- **Manifest**: when `MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH` is set, every
  script comes from the manifest's recorded CBOR. The log says
  `Contract source selected: deployment-manifest`. A manifest that does not
  match the config or run state is refused with
  `Configured deployment manifest cannot be used as contract source`.
- **Derived**: otherwise the scripts are built from `onchain/aiken/plutus.json`
  and the configured hub-oracle one-shot. The log says
  `Contract source selected: state_queue=real, …`.

## Consensus profile

`MIDGARD_CONSENSUS_PROFILE` in `demo/midgard-core/src/consensus-profile.ts`
lists the protocol's encodings and schema versions, for example
`forcedTransactionSourceEncoding: "midgard-forced-submission-v1"`. Its
canonical JSON is hashed into `MIDGARD_CONSENSUS_PROFILE_DIGEST`.

- Every finalized manifest records `consensusProfile` and
  `consensusProfileDigest`. Parsing refuses one that differs from the compiled
  profile: "Deployment manifest consensusProfileDigest must exactly match
  canonical V1" [runtime: `parseDeploymentManifest` in
  `demo/midgard-core/src/deployment-manifest-identity.ts`; the same check in
  `demo/midgard-node/src/deployment-manifest.ts`].
- `lucid-midgard` (builder, provider payload), `midgard-validation`
  (phase A, event replay) and the watcher rule bundle also reject a foreign
  profile [runtime: `isMidgardConsensusProfile`].

So **any** field change, including a single encoding name, orphans every
existing deployment. Redeploy.

## Deployment profile

`config/deployments/<name>.yaml` sets network, finality depth, timings, limits
and economics. `pnpm --dir demo deployment:build <name>` validates it,
generates `demo/midgard-core/src/generated-deployment-profiles.ts` and the Aiken
environments, compiles with `aiken build --env <name with underscores>`, and
writes `onchain/aiken/plutus.json.deployment.json` with the profile, its
SHA-256 digest and the blueprint hash [script: `demo/scripts/deployment-profiles.mjs`].
It refuses a compiler other than the one pinned in `.github/workflows/aiken-ci.yml`.

- `MIDGARD_DEPLOYMENT_PROFILE` must name the compiled profile
  [runtime: `requireSelectedDeploymentProfile`].
- Manifests carry `deploymentProfile` and `deploymentProfileDigest`; a
  mismatch with the compiled profile is refused
  [runtime: `verifyDeploymentProfileBinding` in
  `demo/midgard-core/src/deployment-profile.ts`].
- The profile's constants are compiled into the validators, so the script
  hashes change too. The deployments README says to deploy fresh and not
  attach changed constants to an existing deployment
  ([`config/deployments/README.md`](../../../../config/deployments/README.md)).
- Generated files that drift from the YAML fail `deployment:check`
  [ci: Aiken CI/Check deployment profiles], which CI runs for
  `preprod-testing` only.

The local devnet uses `local-devnet-testing` with `NETWORK=Custom`. The
checkout's generated selection is `preprod-testing`, so building for the local
devnet means `pnpm --dir demo deployment:build local-devnet-testing` and a
rebuild of the TypeScript workspace (deployments README).

## Blueprint

- The node refuses to load a `plutus.json` whose SHA-256 differs from the
  `blueprintHash` in its adjacent build record: "Blueprint does not match its
  deployment profile build record" [runtime: `verifyBlueprintDeploymentProfile`
  in `demo/midgard-node/src/services/midgard-contracts.ts`]. A raw
  `aiken build` therefore makes the node refuse to start until
  `deployment:build` is rerun.
- **Blind spot.** The startup manifest check
  (`verifyDeploymentManifestAgainstConfig` in
  `demo/midgard-node/src/commands/contract-deployment-info.ts`) compares only
  network, reference-script address, hub-oracle one-shot and economics
  profile. Nothing at `listen` compares the manifest's `artifacts.blueprintHash`
  with the blueprint on disk. With a manifest as the contract source, the node
  keeps running the scripts the manifest recorded; a changed validator is not
  under test until you redeploy. With derived contracts, a changed validator
  gives new script hashes with nothing deployed at them; with
  `RUN_GENESIS_ON_STARTUP=false` startup logs "Skipping protocol initialization
  on startup" and carries on (`listen-startup.ts`). `[review]`
- The watcher-journeys record states the same rule from experience: a source
  change "cannot change the validators already deployed", and the remaining
  families needed "a fresh deployment with the new applied scripts"
  ([`docs/fault-proofs/automatic-watcher-journeys.md`](../../../../docs/fault-proofs/automatic-watcher-journeys.md)).

## Database schema

`listen` checks the schema before anything else and refuses any version other
than the one the binary expects: "Database schema is not compatible: …"
[runtime: `MigrationRunner.assertCompatible` via
`demo/midgard-node/src/database/init.ts`]. Apply migrations with
`node dist/index.js db:migrate` (or the compose `midgard-node-migrate`
service); this is a relaunch, not a redeploy.

## Phase4 runs

Phase4 bootstrap compiles with `aiken build --env testnet`
(`phase4-process/scripts/protocol-bootstrap.sh`), not `deployment:build`.
`reset.sh` recomputes and compares, before stopping anything, the `src` and
`dist` trees of `midgard-node` and `midgard-node-tools`, the run's genesis and
config, `acceptance.env`, the compose file, the phase4 assets directory, the
four image IDs and the PHAS registration proof, and refuses any drift
[script: `phase4-process/scripts/reset.sh`].

**Blind spot.** The snapshot identity records `blueprintSha256`, but `reset.sh`
checks only that it is 64 hex characters; it never recomputes it. Validators
live under `onchain/aiken`, outside every tree it hashes, so a validator-only
change passes reset and the run keeps exercising the scripts deployed at
bootstrap. `[review]`

Any code change after the snapshot, validator or TypeScript, means a new run.

## Deciding in one pass

Ask, in order:

1. Did `consensus-profile.ts` change? Redeploy.
2. Did a `config/deployments/*.yaml` or any validator change? Run
   `deployment:build`, rebuild, redeploy.
3. Did a migration land? `db:migrate`, then relaunch.
4. Otherwise relaunch. For a phase4 run, any code change still means a new run.

When unsure whether a running deployment still matches, run
`node dist/index.js deployment-status` from `demo/midgard-node`: it prints the
manifest check with a `recommendation` (`attach`, `correct_attach_config` or
`fresh_redeploy_required`) and the live protocol status. It reads the chain
through the configured provider, so run it only against a deployment that is
yours.
