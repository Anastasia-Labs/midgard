# Shared deployment profiles

Edit the YAML files here, then regenerate and rebuild. A Cardano network is not
a deployment profile: both `preprod-public` and `preprod-testing` use `Preprod`.

| Profile                | Cardano network | Economics schedule |
| ---------------------- | --------------- | ------------------ |
| `mainnet`              | `Mainnet`       | Public             |
| `preprod-public`       | `Preprod`       | Public             |
| `preprod-testing`      | `Preprod`       | Bounded testing    |
| `local-devnet-testing` | `Custom`        | Bounded testing    |

The public profiles retain the existing testnet timing values, including seven-day
block maturity, a five-minute dispute response window, a one-hour operator shift,
and the existing **30 millisecond** registration interval. Mainnet's former
30 millisecond operator shift is replaced by the one-hour shift used off chain.
Testing economics remain explicit; selecting a test network does not select them.
The existing economics schedule identifiers are retained as manifest data, not
runtime selectors. Profiles sharing a schedule identifier must have identical
economics. Settle launch parameter choices before deploying; generation does not
authorize deployment or alter an existing deployment.

### Fast non-interactive testing

`preprod-testing` and `local-devnet-testing` use five-minute block maturity, a ten-minute operator shift,
30-second registration, and the existing eight-minute maximum transaction validity. DA
attestation has a four-minute timeout; full responses have two minutes and small
responses have one minute. Economics remain the bounded testing schedule. Runtime
retention polling defaults to 60 seconds so the shorter L1
freshness deadline works.

`l1_finality.confirmation_depth` selects the L1 confirmation count. Both testing
profiles use 3; `mainnet` and `preprod-public` use 30. The count is included in
the profile digest and finalized manifest, and runtime services must match it.
Three confirmations are a testing policy, not a production security guarantee.

These profiles are for **non-interactive fault-proof testing only**, not interactive
fault proofs or production security. The 32-round interactive schedule retains a
one-minute per-response timeout and cannot fit inside five-minute maturity;
the on-chain maturity guard therefore refuses interactive dispute opening.
Validation explicitly requires this exclusion for both testing profiles; public
profiles still require the complete interactive schedule to fit in half maturity.
Non-interactive proofs must be submitted before maturity. L1 inclusion and node
polling add latency, so finalization is not guaranteed at exactly five minutes.

The local profile targets the real-stack deposit/transfer/withdrawal journey,
two-member DA attestation and public retrieval, automatic merge and reserve payout,
and reconciliation across chain, SQL, cache, and native ledger. It retains the
`Custom` network and bounded economics, with the same protocol timings as Preprod
testing. This profile does not change Cardano slot length or block production;
configure the local chain separately using the verified
Preprod-like consensus and protocol parameters. Faster protocol maturity does not
make a local chain equivalent to an emulator with an advanceable clock.

Before a fresh local deployment, build with
`pnpm --dir demo deployment:build local-devnet-testing`, rebuild the off-chain
packages, and use `MIDGARD_DEPLOYMENT_PROFILE=local-devnet-testing` with
`NETWORK=Custom`. Use the new matching manifest and clean run-specific state; do
not attach the changed constants to an existing deployment. This update retains
the checkout's `preprod-testing` selection; configuring the local profile does not
compile or deploy it. Generation with an explicit profile selects the off-chain
configuration, while `deployment:build` also compiles and binds its blueprint.

## Build

From the repository root, with the compiler pinned in
`.github/workflows/aiken-ci.yml` on `PATH`:

```sh
pnpm --dir demo deployment:build preprod-testing
pnpm --dir demo build
```

The first command validates the selected profile and the other checked-in
profiles, generates the Aiken environments and typed off-chain configuration,
then runs `aiken build --env preprod_testing`. Hyphens in profile filenames become
underscores in Aiken environment names. It records the resolved profile, its
SHA-256 digest, and the blueprint hash in `onchain/aiken/plutus.json.deployment.json`.
It rejects an unpinned compiler and invalidates the previous build record before
compiling. The second command checks generated files and builds the TypeScript
workspace. Rebuild off-chain packages whenever the selected profile changes.

Deploy the blueprint and its adjacent build record together. Node blueprint
loading verifies the record against the generated off-chain profile and blueprint
bytes. The Docker image includes both files. A raw `aiken build` is useful for
contract development but does not produce a deployable profile binding.

For generation and drift checking without compilation:

```sh
pnpm --dir demo deployment:generate preprod-public
pnpm --dir demo deployment:check preprod-public
node --test demo/scripts/deployment-profiles.test.mjs
```

The checkout's generated off-chain selection is `preprod-testing`. Production
builds must explicitly select their deployment. `env/default.ak` is generated
from mainnet; `env/testnet.ak` remains a generated development alias for
preprod-testing. The four named environments are generated too. Do not edit
generated environments or `demo/midgard-core/src/generated-deployment-profiles.ts`.
`env.ak.template` holds only constants that do not vary by deployment.

Validation rejects unknown fields, incorrect network/name combinations, unsafe or
nonpositive integers, inconsistent bond/reward values, invalid DA timing order,
operator shifts shorter than grace/validity windows, and incompatible dispute
schedules (with the explicit non-interactive testing rule above). Digests use SHA-256 over recursively
key-sorted JSON, independent of YAML formatting and key order.

## Runtime and manifests

Replace `MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE` with:

```dotenv
MIDGARD_DEPLOYMENT_PROFILE=preprod-testing
NETWORK=Preprod
```

The runtime selection and network must match the compiled profile. Economics come
from that profile. Existing operator bond and slash settings, if supplied, must
equal the generated values; they cannot override a profile.

Finalized manifests include `deploymentProfile` and `deploymentProfileDigest` in
their authenticated identity, alongside the actual blueprint and script hashes.
Readers reject missing, changed, or mismatched profiles, networks, economics,
and timing. This changes the unlaunched manifest format in place. It does not
migrate or reset deployed state. Validators receive compiled constants and never
read YAML.
