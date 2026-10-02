# Shared deployment profiles

Edit the YAML files here, then regenerate and rebuild. A Cardano network is not
a deployment profile: both `preprod-public` and `preprod-testing` use `Preprod`.

| Profile                    | Cardano network | Economics schedule             |
| -------------------------- | --------------- | ------------------------------ |
| `mainnet`                  | `Mainnet`       | Public                         |
| `preprod-public`           | `Preprod`       | Public                         |
| `preprod-testing`          | `Preprod`       | Bounded testing                |
| `local-devnet-testing`     | `Custom`        | Bounded testing                |
| `preprod-emulator-testing` | `Preprod`       | Bounded testing; emulator only |

The public profiles retain the existing testnet timing values, including seven-day
block maturity, a five-minute dispute response window, a one-hour operator shift,
and the existing **30 millisecond** registration interval. Mainnet's former
30 millisecond operator shift is replaced by the one-hour shift used off chain.
Testing economics remain explicit; selecting a test network does not select them.

Every profile allows twenty minutes without a block commitment, and twenty
minutes for a deposit, withdrawal or transaction order to be included, before an
operator can be struck for inactivity. Validation requires the negligence timeout
to be at least the commitment gap, so a neglected event never licenses an earlier
strike than the gap, and the gap to be shorter than the operator shift, so an
operator whose shift starts at the last block's end time (its commit's validity
upper bound) can still be struck.

`timing.event_wait_ms` is the delay between a user event's transaction
valid-to and its on-chain `inclusion_time`; no block may include the event
earlier. The public profiles use the worst-case wall time for N Cardano blocks,
3N/f slots at f = 0.05 and one-second slots: 36 hours on `mainnet` (N = k = 2160)
and 30 minutes on `preprod-public` (N = 30). Both live testing profiles use a fixed
five minutes.
Validation requires each public profile's event wait to be at least the maximum
validity range plus confirmation-depth blocks at twice the 20-second mean block
time: 28 minutes for `mainnet` and `preprod-public`. Every event a block must
include was on L1 at least that validity range less than the event wait before
the commit became valid, so an honest block omits one only if L1 rolls back past
that span. Under the 3N/f standard above, `preprod-public`'s 30 minutes leave 22
blocks after the eight-minute validity range, not 30. The live testing profiles are
exempt: a short L1 fork there can make an honest block omit an event.
The existing economics schedule identifiers are retained as manifest data, not
runtime selectors. Profiles sharing a schedule identifier must have identical
economics. Settle launch parameter choices before deploying; generation does not
authorize deployment or alter an existing deployment.

### Interactive emulator tests

`preprod-emulator-testing` is selected only by the interactive Vitest projects
in `midgard-fault-proofs` and `midgard-watcher`. It uses four-hour block maturity,
one-minute dispute responses and bounded testing economics. Its event wait,
DA challenge window and bond withdrawal delay satisfy the full profile checks.
Emulator tests advance the ledger clock directly; four hours of protocol time
adds no four-hour wall-clock wait.

The test setup verifies generated profiles and caches a separately stamped
blueprint at `onchain/aiken/build/interactive-emulator/plutus.json`. Its Vite
plugin selects the matching TypeScript profile only within that test project's
module graph. The checked-in selected profile and default `plutus.json` remain
`preprod-testing`; live testing and devnet timings stay unchanged. New interactive
journey files belong in `interactiveTests` in the fault-proof Vitest config.

### Live testing profiles

`preprod-testing` and `local-devnet-testing` use fifteen-minute block maturity,
a thirty-minute operator shift, 30-second registration, and the existing eight-minute
maximum transaction validity. DA attestation has a ten-minute timeout; full
responses have fourteen minutes and small responses have twelve minutes. A
response window opens at the challenge open's inclusive upper validity bound,
so a backdated open cannot shorten it. Validation requires every profile's
windows to cover a minimum response budget: the confirmation depth, the five
chained publications of a 64 KiB payload and one poll block, at twice the
twenty-second mean block time (ten minutes forty seconds for the testing
profiles, 24 minutes for the public ones). The full budget also counts every
bounded wait on the way to an answer;
`demo/da-committee-node/tests/availability-response-budget.test.ts` adds them up
for both testing profiles and both public profiles. The fourteen-minute full
window still holds only about 38 chained publications per tranche at
twenty-second blocks, roughly 530 KB, far short of the 300 publications of a
whole 4 MiB tranche. On these profiles a
challenge against a larger payload cannot be answered in full. Attestation waits
for ten descendant blocks before signing and then lands three
confirmation-serialized transactions, about four and a third minutes on average
at twenty-second blocks (520 seconds at twice the mean, inside the ten-minute
timeout); a single dropped transaction alone costs 160 seconds, which takes that
doubled estimate past the timeout. Economics remain the bounded
testing schedule. The node derives its retention defaults from the DA timeout:
the retention poll runs every quarter timeout (150 seconds) and the L1 view
deadline, `L1_VIEW_FATAL_MS`, is the timeout itself (ten minutes).

`l1_finality.confirmation_depth` selects the L1 confirmation count. The two live
testing profiles use 10 and `preprod-emulator-testing` uses 3; `mainnet` and
`preprod-public` use 30. The count is included in the profile digest and
finalized manifest, and runtime services must match it. The testing counts are a
testing policy, not a production security guarantee.

Fault proofs are **accepted as non-functional** on the two live testing
profiles (owner ruling, 2026-09-27), and neither profile provides production
security. Fifteen-minute maturity is shorter than the consensus profile's
`minValidationDisputeMaturityMs` (7,920,000 ms, two hours twelve minutes) and
than the transition-trace proof path. The 32-round interactive schedule retains a
one-minute per-response timeout and cannot fit inside fifteen-minute maturity;
the on-chain maturity guard therefore refuses interactive dispute opening.
Validation explicitly requires this exclusion for both live testing profiles; public
profiles still require the complete interactive schedule to fit in half maturity.
Automated fault-proof coverage runs in the emulator on `preprod-emulator-testing`
(see [Interactive emulator tests](#interactive-emulator-tests)). L1 inclusion and
node polling add latency, so finalization is not guaranteed at exactly fifteen
minutes.

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

### Pooled DA committee bond

The committee backs every block it attests from one pooled bond. Each profile's
`da_bond` section holds its amounts, and three `timing` keys hold its windows:

| Key                                   | Public profiles         | Live testing profiles |
| ------------------------------------- | ----------------------- | --------------------- |
| `da_bond.da_bond_lovelace`            | 100,000 ADA             | 500 tADA              |
| `da_bond.da_slash_penalty_lovelace`   | 25,000 ADA              | 100 tADA              |
| `da_bond.da_bond_min_top_up_lovelace` | 1,000 ADA               | 5 tADA                |
| `da_bond.da_bond_pool_floor_lovelace` | 5 ADA                   | 5 tADA                |
| `da_bond.challenge_record_lovelace`   | 27 ADA                  | 27 tADA               |
| `timing.da_challenge_window_ms`       | 259,200,000 (3 d)       | 720,000 (12 min)      |
| `timing.da_slash_grace_ms`            | 172,800,000 (2 d)       | 300,000 (5 min)       |
| `timing.da_bond_withdraw_delay_ms`    | 778,080,000 (9 d 8 min) | 2,340,000 (39 min)    |

A slash burns the penalty as fee and pays the rest of one DA bond to the
challenger. The pool floor stays in the pool UTxO for its minimum ADA and never
counts as backing. `challenge_record_lovelace` is the exact lovelace of a
challenge record, the minimum ADA of the largest admitted record; the YAML
comment and the [economics record](../../docs/midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md)
§2.6 carry its measurement. An availability challenge must open before the
block's end time plus the challenge window. The withdrawal delay separates the
two steps of a committee withdrawal, and the timeout slash has the slash grace
to land. The public figures await owner confirmation.

Validation requires:

- `da_attestation_timeout_ms < da_challenge_window_ms ≤ block_maturity_ms`, so a
  block attested at the last moment can still be challenged and a challenge and
  a merge of the same block are never both valid;
- `0 < da_slash_penalty_lovelace < da_bond_lovelace`, and a positive minimum
  top-up, pool floor and challenge-record lovelace;
- a withdrawal delay of at least the maximum validity range plus the later of
  the challenge response deadline (challenge window plus full response window)
  and block maturity, plus the slash grace. An unavailable block is removed
  only as the queue head, after its predecessors merge at maturity, so this is
  stronger than the challenge path alone;
- on public and interactive emulator profiles, a challenge window that exceeds the attestation
  timeout by at least two maximum validity ranges plus the confirmation-depth
  budget (`confirmation_depth` blocks at twice the 20-second mean), 2,160,000 ms
  on public timing. A challenger must first see an Apply that landed at the
  attestation timeout at confirmation depth, then land an open whose validity
  range may be a full maximum range wide. The live testing profiles have two minutes
  of slack and are exempt;
- on public and interactive emulator profiles, the challenge window, the maximum validity range,
  the full response window and the dispute schedule fit before block maturity,
  so fraud stays provable after the latest DA response. This rule used to count
  from the attestation timeout.

The generated environments carry these values as `da_bond_lovelace_v1`,
`da_slash_penalty_lovelace_v1`, `da_bond_min_top_up_lovelace_v1`,
`da_bond_pool_floor_lovelace_v1`, `challenge_record_lovelace_v1`,
`da_challenge_window_ms_v1`, `da_slash_grace_ms_v1` and
`da_bond_withdraw_delay_ms_v1`; `onchain/aiken/lib/midgard/da-bond-profile.test.ak`
checks the relations against each environment, and Aiken CI runs it under every
profile. The availability `ParametersV1` amounts in a finalized manifest must
equal the selected profile's `da_bond` section, so the node takes them from the
profile rather than from an environment variable. The challenger bond stays a
deploy-time manifest value (`MIDGARD_DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE`);
it no longer has to equal the DA bond, and it alone must cover the maximum
publication, settlement and terminal fees.

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
preprod-testing. The five named environments are generated too. Do not edit
generated environments or `demo/midgard-core/src/generated-deployment-profiles.ts`.
`env.ak.template` holds only constants that do not vary by deployment.

Validation rejects unknown fields, incorrect network/name combinations, unsafe or
nonpositive integers, inconsistent bond/reward values, invalid DA timing order,
operator shifts shorter than grace/validity windows or the commitment gap, incompatible dispute
schedules (with the explicit non-interactive testing rule above), and pooled DA
bond values that break the relations above (challenge window order, slash
penalty inside the DA bond, withdrawal delay, and the public challenge-window
margin). Digests use SHA-256 over recursively
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
