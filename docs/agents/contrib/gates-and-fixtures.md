# Reusable behavior gates and fixtures

```sh
node scripts/contrib.mjs gate list
node scripts/contrib.mjs gate deployment-fit --plan
node scripts/contrib.mjs gate recovery-scenarios --seed 42
node scripts/contrib.mjs gate process-boundaries
node scripts/contrib.mjs gate lifecycle
node scripts/contrib.mjs gate runtime-progress
node scripts/contrib.mjs gate policy-matrix
node scripts/contrib.mjs gate input-envelopes
node scripts/contrib.mjs gate database-isolation --seed 17
```

Each gate runs the suites `GATES` in `scripts/contrib/gates.mjs` names for
it. The registry is executable: every literal file exists and every wildcard
collects a nonempty set. These are synthetic/emulator engineering gates; each
suite's assertions determine its coverage.

Shared test support exports `chain-fixture`, `witness-fixture`,
`recovery-scenarios` and `database-identity`. Read those exports before writing
handmade chain points, serialization fixtures or cleanup identities.
`ChainFixture` separates slot, height, ancestry, provider observation,
confirmation count and recovery distance. `witnessFixture` accepts a typed
value and its production encoder, validates encoding before the assertion and
labels deliberate byte mutations. `RecoveryScenario` provides generation and
resource-retention assertions; its adapter supplies authenticated finality.
It is a reference fixture, not the production finality policy.

Node database cleanup uses the schema's table inventory, restores the schema's
seed rows, attests the exact disposable shard and refuses uncatalogued public
tables. Test names are validated before identifier interpolation.
Reset only this invocation's disposable database. [runtime: resetApplicationTables]

For large witnesses, use `measure --file FILE --file FILE --maximum-bytes N`.
It hashes streams, compares exact serialized identity, reports total/unique
retained bytes and refuses a file over the stated bound. SHA-256 equality is
local dedup identity; protocol authentication, trace coordinates, execution
units and L1 carriage are established by the owning envelope/publication gates.
