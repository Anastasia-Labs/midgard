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

The registry is executable: every literal file exists and every wildcard
collects a nonempty set. Deployment fit runs the real signed runtime roster,
persisted deployment/binder checks and family publication suites. Its focused
runner captures the existing CG1 and published-workflow measurement outputs
inside the run directory. Recovery uses production workflow, storage and
funding suites, including the shared descendant/operator cleanup cases.
Boundary gates include actual compiled workers; lifecycle gates exercise the
existing process/network consumers. Policy gates retain ordinary, adopted and
restart coverage in the owning suites. Input-envelope gates run the owning
maximum/first-fault/polarity and publication cases. These are synthetic/emulator
engineering gates; each suite's assertions determine its coverage.

Shared test support exports `chain-fixture`, `witness-fixture`,
`recovery-scenarios` and `database-identity`. Read those exports before writing
handmade chain points, serialization fixtures or cleanup identities.
`ChainFixture` separates slot, height, ancestry, provider observation,
confirmation count and recovery distance. `witnessFixture` accepts a typed
value and its production encoder, validates encoding before the assertion and
labels deliberate byte mutations. `RecoveryScenario` provides generation and
resource-retention assertions; its adapter supplies authenticated finality.
It is a reference fixture, not the production finality policy.

Node database cleanup uses the migration table inventory, restores migration
seed rows, attests the exact disposable shard and refuses uncatalogued public
tables. Test names are validated before identifier interpolation.
Reset only this invocation's disposable database. [runtime: resetApplicationTables]

For large witnesses, use `measure --file FILE --file FILE --maximum-bytes N`.
It hashes streams, compares exact serialized identity, reports total/unique
retained bytes and refuses a file over the stated bound. SHA-256 equality is
local dedup identity; protocol authentication, trace coordinates, execution
units and L1 carriage are established by the owning envelope/publication gates.
