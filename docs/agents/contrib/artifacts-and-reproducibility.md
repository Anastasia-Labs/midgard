# Artifact regeneration and clean reproducibility

```sh
node scripts/contrib.mjs artifacts list
node scripts/contrib.mjs artifacts check --channel native-tx-vector-v1
node scripts/contrib.mjs artifacts sync --channel native-tx-vector-v1
node scripts/contrib.mjs artifacts builds
node scripts/contrib.mjs reproduce --plan
node scripts/contrib.mjs reproduce --execute
```

Select channels through the existing affected-channels tool before running
these commands. The shared channel registry remains authoritative. [review] A channel
without an executable check is refused, with its recorded reason. Historical
measurements do not become release gates simply by being listed.

Checks prepare compiled generator prerequisites. Sync creates a disposable
checkout, overlays source inputs, performs a frozen install, uses the existing
blueprint synchronization guard, runs the declared generator and check, then
publishes only declared output paths through a verified packet. Dirty output
destinations are refused. The retained receipt records input/output ownership.

Scratch preparation must match the destination's complete package input
identity before compilation. A required ignored source that Git cannot overlay
is refused instead of earning a pass from a different input. Commit or make
that input reproducible through its declared owner before retrying.

The reproducibility lane resolves pinned Git commits from their public remotes,
performs a frozen install and selected blueprint/workspace builds in a fresh
checkout, then builds both native owners and checks every executable artifact
channel. Manual/historical channels remain explicit inventory gaps. Absolute
private tarball/link dependencies and unpinned Git branches
are refused. Local caches are allowed for speed; remote pin resolution is
performed independently. A failed or unavailable compiler is a failure, not
evidence of a successful clean build.

`node scripts/contrib/review-controls.mjs` checks the fixed boundary controls
and reintroduces each reviewed artifact/path defect in disposable copies. CI
and preflight require the corresponding regression to fail at its named guard.
