# Optional committee promise capacity profile

Ordinary never-enrolled operation uses the existing verified payload, exact
commitment, canonical replay, signer membership and financial reconciliation
checks. PostgreSQL is the only store backend. Absence of the optional capacity
configuration grants no numerical response-time claim.

The eight `DA_PROMISE_*` artifact path and SHA256 settings explicitly select
controlled JSON admission. Partial selection, invalid artifacts, missing
capabilities, expired policy, unknown evidence and exhausted capacity refuse new
signatures. The initialized retirement singleton is the durable enrollment
witness: removing the settings on restart cannot restore ordinary signing.
Existing signature witnesses and retained bytes keep their existing recovery
and serving paths.

An explicitly requested profile first waits until the committee's L1 follower
reports no reason that holds decisions (an owed wallet seed holds only
submission), then runs one tick on the same service with signer,
coordinator and financial reconciler capabilities withheld. That tick must read
a view from the follower's facts and finish with no error. A follower that is
catching up, waiting for its node or held by an intervention, and a tick that
reads no view or records an error, cannot authorize enrollment. The factory then
binds the service's actual operational pins, initializes the retirement
singleton outside discovery, and enables admission before external loops or
fresh signing begin.

The controlled profile binds measured artifacts to the actual runtime build,
locked dependencies, manifest, genesis, current protocol parameters, the L1
source (the follower's facts and local state query), actor and store backend.
Successful durations and finite chain/fault behavior remain explicit conditional
assumptions. Refusal deadlines fence results;
filesystem and library callbacks are directly awaited and joined. A late
callback is a failed bound, whose owner remains held until completion.

The 72 protected-liability ceiling is independent of the 512-record and 8 MiB
post-mutation ceilings. All limits must hold together; the profile does not
promise that 72 arbitrary coordinator cohorts fit. Prospective signature,
outbox, source and singleton growth are charged before admission. A protected
promise stops consuming capacity once its cutoff or terminal point is observed
on the selected chain; a rollback of that observation charges it again until
the cutoff is observed on the new chain. Certification once that observation is
final (more than 2160 blocks deep) is store-retirement bookkeeping only and
never holds new signing. The signed 15-day retention window is preserved.

The current scheduling limit can constrain signing cadence while a prior
header remains challengeable. This optional committee-signature profile does
not reserve producer capacity before L1 commitment, change ordinary producer
cadence, establish a public SLA, or prove the literal combined B1 workload fits.
Numerical activation requires independent final-source calibration and review.
The profile stays off by default, and its limits must be re-derived against the
current response windows before it is enabled.
