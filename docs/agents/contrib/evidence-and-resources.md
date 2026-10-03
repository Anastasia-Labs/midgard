# Evidence and ownership

Each execution writes owner-only logs and a receipt beneath the temporary
resource registry printed by the command. The terminal summary omits the
thousands of per-file digest entries; `--output FILE` saves the complete result.
Receipts carry argv, cwd, times, duration, exit/signal, input identities, log
hashes, actual assertion counts, filters, seed and database family. Preflight
attaches its step receipt paths to its existing check ledger.

```sh
node scripts/contrib.mjs receipts verify --input /absolute/run/receipt.json
node scripts/contrib.mjs receipts render --input /absolute/run/receipt.json
node scripts/contrib.mjs resources list
node scripts/contrib.mjs resources reclaim --resource 'workspace:/absolute/checkout'
node scripts/contrib.mjs workspace inspect
```

Receipt validation refuses incomplete results, changed inputs, changed logs or
reports, inconsistent counts and version/help probes reclassified as builds.
Preserve the entire run directory when retaining evidence: a receipt whose log
has disappeared cannot prove execution. [script: scripts/contrib/receipts.mjs]

Resource observation lists process start, boot and PID namespace identities.
Launches are recorded before spawning; surviving managed child groups keep an
owner's lease occupied even after its parent dies. Reclaim is explicit and
requires a fully recorded abandoned owner with no surviving managed groups;
it sends no signal, drops no database and deletes no deployment. Another PID
namespace, a legacy record or an unfinished launch remains unknown and cannot
be reclaimed. Child output and elapsed time are bounded; cancellation joins
the invocation's process groups before releasing its lease. Killed orphan
processes can remain zombies until the host reaps them.
