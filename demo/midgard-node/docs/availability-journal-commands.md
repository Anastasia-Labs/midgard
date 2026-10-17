# Availability journal inventory

After building the declared core and node packages, inspect an existing durable
journal without a manifest, actor mnemonic or chain provider:

```sh
node dist/index.js availability-journal holds \
  --journal /var/lib/midgard/availability/actor.sqlite
```

The path must be an existing canonical absolute regular file. The command opens
SQLite read-only and holds one ordinary read transaction across all JSON-lines
pages. This includes committed live WAL contents at capture. A concurrent writer
may continue while the captured inventory stays unchanged. SQLite may need access
to WAL sidecars; read-only access failure is refused without a writable fallback.
The command does not create directories, migrate schema, acquire/release actor
leases or change stored rows. Preserve the journal and its WAL together.

Schema 3 inspection includes metadata presence, every lease and retained intent
across actors, deployments and states, every resource/dependency edge, and every
workflow including retired shadows and stored foreign release evidence. Missing
intent references and mismatched owners/scopes are visible. A dependency without
a local parent is `external_or_unresolved`; this can be an ordinary external
input. Expired lease timestamps and released marker rows describe stored facts;
they do not prove an old unsigned callback drained. Metadata values and free-text
intent diagnostics are omitted. Signed CBOR is never emitted.

Only a footer with `complete:true` and exit 0 means the stored-row inventory
completed. Holds do not cause failure. `complete:false` and exit 1 mean refused,
interrupted or budget-limited inspection; earlier pages are partial. Schemas 1,
2, unknown schemas and incomplete/malformed layouts are refused. Historical
schemas cannot prove full liability coverage from current migration fixtures.
A new invocation captures a new snapshot; do not combine its pages with an old
partial inventory and call the result complete.

Fixed ceilings are 64 rows per page, 100,000 total rows, 16 MiB output including a
reserved footer, 1,024 UTF-8 bytes per projected identifier, 256 KiB per stored
intent/release JSON, and 60 seconds of snapshot lifetime. The JSON ceiling is an
operator memory bound, not a protocol transaction limit: larger retained records
are preserved and produce incomplete inspection. The core API allows reducing
these limits. Text byte lengths are checked by SQL before materialization and JSON
decoding. A synchronous SQLite/filesystem call is not cancellable; lifetime is
checked before and after queries. A lifetime timer also closes the snapshot while
awaiting output. A first SIGINT/SIGTERM, stdout failure or output deadline closes
the reader synchronously, removes command listeners and exits this CLI process
with status 1. OS teardown releases pending output; Node deliberately prevents
ordinary `process.stdout.destroy()` from closing its descriptor. No footer is
retried on a stalled/failed pipe. A stable incomplete code is attempted on stderr;
its delivery also depends on the consumer. Absence of a complete footer never
proves success. Synchronous terminal/file writes are not cancellable while running.
The handle closes on completion/refusal.

This inventory has `authority:stored_unreobserved`. It does not authenticate
canonical chain evidence or authorize release, clearing, pruning, repair,
quarantine recovery, re-signing or submission. Known actor/deployment intent
reconciliation remains `availability-challenge status/recover` with a verified
manifest and dedicated actor; `recover` can resubmit identical signed bytes.
Lease-only, orphan, unknown historical and unsafe-size residues require preserving
the database and establishing owner/evidence provenance. No force-release or
repair command is supplied. Standalone authenticated foreign-release reobservation,
protected-floor recovery, prior-effect reconciliation, historical conflicted-byte
repair and committee exclusive journal proof remain separate unsupported surfaces.
