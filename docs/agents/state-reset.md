# State Reset and Redeploy Rules

For any Midgard node environment connected to real or persistent Cardano network
state, never wipe, reset, delete, or recreate local durable state without also
performing a full, explicit on-chain redeploy/reset of the corresponding
protocol state.

This includes:

- `docker compose down -v`
- Deleting or recreating Postgres volumes
- Clearing local ledger/MPT databases
- Deleting migration, mempool, block, deposit, admission, finalization, or
  confirmed-ledger state
- Using any reset script that discards local node state

If local DBs or durable node state are wiped, the on-chain contracts, reference
scripts, scheduler, state queue, hub oracle, operator lists, and any other
protocol UTxOs must be redeployed from a clean genesis/deployment flow before
running deposits, L2 transactions, commitments, merges, benchmarks, or readiness
checks.

Do not combine a clean local database with previously deployed on-chain protocol
state. That creates unauditable split-brain state and can make deposits,
scheduler alignment, commitments, and finalization appear broken for the wrong
reason.

The only exception is a clearly labeled, read-only forensic diagnostic where no
new deposits, L2 transactions, commitments, merges, or state-changing operations
are submitted.
