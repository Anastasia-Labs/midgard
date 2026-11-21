# Focused test evidence

## Sub-features

- Prepare the selected package and run the named test file.
- Refuse zero collected tests and unavailable prerequisites.
- Preserve and verify execution evidence after the process exits.

## How to get to it (user POV)

Use `node scripts/contrib.mjs test --package midgard-node --file tests/validation-worker-pool.test.ts`.
Read the [suite procedure](../references/running-suites.md) before executing.

## Driving it with contrib

Run the environment doctor, inspect preparation, then execute the selected
file through the command above. Read the returned receipt path and preserve
its directory. Verify it with
`node scripts/contrib.mjs receipts verify --input <receipt-path>`.
Record the collected test count and actual exit code. [review]

The proof includes the selected file, assertion results, prerequisite identity,
and receipt verification. A missing capability is an incomplete result. A
deliberately unmatched `--name` must fail collection, not supply a pass. [review]

## Gotchas

This feature can build artifacts and use test databases. Resource ownership,
database naming and cleanup belong to the guarded runner. Its receipt is not
live-chain, rollback or fault-proof acceptance evidence.
