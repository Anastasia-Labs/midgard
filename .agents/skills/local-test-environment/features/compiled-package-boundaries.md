# Compiled package boundaries

## Sub-features

- Prepare declared compiled dependencies and install the production closure.
- Resolve public package specifiers under ordinary Node ESM and CommonJS conditions.
- Start installed command-line bins with `--help`.

## How to get to it (user POV)

Run `node scripts/contrib.mjs boundary --package midgard-core`.
Read [compiled boundary behavior](../../../../docs/agents/contrib/tests-and-builds.md)
before interpreting its result.

## Driving it with contrib

Run the environment doctor, execute the command and preserve the returned
receipt and child-process logs. Check the advertised export census, completed
import/require records and bin exit codes. Verify the receipt through
`contrib receipts verify --input <receipt-path>`. [review]

When changing package exports, prove the check fails on a disposable fixture
with the relevant broken runtime import before accepting the repaired result.
Exercise the actual declared consumer entrypoint, not source imports. [review]

## Gotchas

The isolated consumer excludes workspace hoisting and source aliases. Nested,
array and default exports use Node's own resolution; source/type-only branches
are recorded separately. Runtime wildcards refuse incomplete coverage. An
export that exits successfully before loading completes cannot earn a pass.

The receipt retains the installation inventory and logs after the temporary
consumer is removed. Import and CLI-help success establish installed entrypoint
loading; they do not establish live startup, recovery or rollup acceptance.
