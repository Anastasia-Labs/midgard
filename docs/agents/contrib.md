# Deterministic contributor workflows

Use `node scripts/contrib.mjs --help` before assembling a focused test, build,
artifact regeneration, integration packet, program handoff or diagnostic query.
The commands route to the existing owners. Their receipts record execution;
they do not decide whether an assertion proves a protocol property. [review]

Successful commands print compact results and retain bounded logs. Set
`MIDGARD_CONTRIB_VERBOSE=1` to stream child output while diagnosing a failure.

## Focused tests and builds

Read [focused tests and builds](contrib/tests-and-builds.md) for the commands, ownership and limits.

## Lane worktrees

Read [lane worktrees](contrib/worktrees.md) to create, set up and remove a lane's checkout.

## Evidence and ownership

Read [evidence and ownership](contrib/evidence-and-resources.md) for the commands, ownership and limits.

## Reusable behavior gates and fixtures

Read [reusable behavior gates and fixtures](contrib/gates-and-fixtures.md) for the commands, ownership and limits.

## Artifact regeneration and clean reproducibility

Read [artifact regeneration and clean reproducibility](contrib/artifacts-and-reproducibility.md) for the commands, ownership and limits.

## Integration and program resumes

Read [integration and program resumes](contrib/integration-and-programs.md) for the commands, ownership and limits.

## Devnets, acceptance and read-only diagnostics

Read [devnets, acceptance and read-only diagnostics](contrib/devnets-and-diagnostics.md) for the commands, ownership and limits.
