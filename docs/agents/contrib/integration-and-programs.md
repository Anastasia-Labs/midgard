# Integration and program resumes

```sh
node scripts/contrib.mjs workspace inspect --output workspace.json
node scripts/contrib.mjs packet create --base COMMIT --file path/to/file.ts --output packet.json
node scripts/contrib.mjs packet verify --input packet.json
node scripts/contrib.mjs packet apply --input packet.json
node scripts/contrib.mjs program validate --input program.json
node scripts/contrib.mjs program render --input program.json
node scripts/contrib.mjs locate --query worker
node scripts/contrib.mjs locate --symbol runCommitBlockHeaderWorkerProgram
```

Workspace inspection reports missing, inaccessible or vanished worktrees as
`inspection: unavailable`, with unknown changes and `complete: false`; it
never prunes their registrations. Default output gives totals, the current
checkout's change count and ten overlap examples. `--output` preserves the
complete worktree and path inventory for overlap investigation.

Packets contain an exact base, explicit paths, before/after hashes, file modes
and content. Verification checks every destination before application and
refuses another HEAD, staged edits, path traversal, symlink escapes, blueprint,
secrets and Git metadata. Application changes the working tree; the existing
safe-commit runner retains ownership of staging and committing.

Program files use `midgard-work-program/v1`, an `id`, `tasks` and `decisions`.
Each task carries `id`, `state`, `dependsOn`, explicit `paths`,
`requiredReceipts` and `issues` with `relation: exact | related`. Implemented
tasks add `owner`, full `base` and `candidate` hashes. Reviewed, published and
accepted states add their separate evidence. Accepted tasks require valid
receipts; a dependency cycle or missing decision record is refused. Rendering
does not change task states or close issues. Decisions name a repository
`path` and optional `supersededBy` identity.

The package map is derived from exports, bins and scripts. Symbol discovery
uses TypeScript declarations and the existing facet traversal. `boundary`
imports built runtime exports and invokes built CLI help in child processes,
so source-only test resolution cannot conceal ESM startup failures.
