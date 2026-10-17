# Codex and Claude host adapter

This is the execution contract for the Midgard pstack port. Read it before
any workflow dispatches agents, selects a model, reads history, or schedules work.

## Configuration and roles

Read `.agents/pstack-models.json` from the repository root at workflow start.
Each role maps to a seat or nonempty seat array. A seat has `provider` and
optional `model` and `effort`. `native` means the current host. Explicit providers
are `codex` and `claude`. Model and reasoning effort are separate values;
there are no provider-specific effort suffixes appended to model IDs.

Omitted models inherit the native parent or the external CLI's runtime default.
`auto` and `inherit-parent` model values are aliases only on native seats.
Explicit model IDs come from the user or a verified host capability. A missing
role, rejected model, unavailable provider, or authentication error is a gap,
not a reason to invent a substitute. Report actual provider and model; if the
runtime does not disclose its default, say `default, exact model unreported`.
Two calls to one provider are independent attempts, not cross-provider review.

## Native agents

Use the current host's actual tool schema.

- Codex uses its exposed collaboration tools to spawn, message, wait, and
  interrupt agents. Use only fields the schema accepts. If model or effort
  overrides are unavailable, inherit the parent and report the limitation.
  A native Codex subagent does not become Claude by receiving a Claude model ID.
- Claude uses its exposed Agent tool (or Task in an older host). Use only its
  documented fields and available native model choices. A native Claude
  subagent does not become Codex by receiving a GPT model ID.
- When there is no native delegation tool, work directly for a single seat.
  Use the external adapter when an independent perspective is required and
  the corresponding CLI is available. Report missing seats otherwise.

Give each worker a complete bounded brief: goal, scope, exact SHAs or paths,
applicable repository instructions, verification, output, and authorization.
For a playbook worker, point it at `poteto-mode/SKILL.md`; this replaces a
host-specific custom agent name. Comment review uses
[the comment-review prompt](comment-reviewer.md).

Independent writers get separate worktrees and `codex/` branches. Native
agents may share the checkout only for read work or explicitly disjoint owned
paths. Stay within host concurrency and nesting limits. A CLI process is not a
native child with inherited context, MCP access, or messaging.

## External read-only review and design

The bundled dispatcher uses installed CLIs, argument arrays, and stdin.
It does not launch a shell or bypass approval/sandbox controls. Supply relevant
repository instructions, exact diffs, source excerpts, and the review rubric
in a sanitized prompt file. CLI seats return text; the parent owns saving
artifacts and applying changes. Runtime commands and MCP investigation belong
to the parent or native workers.

```bash
node .agents/skills/poteto-mode/scripts/dispatch.mjs \
  --role 'interrogate reviewers' --seat 0 --host codex \
  --workspace /absolute/path/to/midgard \
  --prompt /tmp/review-brief.txt --output /tmp/codex-review.txt --dry-run
```

Seat 0 uses Codex and seat 1 Claude in the shipped panels. Change `--host`
to `claude` when that is the current host. Remove `--dry-run` to execute.
The dry run validates paths and configuration and prints the executable,
arguments, workspace, and output path without printing the prompt or starting
a provider process. Codex execution additionally enumerates configured MCP
servers, adds per-server disable overrides, and verifies that all are disabled
before inference; these dynamic overrides are resolved only on execution. Output creation is exclusive; choose a new path per seat.

Codex runs with a read-only sandbox, approval policy `never`, ephemeral
history, disabled hooks/plugins/apps, and verified per-server MCP disable overrides. Claude runs
in print mode with only Read/Glob/Grep tools, no MCP servers or hooks, no
session persistence, and `dontAsk` permissions. These restrictions deliberately
exclude runtime tests, external writes, and writable candidate generation.
Managed host policies can further restrict access. Check the CLI help when a
version rejects a flag; an unsupported invocation remains a failure.

The process has a bounded timeout and output size. A failed, timed-out, or
empty response is not a reviewer verdict. Resolve missing capabilities with
existing authorization; do not change global settings or log in automatically.
For writable work use native agents within the user's existing permission
policy and run verification in their worktree.

## History and handoff

Prefer the current conversation and a task capsule. In Codex desktop, use
available chat read/list tools for the specifically identified project chats.
In either CLI, use a user-supplied transcript or exact session identified by the
host. Confirm repository/cwd metadata before reading raw history. There is no
assumed universal transcript directory or JSONL layout. Keep unrelated
projects, credentials, and private chat data out of the evidence corpus.

If history is unavailable, create a capsule from observed git state, receipts,
and current conversation. Label missing transcript coverage. Do not claim to
have audited tool usage from a digest. Handoffs record the branch, head SHA,
dirty paths, owned processes, checks, authorization, blockers, and next action.
Compaction continues the same task; it is not an implicit pause request.

## Waiting, scheduling, and authorization

For in-session work, use bounded native waits or subprocess polling and keep
the user informed. Codex desktop schedules use its automation tool when the
user requests scheduled work. Claude scheduling uses an actually exposed
host capability. Scheduling needs a concrete requested cadence and stop
predicate; a durable checkpoint is the fallback when no scheduler exists.
A generic loop command is not assumed to exist on either host.

Pstack adds no authority. Existing authorization persists across turns.
Prepare reviewable results before any genuinely missing approval. External
messages, merges, deployments, resets, and deletion stay within the user's
explicit scope and repository requirements. Before durable-state resets read
[Midgard state-reset guidance](../../../../docs/agents/state-reset.md).
