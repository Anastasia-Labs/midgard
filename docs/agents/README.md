# Agent Documentation Structure

Use this directory for guidance that is important but not needed in every
single agent turn.

## Current Structure

- `docs/agents/production-l2.md`: repository-wide safety, correctness, and
  tradeoff policy.
- `docs/agents/state-reset.md`: durable-state deletion and on-chain redeploy
  requirements.
- `docs/agents/transaction-finalization.md`: transaction completion rules.
- `docs/agents/contracts.md`: trusted deployment parameters and the positive and
  negative emulator scenarios a contract change carries.
- `docs/agents/naming-and-versioning.md`: identifier, file, and version naming
  policy (what carries a version and what does not).
- `docs/agents/verification.md`: the checks to run for each kind of change, and
  the local test environment.
- `docs/agents/component-configuration.md`: per-service `config.yaml` loading,
  precedence, and where each component keeps its wallet credentials.

- `docs/agents/domain.md`: terminology and architectural decision records.
- `docs/agents/withdraw-zero-yielding.md`: spending-to-rewarding delegation.
- `docs/agents/issue-tracker.md`: GitHub issue workflows.
- `docs/agents/triage-labels.md`: repository triage roles.

## Directory Guidance

- `demo/AGENTS.md`: workspace imports, builds, and operational guidance pointers.
- `demo/midgard-node/AGENTS.md`: operator command placement and source consumers.
- `demo/midgard-node-tools/devnet/AGENTS.md`: devnet acceptance configuration.
- `onchain/aiken/AGENTS.md`: Aiken constraints, builds, and validator guidance.

Keep directory-specific rules in the nearest applicable `AGENTS.md`. Shared
procedures such as contract parameter changes stay here because they span
validators, SDK builders, and deployment fixtures.

Add a new domain file only when repeated repository-specific guidance cannot be
derived from code, tests, or an existing skill. Do not create placeholder files
or cite proposed paths as though they exist. Blind spot: the check resolves
relative links and backticked repository paths; a path written as plain prose
is not checked. [script: scripts/agents/check-doc-links.mjs]

Keep the root `AGENTS.md` limited to context and rules that apply to every task
in the repository.

## Enforcement Tags

Every rule in `AGENTS.md`, the nested `AGENTS.md` files, `docs/agents/*.md` and
`.agents/skills/*/SKILL.md` ends with exactly one tag that names what enforces
it. A rule is a list item or paragraph that uses a directive word (must, never,
always, do not, don't, may not, required), or any list item in a section whose
heading names rules. Routing pointers ("read X before Y") are not rules.
[script: scripts/agents/check-enforcement-tags.mjs]

| Tag                            | Meaning: this fails when the rule is broken                                             |
| ------------------------------ | --------------------------------------------------------------------------------------- |
| `[eslint: <rule>]`             | An ESLint rule configured in a tracked `eslint.config.*`.                               |
| `[hook: pre-commit]`           | A tracked hook in `.githooks/`; it runs only where the hooks are installed.             |
| `[ci: <workflow>/<step name>]` | The step with that exact name in `.github/workflows/<workflow>.yml`.                    |
| `[script: <path>]`             | A tracked script or test file, by repository path.                                      |
| `[aiken-test: <module>/]`      | Tests in the Aiken module (or directory) under `onchain/aiken/lib` or `validators`.     |
| `[runtime: <symbol>]`          | Code that refuses the broken state when it runs; the symbol is defined in tracked code. |
| `[review]`                     | Nothing automatic. A reviewer has to catch it.                                          |

When an enforcing check has a known gap, write it next to the rule as
`Blind spot: ...`, before the tag. `node scripts/agents/check-enforcement-tags.mjs`
fails on a rule with no tag or more than one, on a tag that does not resolve,
on a `[review]` rule that a known check already enforces, and on a file over its
line budget; the budgets are listed in that script.
