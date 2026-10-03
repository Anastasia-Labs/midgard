---
name: poteto-mode
description: "Use pstack rigor for an engineering task in Midgard: choose a playbook, ground decisions, implement, and verify with Codex or Claude."
disable-model-invocation: true
---

# Poteto mode for Midgard

Apply this workflow to the requested task and subsequent related turns until the user opts out. There is no host-specific sticky-mode hook; carry a short mode marker and the remaining work into a handoff when context is compacted.

## Start

1. Read the applicable `AGENTS.md` files and inspect dirty state. Midgard's repository rules and the user's scope govern every pstack workflow. [review]
2. Before using models, delegation, history, or scheduling, read [the host adapter](references/host-adapter.md). Read `.agents/pstack-models.json` from the repository root. [review]
3. Pick a playbook from the table below and read that file. Track its steps with the host's planning tool or a short local checklist. Record a reason for a skipped applicable step. [review]
4. Define the observable result and verification before changing code. Name the data shape and trace the affected callers with [how](../how/SKILL.md). Use [why](../why/SKILL.md) when historical constraints matter. [review]

## Route specialist work

| Trigger | Read before acting |
| --- | --- |
| A consequential interface or ownership change | [architect](../architect/SKILL.md) |
| Competing designs or implementations | [arena](../arena/SKILL.md) |
| Independent coverage or measurement slices | [swarm](../swarm/SKILL.md) |
| Adversarial review | [interrogate](../interrogate/SKILL.md) |
| A migration without a suitable playbook | [figure-it-out](../figure-it-out/SKILL.md) |
| Performance measurement | [benchmark-checklist](../benchmark-checklist/SKILL.md) |
| Long work or a later handoff | [show-me-your-work](../show-me-your-work/SKILL.md) |
| A relevant engineering principle | [the principle index](references/principles.md), then the named leaf |

Read only the applicable references. Independent perspectives help when they can falsify a consequential decision; use direct work for routine changes. Each delegated brief contains the task, scope, exact files or SHAs, applicable instructions, verification, and output contract. Review returned artifacts yourself.

## Use Midgard's existing owners

| Change | Read before editing or testing |
| --- | --- |
| Agent instructions or skills | [editing-agent-instructions](../editing-agent-instructions/SKILL.md) |
| Test assertions | [writing-tests](../writing-tests/SKILL.md) |
| Aiken build or compiler failure | [aiken-contract-build](../aiken-contract-build/SKILL.md) |
| Consensus behavior | [reviewing-consensus-changes](../reviewing-consensus-changes/SKILL.md) |
| Goldens or execution ledgers | [regenerating-goldens-and-ledgers](../regenerating-goldens-and-ledgers/SKILL.md) |
| Test database or blueprint setup | [local-test-environment](../local-test-environment/SKILL.md) |
| Live devnet behavior | [running-the-devnet](../running-the-devnet/SKILL.md) |
| Preprod acceptance | [midgard-e2e-acceptance](../midgard-e2e-acceptance/SKILL.md) |
| TypeScript cleanup | [midgard-typescript-cleanup](../midgard-typescript-cleanup/SKILL.md) |
| A pure module split | [splitting-oversized-modules](../splitting-oversized-modules/SKILL.md) |
| Intermittent tests or failed CI | [fixing-flaky-tests](../fixing-flaky-tests/SKILL.md) or [debugging-ci-failures](../debugging-ci-failures/SKILL.md) |
| Commits, PR bodies, and reports | [committing-safely](../committing-safely/SKILL.md) and [writing-reports-and-prs](../writing-reports-and-prs/SKILL.md) |

Pstack principles guide judgment within Midgard's correctness, safety, liveness, performance, convenience order. Preserve protocol rationale, audit evidence, and required checks when simplifying code or comments. Planned local experiments stay isolated until they pass their integration gates. [review]

## Playbooks

| Task | Read |
| --- | --- |
| Read-only code question | [Investigation](playbooks/investigation.md) |
| Reproduce and fix a defect | [Bug fix](playbooks/bug-fix.md) |
| Improve measured slowness | [Performance](playbooks/perf-issue.md) |
| Repeated metric improvement | [Hillclimb](playbooks/hillclimb.md) |
| Diagnose a live symptom | [Runtime forensics](playbooks/runtime-forensics.md) |
| Diagnose a captured profile | [Trace forensics](playbooks/trace-forensics.md) |
| Add or change behavior | [Feature](playbooks/feature.md) |
| Preserve behavior while changing structure | [Refactoring](playbooks/refactoring.md) |
| Test an empirical design fork | [Prototype](playbooks/prototype.md) |
| Match a UI reference | [Visual parity](playbooks/visual-parity.md) |
| Write or edit a skill | [Authoring a skill](playbooks/authoring-a-skill.md) |
| Compare prompts or skills | [Eval](playbooks/eval.md) |
| Bring a PR to merge-ready | [Babysit](playbooks/babysit.md) |
| Land an authorized verified stack | [Shipping](playbooks/shipping.md) |
| Drive one long task to completion | [Autonomous run](playbooks/autonomous-run.md) |
| Coordinate a standing program | [Orchestrate](playbooks/orchestrate.md) |
| Drive an independent PR queue to merge | [Autopilot full](playbooks/autopilot-full.md) |
| Build a stack for the user to land | [Autopilot stack](playbooks/autopilot-stack.md) |
| Resume prior work | [Session pickup](playbooks/session-pickup.md) |
| Pause explicitly requested work | [Pause safely](playbooks/pause-safely.md) |
| Plan phases or a PR stack | [Multi-phase plan](playbooks/multi-phase-plan.md) |
| Audit and reclaim worktree space | [Worktree cleanup](playbooks/worktree-cleanup.md) |
| Create a PR within the requested scope | [Opening a PR](playbooks/opening-a-pr.md) |

## Finish

Run the checks prescribed by [Midgard verification](../../../docs/agents/verification.md) and the narrow checks that prove the touched behavior. A missing capability is an explicit gap. Report what changed, why, exact checks and results, remaining limits, and any created PR links. Apply [unslop](../unslop/SKILL.md) to prose while keeping the user's preferred level of detail.
