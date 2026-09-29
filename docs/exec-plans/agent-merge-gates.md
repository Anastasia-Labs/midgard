# Preparing required merge gates

Status: local proposal, 2026-09-28. No GitHub policy or active workflow has been
changed. This prepares W1.8 in [the hardening plan](agent-contribution-hardening.md);
activation follows an owner decision about target branches and a green
integration checkpoint.

## Generate the review artifacts

From the repository root, with the demo dependencies installed:

```bash
node scripts/ci/prepare-merge-gates.mjs --branch main --branch tx-validation
node scripts/ci/prepare-merge-gates.mjs --branch main --branch tx-validation --format ruleset > /tmp/midgard-ruleset.json
node scripts/ci/prepare-merge-gates.mjs --branch main --branch tx-validation --format patch > /tmp/midgard-required-ci.patch
git apply --check /tmp/midgard-required-ci.patch
```

The branch arguments are explicit proposal inputs, not authorization to protect
those branches. Choose the real integration branch before activation. The script
reads current workflow definitions, emits a disabled ruleset, and prepares a
patch that makes the required workflows run on every pull request. It writes
only scratch diff inputs and stdout; it has no API client or apply mode.
Regenerate after workflow changes rather than applying an old patch by force.

The candidate requires these five contexts, derived from job names or IDs:

- Repository tool tests and workflow lint
- skills
- Aiken CI gate
- Node CI gate
- watcher (the native Go chain-sync checks; TypeScript watcher checks are in Node CI)

It proposes one approving review, approval of the latest push by another person,
resolution of review threads, up-to-date required checks, and refusal of force
pushes and branch deletion. The bypass list is empty. It does not require linear
history or a merge queue. The JSON follows the
[GitHub repository ruleset API](https://docs.github.com/en/rest/repos/rules#create-a-repository-ruleset).

## Trigger and cost implications

A required check behind a workflow-level path filter can remain pending when
that workflow is skipped. The generated patch removes pull-request filters from
Aiken, Node and Watcher CI; the repository-tools and skills workflows already
run on every PR. It preserves push filters, jobs and job dependencies.
Consequently, even a documentation-only PR runs these suites. This proposal
prioritizes a dependable gate; measure its cost before activation. A later
optimization needs unconditional summary checks and tested change detection
that fails closed, not simply restored workflow-level filters.

Docs Site and Latex CI remain path-selected and are not required by this first
ruleset. They still run for matching changes, but this policy alone cannot
prevent merging their failures. Pages deployment is not a contribution check.
Do not describe this proposal as requiring every repository workflow.

## Activation sequence after the integration checkpoint

1. Confirm the protected branches, review threshold and bypass policy with the
   owner. Read existing repository and organization rules before changing any
   policy; this proposal must not replace unrelated protection.
2. Regenerate the patch on the integrated branch. Update the existing workflow
   trigger scenario table for unconditional PR execution, run its tests and the
   workflow linter, and review the diff. The preparation script deliberately
   does not rewrite the table: an independent expected scenario is the oracle.
3. Run CI on the exact candidate PR head with a current base, including docs-only
   and code-change trigger cases. Resolve integration conflicts first; a PR that
   cannot produce a merge ref is not evidence of passing CI. Check failure and
   cancellation behavior of both summary gates. A local test does not replace
   these remote runs.
4. Read actual check-run names and their GitHub App IDs on that head. Match every
   context in the proposal and bind each required check's `integration_id` to
   its observed provider. The offline proposal omits these IDs because a local
   workflow file does not establish remote check provenance. No missing check,
   skipped check, old-head run or inaccessible API response is success.
5. Present the concrete ruleset and workflow diff for activation approval. Apply
   only the reviewed policy after the owner approves it; verify the resulting
   branch rules through the API. Keep the approved payload and previous policy
   for recovery. Do not weaken a failed gate to complete an activation.

Done when the trigger changes have current-head CI evidence and the owner has
approved and verified the live policy. Until then W1.8 remains open. Preparation
is tested locally with `node --test scripts/ci/prepare-merge-gates.test.mjs`;
those tests validate policy generation and patch applicability, not GitHub's
remote enforcement or the correctness of the checks themselves.
