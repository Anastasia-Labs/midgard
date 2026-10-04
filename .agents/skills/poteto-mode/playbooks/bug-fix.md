# Bug fix

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. Reproduce the defect on the actual affected path. Read [writing-tests](../../writing-tests/SKILL.md) and establish a regression that fails on the unfixed code. If the environment cannot reproduce it, record the attempted command and the gap.

2. Trace candidate mechanisms with [how](../../how/SKILL.md), runtime evidence, and history when relevant. Eliminate hypotheses before choosing a fix.

3. Plan the smallest root-cause change. Use architect for a consequential interface change. Implement directly or delegate using the configured bug-fix role.

4. Run the same regression after the fix, required checks for the changed paths, and relevant runtime acceptance. Consensus changes also use [reviewing-consensus-changes](../../reviewing-consensus-changes/SKILL.md).

5. Report the failure before, passing result after, root cause, changed behavior, and remaining gaps.
