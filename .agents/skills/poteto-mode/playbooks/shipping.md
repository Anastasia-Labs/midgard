# Shipping

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. Confirm the user authorized merging this PR or stack. Resolve actual PR bases and head SHAs from GitHub and git.

2. Independently verify each current PR head against its required checks, review findings, and relevant runtime acceptance. Record a verdict tied to that SHA.

3. Land only the contiguous verified run starting from the stack root. Recheck head and base before each merge; restacking invalidates receipts for changed heads.

4. Retarget children to their intended base after a merge and verify resulting patch changes. Preserve branch history and unrelated work.

5. Confirm merged state and report merged URLs, final SHAs, checks, and any unlanded PR with its reason.
