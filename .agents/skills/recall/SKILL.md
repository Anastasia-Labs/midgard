---
name: recall
description: "Rebuild recent working context on a named topic from scoped chat history, repository state, and available records."
disable-model-invocation: true
---

# Recall

Read [the host adapter](../poteto-mode/references/host-adapter.md) before dispatching agents, selecting models, accessing history, or scheduling work. It owns these operations for every pstack skill.

1. Classify the request. Use the session-pickup playbook for one specific handoff and automate-me for working preferences. An existing complete state capsule can replace history mining. [review]
2. Bound the topic, workspace, and time range. For unspecified recent work, use the last seven days and state that assumption. [review]
3. Read relevant chat history through the adapter. For several independently scoped chats, partition the read work. Return the goal, decisions, open work, corrections, and artifact references per chat. If history is unavailable, use the capsule and git state and label the gap. [review]
4. For a named subsystem or defect, use [why](../why/SKILL.md) to consult relevant shared records for prior fixes, reversions, and current symptoms. [review]
5. Check branch, PR, and issue status against live git or connected read tools. A transcript's old status is a lead, not the current state. [review]
6. Return a capsule of at most five bullets, one status line per thread, recurring problems, and the next concrete action. Preserve distinctions between merged, open, in flight, verified but uncommitted, reverted, and planned work. [review]
