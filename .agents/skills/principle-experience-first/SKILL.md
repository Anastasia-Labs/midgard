---
name: principle-experience-first
description: "Apply when product, UX, or feature-scope tradeoffs come up. Choose user delight over implementation convenience; ship fewer polished features over more rough ones."
disable-model-invocation: true
---

# Experience First

Choose a polished user experience within Midgard's correctness, safety, and liveness requirements.

- Every feature, control, and option must be justified [review]
- Ship less, ship better (polished experience with three features beats rough one with ten) [review]
- Prototype before committing (design decisions are cheaper in throwaway HTML than production code) [review]
- Get the details right (transitions, alignment, spacing, feedback, error states) [review]
- Tighten the core loop (every feature should serve the central workflow or get out of the way) [review]

The user is whoever consumes the work. For a UI that is the end user. For a library or an internal API it is the colleague who imports it. The engineer who maintains the code next is a user too. Weigh their experience the same way, and explain impact from their perspective.

Foundations should serve the experience. Foundational thinking governs the *sequence* of work. This principle governs the *target*.
