---
name: principle-subtract-before-you-add
description: "Apply when sequencing an addition, refactor, or rewrite. Remove dead code, redundant validators, and stub references first, then build on the simpler base."
disable-model-invocation: true
---

# Subtract Before You Add

When evolving a system, remove complexity first, then build.

**Why:** Adding to a complex system compounds complexity. Removing first leaves less code, reveals the essential structure, and usually makes the next design obvious. Default to subtraction.

Make simplification a continual investment. Leave the design slightly simpler and more capable behind the same or smaller surface than you found it.

**The pattern:**
- Sequence removal before construction [review]
- Cut before you polish (get to the minimum before investing in quality) [review]
- Design for observed usage, not speculative edge cases [review]
- No speculative validators, parsers, or guards beyond what the spec demands [review]
- Simplify prompts (remove redundant instructions, excessive templates) [review]
- When a reference has no novel content, delete it rather than leaving a stub [review]
