---
name: principle-boundary-discipline
description: "Apply when wiring validation, error handling, or framework adapters. Concentrate guards at system boundaries (CLI, config, network, external APIs); trust internal types and keep business logic in pure functions."
disable-model-invocation: true
---

# Boundary Discipline

Place validation, type narrowing, and error handling at system boundaries. Trust internal code unconditionally. Business logic lives in pure functions. The shell is thin and mechanical.

**Why:** Scattered validation is noisy, redundant, and gives a false sense of safety. Keep logic out of framework wiring so it can be tested without the framework.

**The pattern:**
- **At boundaries** (CLI args, config files, external APIs, network protocols): validate, return errors, handle defensively. [review]
- **Inside the system:** typed data, error propagation, no re-validation. Trust the types. [review]
- **Across the boundary.** Expose domain concepts, not the boundary's private representation. Keep general-purpose mechanism inside and special-purpose policy at the edge. [review]

**Applications:**

Validation and error handling:
- Validate config at parse time (the boundary), not inside business logic [review]
- Parse raw data into domain types at the boundary [review]
- Do not re-export transport, storage, framework, or wire types through the public surface [review]
- No redundant nil checks deep in call chains if the boundary already validated [review]

Code organization:
- Business logic in pure functions with no framework dependencies [review]
- Parse functions: pure transforms from raw bytes to typed state [review]
- Prompt construction: structured state in, string out [review]
- Scoring and assessment: pure transforms from state to results [review]

**The tests:**
- "Is this data crossing a system boundary right now?" If not, validation is redundant. [review]
- "Can this be a pure function that the shell just calls?" If yes, extract it. [review]
