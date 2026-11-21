---
name: principle-outcome-oriented-execution
description: "Apply during planned rewrites and migrations with explicit phase boundaries. Converge on the target architecture; don't preserve smooth intermediate states with throwaway compatibility code."
disable-model-invocation: true
---

# Outcome-Oriented Execution

Optimize for the intended, verifiable end state rather than preserving smooth intermediate states.

**Why:** Keeping every intermediate step fully stable often creates temporary compatibility code that becomes long-lived debt. Converge on the target architecture and prove correctness at explicit verification boundaries.

**Core rule:**
- Prioritize end-state integrity over transitional stability [review]
- Experimental intermediate states stay isolated; integrate only a working, verified increment [review]

**Guardrails:**
- Use this for planned rewrites and migrations with explicit phase boundaries [review]
- Declare where temporary breakage is acceptable [review]
- Keep high-signal checks for actively touched areas while migrating [review]
- Require full static and runtime verification at plan completion [review]
