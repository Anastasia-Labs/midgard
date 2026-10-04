---
name: diagnosing-runtime-progress
description: Diagnose a Midgard service that is alive or ready but not completing eligible work, including dependency outages, safety holds and recovery stalls. Use for pending obligations with no useful progress, conflicting provider tips, unexplained healthy readiness, or repeated worker failures; collect observations without changing journal/schema state.
---

# Diagnosing runtime progress

Read [read-only diagnostics](../../../docs/agents/contrib.md#devnets-acceptance-and-read-only-diagnostics)
before collecting a bundle or implementing a polling script.

Capture local readiness/pipeline observations with `contrib diagnose --url` or
use an existing exported snapshot. Record the observation's effective
configuration, artifact identity and declared workload cadence. Missing facts
remain unknown; an empty eligible queue is idle.

Classify process death, dependency outage, a justified integrity hold and a
stall of eligible work separately. Reproduce a suspected caller defect through
the owning `runtime-progress` or `lifecycle` gate. Receipt verification retains
the exact executed assertions and distinguishes setup failure from refusal.

Clear a hold only with fresh evidence through the existing authorized recovery
path; read state-reset guidance before any reset/redeploy decision. [review]
Finish with the responsible reason, reproducing gate or missing observation,
and the smallest authorized next action.
