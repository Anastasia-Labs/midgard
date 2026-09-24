import { it } from "vitest";

import { runHistoryOwnerRollbackJourney } from "./helpers/history-owner-rollback-journey.js";

it("repairs an actual accepted descendant after authenticated owner rollback and reaccepts both original signed payloads", async () => {
  await runHistoryOwnerRollbackJourney({
    dependency: "descendant",
    evidencePath: process.env.MIDGARD_L1_ROLLBACK_DESCENDANT_EVIDENCE_PATH,
  });
});

it("repairs an actual reference-only dependency while preserving canonical funding and reaccepts the original signed payload", async () => {
  await runHistoryOwnerRollbackJourney({
    dependency: "reference",
    evidencePath: process.env.MIDGARD_L1_ROLLBACK_REFERENCE_EVIDENCE_PATH,
  });
});

/** Full production-service restart with retained SQL/Level in the same process;
 * the accepted emulator chain survives. This is not an OS-process crash. */
it("restarts production services at a pending rollback, preserves the signed commitment, then repairs and reaccepts after authenticated expiry", async () => {
  await runHistoryOwnerRollbackJourney({
    dependency: "spend",
    restartWhilePending: true,
    evidencePath: process.env.MIDGARD_L1_ROLLBACK_RESTART_EVIDENCE_PATH,
  });
});
