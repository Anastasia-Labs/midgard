import { it } from "vitest";

import { runHistoryOwnerRollbackJourney } from "./helpers/history-owner-rollback-journey.js";

it("repairs an actual accepted orphan-funded L2 transfer and admits a fresh incarnation of the same deposit ID", async () => {
  await runHistoryOwnerRollbackJourney({ dependency: "spend" });
});
