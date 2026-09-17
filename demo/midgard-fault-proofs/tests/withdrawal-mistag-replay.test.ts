import { describe, expect, it } from "vitest";

import { prepareWithdrawalMistagReplay } from "../src/withdrawal-mistag/replay.js";
import { WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { withdrawalMistagRetainedFixture } from "./support/withdrawal-mistag-retained.js";
describe("withdrawal mistag retained replay", () => {
  it.each(["valid-marked-invalid", "invalid-marked-valid"] as const)(
    "authenticates the prior ledger and refuses honest %s tags",
    async (direction) => {
      for (const honest of [false, true]) {
        const fixture = await withdrawalMistagRetainedFixture({
          direction,
          honest,
        });
        const result = await WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY.replay(
          fixture.evidence,
          fixture.context,
        );
        expect(result.detections).toHaveLength(honest ? 0 : 1);
        await expect(
          prepareWithdrawalMistagReplay({
            current: fixture.evidence.reconstruction,
            index: 0,
          }),
        ).rejects.toThrow("authenticated predecessor ledger unavailable");
        await expect(
          prepareWithdrawalMistagReplay({
            current: fixture.evidence.reconstruction,
            predecessor: fixture.predecessor.reconstruction,
            index: 1,
          }),
        ).rejects.toThrow("source index is absent");
      }
    },
  );
});
