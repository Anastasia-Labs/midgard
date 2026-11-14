import { describe, expect, it } from "vitest";

import { assertWorkflowFundingReservationReadyToSubmit } from "../src/workflow/funding-reservation-permit.js";
import { slashFundingFixture } from "./workflow-runtime.slash-funding-fixture.js";

describe("permissionless slash funding", () => {
  it("admits a third-party funded slash paying the authenticated prover without reserving their reward", async () => {
    const runtime = await slashFundingFixture({ thirdPartyReward: true });
    try {
      await runtime.admit();
      expect(runtime.prepare).toHaveBeenCalledWith(
        expect.objectContaining({
          transition: expect.objectContaining({
            consumedOutRefs: [],
            producedInputs: [],
          }),
        }),
      );
      await expect(
        assertWorkflowFundingReservationReadyToSubmit({
          journal: runtime.journal,
          transactionHash: runtime.signed.toHash(),
        }),
      ).resolves.toBeUndefined();
    } finally {
      runtime.close();
    }
  });

  it.each([
    {
      label: "wrong authenticated reward address",
      thirdPartyReward: true,
      foreignRewardAddress: true,
    },
    {
      label: "changed proof datum",
      thirdPartyReward: true,
      changedProofDatum: true,
    },
    {
      label: "missing proof NFT",
      thirdPartyReward: true,
      missingProofToken: true,
    },
  ])(
    "refuses third-party slash with $label before durable preparation",
    async (options) => {
      const runtime = await slashFundingFixture(options);
      try {
        await expect(runtime.admit()).rejects.toThrow(
          /authenticated fraud-proof/,
        );
        expect(runtime.prepare).not.toHaveBeenCalled();
      } finally {
        runtime.close();
      }
    },
  );
});
