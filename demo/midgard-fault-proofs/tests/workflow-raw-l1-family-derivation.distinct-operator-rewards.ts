import { expect, it } from "vitest";

import { deriveFraudProofRawL1FamilyStage } from "../src/workflow/index.js";
import {
  fixture,
  releaseEconomics,
} from "./support/raw-l1-terminal-fixture.js";

it("pairs the target reward with its operator bond after another operator's descendant slash", async () => {
  const value = await fixture({
    descendant: true,
    descendantOperatorCredential: "77".repeat(28),
  });
  await expect(
    deriveFraudProofRawL1FamilyStage({
      snapshot: value.snapshot,
      definition: value.definition,
      releaseEconomics,
    }),
  ).resolves.toMatchObject({
    kind: "removed",
    terminal: {
      economics: {
        proverRewardOutputOutRef: value.rewardOutRef,
      },
    },
  });
});

it("refuses duplicate selected target rewards after another operator's descendant slash", async () => {
  const value = await fixture({
    descendant: true,
    descendantOperatorCredential: "77".repeat(28),
    duplicateReward: true,
  });
  await expect(
    deriveFraudProofRawL1FamilyStage({
      snapshot: value.snapshot,
      definition: value.definition,
      releaseEconomics,
    }),
  ).rejects.toThrow(/one exact ADA-only enterprise reward/u);
});
