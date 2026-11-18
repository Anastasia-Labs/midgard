import { rm } from "node:fs/promises";

import {
  DEPLOYMENT_PROFILES,
  type DeploymentProfileName,
} from "@al-ft/midgard-core/deployment-profile";
import { afterAll, describe, expect, it } from "vitest";

import { stackAbsorbWithinMs } from "./journey-timing.js";
import { ensureStackVerifiedPredecessor } from "./stack-verified-predecessor.js";
import {
  drillDirectories,
  verifiedPredecessorDrill,
} from "./stack-verified-predecessor.test-support.js";

afterAll(() =>
  Promise.all(
    drillDirectories.map((path) => rm(path, { recursive: true, force: true })),
  ),
);

/**
 * How long, on the stack's own clock, a heal waits before refusing a
 * transfer our node never commits, on a stack of deployment `profile`.
 */
const refusedAfterMs = async (profile: DeploymentProfileName) => {
  const drill = await verifiedPredecessorDrill();
  const startedAt = drill.now();
  await expect(
    ensureStackVerifiedPredecessor({
      ...drill.ports(),
      absorbWithinMs: stackAbsorbWithinMs(DEPLOYMENT_PROFILES[profile].timing),
    }),
  ).rejects.toEqual(
    expect.objectContaining({
      name: "StackVerifiedPredecessorRefusal",
      reason: "transfer-not-committed",
      headerHash: drill.rootBlock.headerHash,
    }),
  );
  expect(drill.node.submitted).toHaveLength(1);
  return drill.now() - startedAt;
};

describe("the absorption bound of an attached stack", () => {
  it("refuses the same stalled transfer later on a stack whose deployment has longer shifts and attestation", async () => {
    const testing = await refusedAfterMs("preprod-testing");
    const rehearsal = await refusedAfterMs("preprod-rehearsal");
    expect(rehearsal).toBeGreaterThan(testing);
    // Each still waited past one whole shift of its own deployment, the
    // least our node may wait before it commits.
    expect(testing).toBeGreaterThan(
      DEPLOYMENT_PROFILES["preprod-testing"].timing.operator_shift_ms,
    );
    expect(rehearsal).toBeGreaterThan(
      DEPLOYMENT_PROFILES["preprod-rehearsal"].timing.operator_shift_ms,
    );
  });
});
