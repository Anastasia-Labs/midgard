import "./contract-deployment-info.contract-deployment-info.js";

import { deploymentDaRetentionDays } from "@al-ft/midgard-core/da-transport";
import { assertRetentionWindowCoversDeployment } from "@al-ft/midgard-core/retention-window";
import { describe, expect, it as unitIt } from "vitest";

import { deploymentDaTransportProfile } from "../src/commands/contract-deployment-info.js";

describe("deployment DA transport profile", () => {
  unitIt("commits the canonical retention window, not local pruning", () => {
    const profile = deploymentDaTransportProfile({
      MIDGARD_DA_PAYLOAD_ENVELOPE: "zstd",
      MIDGARD_DA_ZSTD_LEVEL: 3,
    });
    expect(profile.retentionDays).toBe(deploymentDaRetentionDays());
    expect(
      assertRetentionWindowCoversDeployment({
        da: { transportProfile: profile },
      }),
    ).toBe(deploymentDaRetentionDays());
  });
});
