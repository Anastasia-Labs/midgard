import "./contract-deployment-info.contract-deployment-info.js";

import {
  assertRetentionWindowCoversDeployment,
  MIDGARD_RETENTION_WINDOW,
} from "@al-ft/midgard-core/retention-window";
import { describe, expect, it as unitIt } from "vitest";

import { deploymentDaTransportProfile } from "../src/commands/contract-deployment-info.js";

describe("deployment DA transport profile", () => {
  unitIt("commits the canonical retention window, not local pruning", () => {
    const profile = deploymentDaTransportProfile({
      MIDGARD_DA_PAYLOAD_ENVELOPE: "zstd",
      MIDGARD_DA_ZSTD_LEVEL: 3,
    });
    expect(profile.retentionDays).toBe(MIDGARD_RETENTION_WINDOW.retentionDays);
    expect(
      assertRetentionWindowCoversDeployment({
        da: { transportProfile: profile },
      }),
    ).toBe(MIDGARD_RETENTION_WINDOW.retentionDays);
  });
});
