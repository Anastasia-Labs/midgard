import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import { assertDeploymentManifestMatchesConfig } from "../src/services/midgard-contracts.assert-deployment-manifest-matches-config.js";

/** Only the fields the startup check compares; every one matches `CONFIG`. */
const manifestDeclaring = (retentionDays: number) =>
  ({
    network: "Preprod",
    referenceScriptDeployAddress: "addr_test1reference",
    hubOracleOneShot: { txHash: "ab".repeat(32), outputIndex: 0 },
    economics: {
      profile: "bounded-acceptance-v1",
      requiredBondLovelace: "900000000",
      slashingPenaltyLovelace: "500000000",
    },
    da: { transportProfile: { retentionDays } },
  }) as unknown as DeploymentManifest;

const CONFIG = {
  NETWORK: "Preprod",
  L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: "addr_test1reference",
  HUB_ORACLE_ONE_SHOT_TX_HASH: "ab".repeat(32),
  HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: 0,
  DEPLOYMENT_ECONOMICS_PROFILE: "bounded-acceptance-v1",
  OPERATOR_REQUIRED_BOND_LOVELACE: 900_000_000n,
  OPERATOR_SLASHING_PENALTY_LOVELACE: 500_000_000n,
} as const;

const check =
  (retentionDays: number | undefined, manifestDays = 15) =>
  () =>
    assertDeploymentManifestMatchesConfig(
      manifestDeclaring(manifestDays),
      "/deploy/manifest.json",
      { ...CONFIG, RETENTION_DAYS: retentionDays },
    );

describe("startup check of RETENTION_DAYS against the verified manifest (B5)", () => {
  it("refuses to start on a window shorter than the manifest's declared retention", () => {
    expect(check(14)).toThrow(
      /Deployment manifest at "\/deploy\/manifest\.json" does not match node config: da\.transportProfile\.retentionDays: RETENTION_DAYS=14 is shorter than the verified deployment manifest da\.transportProfile\.retentionDays=15/u,
    );
    // The manifest, not the compiled 15-day window, sets the floor.
    expect(check(15, 21)).toThrow(/RETENTION_DAYS=15 is shorter/u);
  });

  it("starts when RETENTION_DAYS is unset, 0, equal to or longer than the manifest window", () => {
    for (const days of [undefined, 0, 15, 30])
      expect(check(days)).not.toThrow();
    expect(check(21, 21)).not.toThrow();
  });
});
