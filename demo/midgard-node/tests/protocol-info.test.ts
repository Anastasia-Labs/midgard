import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { makeDeploymentMarkerV1 } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { describe, expect, it } from "vitest";

import { encodeProtocolInfo } from "@/commands/protocol-info.js";

const nodeConfig = {
  NETWORK: "Preview",
  MIN_FEE_A: 44n,
  MIN_FEE_B: 155381n,
  MAX_SUBMIT_TX_CBOR_BYTES:
    MIDGARD_CONSENSUS_PROFILE_V1.limits.maxTxCanonicalCborBytes,
  VALIDATION_STRICTNESS_PROFILE: "phase1_midgard",
} as const;
const deploymentMarker = makeDeploymentMarkerV1("ab".repeat(32));

describe("encodeProtocolInfo", () => {
  it("fails closed while the compiled V1 release-evidence gate is incomplete", () => {
    expect(() =>
      encodeProtocolInfo({
        nodeConfig,
        currentSlot: 123456,
        deploymentMarker,
      }),
    ).toThrow(/not activated/u);
  });

  it("does not advertise V1 before its compiled L1 evidence gate is complete", () => {
    expect(() =>
      encodeProtocolInfo({
        nodeConfig,
        currentSlot: 123456,
        deploymentMarker,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      }),
    ).toThrow(/not activated/u);
  });

  it("rejects unsafe numeric current slots", () => {
    expect(() =>
      encodeProtocolInfo({
        nodeConfig,
        currentSlot: Number.MAX_SAFE_INTEGER + 1,
        deploymentMarker,
      }),
    ).toThrow("currentSlot must be a non-negative safe integer");
  });

  it("rejects negative fee parameters", () => {
    expect(() =>
      encodeProtocolInfo({
        nodeConfig: {
          ...nodeConfig,
          MIN_FEE_A: -1n,
        },
        currentSlot: 1,
        deploymentMarker,
      }),
    ).toThrow("MIN_FEE_A must be non-negative");
  });

  it("rejects invalid submit size configuration", () => {
    expect(() =>
      encodeProtocolInfo({
        nodeConfig: {
          ...nodeConfig,
          MAX_SUBMIT_TX_CBOR_BYTES: 0,
        },
        currentSlot: 1,
        deploymentMarker,
      }),
    ).toThrow("MAX_SUBMIT_TX_CBOR_BYTES must be a positive safe integer");
  });

  it("rejects a non-exact SDK deployment marker before advertising V1", () => {
    expect(() =>
      encodeProtocolInfo({
        nodeConfig,
        currentSlot: 1,
        deploymentMarker: {
          ...deploymentMarker,
          legacyFingerprint: deploymentMarker.manifestId,
        },
      }),
    ).toThrow(/exactly schemaVersion and manifestId/u);
  });
});
