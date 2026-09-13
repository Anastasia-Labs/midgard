import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import { encodeProtocolInfo } from "../src/commands/protocol-info.js";

// An operator-visible submit cap chosen independently of the compiled V1
// bound, so the advertised value cannot be right by construction.
const CONFIGURED_SUBMIT_CAP_BYTES = 123_456;
const nodeConfig = {
  NETWORK: "Preview",
  MIN_FEE_A: 44n,
  MIN_FEE_B: 155381n,
  MAX_SUBMIT_TX_CBOR_BYTES: CONFIGURED_SUBMIT_CAP_BYTES,
  VALIDATION_STRICTNESS_PROFILE: "phase1_midgard",
} as const;
const MANIFEST_ID = "ab".repeat(32);
const deploymentMarker = makeDeploymentMarker(MANIFEST_ID);

describe("encodeProtocolInfo", () => {
  it("advertises the configured network, fees, cap, marker, and V1 wire identity", () => {
    const { consensusProfile, ...advertised } = encodeProtocolInfo({
      nodeConfig,
      currentSlot: 123456,
      deploymentMarker,
    });

    // The published shape is the /protocol-info wire contract: decimal
    // strings for the unbounded numbers, the exact two supported script
    // languages with their canonical tags, and the explicit statement that a
    // client's local validation is not authoritative.
    expect(advertised).toStrictEqual({
      apiVersion: 1,
      midgardNativeTxVersion: 1,
      deploymentMarker: {
        schemaVersion: "midgard-deployment-marker-v1",
        manifestId: MANIFEST_ID,
      },
      network: "Preview",
      currentSlot: "123456",
      protocolFeeParameters: {
        minFeeA: "44",
        minFeeB: "155381",
      },
      submissionLimits: {
        maxSubmitTxCborBytes: CONFIGURED_SUBMIT_CAP_BYTES,
      },
      validation: {
        strictnessProfile: "phase1_midgard",
        localValidationIsAuthoritative: false,
      },
      codecSupportedScriptLanguages: [
        { name: "PlutusV3", tag: 2 },
        { name: "MidgardV1", tag: 0x80 },
      ],
      supportedScriptLanguages: [
        { name: "PlutusV3", tag: 2 },
        { name: "MidgardV1", tag: 0x80 },
      ],
    });
    // The advertised profile is the compiled one, never a caller-supplied
    // substitute.
    expect(consensusProfile).toBe(MIDGARD_CONSENSUS_PROFILE);
  });

  it("advertises a bigint slot in the same decimal-string representation", () => {
    expect(
      encodeProtocolInfo({
        nodeConfig,
        currentSlot: 9_007_199_254_740_993n,
        deploymentMarker,
      }).currentSlot,
    ).toBe("9007199254740993");
  });

  it("advertises the compiled consensus profile when it is passed explicitly", () => {
    expect(
      encodeProtocolInfo({
        nodeConfig,
        currentSlot: 123456,
        deploymentMarker,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      }).consensusProfile,
    ).toBe(MIDGARD_CONSENSUS_PROFILE);
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

  it("advertises a submit cap exactly at the compiled V1 maximum", () => {
    const bound = MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes;

    expect(
      encodeProtocolInfo({
        nodeConfig: {
          ...nodeConfig,
          MAX_SUBMIT_TX_CBOR_BYTES: bound,
        },
        currentSlot: 1,
        deploymentMarker,
      }).submissionLimits.maxSubmitTxCborBytes,
    ).toBe(bound);
  });

  it("rejects a submit cap one byte above the compiled V1 maximum", () => {
    expect(() =>
      encodeProtocolInfo({
        nodeConfig: {
          ...nodeConfig,
          MAX_SUBMIT_TX_CBOR_BYTES:
            MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes + 1,
        },
        currentSlot: 1,
        deploymentMarker,
      }),
    ).toThrow(
      "MAX_SUBMIT_TX_CBOR_BYTES must not exceed the canonical V1 transaction bound",
    );
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
