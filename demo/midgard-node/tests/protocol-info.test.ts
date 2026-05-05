import { describe, expect, it } from "vitest";
import { encodeProtocolInfo } from "@/commands/protocol-info.js";
import { MIDGARD_SUPPORTED_SCRIPT_LANGUAGES } from "@/midgard-tx-codec/index.js";

const nodeConfig = {
  NETWORK: "Preview",
  MIN_FEE_A: 44n,
  MIN_FEE_B: 155381n,
  MAX_SUBMIT_TX_CBOR_BYTES: 32768,
  VALIDATION_STRICTNESS_PROFILE: "phase1_midgard",
} as const;

describe("encodeProtocolInfo", () => {
  it("serializes stable builder facts without JSON bigint fields", () => {
    expect(
      encodeProtocolInfo({
        nodeConfig,
        currentSlot: 123456,
      }),
    ).toEqual({
      apiVersion: 1,
      network: "Preview",
      midgardNativeTxVersion: 1,
      currentSlot: "123456",
      supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
      protocolFeeParameters: {
        minFeeA: "44",
        minFeeB: "155381",
      },
      submissionLimits: {
        maxSubmitTxCborBytes: 32768,
      },
      validation: {
        strictnessProfile: "phase1_midgard",
        localValidationIsAuthoritative: false,
      },
    });
  });

  it("rejects unsafe numeric current slots", () => {
    expect(() =>
      encodeProtocolInfo({
        nodeConfig,
        currentSlot: Number.MAX_SAFE_INTEGER + 1,
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
      }),
    ).toThrow("MAX_SUBMIT_TX_CBOR_BYTES must be a positive safe integer");
  });
});
