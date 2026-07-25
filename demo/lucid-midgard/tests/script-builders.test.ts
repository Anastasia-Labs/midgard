import { MIDGARD_SUPPORTED_SCRIPT_LANGUAGES } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { describe, expect, it } from "vitest";

import {
  LucidMidgard,
  type MidgardProvider,
} from "../src/index.js";

const fakeProvider: MidgardProvider = {
  getUtxos: async () => [],
  getUtxoByOutRef: async () => undefined,
  getProtocolInfo: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 0n, minFeeB: 0n },
    submissionLimits: { maxSubmitTxCborBytes: MIDGARD_CONSENSUS_PROFILE_V1.limits.maxTxCanonicalCborBytes },
    validation: {
      strictnessProfile: "production",
      localValidationIsAuthoritative: false,
    },
  }),
  getProtocolParameters: async () => ({
    minFeeA: 0n,
    minFeeB: 0n,
    networkId: 0n,
  }),
  getCurrentSlot: async () => 0n,
  submitTx: async () => ({
    txId: "00".repeat(32),
    status: "queued",
    httpStatus: 202,
    duplicate: false,
  }),
  getTxStatus: async (txId) => ({ kind: "queued", txId }),
  diagnostics: () => ({
    endpoint: "memory://canonical-v1",
    protocolInfoSource: "node",
  }),
};

const dummyRedeemer = {
  data: Buffer.from([0x80]),
  exUnits: { mem: 1n, steps: 1n },
};

describe("V1 script and mint feature surface", () => {
  it("retains mint/burn, scripts, observers, and receive redeemers", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);
    const builder = midgard
      .newTx()
      .attach.Script({
        kind: "plutus-v3",
        language: "PlutusV3",
        script: Buffer.from([0x01]),
      })
      .mintAssets("00".repeat(28), { abcd: 1n }, dummyRedeemer)
      .observe("11".repeat(28), dummyRedeemer)
      .receiveRedeemer("22".repeat(28), dummyRedeemer);

    expect(builder.config().midgardNativeTxVersion).toBe(1);
    expect(builder.snapshot().scripts).toMatchObject({
      scripts: [{ language: "PlutusV3" }],
      mints: [{ policyId: "00".repeat(28) }],
      observers: [{ scriptHash: "11".repeat(28) }],
      receiveRedeemers: [{ scriptHash: "22".repeat(28) }],
    });
  });
});
