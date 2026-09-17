import {
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeScript,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk";
import { afterEach, expect, it, vi } from "vitest";

import * as eligibility from "../src/execution-native-script-invalid/evidence-machine.js";
import { submitExecutionNativeScriptInvalidStep04 } from "../src/execution-native-script-invalid/submit-step-04-route.js";

type Step04Input = Parameters<
  typeof submitExecutionNativeScriptInvalidStep04
>[0];

const builders = vi.hoisted(() => ({
  direct: vi.fn(async () => "direct transaction"),
  staged: vi.fn(async () => "staged transaction"),
}));
vi.mock(
  "../src/execution-native-script-invalid/submit-step-04-direct.js",
  () => ({
    submitExecutionNativeScriptInvalidStep04Direct: builders.direct,
  }),
);
vi.mock("../src/execution-native-script-invalid/submit-step-04.js", () => ({
  submitExecutionNativeScriptInvalidStep04StartSignerScan: builders.staged,
}));

afterEach(() => {
  vi.clearAllMocks();
  vi.restoreAllMocks();
});

const scriptFor = (size: 3 | 1024 | 1025) => {
  const nativeScript = {
    type: "all" as const,
    scripts:
      size === 3
        ? []
        : [
            ...Array.from({ length: 31 }, (_, index) => ({
              type: "sig" as const,
              keyHash: Buffer.alloc(28, index),
            })),
            ...Array.from({ length: 8 }, () => ({
              type: "after" as const,
              slot: 0n,
            })),
            { type: "after" as const, slot: size === 1024 ? 24n : 256n },
          ],
  };
  const scriptBytes = encodeMidgardNativeScript(nativeScript);
  expect(scriptBytes.length).toBe(size);
  return encodeMidgardVersionedScript({
    language: "NativeCardano",
    nativeScript,
    scriptBytes,
  });
};

const inputFor = (signerCount: number, scriptBytes: 3 | 1024 | 1025) => {
  const keys = Array.from({ length: signerCount }, (_, index) =>
    Buffer.alloc(32, index),
  );
  // The builders are mocked at the transaction boundary. Their opaque runtime
  // objects must be forwarded unchanged; codecs and route eligibility are real.
  const input: Step04Input = {
    lucid: {} as Step04Input["lucid"],
    contracts: {} as Step04Input["contracts"],
    signer: {} as Step04Input["signer"],
    categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.executionNativeScriptInvalid,
    threadOutRef: `${"11".repeat(32)}#0`,
    nativeTxCompactCbor: "80",
    witnessSet: {
      addr_tx_wits_hash: "22".repeat(32),
      script_tx_wits_hash: "33".repeat(32),
      redeemer_tx_wits_hash: "44".repeat(32),
    },
    scriptItemCbor: scriptFor(scriptBytes),
    addressWitnessItems: keys.map((verificationKey) =>
      encodeMidgardAddressWitnessItem({
        verificationKey,
        signature: Buffer.alloc(64),
      }),
    ),
    referenceScriptUtxo: {} as Step04Input["referenceScriptUtxo"],
    witnessReferenceScripts: {},
    preSubmitBoundary: vi.fn(),
    awaitConfirmation: false,
  };
  return { input, keys };
};

it.each([
  { signerCount: 0, scriptBytes: 3, direct: true },
  { signerCount: 1, scriptBytes: 3, direct: true },
  { signerCount: 28, scriptBytes: 1024, direct: true },
  { signerCount: 29, scriptBytes: 3, direct: false },
  { signerCount: 1, scriptBytes: 1025, direct: false },
  { signerCount: 29, scriptBytes: 1025, direct: false },
] as const)(
  "selects direct=$direct for $signerCount signers and $scriptBytes script bytes",
  async ({ signerCount, scriptBytes, direct }) => {
    const predicate = vi.spyOn(
      eligibility,
      "executionNativeScriptInvalidUsesDirectRoute",
    );
    const { input, keys } = inputFor(signerCount, scriptBytes);
    await expect(submitExecutionNativeScriptInvalidStep04(input)).resolves.toBe(
      direct ? "direct transaction" : "staged transaction",
    );
    expect(predicate).toHaveBeenCalledExactlyOnceWith({
      signerCount,
      scriptBytes,
    });
    if (direct) {
      expect(builders.direct).toHaveBeenCalledExactlyOnceWith({
        ...input,
        addressWitnessVerificationKeys: keys,
      });
      expect(builders.staged).not.toHaveBeenCalled();
    } else {
      expect(builders.staged).toHaveBeenCalledExactlyOnceWith(input);
      expect(builders.direct).not.toHaveBeenCalled();
    }
  },
);
