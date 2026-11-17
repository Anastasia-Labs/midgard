/** The script families over a block whose only leaf the operator committed
 * `TxIsInvalid` while carrying a Plutus mint policy, its witness and a
 * redeemer: every field a script family reads is present, but the step
 * validators open an accepted subject only at validity code 0, so no script
 * family may open the leaf. A journaled artifact naming the leaf is re-planned
 * to nothing. The installed catalogue's decision on the same leaf
 * (`l2TxMistag`) is in
 * `invalid-tagged-normal-leaf.catalogue-classification.test.ts`. */
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardMintPolicyItem,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { prepareDistinctAssetAccumulationLimitReplay } from "../src/distinct-asset-accumulation-limit/replay.js";
import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { prepareExecutionNativeScriptInvalidReplay } from "../src/execution-native-script-invalid/replay.js";
import { prepareExecutionSourceScriptDecodingReplay } from "../src/execution-source-script-decoding/replay.js";
import { prepareMissingRedeemerReplay } from "../src/missing-redeemer/replay.js";
import { prepareMissingScriptSourceReplay } from "../src/missing-script-source/replay.js";
import { prepareReceivePurposeLanguageReplay } from "../src/receive-purpose-language/replay.js";
import { prepareScriptIntegrityHashMismatchReplay } from "../src/script-integrity-hash-mismatch/replay.js";
import { prepareUnusedRedeemerReplay } from "../src/unused-redeemer/replay.js";
import { prepareUnusedScriptWitnessReplay } from "../src/unused-script-witness/replay.js";
import {
  authenticatedHeaderObservation,
  buildFixtureTransaction,
} from "./helpers/canonical-block-evidence-fixture.js";
import { redeemerItem } from "./support/missing-redeemer-emulator.purposes.js";
import { plutusScript } from "./support/missing-redeemer-emulator.subject.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";

const script = plutusScript(1);

const invalidScriptLeafEvidence = async () => {
  const block = await buildDecodingBlockFixture({
    operatorVkey: "aa".repeat(28),
    startTime: 1_000_000n,
    priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    subject: {
      kind: "normal",
      nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
        buildFixtureTransaction({
          validity: "TxIsInvalid",
          spendInputs: [],
          fee: 0n,
          mintPolicyItems: [
            encodeMidgardMintPolicyItem({
              policyId: Buffer.from(hashMidgardVersionedScript(script), "hex"),
              assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
            }),
          ],
          scriptWitnesses: [encodeMidgardVersionedScript(script)],
          redeemerWitnesses: [
            redeemerItem("Mint", 0n, Buffer.from(Data.to(0n), "hex")),
          ],
        }).canonicalCbor,
      ),
    },
  });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(block),
    payloadEnvelopeCbor: block.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "test/script-family-invalid-leaf",
      grade: "security",
    },
  });
};

describe("script families over a leaf committed TxIsInvalid", () => {
  it.each([
    [
      "executionSourceScriptDecoding",
      prepareExecutionSourceScriptDecodingReplay,
    ],
    ["receivePurposeLanguage", prepareReceivePurposeLanguageReplay],
    ["unusedScriptWitness", prepareUnusedScriptWitnessReplay],
    ["missingScriptSource", prepareMissingScriptSourceReplay],
    ["missingRedeemer", prepareMissingRedeemerReplay],
    ["unusedRedeemer", prepareUnusedRedeemerReplay],
    ["executionNativeScriptInvalid", prepareExecutionNativeScriptInvalidReplay],
    ["scriptIntegrityHashMismatch", prepareScriptIntegrityHashMismatchReplay],
    [
      "distinctAssetAccumulationLimit",
      prepareDistinctAssetAccumulationLimitReplay,
    ],
  ] as const)("%s plans no finding at the leaf", async (_family, prepare) => {
    const evidence = await invalidScriptLeafEvidence();
    await expect(
      prepare({
        current: evidence.reconstruction,
        coordinate: { sourceKind: 0, sourceIndex: 0 },
      }),
    ).resolves.toBeNull();
  });
});
