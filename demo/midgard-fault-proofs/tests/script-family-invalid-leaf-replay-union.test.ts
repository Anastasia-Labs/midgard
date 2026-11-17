/** The script families beside `l2TxMistag` over a block whose only leaf the
 * operator committed `TxIsInvalid` while carrying a Plutus mint policy, its
 * witness and a redeemer: every field a script family reads is present, but
 * the step validators open an accepted subject only at validity code 0, so
 * no script family may open the leaf. The block is decided as `l2TxMistag`. */
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
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  createCompleteCanonicalReplayUnion,
  DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
  MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
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

// Catalogue order, as the installed catalogue replay runs them.
const MEMBERS = [
  L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
  MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
];

describe("script families beside a leaf committed TxIsInvalid", () => {
  it("decides the block as l2TxMistag", async () => {
    const evidence = await invalidScriptLeafEvidence();
    const replay = await createCompleteCanonicalReplayUnion(MEMBERS).replay(
      evidence,
      {},
    );
    expect(replay.detections.map((detection) => detection.violationId)).toEqual(
      ["l2-tx-mistag"],
    );
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: replay.detections,
      }),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "l2TxMistag",
    });
  });

  // A journaled artifact naming the leaf is re-planned to nothing.
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
