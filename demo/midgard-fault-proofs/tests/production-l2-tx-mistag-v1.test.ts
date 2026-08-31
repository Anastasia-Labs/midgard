import {
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonicalV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import {
  type L2TransactionSourceV1,
  L2TransactionSourceV1 as L2TransactionSourceV1Codec,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import type { CanonicalBlockClassificationV1 } from "../src/workflow/classification-v1.js";
import { classifyCanonicalBlockViolationsV1 } from "../src/workflow/classification-v1.js";
import { L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY_V1 } from "../src/workflow/complete-replay-v1.js";
import {
  admitProductionL2TxMistagArtifactV1,
  prepareProductionL2TxMistagArtifactV1,
} from "../src/workflow/production-l2-tx-mistag-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  type FixtureTransactionV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const mistaggedTransaction = (): FixtureTransactionV1 => {
  const full = materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsInvalid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      fee: 7n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  const canonicalCbor = encodeMidgardNativeTxCanonicalV1(full);
  const proofSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(canonicalCbor);
  const txId = computeMidgardNativeTxIdV1(full).toString("hex");
  const source: L2TransactionSourceV1 = {
    tx_id: txId,
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return {
    txId,
    canonicalCbor,
    compactCbor: proofSource.compactCbor,
    source,
    sourceValueBytes: Buffer.from(
      Data.to(source, L2TransactionSourceV1Codec),
      "hex",
    ),
  };
};

const isL2TxMistagClassification = (
  classification: CanonicalBlockClassificationV1,
): classification is Extract<
  CanonicalBlockClassificationV1,
  { readonly decision: "fault_detected" }
> & { readonly category: "l2TxMistag" } =>
  classification.decision === "fault_detected" &&
  classification.category === "l2TxMistag";

const fixtureEvidence = async () => {
  const fixture = await buildCanonicalBlockFixtureV1({
    transactions: [
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(4, 0n)],
        fee: 1n,
      }),
      mistaggedTransaction(),
    ],
  });
  return await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/production-l2-tx-mistag-test",
      grade: "security",
    },
  });
};

describe("production l2-tx-mistag public-evidence workflow V1", () => {
  it("selects the exact code-1 source leaf and replays its counted-root proof", async () => {
    const evidence = await fixtureEvidence();
    const replay =
      await L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    const classification = await classifyCanonicalBlockViolationsV1({
      evidence,
      detections: replay.detections,
    });
    if (!isL2TxMistagClassification(classification)) {
      throw new Error("fixture did not classify as l2TxMistag");
    }
    const artifact = await prepareProductionL2TxMistagArtifactV1({
      evidence,
      classification,
    });
    expect(artifact).toMatchObject({
      transactionIndex: Number(classification.selected.position),
      detectionId: `l2-tx-mistag:${classification.selected.position.toString()}:${artifact.nativeTxId}:1`,
    });
    await expect(
      admitProductionL2TxMistagArtifactV1(artifact),
    ).resolves.toEqual(expect.objectContaining({ artifact }));
  });

  it("rejects forged roots, proof bytes, detection positions, and transaction counts", async () => {
    const evidence = await fixtureEvidence();
    const replay =
      await L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    const classification = await classifyCanonicalBlockViolationsV1({
      evidence,
      detections: replay.detections,
    });
    if (!isL2TxMistagClassification(classification)) {
      throw new Error("fixture did not classify as l2TxMistag");
    }
    const artifact = await prepareProductionL2TxMistagArtifactV1({
      evidence,
      classification,
    });
    await expect(
      admitProductionL2TxMistagArtifactV1({
        ...artifact,
        transactionsPhasRoot: "ff".repeat(32),
      }),
    ).rejects.toThrow("does not open its PHAS root");
    await expect(
      admitProductionL2TxMistagArtifactV1({
        ...artifact,
        txMembershipProofCbor: "d87980",
      }),
    ).rejects.toThrow();
    await expect(
      admitProductionL2TxMistagArtifactV1({
        ...artifact,
        transactionIndex: artifact.transactionIndex === 0 ? 1 : 0,
      }),
    ).rejects.toThrow("changed its detection identity");
    await expect(
      admitProductionL2TxMistagArtifactV1({
        ...artifact,
        transactionCount: artifact.transactionCount + 1,
      }),
    ).rejects.toThrow("does not open the counted root");
  });
});
