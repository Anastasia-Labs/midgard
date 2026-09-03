import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import {
  type L2TransactionSource,
  L2TransactionSource as L2TransactionSourceCodec,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence-v1.js";
import type { CanonicalBlockClassification } from "../src/workflow/classification-v1.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification-v1.js";
import { L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay-v1.js";
import {
  admitL2TxMistagArtifact,
  prepareL2TxMistagArtifact,
} from "../src/workflow/production-l2-tx-mistag-v1.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  type FixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const mistaggedTransaction = (): FixtureTransaction => {
  const full = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
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
  const canonicalCbor = encodeMidgardNativeTxCanonical(full);
  const proofSource =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
  const txId = computeMidgardNativeTxId(full).toString("hex");
  const source: L2TransactionSource = {
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
      Data.to(source, L2TransactionSourceCodec),
      "hex",
    ),
  };
};

const isL2TxMistagClassification = (
  classification: CanonicalBlockClassification,
): classification is Extract<
  CanonicalBlockClassification,
  { readonly decision: "fault_detected" }
> & { readonly category: "l2TxMistag" } =>
  classification.decision === "fault_detected" &&
  classification.category === "l2TxMistag";

const fixtureEvidence = async () => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(4, 0n)],
        fee: 1n,
      }),
      mistaggedTransaction(),
    ],
  });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
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
      await L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: replay.detections,
    });
    if (!isL2TxMistagClassification(classification)) {
      throw new Error("fixture did not classify as l2TxMistag");
    }
    const artifact = await prepareL2TxMistagArtifact({
      evidence,
      classification,
    });
    expect(artifact).toMatchObject({
      transactionIndex: Number(classification.selected.position),
      detectionId: `l2-tx-mistag:${classification.selected.position.toString()}:${artifact.nativeTxId}:1`,
    });
    await expect(admitL2TxMistagArtifact(artifact)).resolves.toEqual(
      expect.objectContaining({ artifact }),
    );
  });

  it("rejects forged roots, proof bytes, detection positions, and transaction counts", async () => {
    const evidence = await fixtureEvidence();
    const replay =
      await L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: replay.detections,
    });
    if (!isL2TxMistagClassification(classification)) {
      throw new Error("fixture did not classify as l2TxMistag");
    }
    const artifact = await prepareL2TxMistagArtifact({
      evidence,
      classification,
    });
    await expect(
      admitL2TxMistagArtifact({
        ...artifact,
        transactionsPhasRoot: "ff".repeat(32),
      }),
    ).rejects.toThrow("does not open its PHAS root");
    await expect(
      admitL2TxMistagArtifact({
        ...artifact,
        txMembershipProofCbor: "d87980",
      }),
    ).rejects.toThrow();
    await expect(
      admitL2TxMistagArtifact({
        ...artifact,
        transactionIndex: artifact.transactionIndex === 0 ? 1 : 0,
      }),
    ).rejects.toThrow("changed its detection identity");
    await expect(
      admitL2TxMistagArtifact({
        ...artifact,
        transactionCount: artifact.transactionCount + 1,
      }),
    ).rejects.toThrow("does not open the counted root");
  });
});
