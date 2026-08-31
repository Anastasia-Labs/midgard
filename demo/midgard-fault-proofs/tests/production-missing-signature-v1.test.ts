import {
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import { encodeData } from "../src/transition-trace/reconstruct.js";
import {
  type CanonicalBlockClassificationV1,
  classifyCanonicalBlockViolationsV1,
} from "../src/workflow/classification-v1.js";
import { MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1 } from "../src/workflow/complete-replay-v1.js";
import {
  admitProductionMissingSignatureArtifactV1,
  prepareProductionMissingSignatureArtifactV1,
  type ProductionMissingSignatureArtifactV1,
} from "../src/workflow/production-missing-signature-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  type FixtureTransactionV1,
  h32,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const TARGET_VKEY = "11".repeat(32);
const TARGET_HASH = SDK.missingSignatureVkeyHashV1(TARGET_VKEY);

const transaction = ({
  inputByte,
  fee,
  requiredSignerHashes = [],
  witnesses = [],
}: {
  readonly inputByte: number;
  readonly fee: bigint;
  readonly requiredSignerHashes?: readonly string[];
  readonly witnesses?: readonly SDK.MidgardAddressWitness[];
}): FixtureTransactionV1 => {
  const full = materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([outRefCbor(inputByte, 0n)]),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: encodeCbor(
        requiredSignerHashes.map((hash) => Buffer.from(hash, "hex")),
      ),
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: SDK.encodeAddressWitnessPreimage(witnesses),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  const canonicalCbor = encodeMidgardNativeTxCanonicalV1(full);
  const proofSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(canonicalCbor);
  const source: SDK.L2TransactionSourceV1 = {
    tx_id: computeMidgardNativeTxIdV1(full).toString("hex"),
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return {
    txId: source.tx_id,
    canonicalCbor,
    compactCbor: Buffer.from(proofSource.compactCbor),
    source,
    sourceValueBytes: encodeData(source, SDK.L2TransactionSourceV1Schema),
  };
};

const canonicalEvidence = async (
  transactions: readonly FixtureTransactionV1[],
) => {
  const fixture = await buildCanonicalBlockFixtureV1({ transactions });
  return await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/missing-signature-test",
      grade: "security",
    },
  });
};

const missing = () =>
  transaction({
    inputByte: 0x31,
    fee: 1n,
    requiredSignerHashes: [TARGET_HASH],
  });

const publicVkeySource = () =>
  transaction({
    inputByte: 0x32,
    fee: 2n,
    witnesses: [
      {
        verification_key: TARGET_VKEY,
        signature: "aa".repeat(64),
      },
    ],
  });

type MissingSignatureClassificationV1 = Extract<
  CanonicalBlockClassificationV1,
  { readonly decision: "fault_detected" }
> & { readonly category: "missingSignature" };

const requireMissingSignatureClassification = (
  value: CanonicalBlockClassificationV1,
): MissingSignatureClassificationV1 => {
  if (
    value.decision !== "fault_detected" ||
    value.category !== "missingSignature"
  ) {
    throw new Error("fixture did not classify as missingSignature");
  }
  return { ...value, category: "missingSignature" };
};

describe("production missing-signature workflow evidence", () => {
  it("replays every accepted transaction and re-derives the exact public proof artifact", async () => {
    const evidence = await canonicalEvidence([missing(), publicVkeySource()]);
    const decision =
      await MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    expect(decision.detections).toHaveLength(1);
    expect(decision.detections[0]).toMatchObject({
      violationId: SDK.MISSING_SIGNATURE_VIOLATION_ID_V1,
    });
    const classification = requireMissingSignatureClassification(
      await classifyCanonicalBlockViolationsV1({
        evidence,
        detections: decision.detections,
      }),
    );
    expect(classification).toMatchObject({
      decision: "fault_detected",
      category: "missingSignature",
    });
    const artifact = await prepareProductionMissingSignatureArtifactV1({
      evidence,
      classification,
    });
    const admitted = await admitProductionMissingSignatureArtifactV1(artifact);
    expect(admitted.resolvedVkey).toBe(TARGET_VKEY);
    expect(admitted.requiredSignerHashes).toEqual([TARGET_HASH]);
    expect(admitted.addrTxWits).toEqual([]);
    expect(admitted.txInclusion.nativeTxId).toBe(
      decision.detections[0]!.detectionId.split(":")[3],
    );
  });

  it("refuses an operator-substituted vkey even when it is well formed", async () => {
    const evidence = await canonicalEvidence([missing(), publicVkeySource()]);
    const decision =
      await MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    const classification = requireMissingSignatureClassification(
      await classifyCanonicalBlockViolationsV1({
        evidence,
        detections: decision.detections,
      }),
    );
    const artifact = await prepareProductionMissingSignatureArtifactV1({
      evidence,
      classification,
    });
    const poisoned: ProductionMissingSignatureArtifactV1 = {
      ...artifact,
      resolvedVkey: h32(0x22),
    };
    await expect(
      admitProductionMissingSignatureArtifactV1(poisoned),
    ).rejects.toThrow("not the deterministic committed public preimage");
  });

  it("routes an unknown public vkey preimage away from the direct family", async () => {
    const evidence = await canonicalEvidence([missing()]);
    const decision =
      await MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    const classification = requireMissingSignatureClassification(
      await classifyCanonicalBlockViolationsV1({
        evidence,
        detections: decision.detections,
      }),
    );
    await expect(
      prepareProductionMissingSignatureArtifactV1({
        evidence,
        classification,
      }),
    ).rejects.toThrow("requires validationTraceDispute");
  });

  it("rejects a durable transaction/source substitution before proof construction", async () => {
    const evidence = await canonicalEvidence([missing(), publicVkeySource()]);
    const decision =
      await MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    const classification = requireMissingSignatureClassification(
      await classifyCanonicalBlockViolationsV1({
        evidence,
        detections: decision.detections,
      }),
    );
    const artifact = await prepareProductionMissingSignatureArtifactV1({
      evidence,
      classification,
    });
    const first = artifact.transactions[0]!;
    const poisoned: ProductionMissingSignatureArtifactV1 = {
      ...artifact,
      transactions: [
        { ...first, l2TransactionSourceCbor: "80" },
        ...artifact.transactions.slice(1),
      ],
    };
    await expect(
      admitProductionMissingSignatureArtifactV1(poisoned),
    ).rejects.toThrow();
  });
});
