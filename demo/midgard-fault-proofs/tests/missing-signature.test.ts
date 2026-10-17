import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { encodeData } from "../src/transition-trace/reconstruct.js";
import {
  type CanonicalBlockClassification,
  classifyCanonicalBlockViolations,
} from "../src/workflow/classification.js";
import { MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  admitMissingSignatureArtifact,
  type MissingSignatureArtifact,
  prepareMissingSignatureArtifact,
} from "../src/workflow/missing-signature.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  type FixtureTransaction,
  h32,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

const TARGET_VKEY = "11".repeat(32);
const TARGET_HASH = SDK.missingSignatureVkeyHash(TARGET_VKEY);

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
}): FixtureTransaction => {
  const full = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
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
  const canonicalCbor = encodeMidgardNativeTxCanonical(full);
  const proofSource =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
  const source: SDK.L2TransactionSource = {
    tx_id: computeMidgardNativeTxId(full).toString("hex"),
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
    sourceValueBytes: encodeData(source, SDK.L2TransactionSourceSchema),
  };
};

const canonicalEvidence = async (
  transactions: readonly FixtureTransaction[],
) => {
  const fixture = await buildCanonicalBlockFixture({ transactions });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
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

type MissingSignatureClassification = Extract<
  CanonicalBlockClassification,
  { readonly decision: "fault_detected" }
> & { readonly category: "missingSignature" };

const requireMissingSignatureClassification = (
  value: CanonicalBlockClassification,
): MissingSignatureClassification => {
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
      await MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    expect(decision.detections).toHaveLength(1);
    expect(decision.detections[0]).toMatchObject({
      violationId: SDK.MISSING_SIGNATURE_VIOLATION_ID,
    });
    const classification = requireMissingSignatureClassification(
      await classifyCanonicalBlockViolations({
        evidence,
        detections: decision.detections,
      }),
    );
    expect(classification).toMatchObject({
      decision: "fault_detected",
      category: "missingSignature",
    });
    const artifact = await prepareMissingSignatureArtifact({
      evidence,
      classification,
    });
    const admitted = await admitMissingSignatureArtifact(artifact);
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
      await MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    const classification = requireMissingSignatureClassification(
      await classifyCanonicalBlockViolations({
        evidence,
        detections: decision.detections,
      }),
    );
    const artifact = await prepareMissingSignatureArtifact({
      evidence,
      classification,
    });
    const poisoned: MissingSignatureArtifact = {
      ...artifact,
      resolvedVkey: h32(0x22),
    };
    await expect(admitMissingSignatureArtifact(poisoned)).rejects.toThrow(
      "not the deterministic committed public preimage",
    );
  });

  it("routes an unknown public vkey preimage away from the direct family", async () => {
    const evidence = await canonicalEvidence([missing()]);
    const decision =
      await MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    const classification = requireMissingSignatureClassification(
      await classifyCanonicalBlockViolations({
        evidence,
        detections: decision.detections,
      }),
    );
    await expect(
      prepareMissingSignatureArtifact({
        evidence,
        classification,
      }),
    ).rejects.toThrow("requires validationTraceDispute");
  });

  it("rejects a durable transaction/source substitution before proof construction", async () => {
    const evidence = await canonicalEvidence([missing(), publicVkeySource()]);
    const decision =
      await MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    const classification = requireMissingSignatureClassification(
      await classifyCanonicalBlockViolations({
        evidence,
        detections: decision.detections,
      }),
    );
    const artifact = await prepareMissingSignatureArtifact({
      evidence,
      classification,
    });
    const first = artifact.transactions[0]!;
    const poisoned: MissingSignatureArtifact = {
      ...artifact,
      transactions: [
        { ...first, l2TransactionSourceCbor: "80" },
        ...artifact.transactions.slice(1),
      ],
    };
    await expect(admitMissingSignatureArtifact(poisoned)).rejects.toThrow();
  });
});
