import { createHash } from "node:crypto";

import {
  adjudicateMidgardNativeTxFullV1Validity,
  decodeMidgardFieldPreimageV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengthsV1,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  encodeMidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core/codec";
import { unwrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  canonicalBlockEvidenceFromVerifiedPayloadV1,
  type CanonicalBlockEvidenceV1,
} from "../evidence/canonical-block-evidence-v1.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetectionV1 } from "../workflow/classification-v1.js";
import {
  fieldPreimageLengthCommittedClaimV1,
  prepareAcceptedFieldPreimageLengthMismatchV1,
} from "./prepare-accepted-v1.js";
import type { FieldPreimageLengthProductionStageV1 } from "./production-config-v1.js";
import {
  type PreparedFieldPreimageLengthWorkflowV1,
  prepareFieldPreimageLengthWorkflowV1,
} from "./workflow-v1.js";

export const FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID_V1 =
  "field-preimage-length-mismatch" as const;

/** Complete canonical scan of forced wrongful-rejection contradictions. */
export const detectFieldPreimageLengthCompleteReplayV1 = (
  block: CanonicalBlockEvidenceV1,
): readonly CanonicalViolationDetectionV1[] => {
  const detections: CanonicalViolationDetectionV1[] = [];
  for (const [
    position,
    forced,
  ] of block.reconstruction.forcedTransactions.entries()) {
    if (forced.value.verdict === "ForcedTxValid") continue;
    const reason = forced.value.verdict.ForcedTxInvalid.reason;
    if (
      typeof reason === "string" ||
      !("FieldPreimageLengthMismatch" in reason)
    ) {
      continue;
    }
    const fieldIndex = Number(reason.FieldPreimageLengthMismatch.field_index);
    const adjudicated = adjudicateMidgardNativeTxFullV1Validity(
      decodeMidgardNativeTxFullV1FromCanonicalCbor(forced.fullTransactionCbor),
      "TxIsInvalid",
    );
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      encodeMidgardNativeTxCanonicalV1(adjudicated),
    );
    if (
      material.transactionId.toString("hex") !== forced.value.tx_id ||
      material.proofSource.compactCbor.toString("hex") !==
        forced.value.source.compact_cbor ||
      material.proofSource.witnessSetCompactCbor.toString("hex") !==
        forced.value.source.witness_set_compact_cbor
    ) {
      throw new Error(
        "fieldPreimageLengthMismatch forced preimage differs from its committed leaf",
      );
    }
    const preimage = material.fieldPreimages[fieldIndex];
    if (preimage === undefined) {
      throw new Error(
        "fieldPreimageLengthMismatch forced coordinate is absent",
      );
    }
    const declaredLength = decodeMidgardNativeTxProofFieldLengthsV1(
      Buffer.from(forced.value.source.field_preimage_lengths_cbor, "hex"),
    )[fieldIndex]!;
    // A truthful forced rejection is healthy for this family. Only equality
    // contradicts the operator's exact mismatch reason.
    if (declaredLength !== preimage.length) continue;
    prepareFieldPreimageLengthWorkflowV1({
      headerHash: block.headerHash,
      transactionId: forced.value.tx_id,
      direction: "wrongfulRejection",
      fieldIndex,
      fieldPreimageLengthsCbor: material.proofSource.fieldPreimageLengthsCbor,
      fieldPreimage: preimage,
      forcedRejectionReason: reason,
    });
    detections.push({
      detectionId: `${FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID_V1}:${position.toString()}:${forced.value.tx_id}:${fieldIndex.toString()}:wrongfulRejection`,
      headerHash: block.headerHash,
      violationId: FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID_V1,
      position: BigInt(position),
    });
  }
  return Object.freeze(detections);
};

export type AuthenticatedFieldPreimageLengthProductionEvidenceV1 = Readonly<{
  prepared: PreparedFieldPreimageLengthWorkflowV1;
  fieldMaterial: Readonly<{
    nativeTxCompactCbor: string;
    witnessSetCompactCbor: string;
    itemCbors: readonly string[];
  }>;
  stageEvidence: Omit<
    FieldPreimageLengthProductionStageV1,
    "fraudulentBlockOutRef" | "threadOutRef" | "cancelStepIndex"
  >;
}>;

export type RoutedFieldPreimageLengthProductionEvidenceV1 =
  AuthenticatedFieldPreimageLengthProductionEvidenceV1 &
    Readonly<{
      position: bigint;
      payloadEnvelopeSha256: string;
      payloadSha256: string;
    }>;

const inlineCarriage = (preimage: Uint8Array): SDK.FieldCarriageV1 => ({
  Inline: { preimage: Buffer.from(preimage).toString("hex") },
});

const canonicalData = <T>(
  cbor: string,
  schema: Parameters<typeof Data.Nullable>[0],
  label: string,
): T => {
  let decoded: T;
  try {
    decoded = Data.from(cbor, schema as never) as T;
  } catch (cause) {
    throw new Error(`${label} does not decode: ${String(cause)}`);
  }
  if (Data.to(decoded as never, schema as never) !== cbor) {
    throw new Error(`${label} is not canonical Data`);
  }
  return decoded;
};

const exactAcceptedRawFindingV1 = async ({
  observation,
  payloadEnvelopeCbor,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservationV1;
  readonly payloadEnvelopeCbor: Uint8Array;
}): Promise<RoutedFieldPreimageLengthProductionEvidenceV1> => {
  const payloadCbor = Buffer.from(
    (
      await unwrapDaPayloadV1(payloadEnvelopeCbor, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
      })
    ).innerBytes,
  );
  const payload = SDK.decodeDaPayloadV1(payloadCbor);
  if (!SDK.encodeDaPayloadV1(payload).equals(payloadCbor)) {
    throw new Error("fieldPreimageLengthMismatch DA payload is not canonical");
  }
  const body = payload.block_body;
  const embeddedHash = await Effect.runPromise(
    SDK.hashBlockHeaderV1(body.header),
  );
  if (
    embeddedHash !== body.header_hash ||
    embeddedHash !== observation.headerHash ||
    Data.to(body.header as never, SDK.HeaderV1 as never) !==
      Data.to(observation.header as never, SDK.HeaderV1 as never)
  ) {
    throw new Error(
      "fieldPreimageLengthMismatch retained DA changed the authenticated L1 header",
    );
  }

  const preimages = new Map<string, Buffer>();
  for (const [index, [key, value]] of body.transaction_preimages.entries()) {
    if (!/^[0-9a-f]{64}$/u.test(key) || preimages.has(key)) {
      throw new Error(
        `fieldPreimageLengthMismatch transaction_preimages[${index.toString()}] has a duplicate or non-canonical key`,
      );
    }
    preimages.set(key, Buffer.from(value, "hex"));
  }

  const findings: {
    readonly position: bigint;
    readonly transactionId: string;
    readonly fieldIndex: number;
    readonly canonicalTransactionCbor: Buffer;
  }[] = [];
  for (const [index, [key, valueCbor]] of body.transactions.entries()) {
    const source = canonicalData<SDK.L2TransactionSourceV1>(
      valueCbor,
      SDK.L2TransactionSourceV1Schema,
      `fieldPreimageLengthMismatch transactions[${index.toString()}]`,
    );
    const canonicalTransactionCbor = preimages.get(key);
    if (canonicalTransactionCbor === undefined) {
      throw new Error(
        `fieldPreimageLengthMismatch transaction_preimages omitted ${key}`,
      );
    }
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      canonicalTransactionCbor,
    );
    if (
      source.tx_id !== key ||
      material.transactionId.toString("hex") !== key ||
      source.source.compact_cbor !==
        material.proofSource.compactCbor.toString("hex") ||
      source.source.witness_set_compact_cbor !==
        material.proofSource.witnessSetCompactCbor.toString("hex")
    ) {
      throw new Error(
        `fieldPreimageLengthMismatch transactions[${index.toString()}] differs outside its field-length vector`,
      );
    }
    const declared = decodeMidgardNativeTxProofFieldLengthsV1(
      Buffer.from(source.source.field_preimage_lengths_cbor, "hex"),
    );
    const canonical = decodeMidgardNativeTxProofFieldLengthsV1(
      material.proofSource.fieldPreimageLengthsCbor,
    );
    for (let fieldIndex = 0; fieldIndex < canonical.length; fieldIndex += 1) {
      if (declared[fieldIndex] !== canonical[fieldIndex]) {
        findings.push({
          position: BigInt(index),
          transactionId: key,
          fieldIndex,
          canonicalTransactionCbor,
        });
      }
    }
  }
  if (preimages.size !== body.transactions.length) {
    throw new Error(
      "fieldPreimageLengthMismatch transaction_preimages contains an uncommitted preimage",
    );
  }
  if (findings.length !== 1) {
    throw new Error(
      `fieldPreimageLengthMismatch retained raw source yielded ${findings.length.toString()} exact accepted findings`,
    );
  }
  const finding = findings[0]!;
  const direct = await prepareAcceptedFieldPreimageLengthMismatchV1({
    headerHash: observation.headerHash,
    committedTransactionsRoot: observation.header.transactionsRoot,
    l2TransactionCount: observation.header.l2TransactionCount,
    entries: body.transactions,
    transactionId: finding.transactionId,
    canonicalTransactionCbor: finding.canonicalTransactionCbor,
    fieldIndex: finding.fieldIndex,
    deferNonInlineClaim: true,
  });
  const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
    finding.canonicalTransactionCbor,
  );
  const fieldPreimage = material.fieldPreimages[finding.fieldIndex]!;
  return Object.freeze({
    prepared: direct.prepared,
    position: finding.position,
    payloadEnvelopeSha256:
      computeDaSha256Hash(payloadEnvelopeCbor).toString("hex"),
    payloadSha256: computeDaSha256Hash(payloadCbor).toString("hex"),
    fieldMaterial: Object.freeze({
      nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
      witnessSetCompactCbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
      itemCbors: Object.freeze(
        decodeMidgardFieldPreimageV1(fieldPreimage).map((item) =>
          item.toString("hex"),
        ),
      ),
    }),
    stageEvidence: Object.freeze({
      acceptedInclusion: direct.inclusion,
      ...(direct.claim === null ? {} : { acceptedClaim: direct.claim }),
    }),
  });
};

const exactForcedFindingV1 = async (
  block: Awaited<
    ReturnType<typeof canonicalBlockEvidenceFromVerifiedPayloadV1>
  >,
): Promise<AuthenticatedFieldPreimageLengthProductionEvidenceV1> => {
  const findings: AuthenticatedFieldPreimageLengthProductionEvidenceV1[] = [];
  for (const forced of block.reconstruction.forcedTransactions) {
    if (forced.value.verdict === "ForcedTxValid") continue;
    const reason = forced.value.verdict.ForcedTxInvalid.reason;
    if (
      typeof reason === "string" ||
      !("FieldPreimageLengthMismatch" in reason)
    ) {
      continue;
    }
    const fieldIndex = Number(reason.FieldPreimageLengthMismatch.field_index);
    const adjudicated = adjudicateMidgardNativeTxFullV1Validity(
      decodeMidgardNativeTxFullV1FromCanonicalCbor(forced.fullTransactionCbor),
      "TxIsInvalid",
    );
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      encodeMidgardNativeTxCanonicalV1(adjudicated),
    );
    if (
      material.transactionId.toString("hex") !== forced.value.tx_id ||
      material.proofSource.compactCbor.toString("hex") !==
        forced.value.source.compact_cbor ||
      material.proofSource.witnessSetCompactCbor.toString("hex") !==
        forced.value.source.witness_set_compact_cbor
    ) {
      throw new Error(
        "fieldPreimageLengthMismatch forced preimage differs from its committed leaf",
      );
    }
    const preimage = material.fieldPreimages[fieldIndex];
    if (preimage === undefined) {
      throw new Error(
        "fieldPreimageLengthMismatch forced coordinate is absent",
      );
    }
    const basePrepared = prepareFieldPreimageLengthWorkflowV1({
      headerHash: block.headerHash,
      transactionId: forced.value.tx_id,
      direction: "wrongfulRejection",
      fieldIndex,
      fieldPreimageLengthsCbor: Buffer.from(
        forced.value.source.field_preimage_lengths_cbor,
        "hex",
      ),
      fieldPreimage: preimage,
      forcedRejectionReason: reason,
    });
    const prepared: PreparedFieldPreimageLengthWorkflowV1 = Object.freeze({
      ...basePrepared,
      evidenceDigest: createHash("sha256")
        .update(basePrepared.evidenceDigest, "hex")
        .update(
          Data.to(forced.key as never, SDK.OutputReferenceSchema as never),
          "hex",
        )
        .update(
          Data.to(reason as never, SDK.RejectionReasonV1Schema as never),
          "hex",
        )
        .digest("hex"),
    });
    const eventKey: SDK.EventKey = {
      ForcedTransactionEventKey: { tx_order_id: forced.key },
    };
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey,
    });
    findings.push(
      Object.freeze({
        prepared,
        fieldMaterial: Object.freeze({
          nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
          witnessSetCompactCbor:
            material.proofSource.witnessSetCompactCbor.toString("hex"),
          itemCbors: Object.freeze(
            decodeMidgardFieldPreimageV1(preimage).map((item) =>
              item.toString("hex"),
            ),
          ),
        }),
        stageEvidence: Object.freeze({
          forcedDirection: 1n,
          forcedHeader: block.header,
          forcedMembership: membership,
          ...(prepared.carriage === "Inline"
            ? {
                forcedClaim: fieldPreimageLengthCommittedClaimV1({
                  fieldIndex,
                  witnessSetCompactCbor:
                    material.proofSource.witnessSetCompactCbor,
                  carriage: inlineCarriage(preimage),
                }),
              }
            : {}),
        }),
      }),
    );
  }
  if (findings.length !== 1) {
    throw new Error(
      `fieldPreimageLengthMismatch canonical retained DA yielded ${findings.length.toString()} exact forced findings`,
    );
  }
  return findings[0]!;
};

/**
 * Reopens the exact already-fetched retained-DA envelope after the canonical
 * reconstructor reported the narrowly typed field-length source mismatch.
 * The caller must have authenticated the observation and DA provenance first.
 */
export const fieldPreimageLengthProductionEvidenceFromVerifiedPayloadV1 =
  exactAcceptedRawFindingV1;

/**
 * Package-owned production classifier. It first admits the full canonical
 * block. Only the exact source/preimage authentication failure may fall back
 * to the raw transactions-root branch; every other reconstruction failure is
 * preserved as a rejection.
 */
export const detectAuthenticatedFieldPreimageLengthProductionEvidenceV1 =
  async ({
    observation,
    sources,
  }: {
    readonly observation: SDK.AuthenticatedStateQueueHeaderObservationV1;
    readonly sources: readonly RetainedDaPayloadSource[];
  }): Promise<AuthenticatedFieldPreimageLengthProductionEvidenceV1> => {
    const admitted = await SDK.admitAuthenticatedStateQueueHeaderObservationV1({
      observation,
    });
    const fetched = await fetchRetainedDaPayloadByHeaderHash({
      headerHash: admitted.headerHash,
      sources,
    });
    const provenance = SDK.assertSecurityGradeEvidenceV1(
      SDK.admitEvidenceProvenanceV1({ provenance: fetched.provenance }),
    );
    try {
      return await exactForcedFindingV1(
        await canonicalBlockEvidenceFromVerifiedPayloadV1({
          observation: admitted,
          payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
          daProvenance: provenance,
        }),
      );
    } catch (cause) {
      if (
        !(cause instanceof Error) ||
        cause.name !== "TransitionTraceChallengerError" ||
        !cause.message.startsWith("Failed to authenticate transactions[")
      ) {
        throw cause;
      }
      return await exactAcceptedRawFindingV1({
        observation: admitted,
        payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
      });
    }
  };
