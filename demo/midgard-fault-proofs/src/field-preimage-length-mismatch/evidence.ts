import { createHash } from "node:crypto";

import {
  adjudicateMidgardNativeTxFullValidity,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengths,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../evidence/canonical-block-evidence.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import type { FieldPreimageLengthStage } from "./config.js";
import {
  fieldPreimageLengthCommittedClaim,
  prepareAcceptedFieldPreimageLengthMismatch,
} from "./prepare-accepted.js";
import {
  type PreparedFieldPreimageLengthWorkflow,
  prepareFieldPreimageLengthWorkflow,
} from "./workflow.js";

export const FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID =
  "field-preimage-length-mismatch" as const;

/** Complete canonical scan of forced wrongful-rejection contradictions. */
export const detectFieldPreimageLengthCompleteReplay = (
  block: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] => {
  const detections: CanonicalViolationDetection[] = [];
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
    const adjudicated = adjudicateMidgardNativeTxFullValidity(
      decodeMidgardNativeTxFullFromCanonicalCbor(forced.fullTransactionCbor),
      "TxIsInvalid",
    );
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      encodeMidgardNativeTxCanonical(adjudicated),
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
    const declaredLength = decodeMidgardNativeTxProofFieldLengths(
      Buffer.from(forced.value.source.field_preimage_lengths_cbor, "hex"),
    )[fieldIndex]!;
    // A truthful forced rejection is healthy for this family. Only equality
    // contradicts the operator's exact mismatch reason.
    if (declaredLength !== preimage.length) continue;
    prepareFieldPreimageLengthWorkflow({
      headerHash: block.headerHash,
      transactionId: forced.value.tx_id,
      direction: "wrongfulRejection",
      fieldIndex,
      fieldPreimageLengthsCbor: material.proofSource.fieldPreimageLengthsCbor,
      fieldPreimage: preimage,
      forcedRejectionReason: reason,
    });
    detections.push({
      detectionId: `${FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID}:${position.toString()}:${forced.value.tx_id}:${fieldIndex.toString()}:wrongfulRejection`,
      headerHash: block.headerHash,
      violationId: FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID,
      position: BigInt(position),
    });
  }
  return Object.freeze(detections);
};

export type AuthenticatedFieldPreimageLengthEvidence = Readonly<{
  prepared: PreparedFieldPreimageLengthWorkflow;
  fieldMaterial: Readonly<{
    nativeTxCompactCbor: string;
    witnessSetCompactCbor: string;
    itemCbors: readonly string[];
  }>;
  stageEvidence: Omit<
    FieldPreimageLengthStage,
    "fraudulentBlockOutRef" | "threadOutRef" | "cancelStepIndex"
  >;
}>;

export type RoutedFieldPreimageLengthEvidence =
  AuthenticatedFieldPreimageLengthEvidence &
    Readonly<{
      position: bigint;
      payloadEnvelopeSha256: string;
      payloadSha256: string;
    }>;

const inlineCarriage = (preimage: Uint8Array): SDK.FieldCarriage => ({
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

const exactAcceptedRawFinding = async ({
  observation,
  payloadEnvelopeCbor,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
}): Promise<RoutedFieldPreimageLengthEvidence> => {
  const payloadCbor = Buffer.from(
    (
      await unwrapDaPayload(payloadEnvelopeCbor, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      })
    ).innerBytes,
  );
  const payload = SDK.decodeDaPayload(payloadCbor);
  if (!SDK.encodeDaPayload(payload).equals(payloadCbor)) {
    throw new Error("fieldPreimageLengthMismatch DA payload is not canonical");
  }
  const body = payload.block_body;
  const embeddedHash = await Effect.runPromise(
    SDK.hashBlockHeader(body.header),
  );
  if (
    embeddedHash !== body.header_hash ||
    embeddedHash !== observation.headerHash ||
    Data.to(body.header as never, SDK.Header as never) !==
      Data.to(observation.header as never, SDK.Header as never)
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
    const source = canonicalData<SDK.L2TransactionSource>(
      valueCbor,
      SDK.L2TransactionSourceSchema,
      `fieldPreimageLengthMismatch transactions[${index.toString()}]`,
    );
    const canonicalTransactionCbor = preimages.get(key);
    if (canonicalTransactionCbor === undefined) {
      throw new Error(
        `fieldPreimageLengthMismatch transaction_preimages omitted ${key}`,
      );
    }
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
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
    const declared = decodeMidgardNativeTxProofFieldLengths(
      Buffer.from(source.source.field_preimage_lengths_cbor, "hex"),
    );
    const canonical = decodeMidgardNativeTxProofFieldLengths(
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
  const direct = await prepareAcceptedFieldPreimageLengthMismatch({
    headerHash: observation.headerHash,
    committedTransactionsRoot: observation.header.transactionsRoot,
    l2TransactionCount: observation.header.l2TransactionCount,
    entries: body.transactions,
    transactionId: finding.transactionId,
    canonicalTransactionCbor: finding.canonicalTransactionCbor,
    fieldIndex: finding.fieldIndex,
    deferNonInlineClaim: true,
  });
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
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
        decodeMidgardFieldPreimage(fieldPreimage).map((item) =>
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

const exactForcedFinding = async (
  block: Awaited<ReturnType<typeof canonicalBlockEvidenceFromVerifiedPayload>>,
): Promise<AuthenticatedFieldPreimageLengthEvidence> => {
  const findings: AuthenticatedFieldPreimageLengthEvidence[] = [];
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
    const adjudicated = adjudicateMidgardNativeTxFullValidity(
      decodeMidgardNativeTxFullFromCanonicalCbor(forced.fullTransactionCbor),
      "TxIsInvalid",
    );
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      encodeMidgardNativeTxCanonical(adjudicated),
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
    const basePrepared = prepareFieldPreimageLengthWorkflow({
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
    const prepared: PreparedFieldPreimageLengthWorkflow = Object.freeze({
      ...basePrepared,
      evidenceDigest: createHash("sha256")
        .update(basePrepared.evidenceDigest, "hex")
        .update(
          Data.to(forced.key as never, SDK.OutputReferenceSchema as never),
          "hex",
        )
        .update(
          Data.to(reason as never, SDK.RejectionReasonSchema as never),
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
            decodeMidgardFieldPreimage(preimage).map((item) =>
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
                forcedClaim: fieldPreimageLengthCommittedClaim({
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
export const fieldPreimageLengthEvidenceFromVerifiedPayload =
  exactAcceptedRawFinding;

/**
 * Package-owned production classifier. It first admits the full canonical
 * block. Only the exact source/preimage authentication failure may fall back
 * to the raw transactions-root branch; every other reconstruction failure is
 * preserved as a rejection.
 */
export const detectAuthenticatedFieldPreimageLengthEvidence = async ({
  observation,
  sources,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
}): Promise<AuthenticatedFieldPreimageLengthEvidence> => {
  const admitted = await SDK.admitAuthenticatedStateQueueHeaderObservation({
    observation,
  });
  const fetched = await fetchRetainedDaPayloadByHeaderHash({
    headerHash: admitted.headerHash,
    sources,
  });
  const provenance = SDK.assertSecurityGradeEvidence(
    SDK.admitEvidenceProvenance({ provenance: fetched.provenance }),
  );
  try {
    return await exactForcedFinding(
      await canonicalBlockEvidenceFromVerifiedPayload({
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
    return await exactAcceptedRawFinding({
      observation: admitted,
      payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    });
  }
};
