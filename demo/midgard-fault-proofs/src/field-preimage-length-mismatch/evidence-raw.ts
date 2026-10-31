import {
  decodeMidgardNativeTxProofFieldLengths,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core/codec";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { RoutedFieldPreimageLengthEvidence } from "./evidence.js";
import { forcedFieldPreimageLengthRawFinding } from "./evidence-forced.js";
import { prepareAcceptedFieldPreimageLengthMismatch } from "./prepare-accepted.js";

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

export const exactFieldPreimageLengthRawFinding = async ({
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

  let forcedFailure: unknown;
  try {
    const forced = await forcedFieldPreimageLengthRawFinding({
      headerHash: observation.headerHash,
      header: observation.header,
      body,
    });
    if (forced !== undefined)
      return Object.freeze({
        ...forced,
        payloadEnvelopeSha256:
          computeDaSha256Hash(payloadEnvelopeCbor).toString("hex"),
        payloadSha256: computeDaSha256Hash(payloadCbor).toString("hex"),
      });
  } catch (cause) {
    forcedFailure = cause;
  }

  try {
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
    if (findings.length === 0) {
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
      }),
      stageEvidence: Object.freeze({
        acceptedInclusion: direct.inclusion,
        ...(direct.claim === null ? {} : { acceptedClaim: direct.claim }),
      }),
    });
  } catch (normalFailure) {
    if (forcedFailure !== undefined)
      throw new AggregateError(
        [forcedFailure, normalFailure],
        "Neither authenticated field-length source branch yielded a provable finding",
        { cause: normalFailure },
      );
    throw normalFailure;
  }
};
