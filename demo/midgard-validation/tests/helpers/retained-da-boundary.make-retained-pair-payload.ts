import { appendFileSync } from "node:fs";
import { isAbsolute } from "node:path";

import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

const ZERO_HASH_28 = "00".repeat(28);

const EMPTY_ROOT = SDK.EMPTY_MERKLE_TREE_ROOT;

export type RetainedDaAdmission =
  | "required"
  | "diagnostic-synthetic-script-witnesses";

export const appendBoundaryCorpusEntry = ({
  corpusLabel,
  productionAdmission: admission,
  transactionIdHex,
  transactionCommitmentHex,
  canonicalTransactionCbor,
  canonicalMaterialSidecarCbor,
  sourceRawScriptAuditHash,
  resolvedReferenceUtxos,
}: {
  readonly corpusLabel: string | undefined;
  readonly productionAdmission: RetainedDaAdmission;
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly canonicalTransactionCbor: Buffer;
  readonly canonicalMaterialSidecarCbor: Uint8Array | undefined;
  readonly sourceRawScriptAuditHash: string | undefined;
  readonly resolvedReferenceUtxos: readonly SDK.DaPayloadEntry[] | undefined;
}): void => {
  const corpusPath = process.env.MIDGARD_BOUNDARY_CORPUS_JSONL;
  if (corpusPath === undefined || corpusLabel === undefined) {
    return;
  }
  if (!isAbsolute(corpusPath)) {
    throw new Error("MIDGARD_BOUNDARY_CORPUS_JSONL must be an absolute path");
  }
  appendFileSync(
    corpusPath,
    `${JSON.stringify({
      label: corpusLabel,
      productionAdmission: admission,
      transactionIdHex,
      transactionCommitmentHex,
      canonicalCborHex: canonicalTransactionCbor.toString("hex"),
      ...(canonicalMaterialSidecarCbor === undefined
        ? {}
        : {
            canonicalMaterialSidecarCborHex: Buffer.from(
              canonicalMaterialSidecarCbor,
            ).toString("hex"),
          }),
      ...(sourceRawScriptAuditHash === undefined
        ? {}
        : { sourceRawScriptAuditHash }),
      ...(resolvedReferenceUtxos === undefined
        ? {}
        : { resolvedReferenceUtxos }),
    })}\n`,
    "utf8",
  );
};

export type RetainedClassificationMeasurement = {
  readonly sourceKind: "normal" | "forced";
  readonly retainedPreimageBytes: number;
  readonly revealStepCount: number;
  readonly reconstructedCanonicalBytes: number;
  /**
   * Blake2b-256 digests of the retained preimage and of the transaction
   * rebuilt by the terminal fold. Byte equality is already enforced inside
   * this harness; exposing both digests lets a boundary case assert canonical
   * signed-byte identity explicitly instead of only counting bytes.
   */
  readonly retainedPreimageDigestHex: string;
  readonly reconstructedCanonicalDigestHex: string;
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
};

export type RetainedDaBoundaryMeasurement = {
  readonly forcedTransactionCommitmentHex: string;
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly innerPayloadBytes: number;
  readonly storedPayloadBytes: number;
  readonly normal: RetainedClassificationMeasurement;
  readonly forced: RetainedClassificationMeasurement;
};

export type RetainedDaCanonicalScriptProjection = {
  readonly canonicalTransactionCbor: Buffer;
  readonly canonicalMaterialSidecarCbor: Buffer;
  readonly sourceRawScriptAuditHash: string;
};

const sourceValueHex = (source: SDK.L2TransactionSource): string =>
  aikenSerialisedPlutusDataCbor(
    Data.to(source as never, SDK.L2TransactionSourceSchema as never),
  );

const forcedSourceValueHex = (source: SDK.ForcedInclusionTxV1): string =>
  aikenSerialisedPlutusDataCbor(
    Data.to(
      {
        ...source,
        verdict: "ForcedTxValid",
      } as never,
      SDK.ForcedInclusionTxV1Schema as never,
    ),
  );

export const makeRetainedPairPayload = ({
  transactionIdHex,
  forcedOrderIdHex,
  transactionCborHex,
  forcedTransactionCborHex,
  forcedSource,
  source,
}: {
  readonly transactionIdHex: string;
  readonly forcedOrderIdHex: string;
  readonly transactionCborHex: string;
  readonly forcedTransactionCborHex: string;
  readonly forcedSource: SDK.ForcedInclusionTxV1;
  readonly source: SDK.L2TransactionSource;
}): SDK.DaPayload => {
  const counts: SDK.DaPayloadCounts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 1n,
    depositCount: 0n,
    totalEventCount: 2n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };
  const header: SDK.Header = {
    prevUtxosRoot: EMPTY_ROOT,
    utxosRoot: EMPTY_ROOT,
    withdrawalsRoot: EMPTY_ROOT,
    forcedTransactionsRoot: EMPTY_ROOT,
    transactionsRoot: EMPTY_ROOT,
    depositsRoot: EMPTY_ROOT,
    transitionTraceRoot: EMPTY_ROOT,
    eventToStepRoot: EMPTY_ROOT,
    validationTracesRoot: EMPTY_ROOT,
    withdrawalCount: counts.withdrawalCount,
    forcedTransactionCount: counts.forcedTransactionCount,
    l2TransactionCount: counts.l2TransactionCount,
    depositCount: counts.depositCount,
    totalEventCount: counts.totalEventCount,
    transitionStepCount: counts.transitionStepCount,
    validationTraceCount: counts.validationTraceCount,
    startTime: 0n,
    endTime: 1n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: ZERO_HASH_28,
    operatorVkey: ZERO_HASH_28,
    protocolVersion: 1n,
  };
  return {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: ZERO_HASH_28,
      header,
      utxos: [],
      withdrawals: [],
      forced_transactions: [
        [forcedOrderIdHex, forcedSourceValueHex(forcedSource)],
      ],
      transactions: [[transactionIdHex, sourceValueHex(source)]],
      transaction_preimages: [[transactionIdHex, transactionCborHex]],
      forced_transaction_preimages: [
        [forcedOrderIdHex, forcedTransactionCborHex],
      ],
      cek_program_material: [],
      deposits: [],
      transition_trace: [],
      event_to_step: [],
      validation_traces: [],
      validation_trace_witnesses: [],
      counts,
    },
  };
};

export const requireEntry = (
  entries: readonly SDK.DaPayloadEntry[],
  keyHex: string,
  fieldName: string,
): SDK.DaPayloadEntry => {
  const matches = entries.filter(([entryKeyHex]) => entryKeyHex === keyHex);
  if (matches.length !== 1) {
    throw new Error(`${fieldName} must retain exactly one entry for ${keyHex}`);
  }
  return matches[0]!;
};

export const assertSourceMatches = ({
  retainedSource,
  transactionIdHex,
  compactCborHex,
  witnessSetCompactCborHex,
  fieldPreimageLengthsCborHex,
  fieldName,
}: {
  readonly retainedSource: SDK.L2TransactionSource;
  readonly transactionIdHex: string;
  readonly compactCborHex: string;
  readonly witnessSetCompactCborHex: string;
  readonly fieldPreimageLengthsCborHex: string;
  readonly fieldName: string;
}): void => {
  if (
    retainedSource.tx_id !== transactionIdHex ||
    retainedSource.source.compact_cbor !== compactCborHex ||
    retainedSource.source.witness_set_compact_cbor !==
      witnessSetCompactCborHex ||
    retainedSource.source.field_preimage_lengths_cbor !==
      fieldPreimageLengthsCborHex
  ) {
    throw new Error(
      `${fieldName} did not retain the exact transaction proof source`,
    );
  }
};
