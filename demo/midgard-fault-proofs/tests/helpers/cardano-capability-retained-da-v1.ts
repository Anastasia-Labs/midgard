import {
  decodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardCekProgramMaterialDaValueV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import {
  MIDGARD_VALIDATION_MACHINE_V1_VERSION,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_V1_VERSION,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import { encodeMidgardValidationTraceDescriptorV1 } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterialV1 } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/index.js";

const hash32 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);

const hash28 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(28);

const encodedEntry = <K, V>({
  key,
  keySchema,
  value,
  valueSchema,
}: {
  readonly key: K;
  readonly keySchema: Parameters<typeof Data.Nullable>[0];
  readonly value: V;
  readonly valueSchema: Parameters<typeof Data.Nullable>[0];
}): SDK.DaPayloadEntry => [
  Data.to(key as never, keySchema as never),
  Data.to(value as never, valueSchema as never),
];

const sorted = (entries: readonly SDK.DaPayloadEntry[]): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const countedRoot = (
  domain: Parameters<typeof buildCountedRoot>[0],
  entries: readonly SDK.DaPayloadEntry[],
) =>
  buildCountedRoot(
    domain,
    entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );

const validationDescriptor = (
  eventKey: SDK.EventKey,
  index: number,
): SDK.DaPayloadEntry => [
  Data.to(eventKey as never, SDK.EventKeySchema as never),
  encodeMidgardValidationTraceDescriptorV1({
    schemaVersion: MIDGARD_VALIDATION_TRACE_DESCRIPTOR_V1_VERSION,
    machineVersion: MIDGARD_VALIDATION_MACHINE_V1_VERSION,
    traceRoot: Buffer.from(hash32(0xa0 + index), "hex"),
    stepCount: 1,
    initialStateHash: Buffer.from(hash32(0xb0 + index), "hex"),
    terminalStateHash: Buffer.from(hash32(0xc0 + index), "hex"),
    verdict: "accepted",
    rejectionCodeHash: Buffer.alloc(32),
  }).toString("hex"),
];

export type StrictRetainedDaPairFixtureV1 = {
  readonly payload: SDK.DaPayloadV1;
  readonly payloadEnvelopeCbor: Buffer;
  readonly header: SDK.HeaderV1;
  readonly headerHash: string;
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly forcedOrderIdHex: string;
};

export const buildStrictRetainedDaPairFixtureV1 = async ({
  canonicalTransactionCbor,
  canonicalMaterialSidecarCbor,
  resolvedReferenceUtxos,
}: {
  readonly canonicalTransactionCbor: Uint8Array;
  readonly canonicalMaterialSidecarCbor?: Uint8Array;
  readonly resolvedReferenceUtxos?: readonly SDK.DaPayloadEntry[];
}): Promise<StrictRetainedDaPairFixtureV1> => {
  const canonicalCbor = Buffer.from(canonicalTransactionCbor);
  const transaction =
    decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalCbor);
  const transactionIdHex =
    computeMidgardNativeTxIdV1(transaction).toString("hex");
  const proofSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(canonicalCbor);
  const transactionCommitmentHex =
    computeMidgardNativeTxProofCommitmentV1(proofSource).toString("hex");
  const source: SDK.L2TransactionSourceV1 = {
    tx_id: transactionIdHex,
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  const forcedOrder: SDK.OutputReference = {
    transactionId: transactionIdHex,
    outputIndex: 0n,
  };
  const forcedOrderIdHex = Data.to(forcedOrder, SDK.OutputReference);
  const forcedSource: SDK.ForcedInclusionTxV1 = {
    ...source,
    verdict: "ForcedTxValid",
  };
  const forcedEventKey: SDK.EventKey = {
    ForcedTransactionEventKey: {
      tx_order_id: forcedOrder,
    },
  };
  const normalEventKey: SDK.EventKey = {
    L2TransactionEventKey: {
      tx_id: transactionIdHex,
    },
  };
  const utxos = sorted(resolvedReferenceUtxos ?? []);
  const utxoRoot = await keyValuePhasRootWithCount(
    utxos.map(([outRefHex, outputHex]) => ({
      key: Buffer.from(outRefHex, "hex"),
      value: buildCanonicalMidgardLedgerEntryOutputMaterialV1({
        outRef: Buffer.from(outRefHex, "hex"),
        outputCbor: Buffer.from(outputHex, "hex"),
      }).descriptorCbor,
    })),
  );
  const steps: readonly SDK.TransitionStep[] = [
    {
      schema_version: 1n,
      step_index: 0n,
      event_key: forcedEventKey,
      phase: "ForcedTransaction",
      pre_utxos_root: utxoRoot.root,
      post_utxos_root: utxoRoot.root,
    },
    {
      schema_version: 1n,
      step_index: 1n,
      event_key: normalEventKey,
      phase: "L2Transaction",
      pre_utxos_root: utxoRoot.root,
      post_utxos_root: utxoRoot.root,
    },
  ];
  const forcedTransactions = [
    encodedEntry({
      key: forcedOrder,
      keySchema: SDK.OutputReference as never,
      value: forcedSource,
      valueSchema: SDK.ForcedInclusionTxV1Schema,
    }),
  ];
  const transactions: SDK.DaPayloadEntry[] = [
    [
      transactionIdHex,
      Data.to(source as never, SDK.L2TransactionSourceV1Schema),
    ],
  ];
  const transitionTrace = steps.map(
    (step): SDK.DaPayloadEntry =>
      encodedEntry({
        key: step.step_index,
        keySchema: Data.Integer() as never,
        value: step,
        valueSchema: SDK.TransitionStepSchema,
      }),
  );
  const eventToStep: readonly SDK.DaPayloadEntry[] = [
    encodedEntry({
      key: forcedEventKey,
      keySchema: SDK.EventKeySchema,
      value: {
        step_index: 0n,
        phase: "ForcedTransaction",
      } satisfies SDK.EventToStepValue,
      valueSchema: SDK.EventToStepValueSchema,
    }),
    encodedEntry({
      key: normalEventKey,
      keySchema: SDK.EventKeySchema,
      value: {
        step_index: 1n,
        phase: "L2Transaction",
      } satisfies SDK.EventToStepValue,
      valueSchema: SDK.EventToStepValueSchema,
    }),
  ];
  const validationTraces: readonly SDK.DaPayloadEntry[] = [
    validationDescriptor(forcedEventKey, 0),
    validationDescriptor(normalEventKey, 1),
  ];
  const cekProgramMaterial = sorted(
    (canonicalMaterialSidecarCbor === undefined
      ? []
      : decodeMidgardCekProgramMaterialSidecarV1(canonicalMaterialSidecarCbor)
    ).map(
      (entry): SDK.DaPayloadEntry => [
        Buffer.from(entry.root).toString("hex"),
        encodeMidgardCekProgramMaterialDaValueV1(entry).toString("hex"),
      ],
    ),
  );
  const roots = {
    withdrawals: await countedRoot(SDK.ROOT_DOMAINS.withdrawals, []),
    forcedTransactions: await countedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      forcedTransactions,
    ),
    transactions: await countedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      transactions,
    ),
    deposits: await countedRoot(SDK.ROOT_DOMAINS.deposits, []),
    transitionTrace: await countedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      transitionTrace,
    ),
    eventToStep: await countedRoot(SDK.ROOT_DOMAINS.eventToStep, eventToStep),
    validationTraces: await countedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      validationTraces,
    ),
  };
  const counts: SDK.DaPayloadCountsV1 = {
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 1n,
    depositCount: 0n,
    totalEventCount: 2n,
    transitionStepCount: 2n,
    validationTraceCount: 2n,
  };
  const header: SDK.HeaderV1 = {
    prevUtxosRoot: utxoRoot.root,
    utxosRoot: utxoRoot.root,
    withdrawalsRoot: roots.withdrawals.root,
    forcedTransactionsRoot: roots.forcedTransactions.root,
    transactionsRoot: roots.transactions.root,
    depositsRoot: roots.deposits.root,
    transitionTraceRoot: roots.transitionTrace.root,
    eventToStepRoot: roots.eventToStep.root,
    validationTracesRoot: roots.validationTraces.root,
    ...counts,
    startTime: 0n,
    endTime: 1n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: hash28(0xe0),
    operatorVkey: hash28(0xe1),
    protocolVersion: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  const payload: SDK.DaPayloadV1 = {
    version: SDK.DA_PAYLOAD_V1_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos,
      withdrawals: [],
      forced_transactions: sorted(forcedTransactions),
      transactions: sorted(transactions),
      transaction_preimages: [
        [transactionIdHex, canonicalCbor.toString("hex")],
      ],
      forced_transaction_preimages: [
        [forcedOrderIdHex, canonicalCbor.toString("hex")],
      ],
      cek_program_material: cekProgramMaterial,
      deposits: [],
      transition_trace: sorted(transitionTrace),
      event_to_step: sorted(eventToStep),
      validation_traces: sorted(validationTraces),
      validation_trace_witnesses: [],
      counts,
    },
  };
  return {
    payload,
    payloadEnvelopeCbor: await wrapDaPayloadV1(SDK.encodeDaPayloadV1(payload), {
      mode: "identity",
    }),
    header,
    headerHash,
    transactionIdHex,
    transactionCommitmentHex,
    forcedOrderIdHex,
  };
};
