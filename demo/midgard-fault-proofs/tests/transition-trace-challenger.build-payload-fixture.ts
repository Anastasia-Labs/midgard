import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { computeMidgardForcedTxProofCommitment } from "@al-ft/midgard-core/codec/forced";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  reconstructDaPayload,
  type TransitionTraceReconstruction,
} from "../src/transition-trace/index.js";
import {
  canonicalPreimageByCommitment,
  encodedEntry,
  sorted,
  traceEntry,
  utxoRootWithDescriptors,
} from "./transition-trace-challenger.native-material.js";

type PayloadFixtureInput = {
  readonly prevUtxosRoot?: string;
  readonly validationDescriptors?: readonly SDK.DaPayloadEntry[];
  readonly retainedWitnesses?: readonly SDK.DaPayloadEntry[];
  readonly utxos?: readonly SDK.DaPayloadEntry[];
  readonly withdrawals?: readonly SDK.DaPayloadEntry[];
  readonly forcedTransactions?: readonly SDK.DaPayloadEntry[];
  readonly transactions?: readonly SDK.DaPayloadEntry[];
  readonly transactionPreimages?: readonly SDK.DaPayloadEntry[];
  readonly deposits?: readonly SDK.DaPayloadEntry[];
  readonly steps?: readonly SDK.TransitionStep[];
  readonly transitionTraceEntries?: readonly SDK.DaPayloadEntry[];
  readonly eventToStep?: readonly SDK.DaPayloadEntry[];
};

export const buildPayloadFixture = async ({
  prevUtxosRoot = SDK.EMPTY_MERKLE_TREE_ROOT,
  validationDescriptors,
  retainedWitnesses = [],
  utxos = [],
  withdrawals = [],
  forcedTransactions = [],
  transactions = [],
  transactionPreimages = [],
  deposits = [],
  steps = [],
  transitionTraceEntries = steps.map(traceEntry),
  eventToStep = [],
}: PayloadFixtureInput): Promise<{
  readonly payload: SDK.DaPayload;
  readonly payloadEnvelopeCbor: Buffer;
  readonly header: SDK.Header;
  readonly headerHash: string;
}> => {
  const forcedTransactionPreimages = forcedTransactions.map(
    ([key, value], index): SDK.DaPayloadEntry => {
      const forced = Data.from(
        value,
        SDK.ForcedInclusionTxV1,
      ) as SDK.ForcedInclusionTxV1;
      const preimage = canonicalPreimageByCommitment.get(
        computeMidgardForcedTxProofCommitment({
          compactCbor: Buffer.from(forced.submitted_source.compact_cbor, "hex"),
          witnessSetCompactCbor: Buffer.from(
            forced.submitted_source.witness_set_compact_cbor,
            "hex",
          ),
          fieldPreimageLengthsCbor: Buffer.from(
            forced.submitted_source.field_preimage_lengths_cbor,
            "hex",
          ),
        }).toString("hex"),
      );
      if (preimage === undefined) {
        throw new Error(
          `missing forced transaction preimage ${index.toString()}`,
        );
      }
      return [key, preimage.toString("hex")];
    },
  );
  const validationEventKeys: SDK.EventKey[] = [
    ...forcedTransactions.map(([key]) => ({
      ForcedTransactionEventKey: {
        tx_order_id: Data.from(key, SDK.OutputReference),
      },
    })),
    ...transactions.map(([key]) => ({
      L2TransactionEventKey: { tx_id: key },
    })),
  ];
  const validationTraces =
    validationDescriptors ??
    validationEventKeys.map(
      (eventKey, index): SDK.DaPayloadEntry =>
        encodedEntry({
          key: eventKey,
          keySchema: SDK.EventKeySchema,
          value: {
            schema_version: 1n,
            machine_version: 1n,
            trace_root: h32(140 + index),
            step_count: 1n,
            initial_state_hash: h32(150 + index),
            terminal_state_hash: h32(160 + index),
            verdict: "Accepted",
            rejection_code_hash: "00".repeat(32),
          } satisfies SDK.ValidationTraceDescriptor,
          valueSchema: SDK.ValidationTraceDescriptorSchema,
        }),
    );
  const utxoRoot = await utxoRootWithDescriptors(utxos);
  const roots = {
    withdrawals: await buildCountedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawals.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    forcedTransactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      forcedTransactions.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    transactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      transactions.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    deposits: await buildCountedRoot(
      SDK.ROOT_DOMAINS.deposits,
      deposits.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    transitionTrace: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      transitionTraceEntries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    eventToStep: await buildCountedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      eventToStep.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    validationTraces: await buildCountedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      validationTraces.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
  };
  const counts = {
    withdrawalCount: BigInt(withdrawals.length),
    forcedTransactionCount: BigInt(forcedTransactions.length),
    l2TransactionCount: BigInt(transactions.length),
    depositCount: BigInt(deposits.length),
    totalEventCount:
      BigInt(withdrawals.length) +
      BigInt(forcedTransactions.length) +
      BigInt(transactions.length) +
      BigInt(deposits.length),
    transitionStepCount: BigInt(transitionTraceEntries.length),
    validationTraceCount: BigInt(validationTraces.length),
  };
  const header: SDK.Header = {
    prevUtxosRoot,
    utxosRoot: utxoRoot.root,
    withdrawalsRoot: roots.withdrawals.root,
    forcedTransactionsRoot: roots.forcedTransactions.root,
    transactionsRoot: roots.transactions.root,
    depositsRoot: roots.deposits.root,
    transitionTraceRoot: roots.transitionTrace.root,
    eventToStepRoot: roots.eventToStep.root,
    validationTracesRoot: roots.validationTraces.root,
    ...counts,
    startTime: 10n,
    endTime: 20n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: h28(90),
    operatorVkey: h28(91),
    protocolVersion: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: sorted(utxos),
      withdrawals: sorted(withdrawals),
      forced_transactions: sorted(forcedTransactions),
      transactions: sorted(transactions),
      deposits: sorted(deposits),
      transition_trace: sorted(transitionTraceEntries),
      event_to_step: sorted(eventToStep),
      transaction_preimages: sorted(transactionPreimages),
      forced_transaction_preimages: sorted(forcedTransactionPreimages),
      cek_program_material: [],
      validation_traces: sorted(validationTraces),
      validation_trace_witnesses: [...retainedWitnesses],
      counts,
    },
  };
  return {
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    header,
    headerHash,
  };
};

export const reconstruct = async (
  fixture: Awaited<ReturnType<typeof buildPayloadFixture>>,
): Promise<TransitionTraceReconstruction> =>
  await reconstructDaPayload({
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    expectedHeaderHash: fixture.headerHash,
    committedHeader: fixture.header,
  });

export const authenticatedObservation = (
  fixture: Awaited<ReturnType<typeof buildPayloadFixture>>,
): SDK.AuthenticatedStateQueueHeaderObservation => ({
  schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  sourceMode: "local_node",
  provenance: {
    trustClass: "authenticated_cardano_l1",
    sourceId: "local-kupmios",
    grade: "security",
  },
  chainPoint: { slot: 42n, blockHash: h32(42) },
  confirmationDepth: 30,
  headerHash: fixture.headerHash,
  header: fixture.header,
});

export const retainedSource = (
  fixture: Awaited<ReturnType<typeof buildPayloadFixture>>,
) => ({
  sourceId: "public-da",
  fetchPayloadByHeaderHash: async () => ({
    ok: true as const,
    provenance: {
      trustClass: "public_or_permissionless_da" as const,
      sourceId: "public-da/peer-1",
      grade: "security" as const,
    },
    sourceId: "public-da",
    sourcePeerId: "peer-1",
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    attempts: [],
  }),
});

export const forcedEventKey = (id: SDK.OutputReference): SDK.EventKey => ({
  ForcedTransactionEventKey: { tx_order_id: id },
});

export const depositEventKey = (id: SDK.OutputReference): SDK.EventKey => ({
  DepositEventKey: { deposit_id: id },
});

export const withdrawalEventKey = (id: SDK.OutputReference): SDK.EventKey => ({
  WithdrawalEventKey: { withdrawal_id: id },
});

export const sdkProof = (proof: MpfProof): SDK.Proof =>
  Data.from(proof.toCBOR().toString("hex"), SDK.Proof) as SDK.Proof;

export type L2ReplayFixture = {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly evidence: {
    readonly stepIndex: bigint;
    readonly spentUtxos: readonly SDK.LedgerDeleteWitness[];
    readonly producedUtxos: readonly SDK.LedgerInsertWitness[];
  };
  readonly replayedPostRoot: string;
};
