import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core/codec";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterialV1 } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  type AcceptedTransactionTransitionMismatchEvidence,
  buildCountedRoot,
  detectTransitionTraceFaults,
  encodeData,
  keyValuePhasRootWithCount,
  reconstructDaPayloadV1,
  TRANSITION_TRACE_FAULT_KINDS,
  type TransitionTraceDetection,
  type TransitionTraceFaultKind,
  type TransitionTraceReconstruction,
} from "../src/transition-trace/index.js";

// ---------------------------------------------------------------------------
// Fixture machinery. Mirrors demo/midgard-fault-proofs/tests/transition-trace-
// challenger.test.ts (trimmed to what a dense-totality suite needs — no tag-4
// output encoding, no MPF branch replay). Kept self-contained rather than
// imported so this file has no coupling to the other test file's internals.
// ---------------------------------------------------------------------------

const h32 = (byte: number): string =>
  (byte & 0xff).toString(16).padStart(2, "0").repeat(32);
const h28 = (byte: number): string =>
  (byte & 0xff).toString(16).padStart(2, "0").repeat(28);

const outRef = (byte: number): SDK.OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});

const address = (byte: number): SDK.AddressData => ({
  paymentCredential: { PublicKeyCredential: [h28(byte)] },
  stakeCredential: null,
});

const depositInfo = (byte: number): SDK.DepositInfo => ({
  l2_address: address(byte),
  l2_network_id: 0n,
  l2_datum: null,
});

const withdrawalInfo = (
  byte: number,
  validity: SDK.WithdrawalValidity,
): SDK.WithdrawalInfo => ({
  body: {
    l2_outref: outRef(byte),
    l2_owner: h28(byte + 1),
    l2_value: new Map(),
    l1_address: address(byte + 2),
    l1_datum: "NoDatum",
  },
  signature: [h32(byte + 3), h32(byte + 4)],
  validity,
});

/**
 * Fixture-local index from a native proof source back to the canonical CBOR
 * the fixture built it from — needed to reconstruct
 * `forced_transaction_preimages` the same way `buildPayloadFixture` does in
 * the reference challenger suite.
 */
const canonicalPreimageByCommitment = new Map<string, Buffer>();

const proofSourceCommitment = (source: SDK.NativeTxProofSourceV1): string =>
  computeMidgardNativeTxProofCommitmentV1({
    compactCbor: Buffer.from(source.compact_cbor, "hex"),
    witnessSetCompactCbor: Buffer.from(source.witness_set_compact_cbor, "hex"),
    fieldPreimageLengthsCbor: Buffer.from(
      source.field_preimage_lengths_cbor,
      "hex",
    ),
  }).toString("hex");

const nativeMaterial = (byte: number) => {
  const canonical: MidgardNativeTxCanonicalV1 = {
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      fee: BigInt(byte),
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
  };
  const full = materializeMidgardNativeTxFromCanonicalV1(canonical);
  const canonicalCbor = encodeMidgardNativeTxCanonicalV1(full);
  const source =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(canonicalCbor);
  const txId = computeMidgardNativeTxIdV1(full).toString("hex");
  canonicalPreimageByCommitment.set(
    computeMidgardNativeTxProofCommitmentV1(source).toString("hex"),
    canonicalCbor,
  );
  return {
    txId,
    canonicalCbor,
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
};

/**
 * The #640 verdict standing in for the pre-format `FailedScript` arm: a
 * forced transaction the operator rejected for a failed Plutus execution at
 * execution index 0.
 */
const forcedTxInvalidPlutus: SDK.OperatorVerdictV1 = {
  ForcedTxInvalid: {
    reason: { PlutusExecutionFailed: { execution_index: 0n } },
  },
};

const forcedTx = (
  byte: number,
  verdict: SDK.OperatorVerdictV1,
): SDK.ForcedInclusionTxV1 => {
  const material = nativeMaterial(byte);
  return {
    tx_id: material.txId,
    source: material.source,
    verdict,
  };
};

const entry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

const sorted = (entries: readonly SDK.DaPayloadEntry[]): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const LEDGER_OUTPUT_CBOR =
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0";

const spendInputItem = (txIdHex: string, outputIndex: number): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.from(txIdHex, "hex"),
    outputIndex,
  });

const rawLedgerEntry = (byte: number): SDK.DaPayloadEntry => [
  spendInputItem(h32(byte), 0).toString("hex"),
  LEDGER_OUTPUT_CBOR,
];

const utxoRootWithDescriptors = (
  utxos: readonly SDK.DaPayloadEntry[],
): Promise<Awaited<ReturnType<typeof keyValuePhasRootWithCount>>> =>
  keyValuePhasRootWithCount(
    utxos.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: buildCanonicalMidgardLedgerEntryOutputMaterialV1({
        outRef: Buffer.from(key, "hex"),
        outputCbor: Buffer.from(value, "hex"),
      }).descriptorCbor,
    })),
  );

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
}): SDK.DaPayloadEntry =>
  entry(encodeData(key, keySchema), encodeData(value, valueSchema));

const traceEntryWithKey = (
  key: bigint,
  step: SDK.TransitionStep,
): SDK.DaPayloadEntry =>
  encodedEntry({
    key,
    keySchema: Data.Integer() as never,
    value: step,
    valueSchema: SDK.TransitionStepSchema,
  });

const traceEntry = (step: SDK.TransitionStep): SDK.DaPayloadEntry =>
  traceEntryWithKey(step.step_index, step);

const eventToStepEntry = (
  key: SDK.EventKey,
  value: SDK.EventToStepValue,
): SDK.DaPayloadEntry =>
  encodedEntry({
    key,
    keySchema: SDK.EventKeySchema,
    value,
    valueSchema: SDK.EventToStepValueSchema,
  });

type PayloadFixtureInput = {
  readonly prevUtxosRoot?: string;
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

const buildPayloadFixture = async ({
  prevUtxosRoot = SDK.EMPTY_MERKLE_TREE_ROOT,
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
  readonly payload: SDK.DaPayloadV1;
  readonly payloadEnvelopeCbor: Buffer;
  readonly header: SDK.HeaderV1;
  readonly headerHash: string;
}> => {
  const forcedTransactionPreimages = forcedTransactions.map(
    ([key, value], index): SDK.DaPayloadEntry => {
      const forced = Data.from(
        value,
        SDK.ForcedInclusionTxV1,
      ) as SDK.ForcedInclusionTxV1;
      const preimage = canonicalPreimageByCommitment.get(
        proofSourceCommitment(forced.source),
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
  const validationTraces = validationEventKeys.map(
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
          rejection_code_hash: h32(170 + index),
        } satisfies SDK.ValidationTraceDescriptorV1,
        valueSchema: SDK.ValidationTraceDescriptorV1Schema,
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
  const header: SDK.HeaderV1 = {
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
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  const payload: SDK.DaPayloadV1 = {
    version: SDK.DA_PAYLOAD_V1_VERSION,
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
  };
};

const reconstruct = async (
  fixture: Awaited<ReturnType<typeof buildPayloadFixture>>,
): Promise<TransitionTraceReconstruction> =>
  await reconstructDaPayloadV1({
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    expectedHeaderHash: fixture.headerHash,
    committedHeader: fixture.header,
  });

const forcedEventKey = (id: SDK.OutputReference): SDK.EventKey => ({
  ForcedTransactionEventKey: { tx_order_id: id },
});

const depositEventKey = (id: SDK.OutputReference): SDK.EventKey => ({
  DepositEventKey: { deposit_id: id },
});

const withdrawalEventKey = (id: SDK.OutputReference): SDK.EventKey => ({
  WithdrawalEventKey: { withdrawal_id: id },
});

// ---------------------------------------------------------------------------
// acceptedTransactionTransitionMismatch fixture. This is the one fault kind
// the 39-test challenger suite never exercises. `detectAcceptedTransaction-
// TransitionMismatches` in src/transition-trace/detect.ts only reads
// `claim.descriptor_membership.value.verdict` and
// `claim.transition_step_membership.value.post_utxos_root`, plus decodes
// `terminalAcceptanceWitnessCbor` itself — it never verifies any of the
// membership proofs — so the proof/root/count fields below only need to
// satisfy the schema shape, not open a real PHAS tree.
// ---------------------------------------------------------------------------

const terminalAcceptanceWitnessCbor = (validatedPostRootHex: string): string =>
  encodeCborArrayRaw([
    encodeCborUnsigned(1n),
    encodeCborBytes(Buffer.alloc(0)),
    encodeCborBytes(Buffer.from(validatedPostRootHex, "hex")),
    encodeCborBytes(Buffer.alloc(0)),
  ]).toString("hex");

const dummyMembershipFields = (domain: SDK.RootDomain) => ({
  domain,
  root: h32(996),
  phas_root: h32(997),
  count: 1n,
  proof: [] as SDK.Proof,
});

const dummyValidationMachineState = (): SDK.ValidationMachineStateV1 => ({
  machine_version: 1n,
  event_key_hash: h32(980),
  transaction_id: h32(981),
  transaction_commitment: h32(982),
  validation_context_hash: h32(983),
  source_kind: "Forced",
  prior_ledger_root: h32(984),
  phase: "Terminal",
  program_counter: 0n,
  work_root: h32(985),
  execution_cpu: 0n,
  execution_memory: 0n,
  verdict: "Accepted",
  rejection_code_hash: h32(986),
  ledger_delta_root: h32(987),
});

const buildAcceptedTransactionTransitionMismatchEvidence = (): {
  readonly committedPostRoot: string;
  readonly evidence: AcceptedTransactionTransitionMismatchEvidence;
} => {
  const eventKey: SDK.EventKey = {
    ForcedTransactionEventKey: { tx_order_id: outRef(690) },
  };
  const committedPostRoot = h32(695);
  const validatedPostRoot = h32(696);
  const descriptor: SDK.ValidationTraceDescriptorV1 = {
    schema_version: 1n,
    machine_version: 1n,
    trace_root: h32(691),
    step_count: 1n,
    initial_state_hash: h32(692),
    terminal_state_hash: h32(693),
    verdict: "Accepted",
    rejection_code_hash: h32(694),
  };
  const step: SDK.TransitionStep = {
    schema_version: 1n,
    step_index: 0n,
    event_key: eventKey,
    phase: "ForcedTransaction",
    pre_utxos_root: h32(697),
    post_utxos_root: committedPostRoot,
  };
  const eventToStepValue: SDK.EventToStepValue = {
    step_index: 0n,
    phase: "ForcedTransaction",
  };
  const dummyForcedInclusionTx: SDK.ForcedInclusionTxV1 = {
    tx_id: h32(689),
    source: {
      compact_cbor: "",
      witness_set_compact_cbor: "",
      field_preimage_lengths_cbor: "",
    },
    verdict: "ForcedTxValid",
  };
  const claim: SDK.ValidationClaimWitnessV1 = {
    version: 1n,
    descriptor_membership: {
      ...dummyMembershipFields(SDK.ROOT_DOMAINS.validationTraces),
      key: eventKey,
      value: descriptor,
    },
    transition_step_membership: {
      ...dummyMembershipFields(SDK.ROOT_DOMAINS.transitionTrace),
      key: 0n,
      value: step,
    },
    event_to_step_membership: {
      ...dummyMembershipFields(SDK.ROOT_DOMAINS.eventToStep),
      key: eventKey,
      value: eventToStepValue,
    },
    source_membership: {
      ForcedValidationSource: {
        membership: {
          ...dummyMembershipFields(SDK.ROOT_DOMAINS.forcedTransactionsV1),
          key: outRef(690),
          value: dummyForcedInclusionTx,
        },
      },
    },
    validation_context_cbor: "",
    initial_state: dummyValidationMachineState(),
    terminal_state: dummyValidationMachineState(),
    initial_state_proof: {
      state_index: 0n,
      state_hash: h32(998),
      siblings: [],
    },
    terminal_state_proof: {
      state_index: 1n,
      state_hash: h32(999),
      siblings: [],
    },
  };
  return {
    committedPostRoot,
    evidence: {
      claim,
      terminalAcceptanceWitnessCbor:
        terminalAcceptanceWitnessCbor(validatedPostRoot),
    },
  };
};

// ---------------------------------------------------------------------------
// One probe per fault kind. Each is an independent minimal fixture (no
// shared mutable state), matching the isolation style of the reference
// challenger suite so a probe's assertion is unambiguously attributable to
// its own fixture.
// ---------------------------------------------------------------------------

type FaultProbe = {
  readonly kind: TransitionTraceFaultKind;
  readonly invariant: string;
  readonly run: () => Promise<readonly TransitionTraceDetection[]>;
};

const countFaultProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const reconstruction = await reconstruct(await buildPayloadFixture({}));
  return detectTransitionTraceFaults({
    ...reconstruction,
    header: { ...reconstruction.header, totalEventCount: 1n },
  });
};

const traceBoundaryProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const depositId = outRef(600);
  const key = depositEventKey(depositId);
  const fixture = await buildPayloadFixture({
    prevUtxosRoot: h32(601),
    deposits: [
      encodedEntry({
        key: depositId,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(602),
        valueSchema: SDK.DepositInfoSchema,
      }),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: key,
        phase: "Deposit",
        pre_utxos_root: h32(603),
        post_utxos_root: h32(604),
      },
    ],
    eventToStep: [eventToStepEntry(key, { step_index: 0n, phase: "Deposit" })],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture));
};

const traceLinkProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const idA = outRef(610);
  const idB = outRef(611);
  const keyA = depositEventKey(idA);
  const keyB = depositEventKey(idB);
  const fixture = await buildPayloadFixture({
    prevUtxosRoot: h32(612),
    deposits: [
      encodedEntry({
        key: idA,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(613),
        valueSchema: SDK.DepositInfoSchema,
      }),
      encodedEntry({
        key: idB,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(614),
        valueSchema: SDK.DepositInfoSchema,
      }),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: keyA,
        phase: "Deposit",
        pre_utxos_root: h32(612),
        post_utxos_root: h32(615),
      },
      {
        schema_version: 1n,
        step_index: 1n,
        event_key: keyB,
        phase: "Deposit",
        pre_utxos_root: h32(616),
        post_utxos_root: h32(617),
      },
    ],
    eventToStep: [
      eventToStepEntry(keyA, { step_index: 0n, phase: "Deposit" }),
      eventToStepEntry(keyB, { step_index: 1n, phase: "Deposit" }),
    ],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture));
};

const eventToStepMismatchProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const id = outRef(620);
  const key = depositEventKey(id);
  const fixture = await buildPayloadFixture({
    deposits: [
      encodedEntry({
        key: id,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(621),
        valueSchema: SDK.DepositInfoSchema,
      }),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: key,
        phase: "Deposit",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
    // Present but wrong: isolates event_to_step_matches_trace from the
    // source-membership checks, which only compare fingerprints/phases
    // against the trace step, not against this (deliberately wrong) mapping.
    eventToStep: [
      eventToStepEntry(key, { step_index: 0n, phase: "Withdrawal" }),
    ],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture));
};

const sourceMembershipMismatchProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const material = nativeMaterial(630);
  const source: SDK.L2TransactionSourceV1 = {
    tx_id: material.txId,
    source: material.source,
  };
  const key: SDK.EventKey = { L2TransactionEventKey: { tx_id: material.txId } };
  const fixture = await buildPayloadFixture({
    transactions: [
      entry(
        Buffer.from(material.txId, "hex"),
        Buffer.from(Data.to(source, SDK.L2TransactionSourceV1), "hex"),
      ),
    ],
    transactionPreimages: [
      entry(Buffer.from(material.txId, "hex"), material.canonicalCbor),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: key,
        // Wrong on purpose: the real source is L2Transaction.
        phase: "Deposit",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
    eventToStep: [eventToStepEntry(key, { step_index: 0n, phase: "Deposit" })],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture));
};

const invalidOneStepTransitionProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const txOrderId = outRef(640);
  const finalUtxo = rawLedgerEntry(640);
  const finalRoot = await utxoRootWithDescriptors([finalUtxo]);
  const key = forcedEventKey(txOrderId);
  const fixture = await buildPayloadFixture({
    utxos: [finalUtxo],
    forcedTransactions: [
      encodedEntry({
        key: txOrderId,
        keySchema: SDK.OutputReference as never,
        value: forcedTx(641, forcedTxInvalidPlutus),
        valueSchema: SDK.ForcedInclusionTxV1Schema,
      }),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: key,
        phase: "ForcedTransaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: finalRoot.root,
      },
    ],
    eventToStep: [
      eventToStepEntry(key, { step_index: 0n, phase: "ForcedTransaction" }),
    ],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture));
};

const omittedDueL1EventProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const reconstruction = await reconstruct(await buildPayloadFixture({}));
  return detectTransitionTraceFaults(reconstruction, {
    omittedDueL1Events: [
      {
        kind: "deposit",
        depositId: outRef(650),
        eventRefInputIndex: 0n,
        eventAssetName: "aa",
      },
    ],
  });
};

const duplicateTraceEventProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const txOrderId = outRef(660);
  const key = forcedEventKey(txOrderId);
  const fixture = await buildPayloadFixture({
    forcedTransactions: [
      encodedEntry({
        key: txOrderId,
        keySchema: SDK.OutputReference as never,
        value: forcedTx(661, forcedTxInvalidPlutus),
        valueSchema: SDK.ForcedInclusionTxV1Schema,
      }),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: key,
        phase: "ForcedTransaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
      {
        schema_version: 1n,
        step_index: 1n,
        event_key: key,
        phase: "ForcedTransaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
    eventToStep: [
      eventToStepEntry(key, { step_index: 0n, phase: "ForcedTransaction" }),
    ],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture));
};

const outOfWindowSourceEventProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const depositId = outRef(670);
  const key = depositEventKey(depositId);
  const fixture = await buildPayloadFixture({
    deposits: [
      encodedEntry({
        key: depositId,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(671),
        valueSchema: SDK.DepositInfoSchema,
      }),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: key,
        phase: "Deposit",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
    eventToStep: [eventToStepEntry(key, { step_index: 0n, phase: "Deposit" })],
  });
  return detectTransitionTraceFaults(await reconstruct(fixture), {
    outOfWindowSourceEvents: [
      {
        kind: "deposit",
        depositId,
        eventRefInputIndex: 0n,
        eventAssetName: "aa",
      },
    ],
  });
};

const acceptedTransactionTransitionMismatchProbe = async (): Promise<
  readonly TransitionTraceDetection[]
> => {
  const reconstruction = await reconstruct(await buildPayloadFixture({}));
  const { evidence } = buildAcceptedTransactionTransitionMismatchEvidence();
  return detectTransitionTraceFaults(reconstruction, {
    acceptedTransactionTransitionMismatches: [evidence],
  });
};

const probes: readonly FaultProbe[] = [
  {
    kind: "countFault",
    invariant: "header_total_event_count",
    run: countFaultProbe,
  },
  {
    kind: "traceBoundary",
    invariant: "trace_start_prev_utxos_root",
    run: traceBoundaryProbe,
  },
  {
    kind: "traceLink",
    invariant: "adjacent_trace_roots",
    run: traceLinkProbe,
  },
  {
    kind: "eventToStepMismatch",
    invariant: "event_to_step_matches_trace",
    run: eventToStepMismatchProbe,
  },
  {
    kind: "sourceMembershipMismatch",
    invariant: "source_phase_matches_trace_phase",
    run: sourceMembershipMismatchProbe,
  },
  {
    kind: "invalidOneStepTransition",
    invariant: "invalid_forced_transaction_is_no_op",
    run: invalidOneStepTransitionProbe,
  },
  {
    kind: "omittedDueL1Event",
    invariant: "due_l1_event_is_in_source_root",
    run: omittedDueL1EventProbe,
  },
  {
    kind: "duplicateTraceEvent",
    invariant: "trace_event_key_unique",
    run: duplicateTraceEventProbe,
  },
  {
    kind: "outOfWindowSourceEvent",
    invariant: "source_event_is_within_block_window",
    run: outOfWindowSourceEventProbe,
  },
  {
    kind: "acceptedTransactionTransitionMismatch",
    invariant: "accepted_transaction_uses_validated_ledger_root",
    run: acceptedTransactionTransitionMismatchProbe,
  },
];

describe("ledger-delta dense trace totality v1", () => {
  it("reconstructs a dense normal-transaction trace with every intermediate root reproduced and no operation missing or extra", async () => {
    const depositId = outRef(700);
    const withdrawalId = outRef(701);
    const material = nativeMaterial(702);
    const finalDepositId = outRef(703);
    const finalUtxo = rawLedgerEntry(704);
    const finalRoot = await utxoRootWithDescriptors([finalUtxo]);

    const depositKey = depositEventKey(depositId);
    const withdrawalKey = withdrawalEventKey(withdrawalId);
    const l2Key: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: material.txId },
    };
    const finalDepositKey = depositEventKey(finalDepositId);
    const l2Source: SDK.L2TransactionSourceV1 = {
      tx_id: material.txId,
      source: material.source,
    };

    const r0 = h32(710);
    const r1 = h32(711);
    const r2 = h32(712);
    const r2b = h32(716);
    const r3 = finalRoot.root;

    const steps: SDK.TransitionStep[] = [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: depositKey,
        phase: "Deposit",
        pre_utxos_root: r0,
        post_utxos_root: r1,
      },
      {
        schema_version: 1n,
        step_index: 1n,
        event_key: withdrawalKey,
        phase: "Withdrawal",
        pre_utxos_root: r1,
        post_utxos_root: r2,
      },
      {
        schema_version: 1n,
        step_index: 2n,
        event_key: l2Key,
        phase: "L2Transaction",
        pre_utxos_root: r2,
        post_utxos_root: r2b,
      },
      {
        schema_version: 1n,
        step_index: 3n,
        event_key: finalDepositKey,
        phase: "Deposit",
        pre_utxos_root: r2b,
        post_utxos_root: r3,
      },
    ];

    const fixture = await buildPayloadFixture({
      prevUtxosRoot: r0,
      utxos: [finalUtxo],
      deposits: [
        encodedEntry({
          key: depositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(713),
          valueSchema: SDK.DepositInfoSchema,
        }),
        encodedEntry({
          key: finalDepositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(714),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      withdrawals: [
        encodedEntry({
          key: withdrawalId,
          keySchema: SDK.OutputReference as never,
          value: withdrawalInfo(715, "WithdrawalIsValid"),
          valueSchema: SDK.WithdrawalInfoSchema,
        }),
      ],
      transactions: [
        entry(
          Buffer.from(material.txId, "hex"),
          Buffer.from(Data.to(l2Source, SDK.L2TransactionSourceV1), "hex"),
        ),
      ],
      transactionPreimages: [
        entry(Buffer.from(material.txId, "hex"), material.canonicalCbor),
      ],
      steps,
      eventToStep: [
        eventToStepEntry(depositKey, { step_index: 0n, phase: "Deposit" }),
        eventToStepEntry(withdrawalKey, {
          step_index: 1n,
          phase: "Withdrawal",
        }),
        eventToStepEntry(l2Key, { step_index: 2n, phase: "L2Transaction" }),
        eventToStepEntry(finalDepositKey, {
          step_index: 3n,
          phase: "Deposit",
        }),
      ],
    });

    const reconstruction = await reconstruct(fixture);

    expect(await detectTransitionTraceFaults(reconstruction)).toEqual([]);
    expect(reconstruction.transitionTrace.map(({ key }) => key)).toEqual([
      0n,
      1n,
      2n,
      3n,
    ]);
    expect(reconstruction.sourceEvents).toHaveLength(4);
    expect(reconstruction.eventToStep).toHaveLength(4);

    const ordered = [...reconstruction.transitionTrace].sort((left, right) =>
      Number(left.key - right.key),
    );
    expect(ordered[0]!.value.pre_utxos_root).toBe(
      reconstruction.header.prevUtxosRoot,
    );
    for (let index = 0; index < ordered.length - 1; index += 1) {
      expect(ordered[index]!.value.post_utxos_root).toBe(
        ordered[index + 1]!.value.pre_utxos_root,
      );
    }
    expect(ordered.at(-1)!.value.post_utxos_root).toBe(
      reconstruction.header.utxosRoot,
    );
    expect(reconstruction.header.utxosRoot).toBe(r3);

    for (const source of reconstruction.sourceEvents) {
      const mapped = reconstruction.eventToStepByFingerprint.get(
        source.fingerprint,
      );
      expect(mapped).toBeDefined();
      const step = reconstruction.traceByStepIndex.get(
        mapped!.value.step_index,
      );
      expect(step).toBeDefined();
      expect(step!.value.event_key).toEqual(source.eventKey);
    }
  });

  it("reconstructs a dense forced-transaction trace with every intermediate root reproduced and no operation missing or extra", async () => {
    const depositId = outRef(720);
    const forcedId = outRef(721);
    const withdrawalId = outRef(722);
    const finalDepositId = outRef(723);
    const finalUtxo = rawLedgerEntry(724);
    const finalRoot = await utxoRootWithDescriptors([finalUtxo]);

    const depositKey = depositEventKey(depositId);
    const forcedKey = forcedEventKey(forcedId);
    const withdrawalKey = withdrawalEventKey(withdrawalId);
    const finalDepositKey = depositEventKey(finalDepositId);

    const r0 = h32(730);
    const r1 = h32(731);
    const r2 = h32(732);
    const r2b = h32(733);
    const r3 = finalRoot.root;

    const steps: SDK.TransitionStep[] = [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: depositKey,
        phase: "Deposit",
        pre_utxos_root: r0,
        post_utxos_root: r1,
      },
      {
        schema_version: 1n,
        step_index: 1n,
        event_key: forcedKey,
        phase: "ForcedTransaction",
        pre_utxos_root: r1,
        post_utxos_root: r2,
      },
      {
        schema_version: 1n,
        step_index: 2n,
        event_key: withdrawalKey,
        phase: "Withdrawal",
        pre_utxos_root: r2,
        post_utxos_root: r2b,
      },
      {
        schema_version: 1n,
        step_index: 3n,
        event_key: finalDepositKey,
        phase: "Deposit",
        pre_utxos_root: r2b,
        post_utxos_root: r3,
      },
    ];

    const fixture = await buildPayloadFixture({
      prevUtxosRoot: r0,
      utxos: [finalUtxo],
      deposits: [
        encodedEntry({
          key: depositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(734),
          valueSchema: SDK.DepositInfoSchema,
        }),
        encodedEntry({
          key: finalDepositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(735),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      withdrawals: [
        encodedEntry({
          key: withdrawalId,
          keySchema: SDK.OutputReference as never,
          value: withdrawalInfo(736, "WithdrawalIsValid"),
          valueSchema: SDK.WithdrawalInfoSchema,
        }),
      ],
      forcedTransactions: [
        encodedEntry({
          key: forcedId,
          keySchema: SDK.OutputReference as never,
          // TxIsValid: a valid forced inclusion legitimately moves the root,
          // so the default no-op check (which only fires for invalid forced
          // transactions) stays quiet here.
          value: forcedTx(737, "ForcedTxValid"),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      steps,
      eventToStep: [
        eventToStepEntry(depositKey, { step_index: 0n, phase: "Deposit" }),
        eventToStepEntry(forcedKey, {
          step_index: 1n,
          phase: "ForcedTransaction",
        }),
        eventToStepEntry(withdrawalKey, {
          step_index: 2n,
          phase: "Withdrawal",
        }),
        eventToStepEntry(finalDepositKey, {
          step_index: 3n,
          phase: "Deposit",
        }),
      ],
    });

    const reconstruction = await reconstruct(fixture);

    expect(await detectTransitionTraceFaults(reconstruction)).toEqual([]);
    expect(reconstruction.transitionTrace.map(({ key }) => key)).toEqual([
      0n,
      1n,
      2n,
      3n,
    ]);
    expect(reconstruction.sourceEvents).toHaveLength(4);
    expect(reconstruction.eventToStep).toHaveLength(4);

    const ordered = [...reconstruction.transitionTrace].sort((left, right) =>
      Number(left.key - right.key),
    );
    expect(ordered[0]!.value.pre_utxos_root).toBe(
      reconstruction.header.prevUtxosRoot,
    );
    for (let index = 0; index < ordered.length - 1; index += 1) {
      expect(ordered[index]!.value.post_utxos_root).toBe(
        ordered[index + 1]!.value.pre_utxos_root,
      );
    }
    expect(ordered.at(-1)!.value.post_utxos_root).toBe(
      reconstruction.header.utxosRoot,
    );
    expect(reconstruction.header.utxosRoot).toBe(r3);

    for (const source of reconstruction.sourceEvents) {
      const mapped = reconstruction.eventToStepByFingerprint.get(
        source.fingerprint,
      );
      expect(mapped).toBeDefined();
      const step = reconstruction.traceByStepIndex.get(
        mapped!.value.step_index,
      );
      expect(step).toBeDefined();
      expect(step!.value.event_key).toEqual(source.eventKey);
    }
  });

  it("maps every enabled fault kind to a classified detection with a total event-to-step mapping", async () => {
    // Positive half: one dense trace touching every L1/L2 event kind
    // reconstructs with a bijective event<->step mapping and zero faults.
    const depositId = outRef(800);
    const withdrawalId = outRef(801);
    const forcedId = outRef(802);
    const material = nativeMaterial(803);
    const depositKey = depositEventKey(depositId);
    const withdrawalKey = withdrawalEventKey(withdrawalId);
    const forcedKey = forcedEventKey(forcedId);
    const l2Key: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: material.txId },
    };
    const l2Source: SDK.L2TransactionSourceV1 = {
      tx_id: material.txId,
      source: material.source,
    };

    const totalFixture = await buildPayloadFixture({
      deposits: [
        encodedEntry({
          key: depositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(804),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      withdrawals: [
        encodedEntry({
          key: withdrawalId,
          keySchema: SDK.OutputReference as never,
          value: withdrawalInfo(805, "WithdrawalIsValid"),
          valueSchema: SDK.WithdrawalInfoSchema,
        }),
      ],
      forcedTransactions: [
        encodedEntry({
          key: forcedId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(806, "ForcedTxValid"),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      transactions: [
        entry(
          Buffer.from(material.txId, "hex"),
          Buffer.from(Data.to(l2Source, SDK.L2TransactionSourceV1), "hex"),
        ),
      ],
      transactionPreimages: [
        entry(Buffer.from(material.txId, "hex"), material.canonicalCbor),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: depositKey,
          phase: "Deposit",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        {
          schema_version: 1n,
          step_index: 1n,
          event_key: withdrawalKey,
          phase: "Withdrawal",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        {
          schema_version: 1n,
          step_index: 2n,
          event_key: forcedKey,
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        {
          schema_version: 1n,
          step_index: 3n,
          event_key: l2Key,
          phase: "L2Transaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(depositKey, { step_index: 0n, phase: "Deposit" }),
        eventToStepEntry(withdrawalKey, {
          step_index: 1n,
          phase: "Withdrawal",
        }),
        eventToStepEntry(forcedKey, {
          step_index: 2n,
          phase: "ForcedTransaction",
        }),
        eventToStepEntry(l2Key, { step_index: 3n, phase: "L2Transaction" }),
      ],
    });
    const totalBase = await reconstruct(totalFixture);
    expect(await detectTransitionTraceFaults(totalBase)).toEqual([]);
    expect(totalBase.sourceEvents).toHaveLength(4);
    expect(totalBase.eventToStep).toHaveLength(4);
    for (const source of totalBase.sourceEvents) {
      const mapped = totalBase.eventToStepByFingerprint.get(source.fingerprint);
      expect(mapped).toBeDefined();
      expect(totalBase.traceByStepIndex.has(mapped!.value.step_index)).toBe(
        true,
      );
    }

    // Negative half: every fault kind the production module declares must be
    // both reachable (its probe below produces it) and correctly classified.
    // TRANSITION_TRACE_FAULT_KINDS is read directly off detect.ts — nothing
    // here re-lists the kind strings — so a new kind added there without a
    // probe fails this assertion instead of silently going unmapped.
    const detectionsByKind = new Map<
      TransitionTraceFaultKind,
      readonly TransitionTraceDetection[]
    >();
    const seenKinds = new Set<TransitionTraceFaultKind>();
    for (const probe of probes) {
      const detections = await probe.run();
      expect(
        detections.some(
          (detection) =>
            detection.kind === probe.kind &&
            detection.invariant === probe.invariant,
        ),
      ).toBe(true);
      detectionsByKind.set(probe.kind, detections);
      for (const detection of detections) {
        seenKinds.add(detection.kind);
      }
    }
    expect([...seenKinds].sort()).toEqual(
      [...TRANSITION_TRACE_FAULT_KINDS].sort(),
    );

    // The one kind the 39-test challenger suite never exercises: confirm its
    // fault proof round-trips through CBOR like every other buildable
    // detection's does.
    const acceptedDetection = detectionsByKind
      .get("acceptedTransactionTransitionMismatch")
      ?.find(
        (detection) =>
          detection.kind === "acceptedTransactionTransitionMismatch",
      );
    if (acceptedDetection === undefined || !acceptedDetection.buildable) {
      throw new Error(
        "expected a buildable acceptedTransactionTransitionMismatch detection",
      );
    }
    expect(() =>
      Data.from(
        Data.to(
          acceptedDetection.proof as never,
          SDK.TransitionFaultProof as never,
        ),
        SDK.TransitionFaultProof as never,
      ),
    ).not.toThrow();
  });

  it("rejects extra, missing, reordered, and substituted operations against an otherwise-valid dense trace", async () => {
    const idA = outRef(900);
    const idB = outRef(901);
    const idC = outRef(902);
    const keyA = depositEventKey(idA);
    const keyB = withdrawalEventKey(idB);
    const keyC = depositEventKey(idC);
    const finalUtxo = rawLedgerEntry(903);
    const finalRoot = await utxoRootWithDescriptors([finalUtxo]);

    const r0 = h32(910);
    const r1 = h32(911);
    const r2 = h32(912);
    const r3 = finalRoot.root;

    const stepA: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 0n,
      event_key: keyA,
      phase: "Deposit",
      pre_utxos_root: r0,
      post_utxos_root: r1,
    };
    const stepB: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 1n,
      event_key: keyB,
      phase: "Withdrawal",
      pre_utxos_root: r1,
      post_utxos_root: r2,
    };
    const stepC: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 2n,
      event_key: keyC,
      phase: "Deposit",
      pre_utxos_root: r2,
      post_utxos_root: r3,
    };

    const depositEntries = [
      encodedEntry({
        key: idA,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(920),
        valueSchema: SDK.DepositInfoSchema,
      }),
      encodedEntry({
        key: idC,
        keySchema: SDK.OutputReference as never,
        value: depositInfo(921),
        valueSchema: SDK.DepositInfoSchema,
      }),
    ];
    const withdrawalEntries = [
      encodedEntry({
        key: idB,
        keySchema: SDK.OutputReference as never,
        value: withdrawalInfo(922, "WithdrawalIsValid"),
        valueSchema: SDK.WithdrawalInfoSchema,
      }),
    ];

    // Control: the unmutated trace is total and reproduces the exact
    // post-state root.
    const baseline = await reconstruct(
      await buildPayloadFixture({
        prevUtxosRoot: r0,
        utxos: [finalUtxo],
        deposits: depositEntries,
        withdrawals: withdrawalEntries,
        steps: [stepA, stepB, stepC],
        eventToStep: [
          eventToStepEntry(keyA, { step_index: 0n, phase: "Deposit" }),
          eventToStepEntry(keyB, { step_index: 1n, phase: "Withdrawal" }),
          eventToStepEntry(keyC, { step_index: 2n, phase: "Deposit" }),
        ],
      }),
    );
    expect(await detectTransitionTraceFaults(baseline)).toEqual([]);
    expect(baseline.transitionTrace.at(-1)!.value.post_utxos_root).toBe(r3);
    expect(baseline.header.utxosRoot).toBe(r3);

    // Extra: a fourth, unbacked step is appended past the true final root.
    const stepD: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 3n,
      event_key: depositEventKey(outRef(930)),
      phase: "Deposit",
      pre_utxos_root: r3,
      post_utxos_root: h32(931),
    };
    const extraReconstruction = await reconstruct(
      await buildPayloadFixture({
        prevUtxosRoot: r0,
        utxos: [finalUtxo],
        deposits: depositEntries,
        withdrawals: withdrawalEntries,
        steps: [stepA, stepB, stepC, stepD],
        // stepD is deliberately left out of event_to_step: it has no backing
        // deposit/withdrawal/tx event, so mapping it would inflate
        // event_to_step's member count past total_event_count and fail
        // reconstruction before the detect layer ever runs.
        eventToStep: [
          eventToStepEntry(keyA, { step_index: 0n, phase: "Deposit" }),
          eventToStepEntry(keyB, { step_index: 1n, phase: "Withdrawal" }),
          eventToStepEntry(keyC, { step_index: 2n, phase: "Deposit" }),
        ],
      }),
    );
    expect(
      extraReconstruction.transitionTrace.at(-1)!.value.post_utxos_root,
    ).not.toBe(r3);
    const extraDetections =
      await detectTransitionTraceFaults(extraReconstruction);
    expect(
      extraDetections.some(
        (detection) =>
          detection.kind === "countFault" &&
          detection.invariant === "header_transition_step_count",
      ),
    ).toBe(true);
    expect(
      extraDetections.some(
        (detection) =>
          detection.kind === "traceBoundary" &&
          detection.invariant === "trace_end_utxos_root",
      ),
    ).toBe(true);

    // Missing: the middle operation is dropped, but the survivor keeps its
    // original positional index — leaving a hole a dense trace cannot have.
    await expect(
      reconstruct(
        await buildPayloadFixture({
          prevUtxosRoot: r0,
          utxos: [finalUtxo],
          deposits: depositEntries,
          withdrawals: [],
          transitionTraceEntries: [traceEntry(stepA), traceEntry(stepC)],
          eventToStep: [
            eventToStepEntry(keyA, { step_index: 0n, phase: "Deposit" }),
            eventToStepEntry(keyC, { step_index: 2n, phase: "Deposit" }),
          ],
        }),
      ),
    ).rejects.toMatchObject({
      code: "invalidPayloadEntries",
      message: expect.stringContaining("outside"),
    });

    // Reordered: A and B swap positions; each keeps its own true pre/post
    // roots, so the chain no longer starts from the committed prev-root.
    const stepBReordered: SDK.TransitionStep = { ...stepB, step_index: 0n };
    const stepAReordered: SDK.TransitionStep = { ...stepA, step_index: 1n };
    const reorderedDetections = await detectTransitionTraceFaults(
      await reconstruct(
        await buildPayloadFixture({
          prevUtxosRoot: r0,
          utxos: [finalUtxo],
          deposits: depositEntries,
          withdrawals: withdrawalEntries,
          transitionTraceEntries: [
            traceEntry(stepBReordered),
            traceEntry(stepAReordered),
            traceEntry(stepC),
          ],
          eventToStep: [
            eventToStepEntry(keyB, { step_index: 0n, phase: "Withdrawal" }),
            eventToStepEntry(keyA, { step_index: 1n, phase: "Deposit" }),
            eventToStepEntry(keyC, { step_index: 2n, phase: "Deposit" }),
          ],
        }),
      ),
    );
    expect(
      reorderedDetections.some(
        (detection) =>
          detection.kind === "traceBoundary" &&
          detection.invariant === "trace_start_prev_utxos_root",
      ),
    ).toBe(true);
    expect(
      reorderedDetections.some((detection) => detection.kind === "traceLink"),
    ).toBe(true);

    // Substituted: B keeps its identity and position, but its committed
    // post-root is swapped for a different value, breaking the link to C.
    const stepBSubstituted: SDK.TransitionStep = {
      ...stepB,
      post_utxos_root: h32(940),
    };
    const substitutedDetections = await detectTransitionTraceFaults(
      await reconstruct(
        await buildPayloadFixture({
          prevUtxosRoot: r0,
          utxos: [finalUtxo],
          deposits: depositEntries,
          withdrawals: withdrawalEntries,
          steps: [stepA, stepBSubstituted, stepC],
          eventToStep: [
            eventToStepEntry(keyA, { step_index: 0n, phase: "Deposit" }),
            eventToStepEntry(keyB, { step_index: 1n, phase: "Withdrawal" }),
            eventToStepEntry(keyC, { step_index: 2n, phase: "Deposit" }),
          ],
        }),
      ),
    );
    expect(
      substitutedDetections.some(
        (detection) =>
          detection.kind === "traceLink" &&
          detection.invariant === "adjacent_trace_roots",
      ),
    ).toBe(true);
  });
});
