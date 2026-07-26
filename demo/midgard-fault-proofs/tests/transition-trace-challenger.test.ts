import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonicalV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core/codec";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DaRequestResponseProtocol,
  decodeDaEventToStepByEventRequestV1Cbor,
  decodeDaPayloadByHeaderRequestV1Cbor,
  decodeDaProofBundleByHeaderRequestV1Cbor,
  decodeDaTraceStepByIndexRequestV1Cbor,
  encodeDaEventToStepByEventResponseV1Cbor,
  encodeDaMetadataByHeaderResponseV1Cbor,
  encodeDaPayloadByHeaderResponseV1Cbor,
  encodeDaProofBundleByHeaderResponseV1Cbor,
  encodeDaTraceStepByIndexResponseV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterialV1 } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  buildEventToStepMismatchFault,
  buildIndexedTraceProof,
  buildInvalidForcedTransactionNoOpWitness,
  buildOmittedDueL1EventFault,
  buildOutOfWindowSourceEventFault,
  buildSourceNonMembershipProof,
  buildSourcePhaseMismatchFault,
  buildTraceBoundaryFault,
  buildTransitionFaultProof,
  DaLibp2pRetainedDaSource,
  detectTransitionTraceFaults,
  encodeData,
  fetchRetainedDaPayloadByHeaderHash,
  keyValuePhasRootWithCount,
  type OmittedDueL1EventEvidence,
  type OutOfWindowSourceEventEvidence,
  reconstructDaPayloadV1,
  type RetainedDaLibp2pTransport,
  type TransitionTraceReconstruction,
} from "../src/transition-trace/index.js";

const h32 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);
const h28 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(28);

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
  validity: SDK.WithdrawalValidity = "IncorrectWithdrawalSignature",
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

const canonicalPreimageByCommitment = new Map<string, Buffer>();

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
  const transactionCommitment =
    computeMidgardNativeTxProofCommitmentV1(source).toString("hex");
  canonicalPreimageByCommitment.set(transactionCommitment, canonicalCbor);
  return {
    txId: computeMidgardNativeTxIdV1(full).toString("hex"),
    transactionCommitment,
    canonicalCbor,
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
};

const forcedTx = (
  byte: number,
  operatorValidity: SDK.MidgardTxValidity = "FailedScript",
): SDK.ForcedInclusionTxV1 => {
  const material = nativeMaterial(byte);
  return {
    tx_id: material.txId,
    transaction_commitment: material.transactionCommitment,
    source: material.source,
    operator_validity: operatorValidity,
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

const rawLedgerEntry = (byte: number): SDK.DaPayloadEntry => [
  `825820${h32(byte)}00`,
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

const traceEntry = (step: SDK.TransitionStep): SDK.DaPayloadEntry =>
  encodedEntry({
    key: step.step_index,
    keySchema: Data.Integer() as never,
    value: step,
    valueSchema: SDK.TransitionStepSchema,
  });

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
        forced.transaction_commitment,
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
      steps.map((step) => ({
        key: encodeData(step.step_index, Data.Integer() as never),
        value: encodeData(step, SDK.TransitionStepSchema),
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
    transitionStepCount: BigInt(steps.length),
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
      transition_trace: sorted(steps.map(traceEntry)),
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

describe("transition-trace challenger tooling", () => {
  const expectBuildableDetection = (
    detections: readonly unknown[],
    expected: {
      readonly kind: string;
      readonly invariant: string;
    },
  ) =>
    expect(detections).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          buildable: true,
          ...expected,
        }),
      ]),
    );

  it("reconstructs every DA payload V1 root and rejects header/root mismatches", async () => {
    const txOrderId = outRef(1);
    const forced = forcedTx(10);
    const finalUtxo = rawLedgerEntry(1);
    const finalRoot = await utxoRootWithDescriptors([finalUtxo]);
    const eventKey = forcedEventKey(txOrderId);
    const step: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 0n,
      event_key: eventKey,
      phase: "ForcedTransaction",
      pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      post_utxos_root: finalRoot.root,
    };
    const fixture = await buildPayloadFixture({
      utxos: [finalUtxo],
      forcedTransactions: [
        encodedEntry({
          key: txOrderId,
          keySchema: SDK.OutputReference as never,
          value: forced,
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      steps: [step],
      eventToStep: [
        eventToStepEntry(eventKey, {
          step_index: 0n,
          phase: "ForcedTransaction",
        }),
      ],
    });

    const result = await reconstruct(fixture);

    expect(result.roots).toEqual({
      utxosRoot: fixture.header.utxosRoot,
      withdrawalsRoot: fixture.header.withdrawalsRoot,
      forcedTransactionsRoot: fixture.header.forcedTransactionsRoot,
      transactionsRoot: fixture.header.transactionsRoot,
      depositsRoot: fixture.header.depositsRoot,
      transitionTraceRoot: fixture.header.transitionTraceRoot,
      eventToStepRoot: fixture.header.eventToStepRoot,
      validationTracesRoot: fixture.header.validationTracesRoot,
    });
    expect(result.counts.totalEventCount).toBe(1n);

    const badHeader = {
      ...fixture.header,
      utxosRoot: h32(99),
    };
    const badHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeaderV1(badHeader),
    );
    const badPayload: SDK.DaPayloadV1 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        header_hash: badHeaderHash,
        header: badHeader,
      },
    };
    await expect(
      reconstructDaPayloadV1({
        payloadEnvelopeCbor: await wrapDaPayloadV1(
          SDK.encodeDaPayloadV1(badPayload),
          { mode: "identity" },
        ),
        expectedHeaderHash: badHeaderHash,
        committedHeader: badHeader,
      }),
    ).rejects.toMatchObject({ code: "rootMismatch" });
  });

  it("builds witness redeemers for each Task08 proof family from reconstructed DA data", async () => {
    const withdrawalId = outRef(2);
    const eventKey = withdrawalEventKey(withdrawalId);
    const withdrawalInfo: SDK.WithdrawalInfo = {
      body: {
        l2_outref: outRef(22),
        l2_owner: h28(23),
        l2_value: new Map(),
        l1_address: address(24),
        l1_datum: "NoDatum",
      },
      signature: [h32(25), h32(26)],
      validity: "IncorrectWithdrawalSignature",
    };
    const step: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 0n,
      event_key: eventKey,
      phase: "Deposit",
      pre_utxos_root: h32(30),
      post_utxos_root: h32(31),
    };
    const fixture = await buildPayloadFixture({
      prevUtxosRoot: h32(29),
      withdrawals: [
        encodedEntry({
          key: withdrawalId,
          keySchema: SDK.OutputReference as never,
          value: withdrawalInfo,
          valueSchema: SDK.WithdrawalInfoSchema,
        }),
      ],
      steps: [step],
      eventToStep: [
        eventToStepEntry(eventKey, {
          step_index: 0n,
          phase: "Deposit",
        }),
      ],
    });
    const reconstruction = await reconstruct(fixture);

    const boundary = await buildTraceBoundaryFault({
      reconstruction,
      side: "TraceStart",
      stepIndex: 0n,
    });
    const eventMismatch = await buildEventToStepMismatchFault({
      reconstruction,
      stepIndex: 0n,
    });
    const sourcePhase = await buildSourcePhaseMismatchFault({
      reconstruction,
      stepIndex: 0n,
    });
    const invalidNoOp = SDK.invalidOneStepTransitionFault(
      await buildInvalidForcedTransactionNoOpWitness({
        reconstruction: await reconstruct(
          await buildPayloadFixture({
            forcedTransactions: [
              encodedEntry({
                key: outRef(3),
                keySchema: SDK.OutputReference as never,
                value: forcedTx(40, "FailedScript"),
                valueSchema: SDK.ForcedInclusionTxV1Schema,
              }),
            ],
            steps: [
              {
                schema_version: 1n,
                step_index: 0n,
                event_key: forcedEventKey(outRef(3)),
                phase: "ForcedTransaction",
                pre_utxos_root: h32(41),
                post_utxos_root: h32(42),
              },
            ],
            eventToStep: [
              eventToStepEntry(forcedEventKey(outRef(3)), {
                step_index: 0n,
                phase: "ForcedTransaction",
              }),
            ],
          }),
        ),
        stepIndex: 0n,
      }),
    );
    const omittedEvidence: OmittedDueL1EventEvidence = {
      kind: "forcedTransaction",
      txOrderId: outRef(4),
      eventRefInputIndex: 0n,
      eventAssetName: "aa",
      validityOverride: "FailedScript",
    };
    const omitted = await buildOmittedDueL1EventFault({
      reconstruction,
      evidence: omittedEvidence,
    });
    const outOfWindowEvidence: OutOfWindowSourceEventEvidence = {
      kind: "withdrawal",
      withdrawalId,
      eventRefInputIndex: 1n,
      eventAssetName: "bb",
      validityOverride: "IncorrectWithdrawalSignature",
    };
    const outOfWindow = await buildOutOfWindowSourceEventFault({
      reconstruction,
      evidence: outOfWindowEvidence,
    });
    const count = SDK.countFault("HeaderTotalCountMismatch");

    for (const fault of [
      boundary,
      eventMismatch,
      sourcePhase,
      invalidNoOp,
      omitted,
      outOfWindow,
      count,
    ]) {
      const proof = buildTransitionFaultProof({ reconstruction, fault });
      expect(() =>
        Data.from(
          Data.to(proof as never, SDK.TransitionFaultProof as never),
          SDK.TransitionFaultProof as never,
        ),
      ).not.toThrow();
    }
    await expect(
      buildIndexedTraceProof({ reconstruction, stepIndex: 0n }),
    ).resolves.toMatchObject({ key: 0n });
  });

  it("detects a wrong final root caused by an invalid forced no-op step", async () => {
    const txOrderId = outRef(5);
    const finalUtxo = rawLedgerEntry(5);
    const finalRoot = await utxoRootWithDescriptors([finalUtxo]);
    const fixture = await buildPayloadFixture({
      utxos: [finalUtxo],
      forcedTransactions: [
        encodedEntry({
          key: txOrderId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(50, "FailedScript"),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: forcedEventKey(txOrderId),
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: finalRoot.root,
        },
      ],
      eventToStep: [
        eventToStepEntry(forcedEventKey(txOrderId), {
          step_index: 0n,
          phase: "ForcedTransaction",
        }),
      ],
    });

    const detections = await detectTransitionTraceFaults(
      await reconstruct(fixture),
    );

    expect(detections).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          buildable: true,
          kind: "invalidOneStepTransition",
          invariant: "invalid_forced_transaction_is_no_op",
        }),
      ]),
    );
  });

  it("detects trace start, link, and final-root faults", async () => {
    const firstDepositId = outRef(6);
    const secondDepositId = outRef(7);
    const firstKey = depositEventKey(firstDepositId);
    const secondKey = depositEventKey(secondDepositId);
    const fixture = await buildPayloadFixture({
      prevUtxosRoot: h32(60),
      deposits: [
        encodedEntry({
          key: firstDepositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(61),
          valueSchema: SDK.DepositInfoSchema,
        }),
        encodedEntry({
          key: secondDepositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(62),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: firstKey,
          phase: "Deposit",
          pre_utxos_root: h32(63),
          post_utxos_root: h32(64),
        },
        {
          schema_version: 1n,
          step_index: 1n,
          event_key: secondKey,
          phase: "Deposit",
          pre_utxos_root: h32(65),
          post_utxos_root: h32(66),
        },
      ],
      eventToStep: [
        eventToStepEntry(firstKey, {
          step_index: 0n,
          phase: "Deposit",
        }),
        eventToStepEntry(secondKey, {
          step_index: 1n,
          phase: "Deposit",
        }),
      ],
    });

    const detections = await detectTransitionTraceFaults(
      await reconstruct(fixture),
    );

    expectBuildableDetection(detections, {
      kind: "traceBoundary",
      invariant: "trace_start_prev_utxos_root",
    });
    expectBuildableDetection(detections, {
      kind: "traceLink",
      invariant: "adjacent_trace_roots",
    });
    expectBuildableDetection(detections, {
      kind: "traceBoundary",
      invariant: "trace_end_utxos_root",
    });
  });

  it("detects header and committed-root count faults", async () => {
    const reconstruction = await reconstruct(await buildPayloadFixture({}));

    const totalMismatch = await detectTransitionTraceFaults({
      ...reconstruction,
      header: {
        ...reconstruction.header,
        totalEventCount: 1n,
      },
    });
    expectBuildableDetection(totalMismatch, {
      kind: "countFault",
      invariant: "header_total_event_count",
    });

    const stepCountMismatch = await detectTransitionTraceFaults({
      ...reconstruction,
      header: {
        ...reconstruction.header,
        transitionStepCount: 1n,
      },
    });
    expectBuildableDetection(stepCountMismatch, {
      kind: "countFault",
      invariant: "header_transition_step_count",
    });
    expectBuildableDetection(stepCountMismatch, {
      kind: "countFault",
      invariant: "transition_trace_root_count",
    });

    const committedRootMismatch = await detectTransitionTraceFaults({
      ...reconstruction,
      header: {
        ...reconstruction.header,
        depositCount: 1n,
        totalEventCount: 1n,
        transitionStepCount: 1n,
      },
    });
    expectBuildableDetection(committedRootMismatch, {
      kind: "countFault",
      invariant: "deposits_root_count",
    });
    expectBuildableDetection(committedRootMismatch, {
      kind: "countFault",
      invariant: "event_to_step_root_count",
    });
    expectBuildableDetection(committedRootMismatch, {
      kind: "countFault",
      invariant: "transition_trace_root_count",
    });
  });

  it("detects dangling trace, source, and event-to-step mappings", async () => {
    const txOrderId = outRef(8);
    const eventKey = forcedEventKey(txOrderId);
    const baseFixture = await buildPayloadFixture({
      forcedTransactions: [
        encodedEntry({
          key: txOrderId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(70, "FailedScript"),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(eventKey, {
          step_index: 0n,
          phase: "ForcedTransaction",
        }),
      ],
    });
    const base = await reconstruct(baseFixture);
    const empty = await reconstruct(await buildPayloadFixture({}));

    const mappedMissingSource = await detectTransitionTraceFaults({
      ...base,
      forcedTransactions: [],
      sourceEvents: [],
      sourceEventsByFingerprint: new Map(),
      rootData: {
        ...base.rootData,
        forcedTransactions: empty.rootData.forcedTransactions,
      },
    });
    expectBuildableDetection(mappedMissingSource, {
      kind: "sourceMembershipMismatch",
      invariant: "mapped_event_has_source_member",
    });

    const sourceMissingTraceMapping = await detectTransitionTraceFaults({
      ...base,
      eventToStep: [],
      eventToStepByFingerprint: new Map(),
      rootData: {
        ...base.rootData,
        eventToStep: empty.rootData.eventToStep,
      },
    });
    expectBuildableDetection(sourceMissingTraceMapping, {
      kind: "eventToStepMismatch",
      invariant: "event_to_step_matches_trace",
    });
    expectBuildableDetection(sourceMissingTraceMapping, {
      kind: "sourceMembershipMismatch",
      invariant: "source_event_has_event_to_step_member",
    });
  });

  it("detects omitted L1 events, out-of-window source events, and duplicate trace events", async () => {
    const depositId = outRef(6);
    const txOrderId = outRef(7);
    const duplicateKey = forcedEventKey(txOrderId);
    const duplicateFixture = await buildPayloadFixture({
      forcedTransactions: [
        encodedEntry({
          key: txOrderId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(60, "FailedScript"),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      deposits: [
        encodedEntry({
          key: depositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(61),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: duplicateKey,
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        {
          schema_version: 1n,
          step_index: 1n,
          event_key: duplicateKey,
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(duplicateKey, {
          step_index: 0n,
          phase: "ForcedTransaction",
        }),
        eventToStepEntry(depositEventKey(depositId), {
          step_index: 1n,
          phase: "Deposit",
        }),
      ],
    });
    const duplicateDetections = await detectTransitionTraceFaults(
      await reconstruct(duplicateFixture),
    );
    expect(duplicateDetections).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          buildable: true,
          kind: "duplicateTraceEvent",
          invariant: "trace_event_key_unique",
        }),
      ]),
    );

    const omittedFixture = await buildPayloadFixture({});
    const omittedDetections = await detectTransitionTraceFaults(
      await reconstruct(omittedFixture),
      {
        omittedDueL1Events: [
          {
            kind: "deposit",
            depositId: outRef(8),
            eventRefInputIndex: 0n,
            eventAssetName: "dd",
          },
          {
            kind: "withdrawal",
            withdrawalId: outRef(9),
            eventRefInputIndex: 1n,
            eventAssetName: "ee",
          },
          {
            kind: "forcedTransaction",
            txOrderId: outRef(10),
            eventRefInputIndex: 2n,
            eventAssetName: "cc",
            validityOverride: "FailedScript",
          },
        ],
      },
    );
    expect(omittedDetections).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          buildable: true,
          kind: "omittedDueL1Event",
          invariant: "due_l1_event_is_in_source_root",
        }),
      ]),
    );
    expect(
      omittedDetections.filter(
        (detection) =>
          detection.buildable &&
          detection.kind === "omittedDueL1Event" &&
          detection.invariant === "due_l1_event_is_in_source_root",
      ),
    ).toHaveLength(3);

    const withdrawalId = outRef(11);
    const outOfWindowForcedId = outRef(12);
    const outOfWindowDepositKey = depositEventKey(depositId);
    const outOfWindowWithdrawalKey = withdrawalEventKey(withdrawalId);
    const outOfWindowForcedKey = forcedEventKey(outOfWindowForcedId);
    const outOfWindowFixture = await buildPayloadFixture({
      deposits: [
        encodedEntry({
          key: depositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(71),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      withdrawals: [
        encodedEntry({
          key: withdrawalId,
          keySchema: SDK.OutputReference as never,
          value: withdrawalInfo(72),
          valueSchema: SDK.WithdrawalInfoSchema,
        }),
      ],
      forcedTransactions: [
        encodedEntry({
          key: outOfWindowForcedId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(73, "FailedScript"),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: outOfWindowWithdrawalKey,
          phase: "Withdrawal",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        {
          schema_version: 1n,
          step_index: 1n,
          event_key: outOfWindowForcedKey,
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        {
          schema_version: 1n,
          step_index: 2n,
          event_key: outOfWindowDepositKey,
          phase: "Deposit",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(outOfWindowWithdrawalKey, {
          step_index: 0n,
          phase: "Withdrawal",
        }),
        eventToStepEntry(outOfWindowForcedKey, {
          step_index: 1n,
          phase: "ForcedTransaction",
        }),
        eventToStepEntry(outOfWindowDepositKey, {
          step_index: 2n,
          phase: "Deposit",
        }),
      ],
    });
    const outOfWindowDetections = await detectTransitionTraceFaults(
      await reconstruct(outOfWindowFixture),
      {
        outOfWindowSourceEvents: [
          {
            kind: "deposit",
            depositId,
            eventRefInputIndex: 0n,
            eventAssetName: "aa",
          },
          {
            kind: "withdrawal",
            withdrawalId,
            eventRefInputIndex: 1n,
            eventAssetName: "bb",
            validityOverride: "IncorrectWithdrawalSignature",
          },
          {
            kind: "forcedTransaction",
            txOrderId: outOfWindowForcedId,
            eventRefInputIndex: 2n,
            eventAssetName: "cc",
            validityOverride: "FailedScript",
          },
        ],
      },
    );
    expect(
      outOfWindowDetections.filter(
        (detection) =>
          detection.buildable &&
          detection.kind === "outOfWindowSourceEvent" &&
          detection.invariant === "source_event_is_within_block_window",
      ),
    ).toHaveLength(3);
  });

  it("builds L2 source membership-mismatch witnesses against raw transaction roots", async () => {
    const material = nativeMaterial(12);
    const txId = material.txId;
    const source: SDK.L2TransactionSourceV1 = {
      tx_id: txId,
      transaction_commitment: material.transactionCommitment,
      source: material.source,
    };
    const eventKey: SDK.EventKey = { L2TransactionEventKey: { tx_id: txId } };
    const phaseMismatchFixture = await buildPayloadFixture({
      transactions: [
        entry(
          Buffer.from(txId, "hex"),
          Buffer.from(Data.to(source, SDK.L2TransactionSourceV1), "hex"),
        ),
      ],
      transactionPreimages: [
        entry(Buffer.from(txId, "hex"), material.canonicalCbor),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase: "Deposit",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(eventKey, {
          step_index: 0n,
          phase: "Deposit",
        }),
      ],
    });
    const phaseMismatchDetections = await detectTransitionTraceFaults(
      await reconstruct(phaseMismatchFixture),
    );

    expect(phaseMismatchDetections).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          buildable: true,
          kind: "sourceMembershipMismatch",
          invariant: "source_phase_matches_trace_phase",
        }),
      ]),
    );

    const emptyReconstruction = await reconstruct(
      await buildPayloadFixture({}),
    );
    await expect(
      buildSourceNonMembershipProof({
        reconstruction: emptyReconstruction,
        eventKey,
      }),
    ).resolves.toMatchObject({
      L2TransactionSourceNonMembership: {
        non_membership: {
          key: txId,
          domain: SDK.ROOT_DOMAINS.transactionsV1,
        },
      },
    });
  });

  it("builds both normal/forced classification fault directions", async () => {
    const forcedId = outRef(13);
    const forcedKey = forcedEventKey(forcedId);
    const forcedFixture = await buildPayloadFixture({
      forcedTransactions: [
        encodedEntry({
          key: forcedId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(13, "TxIsValid"),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: forcedKey,
          phase: "L2Transaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(forcedKey, {
          step_index: 0n,
          phase: "L2Transaction",
        }),
      ],
    });
    const material = nativeMaterial(14);
    const normalSource: SDK.L2TransactionSourceV1 = {
      tx_id: material.txId,
      transaction_commitment: material.transactionCommitment,
      source: material.source,
    };
    const normalKey: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: material.txId },
    };
    const normalFixture = await buildPayloadFixture({
      transactions: [
        entry(
          Buffer.from(material.txId, "hex"),
          Buffer.from(Data.to(normalSource, SDK.L2TransactionSourceV1), "hex"),
        ),
      ],
      transactionPreimages: [
        entry(Buffer.from(material.txId, "hex"), material.canonicalCbor),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: normalKey,
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(normalKey, {
          step_index: 0n,
          phase: "ForcedTransaction",
        }),
      ],
    });

    for (const fixture of [forcedFixture, normalFixture]) {
      const reconstruction = await reconstruct(fixture);
      const detections = await detectTransitionTraceFaults(reconstruction);
      expectBuildableDetection(detections, {
        kind: "sourceMembershipMismatch",
        invariant: "source_phase_matches_trace_phase",
      });
      const fault = await buildSourcePhaseMismatchFault({
        reconstruction,
        stepIndex: 0n,
      });
      expect(() =>
        Data.from(
          Data.to(fault as never, SDK.TransitionFault as never),
          SDK.TransitionFault as never,
        ),
      ).not.toThrow();
    }
  });

  it("fetches retained DA payloads and proof material over libp2p protocols", async () => {
    const fixture = await buildPayloadFixture({});
    const deploymentFingerprint = "11".repeat(32);
    const headerHashBytes = Buffer.from(fixture.headerHash, "hex");
    const payloadHash = computeDaSha256Hash(fixture.payloadEnvelopeCbor);
    const proofBundleBytes = Buffer.from("retained proof bundle");
    const proofBundleHash = computeDaSha256Hash(proofBundleBytes);
    const transitionStepBytes = Buffer.from("transition step");
    const traceMembershipProofBytes = Buffer.from("trace membership proof");
    const eventKeyBytes = Buffer.from("aabbcc", "hex");
    const eventToStepEntryBytes = Buffer.from("event to step entry");
    const eventProofBytes = Buffer.from("event proof");
    const calls: Array<{
      readonly peerId: string;
      readonly protocol: DaRequestResponseProtocol;
    }> = [];

    const transport: RetainedDaLibp2pTransport = {
      request: async ({ peer, protocol, payload }) => {
        calls.push({ peerId: peer.peerId, protocol });
        switch (protocol) {
          case DaRequestResponseProtocol.payloadByHeader: {
            const request = decodeDaPayloadByHeaderRequestV1Cbor(payload);
            expect(request.headerHash.equals(headerHashBytes)).toBe(true);
            return encodeDaPayloadByHeaderResponseV1Cbor({
              status: "found_inline",
              headerHash: request.headerHash,
              payloadHash,
              payloadBytes: fixture.payloadEnvelopeCbor,
              chunkManifest: null,
              reasonCode: null,
            });
          }
          case DaRequestResponseProtocol.metadataByHeader: {
            const request = decodeDaPayloadByHeaderRequestV1Cbor(payload);
            expect(request.headerHash.equals(headerHashBytes)).toBe(true);
            return encodeDaMetadataByHeaderResponseV1Cbor({
              status: "found",
              headerHash: request.headerHash,
              payloadHash,
              payloadSchemaVersion: 1,
              payloadBytes: fixture.payloadEnvelopeCbor.length,
              rootSummaryHash: computeDaSha256Hash(Buffer.from("root summary")),
              proofBundleHash,
              transitionTraceRoot: Buffer.from(
                fixture.header.transitionTraceRoot,
                "hex",
              ),
              eventToStepRoot: Buffer.from(
                fixture.header.eventToStepRoot,
                "hex",
              ),
              retainedUntilSlot: 123,
              localStatus: "verified",
            });
          }
          case DaRequestResponseProtocol.proofBundleByHeader: {
            const request = decodeDaProofBundleByHeaderRequestV1Cbor(payload);
            expect(request.headerHash.equals(headerHashBytes)).toBe(true);
            return encodeDaProofBundleByHeaderResponseV1Cbor({
              status: "found_inline",
              headerHash: request.headerHash,
              proofBundleHash,
              proofBundleBytes,
              chunkManifest: null,
              reasonCode: null,
            });
          }
          case DaRequestResponseProtocol.traceStepByIndex: {
            const request = decodeDaTraceStepByIndexRequestV1Cbor(payload);
            expect(request.headerHash.equals(headerHashBytes)).toBe(true);
            expect(request.stepIndex).toBe(0);
            return encodeDaTraceStepByIndexResponseV1Cbor({
              status: "found",
              headerHash: request.headerHash,
              stepIndex: request.stepIndex,
              transitionStepBytes,
              membershipProofBytes: traceMembershipProofBytes,
            });
          }
          case DaRequestResponseProtocol.eventToStepByEvent: {
            const request = decodeDaEventToStepByEventRequestV1Cbor(payload);
            expect(request.headerHash.equals(headerHashBytes)).toBe(true);
            expect(request.eventKey.equals(eventKeyBytes)).toBe(true);
            return encodeDaEventToStepByEventResponseV1Cbor({
              status: "found",
              headerHash: request.headerHash,
              eventKey: request.eventKey,
              eventToStepEntryBytes,
              membershipOrNonmembershipProofBytes: eventProofBytes,
            });
          }
          default:
            throw new Error(`unexpected protocol ${protocol}`);
        }
      },
    };
    const source = new DaLibp2pRetainedDaSource({
      sourceId: "committee-libp2p",
      deploymentFingerprint,
      peers: [{ peerId: "peer-a" }],
      transport,
    });

    const result = await fetchRetainedDaPayloadByHeaderHash({
      headerHash: fixture.headerHash,
      sources: [source],
      retries: 0,
    });

    expect(result.sourceId).toBe("committee-libp2p");
    expect(result.sourcePeerId).toBe("peer-a");
    expect(result.payloadEnvelopeCbor.equals(fixture.payloadEnvelopeCbor)).toBe(
      true,
    );
    expect(result.metadata).toMatchObject({
      status: "found",
      payloadBytes: fixture.payloadEnvelopeCbor.length,
      localStatus: "verified",
    });

    const proofBundle = await source.fetchProofBundleByHeaderHash(
      fixture.headerHash,
    );
    if (!proofBundle.ok) {
      throw new Error("expected proof bundle response");
    }
    expect(proofBundle.proofBundleHash.equals(proofBundleHash)).toBe(true);
    expect(proofBundle.proofBundleBytes.equals(proofBundleBytes)).toBe(true);

    const traceStep = await source.fetchTraceStepByIndex({
      headerHash: fixture.headerHash,
      stepIndex: 0,
    });
    if (!traceStep.ok) {
      throw new Error("expected trace step response");
    }
    expect(traceStep.transitionStepBytes.equals(transitionStepBytes)).toBe(
      true,
    );
    expect(
      traceStep.membershipProofBytes.equals(traceMembershipProofBytes),
    ).toBe(true);

    const eventToStep = await source.fetchEventToStepByEvent({
      headerHash: fixture.headerHash,
      eventKey: eventKeyBytes,
    });
    if (!eventToStep.ok) {
      throw new Error("expected event-to-step response");
    }
    if (
      eventToStep.eventToStepEntryBytes === null ||
      eventToStep.membershipOrNonmembershipProofBytes === null
    ) {
      throw new Error("expected event-to-step proof bytes");
    }
    expect(
      eventToStep.eventToStepEntryBytes.equals(eventToStepEntryBytes),
    ).toBe(true);
    expect(
      eventToStep.membershipOrNonmembershipProofBytes.equals(eventProofBytes),
    ).toBe(true);

    expect(calls).toEqual([
      {
        peerId: "peer-a",
        protocol: DaRequestResponseProtocol.payloadByHeader,
      },
      {
        peerId: "peer-a",
        protocol: DaRequestResponseProtocol.metadataByHeader,
      },
      {
        peerId: "peer-a",
        protocol: DaRequestResponseProtocol.proofBundleByHeader,
      },
      {
        peerId: "peer-a",
        protocol: DaRequestResponseProtocol.traceStepByIndex,
      },
      {
        peerId: "peer-a",
        protocol: DaRequestResponseProtocol.eventToStepByEvent,
      },
    ]);
  });

  it("fails closed when libp2p sources do not retain the requested payload", async () => {
    const fixture = await buildPayloadFixture({});
    const source = {
      sourceId: "empty-libp2p",
      fetchPayloadByHeaderHash: async () => ({
        ok: false as const,
        sourceId: "empty-libp2p",
        attempts: [
          {
            sourceId: "empty-libp2p",
            sourcePeerId: "peer-a",
            protocol: DaRequestResponseProtocol.payloadByHeader,
            status: "not_found" as const,
            detail: "payload not found",
          },
        ],
      }),
    };

    await expect(
      fetchRetainedDaPayloadByHeaderHash({
        headerHash: fixture.headerHash,
        sources: [source],
        retries: 0,
      }),
    ).rejects.toMatchObject({ code: "fetchFailed" });
  });
});
