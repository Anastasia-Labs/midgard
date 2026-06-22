import * as SDK from "@al-ft/midgard-sdk";
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
  detectTransitionTraceFaults,
  encodeData,
  fetchRetainedDaPayloadByHeaderHash,
  keyValuePhasRootWithCount,
  type OmittedDueL1EventEvidence,
  type OutOfWindowSourceEventEvidence,
  reconstructDaPayloadV2,
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

const txBody = (byte: number): SDK.MidgardTxBodyCompact => ({
  spend_inputs: h32(byte),
  reference_inputs: h32(byte + 1),
  outputs: h32(byte + 2),
  fee: 0n,
  validity_interval: {
    lower_bound: {
      bound_type: "NegativeInfinity",
      is_inclusive: true,
    },
    upper_bound: {
      bound_type: "PositiveInfinity",
      is_inclusive: false,
    },
  },
  required_observers: h32(byte + 3),
  required_signer_hashes: h32(byte + 4),
  mint: h32(byte + 5),
  script_integrity_hash: h32(byte + 6),
  auxiliary_data_hash: h32(byte + 7),
  network_id: "Testnet",
});

const forcedTx = (
  byte: number,
  operatorValidity: SDK.MidgardTxValidity = "FailedScript",
): SDK.ForcedInclusionTx => ({
  tx_compact: {
    body: txBody(byte),
    wits: h32(byte + 8),
  },
  operator_validity: operatorValidity,
});

const entry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

const sorted = (entries: readonly SDK.DaPayloadEntry[]): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
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
  deposits = [],
  steps = [],
  eventToStep = [],
}: PayloadFixtureInput): Promise<{
  readonly payload: SDK.DaPayloadV2;
  readonly payloadCbor: Buffer;
  readonly header: SDK.Header;
  readonly headerHash: string;
}> => {
  const utxoRoot = await keyValuePhasRootWithCount(
    utxos.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const roots = {
    withdrawals: await buildCountedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawals.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    forcedTransactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.forcedTransactions,
      forcedTransactions.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    transactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transactions,
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
    ...counts,
    startTime: 10n,
    endTime: 20n,
    prevHeaderHash: h28(90),
    operatorVkey: h28(91),
    protocolVersion: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayloadV2 = {
    version: SDK.DA_PAYLOAD_V2_VERSION,
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
      counts,
    },
  };
  return {
    payload,
    payloadCbor: SDK.encodeDaPayloadV2(payload),
    header,
    headerHash,
  };
};

const reconstruct = async (
  fixture: Awaited<ReturnType<typeof buildPayloadFixture>>,
  transactionProjector?: (payload: Buffer) => Buffer,
): Promise<TransitionTraceReconstruction> =>
  await reconstructDaPayloadV2({
    payloadCbor: fixture.payloadCbor,
    expectedHeaderHash: fixture.headerHash,
    committedHeader: fixture.header,
    transactionProjector,
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

  it("reconstructs every DA payload v2 root and rejects header/root mismatches", async () => {
    const txOrderId = outRef(1);
    const forced = forcedTx(10);
    const finalUtxo = entry(Buffer.from("01", "hex"), Buffer.from("02", "hex"));
    const finalRoot = await keyValuePhasRootWithCount([
      { key: Buffer.from("01", "hex"), value: Buffer.from("02", "hex") },
    ]);
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
          valueSchema: SDK.ForcedInclusionTxSchema,
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
    });
    expect(result.counts.totalEventCount).toBe(1n);

    const badHeader = {
      ...fixture.header,
      utxosRoot: h32(99),
    };
    const badHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeader(badHeader),
    );
    const badPayload: SDK.DaPayloadV2 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        header_hash: badHeaderHash,
        header: badHeader,
      },
    };
    await expect(
      reconstructDaPayloadV2({
        payloadCbor: SDK.encodeDaPayloadV2(badPayload),
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
                valueSchema: SDK.ForcedInclusionTxSchema,
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
    const finalUtxo = entry(Buffer.from("05", "hex"), Buffer.from("06", "hex"));
    const finalRoot = await keyValuePhasRootWithCount([
      { key: Buffer.from("05", "hex"), value: Buffer.from("06", "hex") },
    ]);
    const fixture = await buildPayloadFixture({
      utxos: [finalUtxo],
      forcedTransactions: [
        encodedEntry({
          key: txOrderId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(50, "FailedScript"),
          valueSchema: SDK.ForcedInclusionTxSchema,
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
          valueSchema: SDK.ForcedInclusionTxSchema,
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
          valueSchema: SDK.ForcedInclusionTxSchema,
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
          valueSchema: SDK.ForcedInclusionTxSchema,
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
    const txId = h32(12);
    const compactCbor = Buffer.from("8400", "hex");
    const identityProjector = (payload: Buffer): Buffer => payload;
    const eventKey: SDK.EventKey = { L2TransactionEventKey: { tx_id: txId } };
    const phaseMismatchFixture = await buildPayloadFixture({
      transactions: [entry(Buffer.from(txId, "hex"), compactCbor)],
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
      await reconstruct(phaseMismatchFixture, identityProjector),
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
          domain: SDK.ROOT_DOMAINS.transactions,
        },
      },
    });
  });

  it("fetches retained DA payloads from committee routes, not operator debug paths by default", async () => {
    const fixture = await buildPayloadFixture({});
    const calls: string[] = [];
    const fetchFn = async (url: string) => {
      calls.push(url);
      if (url.endsWith("/payload")) {
        return new Response(fixture.payloadCbor, {
          status: 200,
          headers: { "content-type": "application/cbor" },
        });
      }
      return new Response(JSON.stringify({ ok: true }), {
        status: 200,
        headers: { "content-type": "application/json" },
      });
    };

    const result = await fetchRetainedDaPayloadByHeaderHash({
      headerHash: fixture.headerHash,
      endpoints: [
        {
          baseUrl: "http://da-committee.test",
          deploymentFingerprint: "deployment-1",
        },
      ],
      fetchFn: fetchFn as typeof fetch,
      retries: 0,
    });

    expect(result.payloadCbor.equals(fixture.payloadCbor)).toBe(true);
    expect(calls[0]).toBe(
      `http://da-committee.test/v1/deployments/deployment-1/headers/${fixture.headerHash}/payload`,
    );
    await expect(
      fetchRetainedDaPayloadByHeaderHash({
        headerHash: fixture.headerHash,
        endpoints: ["http://operator-debug.test"],
        fetchFn: fetchFn as typeof fetch,
        retries: 0,
      }),
    ).rejects.toMatchObject({ code: "fetchFailed" });
  });
});
