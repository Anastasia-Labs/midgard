import {
  encodeCbor,
  encodeMidgardFieldPreimage,
} from "@al-ft/midgard-core/codec";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type { MidgardRejectionReasonV1 } from "@al-ft/midgard-core/validation-trace";
import { EventKey } from "@al-ft/midgard-sdk";
import { Lambda, UPLCEncoder, UPLCProgram, UPLCVar } from "@harmoniclabs/uplc";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  RejectCodes,
  rejectionReasonOfSubject,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  validationMachineLedgerRoot,
} from "../src/index.js";
import type { RejectCode } from "../src/types.js";
import {
  buildValidationOneStepArgument,
  validationMachineStateData,
} from "../src/validation-machine-data.js";
import {
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

/**
 * Shared harness of the emission-order rows. A row is one transaction against
 * one pre-state. The producer (Phase A, then Phase B), the trace builder and
 * the one-step argument of the trace's stopping step must all name the row's
 * expected first fault.
 */

// A fixed key, so that a row's bytes do not change between runs.
export const KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x6b));
export const keyAddress = (networkId: number, keyByte: number) =>
  Buffer.from(
    CML.EnterpriseAddress.new(
      networkId,
      CML.Credential.new_pub_key(
        CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, keyByte))
          .to_public()
          .hash(),
      ),
    )
      .to_address()
      .to_raw_bytes(),
  );
export const KEY_ADDRESS = keyAddress(0, 0x6b);
export const funded = makeOutput(FUNDED_OUTPUT_LOVELACE, KEY_ADDRESS);
export const keySpent = outRefFromByte(0x81);

/** A Plutus V3 spend the rows never execute: each stops before the CEK. */
export const plutus = plutusV3ScriptWitness(
  encodeCbor(
    Buffer.from(
      UPLCEncoder.compile(
        new UPLCProgram([1, 1, 0], new Lambda(new UPLCVar(0))),
      ),
    ),
  ),
);

export const nativeAll: Parameters<typeof nativeScriptWitness>[0] = {
  type: "all",
  scripts: [],
};
export const native = nativeScriptWitness(nativeAll);
export const NATIVE_POLICY = hashScriptWitness(native);

const assetName = Buffer.from("42", "hex");
export const mintOf = (policy: string) =>
  makeMintPreimageCbor(
    new Map([[Buffer.from(policy, "hex"), new Map([[assetName, 5n]])]]),
  );
export const mintedOutput = (policy: string) =>
  makeOutput(
    FUNDED_OUTPUT_LOVELACE,
    KEY_ADDRESS,
    new Map([[policy, new Map([[assetName.toString("hex"), 5n]])]]),
  );

export type NativeTxOptions = NonNullable<Parameters<typeof makeNativeTx>[0]>;

/**
 * An input-resolution reason with its `input_index` left out: the arm, and the
 * source kind where the arm has one. The machine's input ordinal for these
 * arms is pending X-N2C2, so a row asserts only the rest of the reason, plus
 * the producer's subject ordinal: the input's position in its own field.
 */
export type PendingInputIndexReason = {
  readonly pendingInputIndex:
    | { readonly InputNotFound: { readonly source_kind: bigint } }
    | { readonly SpendInputSignerMissing: Record<string, never> };
  readonly producerInputOrdinal: bigint;
};

export type Expected = {
  readonly code: RejectCode;
  /**
   * `null` when the rejection records no subject: a canonical decode refusal,
   * which a direct proof convicts rather than a machine trace.
   */
  readonly reason: MidgardRejectionReasonV1 | PendingInputIndexReason | null;
};

const isPendingInputIndex = (
  expected: Expected["reason"],
): expected is PendingInputIndexReason =>
  typeof expected === "object" &&
  expected !== null &&
  "pendingInputIndex" in expected;

/** A reason as a row asserts it: without `input_index` when that is pending. */
const assertedReason = (
  expected: Expected["reason"],
  actual: MidgardRejectionReasonV1 | null | undefined,
): unknown => {
  if (!isPendingInputIndex(expected)) return actual;
  if (typeof actual !== "object" || actual === null) return actual;
  return Object.fromEntries(
    Object.entries(actual).map(([arm, fields]) => {
      const { input_index: _pending, ...rest } = fields as Record<
        string,
        unknown
      >;
      return [arm, rest];
    }),
  );
};
const expectedReason = (expected: Expected["reason"]): unknown =>
  isPendingInputIndex(expected) ? expected.pendingInputIndex : expected;

export type Row = {
  readonly name: string;
  /** `refusal`: a fault class the machine stops on before the case's scan. */
  readonly kind: "multi" | "control" | "accept" | "refusal";
  /** The producer already emitted this verdict before the reconciliation. */
  readonly guard: boolean;
  /** The Phase B route Phase A admits the row to, when the row pins it. */
  readonly route?: "full" | "plain";
  readonly state: readonly (readonly [Buffer, Buffer])[];
  readonly tx: NativeTxOptions & { readonly spendInputs: readonly Buffer[] };
  /**
   * Raw field-6 items, each a versioned-script item, in place of the encoded
   * `tx.scriptWitnesses`: a row can carry an item no script encoder writes.
   */
  readonly scriptItems?: readonly Buffer[];
  /**
   * A forced transaction: the tx-order mint admits one whose outputs or
   * script items do not decode, so its rows can carry such items.
   */
  readonly source?: "forced";
  readonly expected: Expected | null;
};

/** Every multi-fault row has a single-fault control per fault. */
export const expectControlledRows = (
  rows: readonly Row[],
  prefixes: readonly string[],
) => {
  const names = rows.map(({ name }) => name);
  expect(new Set(names).size).toBe(names.length);
  expect(rows.filter(({ kind }) => kind === "accept")).toHaveLength(1);
  for (const prefix of prefixes) {
    const selected = rows.filter(({ name }) => name.startsWith(prefix));
    const count = (kind: Row["kind"]) =>
      selected.filter((row) => row.kind === kind).length;
    expect(count("multi")).toBeGreaterThan(0);
    expect(count("control")).toBeGreaterThanOrEqual(2);
  }
};

const PHASE_A_CONFIG = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase1_midgard",
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
} as const;

export const transactionOf = (row: Row) => {
  const transaction = makeNativeTx({ ...row.tx, privateKey: KEY });
  const witnessSet =
    row.scriptItems === undefined
      ? transaction.tx.witnessSet
      : {
          ...transaction.tx.witnessSet,
          scriptTxWitsPreimageCbor: encodeMidgardFieldPreimage(row.scriptItems),
        };
  if (row.source === "forced") {
    // A forced transaction's id is its body's, which the key signed.
    const { version, body } = transaction.tx;
    return {
      txId: transaction.txId,
      txCbor: encodeMidgardForcedTxCanonical(
        materializeMidgardForcedTxFromCanonical({ version, body, witnessSet }),
      ),
    };
  }
  if (row.scriptItems === undefined) return transaction;
  // The key signs the body, so the witness set can change under it.
  return encodeRecomputedNativeTx({ ...transaction.tx, witnessSet });
};

/** The event key of a row's source: a forced one names its order. */
const eventKeyOf = (row: Row) =>
  row.source === "forced"
    ? Buffer.from(
        Data.to(
          {
            ForcedTransactionEventKey: {
              tx_order_id: {
                transactionId: "4d".repeat(32),
                outputIndex: 0n,
              },
            },
          },
          EventKey,
        ),
        "hex",
      )
    : encodeCbor([2n, Buffer.alloc(32, 0x41)]);

/** The producer: Phase A, then Phase B on what Phase A admits. */
export const producer = async (row: Row) => {
  const transaction = transactionOf(row);
  const phaseA = await Effect.runPromise(
    runPhaseAValidation(
      [
        {
          txId: transaction.txId,
          txCbor: transaction.txCbor,
          sourceKind: row.source ?? "normal",
          arrivalSeq: 0n,
          createdAt: new Date(0),
        },
      ],
      PHASE_A_CONFIG,
    ),
  );
  if (phaseA.rejected.length > 0) {
    return { rejection: phaseA.rejected[0]!, route: undefined };
  }
  const route = phaseA.accepted[0]!.derived.requiresLocalScriptDiscovery
    ? ("full" as const)
    : ("plain" as const);
  const phaseB = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      phaseA.accepted,
      new Map(row.state.map(([key, output]) => [key.toString("hex"), output])),
      {
        nowCardanoSlotNo: 100n,
        bucketConcurrency: 1,
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      },
    ),
  );
  return { rejection: phaseB.rejected[0] ?? null, route };
};

export const machineTrace = async (row: Row) => {
  const transaction = transactionOf(row);
  const entries = row.state.map(([outRef, output]) => ({ outRef, output }));
  const accepted = row.expected === null;
  const expectedLedgerOps = accepted
    ? [
        ...row.tx.spendInputs.map((key) => ({ type: "delete" as const, key })),
        ...(row.tx.outputs ?? []).map((outputCbor, index) =>
          buildValidationMachineLedgerInsertOp({
            key: outRefFromTxId(transaction.txId, BigInt(index)),
            outputCbor,
          }),
        ),
      ]
    : [];
  const ledgerMutationSteps = accepted
    ? await buildValidationMachineLedgerMutationSteps({
        initialEntries: entries,
        operations: expectedLedgerOps,
      })
    : [];
  // A rejection leaves the ledger as it is: input resolution proves each
  // lookup against this root.
  const unchangedRoot = (await validationMachineLedgerRoot(entries)).toString(
    "hex",
  );
  return await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: eventKeyOf(row),
      sourceKind: row.source ?? "normal",
      blockTimeMs: 1_750_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: accepted
        ? ledgerMutationSteps[0]!.preRoot.toString("hex")
        : unchangedRoot,
      postUtxosRoot: accepted
        ? ledgerMutationSteps.at(-1)!.postRoot.toString("hex")
        : unchangedRoot,
      ledgerWitnessEntries: entries,
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: accepted ? "accepted" : "rejected",
      expectedRejectionCode: row.expected?.code ?? null,
    }),
  );
};

/** The trace names the row's verdict: its code and its asserted reason. */
export const expectTraceVerdict = (
  row: Row,
  trace: DeterministicValidationMachineTrace,
) => {
  expect(trace.verdict).toBe(row.expected === null ? "accepted" : "rejected");
  expect(trace.rejectionCode).toBe(row.expected?.code ?? null);
  if (row.expected !== null) {
    expect(
      assertedReason(row.expected.reason, trace.rejectionReason),
    ).toStrictEqual(expectedReason(row.expected.reason));
  }
};

/** The trace's stopping step: the step into its terminal state. */
export const stoppingStep = (trace: DeterministicValidationMachineTrace) =>
  trace.witnesses.length -
  1 -
  [...trace.witnesses]
    .reverse()
    .findIndex((witness) => witness.phase !== "terminal");

/** The stopping step as on-chain one-step input, each part as CBOR hex. */
export const stoppingStepVector = (
  trace: DeterministicValidationMachineTrace,
) => {
  const stateIndex = stoppingStep(trace);
  const argument = buildValidationOneStepArgument({ trace, stateIndex });
  return {
    phase: trace.witnesses[stateIndex]!.phase,
    pre: Data.to(
      validationMachineStateData(trace.states[stateIndex]!) as never,
    ),
    transition: argument.transitionCbor.toString("hex"),
    auxiliary: argument.auxiliaryCbor.toString("hex"),
  };
};

/** The producer's verdict, then the trace's; returns the trace. */
export const expectProducerAndTraceVerdicts = async (row: Row) => {
  const { rejection, route } = await producer(row);
  if (row.route !== undefined) expect(route).toBe(row.route);
  // An output network mismatch is tx-global and records no subject.
  const producerReason =
    rejection?.subject === undefined
      ? rejection?.code === RejectCodes.NetworkIdMismatch
        ? "NetworkIdMismatch"
        : null
      : rejectionReasonOfSubject(rejection.subject, "machine");
  if (row.expected === null) {
    expect(rejection).toBeNull();
  } else {
    expect(rejection?.code).toBe(row.expected.code);
    expect(assertedReason(row.expected.reason, producerReason)).toStrictEqual(
      expectedReason(row.expected.reason),
    );
    if (isPendingInputIndex(row.expected.reason)) {
      expect(rejection?.subject).toMatchObject({
        index: row.expected.reason.producerInputOrdinal,
      });
    }
  }

  const trace = await machineTrace(row);
  expectTraceVerdict(row, trace);
  // The producer and the trace builder name the same reason, whole.
  expect(trace.rejectionReason).toStrictEqual(producerReason);
  return trace;
};

/**
 * The producer's verdict, then the trace's, then the stopping step's; returns
 * the trace.
 */
export const expectRowVerdictsTrace = async (row: Row) => {
  const trace = await expectProducerAndTraceVerdicts(row);

  // The stopping step's one-step argument builds from this trace.
  const argument = buildValidationOneStepArgument({
    trace,
    stateIndex: stoppingStep(trace),
  });
  expect(argument.semanticResolverIndex).toBeGreaterThanOrEqual(0);
  return trace;
};

/** The producer's verdict, then the trace's, then the stopping step's. */
export const expectRowVerdicts = async (row: Row): Promise<void> => {
  await expectRowVerdictsTrace(row);
};
