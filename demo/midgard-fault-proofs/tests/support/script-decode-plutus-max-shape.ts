/**
 * Shapes, the producer's verdict, and a transaction journal for the
 * two-polarity Plutus maximum-shape suites of `witnessScriptDecoding` (item
 * kind 0, a field-6 witness) and `outputReferenceScriptDecoding` (item kind
 * 1, an output reference script).
 *
 * Every subject is a transaction the producer could see: it spends a funded
 * output and is otherwise valid, so its first fault, if any, is the Plutus
 * item. The honest program ignores its argument and a padding constant and
 * returns unit, and it branches on nothing.
 */
import { encodeCbor, MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import {
  encodeMidgardTxOutput,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
  type RejectionReason,
  rejectionReasonPhase,
} from "@al-ft/midgard-sdk";
import {
  MidgardRedeemerTag,
  resolveRejectionReason,
  runPhaseBValidationWithPatch,
  validatePhaseASingle,
} from "@al-ft/midgard-validation";
import {
  encodeRecomputedNativeTx,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
  TEST_ADDRESS_BYTES,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import type { Emulator, LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { captureEmulatorSubmission } from "./emulator/measurement.js";
import { FlatBits, program } from "./script-decode-walk-shapes.js";

/** The most Walks a record may take (`script-decode-walk-gate.ts`). */
export const WALK_CEILING = 1_500;

/** One emulator block, which each Bind, Walk and Finish takes. */
const EMULATOR_BLOCK_MS = 20_000;

/**
 * The default reference-script auth window plus the longest record the Walk
 * gate allows (Bind, 1,500 Walks, Finish), so a harness can still publish its
 * references after recording a maximum script.
 */
export const MAX_RECORD_AUTH_TIMELOCK_MS =
  REFERENCE_SCRIPT_AUTH_TIMELOCK_MS + (WALK_CEILING + 2) * EMULATOR_BLOCK_MS;

/** The aggregate field bound; a single item is four bytes narrower. */
export const MAXIMUM_FIELD_BYTES = 32_768;
export const MAXIMUM_ITEM_BYTES = MAXIMUM_FIELD_BYTES - 4;
/** The output bound. */
export const MAXIMUM_OUTPUT_BYTES = 16_384;

/** Enough for any output the bound admits, and a five-byte coin. */
const SUBJECT_LOVELACE = 1_000_000_000n;

/** The script bytes: `flat` wrapped as one CBOR byte string. */
const wrapped = (flat: Buffer): Buffer =>
  Buffer.concat([
    Buffer.from([0x59, flat.length >> 8, flat.length & 0xff]),
    flat,
  ]);

/**
 * Flat 1.1.0 followed by `delay` after `delay` (four bits each) until the
 * bytes run out: the densest Flat there is, refused only at its last bit.
 */
export const delayChainScript = (flatBytes: number): Buffer =>
  wrapped(
    Buffer.concat([
      Buffer.from("010100", "hex"),
      Buffer.alloc(flatBytes - 3, 0x11),
    ]),
  );

/**
 * `(program 1.1.0 (lam ctx [(lam pad (con unit ())) (con bytestring P)]))`
 * with `padBytes` bytes of padding: it returns unit for any argument, and
 * decodes.
 */
export const honestScript = (padBytes: number): Buffer =>
  program((bits: FlatBits) => {
    bits.put(2, 4);
    bits.put(3, 4);
    bits.put(2, 4);
    bits.constant([3]);
    bits.constant([1]);
    bits.chunks(Buffer.alloc(padBytes, 0x5a));
  });

/** The widest script of `make` whose `measure` is exactly `length`. */
const exactly = (
  make: (size: number) => Buffer,
  measure: (script: Buffer) => number,
  length: number,
): Buffer => {
  for (let size = length; size > length - 512; size -= 1) {
    const script = make(size);
    if (measure(script) === length) return script;
  }
  throw new Error(`no script measures ${length.toString()} bytes`);
};

/** A PlutusV3 field-6 item `[3, script]`. */
export const plutusItem = (script: Buffer): Buffer => encodeCbor([3, script]);

/** A funded output at the test key whose reference script is `script`. */
export const plutusReferenceOutput = (script: Buffer): Buffer =>
  Buffer.from(
    encodeMidgardTxOutput({
      address: TEST_ADDRESS_BYTES,
      value: { lovelace: SUBJECT_LOVELACE, assets: new Map() },
      script_ref: { language: "PlutusV3", scriptBytes: script },
    }),
  );

const itemLength = (script: Buffer) => plutusItem(script).length;
const outputLength = (script: Buffer) => plutusReferenceOutput(script).length;

/** The delay-chain script whose item fills the field bound. */
export const maximumDelayChainWitnessScript = () =>
  exactly(delayChainScript, itemLength, MAXIMUM_ITEM_BYTES);
/** The delay-chain script whose output is exactly the output bound. */
export const maximumDelayChainReferenceScript = () =>
  exactly(delayChainScript, outputLength, MAXIMUM_OUTPUT_BYTES);
/** The honest script whose item fills the field bound. */
export const maximumHonestWitnessScript = () =>
  exactly(honestScript, itemLength, MAXIMUM_ITEM_BYTES);
/** The honest script whose output is exactly the output bound. */
export const maximumHonestReferenceScript = () =>
  exactly(honestScript, outputLength, MAXIMUM_OUTPUT_BYTES);

/** A spendable pre-state output. */
export type PreStateEntry = Readonly<{ outRef: Buffer; output: Buffer }>;

export type ProducerSubject = Readonly<{
  nativeTx: MidgardNativeTxFull;
  txId: Buffer;
  txCbor: Buffer;
  /** Field 6, item by item. */
  items: readonly Buffer[];
  /** The outputs it spends. */
  state: readonly PreStateEntry[];
}>;

const FUNDED_KEY_INPUT = outRefFromByte(0xa1);
const PLUTUS_LOCKED_INPUT = outRefFromByte(0xa2);

/**
 * The transaction that spends one key-locked funded output into one output
 * and carries `items` in field 6: with an undecodable Plutus item, its first
 * fault is that item (kind 0).
 */
export const witnessSubject = (items: readonly Buffer[]): ProducerSubject => {
  const fixture = makeNativeTx({
    spendInputs: [FUNDED_KEY_INPUT],
    outputs: [makeOutput(SUBJECT_LOVELACE)],
  });
  const nativeTx: MidgardNativeTxFull = {
    ...fixture.tx,
    witnessSet: {
      ...fixture.tx.witnessSet,
      scriptTxWitsPreimageCbor: encodeCbor([...items]),
    },
  };
  return subjectOf(nativeTx, items, [
    { outRef: FUNDED_KEY_INPUT, output: makeOutput(SUBJECT_LOVELACE) },
  ]);
};

/**
 * The transaction that spends one key-locked funded output into `output`:
 * with an undecodable reference script, its first fault is that script
 * (kind 1).
 */
export const referenceScriptSubject = (output: Buffer): ProducerSubject => {
  const fixture = makeNativeTx({
    spendInputs: [FUNDED_KEY_INPUT],
    outputs: [output],
  });
  return subjectOf(
    fixture.tx,
    [],
    [{ outRef: FUNDED_KEY_INPUT, output: makeOutput(SUBJECT_LOVELACE) }],
  );
};

/**
 * The honest transaction: it spends an output locked by `witnessScript`,
 * carried in field 6 and run by one Spend redeemer, into an output whose
 * reference script is `referenceScript`.
 */
export const honestSpendSubject = (
  witnessScript: Buffer,
  referenceScript: Buffer,
): ProducerSubject => {
  const witness = plutusV3ScriptWitness(witnessScript);
  const locked = makeProtectedScriptOutput(
    hashScriptWitness(witness),
    SUBJECT_LOVELACE,
  );
  const fixture = makeNativeTx({
    spendInputs: [PLUTUS_LOCKED_INPUT],
    outputs: [plutusReferenceOutput(referenceScript)],
    scriptWitnesses: [witness],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor([
      {
        tag: MidgardRedeemerTag.Spend,
        index: 0n,
        exUnits: [1_000_000n, 500_000_000n],
      },
    ]),
    scriptLanguages: ["PlutusV3"],
  });
  return subjectOf(
    fixture.tx,
    [plutusItem(witnessScript)],
    [{ outRef: PLUTUS_LOCKED_INPUT, output: locked }],
  );
};

const subjectOf = (
  nativeTx: MidgardNativeTxFull,
  items: readonly Buffer[],
  state: readonly PreStateEntry[],
): ProducerSubject => {
  // Recompute the compact view, id and encoding after a field edit.
  const fixture = encodeRecomputedNativeTx(nativeTx);
  expect(fixture.tx.witnessSet.scriptTxWitsPreimageCbor).toEqual(
    encodeCbor([...items]),
  );
  return {
    nativeTx: fixture.tx,
    txId: fixture.txId,
    txCbor: fixture.txCbor,
    items,
    state,
  };
};

export type ProducerVerdict =
  | Readonly<{ verdict: "accepted" }>
  | Readonly<{ verdict: "rejected"; reason: RejectionReason; phase: number }>;

/**
 * What the producer decides for `subject` over its pre-state: Phase A, then
 * Phase B, and the machine reason and first-fault phase of a rejection.
 */
export const producerVerdict = async (
  subject: ProducerSubject,
): Promise<ProducerVerdict> => {
  const rejected = (
    rejection: Parameters<typeof resolveRejectionReason>[0],
  ) => {
    const resolved = resolveRejectionReason(rejection, "machine");
    if (resolved.kind !== "reason")
      throw new Error("the producer's rejection resolves to no machine reason");
    const reason = resolved.reason as RejectionReason;
    return {
      verdict: "rejected" as const,
      reason,
      phase: rejectionReasonPhase(reason, { body_network_mismatch: false }),
    };
  };
  const phaseA = validatePhaseASingle(
    {
      txId: subject.txId,
      txCbor: subject.txCbor,
      arrivalSeq: 0n,
      createdAt: new Date(0),
    },
    {
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
    },
  );
  if (!("ledgerTx" in phaseA)) return rejected(phaseA);
  const phaseB = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      [phaseA],
      new Map(
        subject.state.map(({ outRef, output }) => [
          outRef.toString("hex"),
          output,
        ]),
      ),
      {
        nowCardanoSlotNo: 1_000n,
        bucketConcurrency: 1,
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      },
    ),
  );
  const [first] = phaseB.rejected;
  if (first !== undefined) return rejected(first);
  expect(phaseB.accepted).toHaveLength(1);
  return { verdict: "accepted" };
};

/** One submitted transaction, or a summary of a run of them. */
type JournalRow = Readonly<{
  label: string;
  transactions: number;
  bytes: number;
  mem: bigint;
  steps: bigint;
}>;

/**
 * Captures every transaction a stage submits, labelled by stage, and checks
 * each against the ledger's limits. A summarized stage (a record's Walks)
 * keeps one row with its count and its largest bytes, memory and steps.
 */
export const createTransactionJournal = (
  emulator: Emulator,
  lucid: LucidEvolution,
) => {
  const rows: JournalRow[] = [];
  const stage = async <T>(
    label: string,
    operation: () => Promise<T>,
    { summarize = false } = {},
  ): Promise<T> => {
    const captured = await captureEmulatorSubmission(emulator, operation);
    const measured = captured.measurements.map((measurement, index) => ({
      label:
        captured.measurements.length === 1
          ? label
          : `${label}#${(index + 1).toString()}`,
      transactions: 1,
      bytes: measurement.completeSignedBytes,
      mem: measurement.executionMemory,
      steps: measurement.executionSteps,
    }));
    if (!summarize) rows.push(...measured);
    else
      rows.push({
        label,
        transactions: measured.length,
        bytes: Math.max(...measured.map((row) => row.bytes)),
        mem: measured.reduce((max, row) => (row.mem > max ? row.mem : max), 0n),
        steps: measured.reduce(
          (max, row) => (row.steps > max ? row.steps : max),
          0n,
        ),
      });
    return captured.result;
  };
  const expectWithinLedgerLimits = () => {
    const limits = lucid.config().protocolParameters;
    if (limits === undefined) throw new Error("ledger parameters absent");
    for (const row of rows) {
      expect(row.bytes, row.label).toBeLessThanOrEqual(limits.maxTxSize);
      expect(row.mem, row.label).toBeLessThanOrEqual(limits.maxTxExMem);
      expect(row.steps, row.label).toBeLessThanOrEqual(limits.maxTxExSteps);
    }
  };
  const report = (tag: string) => {
    expectWithinLedgerLimits();
    console.info(
      `[script-decode-plutus-max-shape] ${tag} ${JSON.stringify(
        rows.map((row) => ({
          ...row,
          mem: row.mem.toString(),
          steps: row.steps.toString(),
        })),
      )}`,
    );
  };
  return { stage, rows, report };
};
