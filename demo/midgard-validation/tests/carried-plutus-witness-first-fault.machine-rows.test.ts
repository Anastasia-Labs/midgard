import {
  encodeCbor,
  ledgerEd25519SignatureReady,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  EventKey,
  type RejectionReason,
  rejectionReasonPhase,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  DirectValidationTraceUnavailable,
  placeholderValidationTraceDescriptorV1,
  RejectCodes,
  validatePhaseASingle,
  validationMachineLedgerRoot,
  validationTransactionCommitmentV1,
} from "../src/index.js";
import { resolveRejectionReason } from "../src/rejection-reason.js";
import { buildValidationOneStepArgument } from "../src/validation-machine-data.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";
import {
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
} from "./validation-fixtures.js";
import { replayValidationMachineEvent } from "./validation-machine-event-replay.trace.js";

// The ledger's Ed25519 check is libsodium's, which initialises
// asynchronously; production entry points await it, and so does this file.
beforeAll(() => ledgerEd25519SignatureReady());

/**
 * A carried Plutus witness whose stored program does not decode is the
 * transaction's fault at that field-6 item, at the witness-script phase, in
 * item order with the native witnesses. Each row asks the producer (Phase A)
 * for its first fault: its first-fault phase is the witness-script phase (5)
 * and the least phase of the row's faults, and the placeholder descriptor
 * commits it. The trace builder and the one-step machine name the same first
 * fault, except an undecodable Plutus item's: the machine has no successor
 * for one, so the trace builder returns that the rejection is not provable
 * by trace; the script-decode family proves it.
 */

/** `[0, h'820700']`: a native payload whose constructor tag does not exist. */
const MALFORMED_NATIVE = "820043820700";
/** A PlutusV3 item whose stored bytes are not a Flat program. */
const UNDECODABLE_PLUTUS = encodeCbor([
  3,
  encodeCbor(Buffer.from("0101004c0101000003", "hex")),
]).toString("hex");
/** A PlutusV3 item holding a decodable Flat program. */
const HONEST_PLUTUS = encodeCbor([
  3,
  buildCekProgramFromRawFlat(Buffer.from("010100200101", "hex")).scriptBytes,
]).toString("hex");

const context = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  blockTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

const spent = outRefFromByte(0x5b);
const entries = [{ outRef: spent, output: makeOutput(FUNDED_OUTPUT_LOVELACE) }];

type SourceKind = "normal" | "forced";

const transactionWith = (items: readonly string[], sourceKind: SourceKind) => {
  const baseline = makeNativeTx({
    spendInputs: [spent],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
  });
  const fixture = encodeRecomputedNativeTx({
    ...baseline.tx,
    witnessSet: {
      ...baseline.tx.witnessSet,
      scriptTxWitsPreimageCbor: encodeCbor(
        items.map((item) => Buffer.from(item, "hex")),
      ),
    },
  });
  return {
    txId: fixture.txId,
    txCbor:
      sourceKind === "normal"
        ? fixture.txCbor
        : encodeMidgardForcedTxCanonical(
            materializeMidgardForcedTxFromCanonical(fixture.tx),
          ),
  };
};

const eventKeyCbor = (sourceKind: SourceKind, txId: Buffer) =>
  Buffer.from(
    Data.to(
      sourceKind === "normal"
        ? { L2TransactionEventKey: { tx_id: txId.toString("hex") } }
        : {
            ForcedTransactionEventKey: {
              tx_order_id: {
                transactionId: "5b".repeat(32),
                outputIndex: 0n,
              },
            },
          },
      EventKey,
    ),
    "hex",
  );

/** The producer's first fault for `items`, with its reason and phase. */
const firstFault = (items: readonly string[], sourceKind: SourceKind) => {
  const transaction = transactionWith(items, sourceKind);
  const produced = validatePhaseASingle(
    {
      txId: transaction.txId,
      txCbor: transaction.txCbor,
      sourceKind,
      arrivalSeq: 0n,
      createdAt: new Date(0),
    },
    { ...context, concurrency: 1, strictnessProfile: "phase1_midgard" },
  );
  if (!("code" in produced)) return expect.fail("the producer admitted it");
  const resolved = resolveRejectionReason(produced, "machine");
  if (resolved.kind !== "reason") return expect.fail("no machine reason");
  const reason = resolved.reason as RejectionReason;
  return {
    transaction,
    produced,
    reason,
    phase: rejectionReasonPhase(reason, { body_network_mismatch: false }),
  };
};

type Expected =
  | { readonly arm: "PlutusScriptUndecodable"; readonly index: bigint }
  | { readonly arm: "WitnessNativeScriptMalformed"; readonly index: bigint };

const ROWS: readonly (readonly [string, readonly string[], Expected])[] = [
  [
    "a lone undecodable Plutus witness",
    [UNDECODABLE_PLUTUS],
    { arm: "PlutusScriptUndecodable", index: 0n },
  ],
  [
    "an undecodable Plutus witness before a malformed native witness",
    [UNDECODABLE_PLUTUS, MALFORMED_NATIVE],
    { arm: "PlutusScriptUndecodable", index: 0n },
  ],
  [
    "a malformed native witness before an undecodable Plutus witness",
    [MALFORMED_NATIVE, UNDECODABLE_PLUTUS],
    { arm: "WitnessNativeScriptMalformed", index: 0n },
  ],
  [
    "an honest Plutus witness before a malformed native witness",
    [HONEST_PLUTUS, MALFORMED_NATIVE],
    { arm: "WitnessNativeScriptMalformed", index: 1n },
  ],
];

describe.each(["normal", "forced"] as const)(
  "%s field-6 witness first fault",
  (sourceKind) => {
    it.each(ROWS)(
      "%s: the producer's fault is first under first-fault-v1, and committed",
      (_name, items, expected) => {
        const { transaction, produced, reason, phase } = firstFault(
          items,
          sourceKind,
        );
        expect(produced.consensusPhase).toBe("phaseANativeScripts");
        expect(produced.subject).toMatchObject(expected);
        // first-fault-v1 places a witness-script fault in phase 5, and no
        // fault of the row, each item alone, in an earlier phase.
        expect(phase).toBe(5);
        const faultPhases = items
          .filter((item) => item !== HONEST_PLUTUS)
          .map((item) => firstFault([item], sourceKind).phase);
        expect(phase).toBe(Math.min(...faultPhases));
        // The placeholder descriptor commits the verdict with this reason.
        const descriptor = placeholderValidationTraceDescriptorV1(
          {
            eventKeyCbor: eventKeyCbor(sourceKind, transaction.txId),
            transactionCommitment: validationTransactionCommitmentV1({
              canonicalTransactionCbor: transaction.txCbor,
              sourceKind,
            }),
          },
          "rejected",
          reason,
        );
        expect(descriptor.stepCount).toBe(1);
      },
    );

    it.each(ROWS)(
      "%s: the trace builder and the machine agree, or it is not provable by trace",
      async (_name, items, expected) => {
        const { transaction, produced, reason } = firstFault(items, sourceKind);
        const exit = await Effect.runPromiseExit(
          replayValidationMachineEvent({
            l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
            ...context,
            sourceKind,
            eventKeyCbor: eventKeyCbor(sourceKind, transaction.txId),
            canonicalTransactionCbor: transaction.txCbor,
            ledgerWitnessEntries: entries,
            priorUtxosRoot: (
              await validationMachineLedgerRoot(entries)
            ).toString("hex"),
          }),
        );
        if (expected.arm === "PlutusScriptUndecodable") {
          // A typed result, not a throw and not a step the machine refuses.
          if (Exit.isSuccess(exit))
            return expect.fail("the trace builder traced an arm-48 stop");
          expect([...Cause.defects(exit.cause)]).toEqual([]);
          const failures = [...Cause.failures(exit.cause)];
          expect(failures).toHaveLength(1);
          expect(failures[0]).toBeInstanceOf(DirectValidationTraceUnavailable);
          expect(
            (failures[0] as DirectValidationTraceUnavailable).rejectionCode,
          ).toBe(RejectCodes.ScriptProgramEncoding);
          return;
        }
        if (Exit.isFailure(exit))
          return expect.fail(`trace builder: ${Cause.pretty(exit.cause)}`);
        const { trace, rejection } = exit.value;
        expect(rejection?.code).toBe(produced.code);
        expect(rejection?.subject).toStrictEqual(produced.subject);
        expect(trace.verdict).toBe("rejected");
        expect(trace.rejectionReason).toStrictEqual(reason);
        expect(trace.states.at(-2)?.phase).toBe("phaseANativeScripts");
        for (const [stateIndex, witness] of trace.witnesses.entries()) {
          if (witness.phase === "terminal") continue;
          expect(
            buildValidationOneStepArgument({ trace, stateIndex })
              .semanticResolverIndex,
          ).toBeGreaterThanOrEqual(0);
        }
      },
    );
  },
);
