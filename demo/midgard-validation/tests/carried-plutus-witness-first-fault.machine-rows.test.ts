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
import { EventKey } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  validatePhaseASingle,
  validationMachineLedgerRoot,
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
 * item order with the native witnesses. Each row asks the producer (Phase A),
 * the trace builder and the one-step machine for the same first fault, and
 * that fault's first-fault phase must be the witness-script phase.
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

/** first-fault-v1 `phase_of_reason_v1` rank of the machine phase. */
const FIRST_FAULT_PHASE_RANK: Readonly<Record<string, number>> = {
  phaseANativeScripts: 5,
};

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

const transactionWith = (items: readonly string[]) => {
  const baseline = makeNativeTx({
    spendInputs: [spent],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
  });
  return encodeRecomputedNativeTx({
    ...baseline.tx,
    witnessSet: {
      ...baseline.tx.witnessSet,
      scriptTxWitsPreimageCbor: encodeCbor(
        items.map((item) => Buffer.from(item, "hex")),
      ),
    },
  });
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
  "%s field-6 witness first fault: producer, trace builder and machine agree",
  (sourceKind) => {
    it.each(ROWS)("%s", async (_name, items, expected) => {
      const fixture = transactionWith(items);
      const txCbor =
        sourceKind === "normal"
          ? fixture.txCbor
          : encodeMidgardForcedTxCanonical(
              materializeMidgardForcedTxFromCanonical(fixture.tx),
            );

      const produced = validatePhaseASingle(
        {
          txId: fixture.txId,
          txCbor,
          sourceKind,
          arrivalSeq: 0n,
          createdAt: new Date(0),
        },
        { ...context, concurrency: 1, strictnessProfile: "phase1_midgard" },
      );
      expect("code" in produced).toBe(true);
      if (!("code" in produced)) return;
      expect(produced.consensusPhase).toBe("phaseANativeScripts");
      expect(produced.subject).toMatchObject(expected);
      const producedReason = resolveRejectionReason(produced, "machine");
      expect(producedReason.kind).toBe("reason");
      if (producedReason.kind !== "reason") return;
      expect(FIRST_FAULT_PHASE_RANK[produced.consensusPhase!]).toBe(5);

      const exit = await Effect.runPromiseExit(
        replayValidationMachineEvent({
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          ...context,
          sourceKind,
          eventKeyCbor: Buffer.from(
            Data.to(
              sourceKind === "normal"
                ? {
                    L2TransactionEventKey: {
                      tx_id: fixture.txId.toString("hex"),
                    },
                  }
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
          ),
          canonicalTransactionCbor: txCbor,
          ledgerWitnessEntries: entries,
          priorUtxosRoot: (await validationMachineLedgerRoot(entries)).toString(
            "hex",
          ),
        }),
      );
      if (Exit.isFailure(exit))
        expect.fail(`trace builder: ${Cause.pretty(exit.cause)}`);
      const { trace, rejection } = exit.value;
      expect(rejection?.code).toBe(produced.code);
      expect(rejection?.subject).toStrictEqual(produced.subject);
      expect(trace.verdict).toBe("rejected");
      expect(trace.rejectionReason).toStrictEqual(producedReason.reason);
      expect(trace.states.at(-2)?.phase).toBe("phaseANativeScripts");
      for (const [stateIndex, witness] of trace.witnesses.entries()) {
        if (witness.phase === "terminal") continue;
        expect(
          buildValidationOneStepArgument({ trace, stateIndex })
            .semanticResolverIndex,
        ).toBeGreaterThanOrEqual(0);
      }
    });
  },
);
