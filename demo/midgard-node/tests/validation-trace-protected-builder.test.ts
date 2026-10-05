import {
  encodeMidgardCekProgramMaterialSidecar,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildMidgardCanonicalCekProgram,
  MidgardRedeemerTag,
  replayValidationMachineEvent,
  validationMachineLedgerRoot,
  ValidationTraceStopped,
} from "@al-ft/midgard-validation";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Lambda, UPLCEncoder, UPLCProgram, UPLCVar } from "@harmoniclabs/uplc";
import { Data } from "@lucid-evolution/lucid";
import { Effect, HashMap, Logger } from "effect";
import { describe, expect, it } from "vitest";

import { buildDeterministicValidationTraceMembers } from "../src/mpf/validation-trace.js";

// This boundary tests the process-local stop and alarm. The adjacent DA suite
// tests wire retention, and does not expose the original evaluation capture.
const fixture = async () => {
  const program = buildMidgardCanonicalCekProgram(
    Buffer.from(
      UPLCEncoder.compile(
        new UPLCProgram([1, 1, 0], new Lambda(new UPLCVar(0))),
      ),
    ),
  );
  const spent = outRefFromByte(0x1d);
  const script = plutusV3ScriptWitness(program.envelopeCbor);
  const ledgerWitnessEntries = [
    {
      outRef: spent,
      output: makeProtectedScriptOutput(
        hashScriptWitness(script),
        FUNDED_OUTPUT_LOVELACE,
      ),
    },
  ];
  const transaction = makeNativeTx({
    spendInputs: [spent],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    scriptWitnesses: [script],
    scriptLanguages: ["PlutusV3"],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor([
      {
        tag: MidgardRedeemerTag.Spend,
        index: 0n,
        exUnits: [1_000_000_000n, 1_000_000_000n],
      },
    ]),
  });
  const eventKey: SDK.EventKey = {
    L2TransactionEventKey: { tx_id: transaction.txId.toString("hex") },
  };
  const context = {
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    blockEndTimeMs: 1_750_000_000_000,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    blockSlot: 100n,
  };
  const replay = await Effect.runPromise(
    replayValidationMachineEvent({
      ...context,
      sourceKind: "normal",
      eventKeyCbor: Buffer.from(Data.to(eventKey, SDK.EventKey), "hex"),
      canonicalTransactionCbor: transaction.txCbor,
      ledgerWitnessEntries,
      programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
        ...program.material.values(),
      ]),
      priorUtxosRoot: (
        await validationMachineLedgerRoot(ledgerWitnessEntries)
      ).toString("hex"),
    }),
  );
  return {
    ...context,
    blockEndTime: new Date(context.blockEndTimeMs),
    transactions: [
      {
        ...replay.replayInput,
        eventKey,
        verdict: replay.replayInput.expectedVerdict,
        rejectionCode: replay.replayInput.expectedRejectionCode,
        ledgerOps: replay.replayInput.expectedLedgerOps,
        programMaterialSidecarCbor:
          replay.replayInput.programMaterialSidecarCbor!,
      },
    ],
  };
};

describe("protected validation trace builder", () => {
  it.each(["disagreement", "unavailable"] as const)(
    "preserves the evaluator verdict and alarms on %s",
    async (reason) => {
      const input = await fixture();
      const transaction = input.transactions[0]!;
      const captures = transaction.scriptEvaluations!;
      expect(captures).toHaveLength(1);
      const changed =
        reason === "disagreement"
          ? {
              ...captures[0]!,
              result: {
                kind: "script_invalid" as const,
                detail: "injected disagreement",
              },
            }
          : { ...captures[0]!, graph: null, execution: null };
      const alarms: {
        message: string;
        annotations: Record<string, unknown>;
      }[] = [];
      const logger = Logger.make(({ message, annotations }) =>
        alarms.push({
          message: String(message),
          annotations: Object.fromEntries(HashMap.toEntries(annotations)),
        }),
      );
      const result = await Effect.runPromise(
        Effect.either(
          buildDeterministicValidationTraceMembers({
            ...input,
            transactions: [{ ...transaction, scriptEvaluations: [changed] }],
          }),
        ).pipe(Effect.provide(Logger.replace(Logger.defaultLogger, logger))),
      );
      expect(result._tag).toBe("Left");
      if (result._tag !== "Left") throw new Error("expected block stop");
      const stop = result.left.cause;
      expect(stop).toBeInstanceOf(ValidationTraceStopped);
      expect(stop).toMatchObject({
        reason,
        committedVerdict: "accepted",
        committedRejectionCode: null,
      });
      expect(transaction.verdict).toBe("accepted");
      expect(captures[0]!.result.kind).toBe("accepted");
      expect(alarms).toHaveLength(1);
      expect(alarms[0]!.message).toContain("Validation trace block stopped");
      expect(alarms[0]!.annotations).toMatchObject({
        alarm: reason,
        committedVerdict: "accepted",
        committedRejectionCode: "none",
      });
    },
  );
});
