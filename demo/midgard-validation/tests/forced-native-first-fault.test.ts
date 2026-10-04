import { writeFile } from "node:fs/promises";

import { hashMidgardValidationWorkWitness } from "@al-ft/midgard-core/validation-trace";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { RejectCodes } from "../src/index.js";
import {
  buildValidationOneStepArgument,
  validationMachineStateData,
} from "../src/validation-machine-data.js";
import {
  nativeFaultFixture,
  redeemerFaultFixture,
} from "./forced-native-first-fault.fixture.js";

/** The raw envelope must retain prior Phase A faults, then use the same native
 * scan as canonical witnesses. Optional output captures actual producer bytes
 * for the independent Aiken one-step predicate, including its source doors. */
describe("forced malformed native first fault", () => {
  it.each([
    ["present", RejectCodes.InvalidFieldType, "phaseANativeScripts"],
    ["mint", RejectCodes.InvalidFieldType, "phaseANativeScripts"],
    ["exhaustedBoundary", RejectCodes.InvalidFieldType, "phaseANativeScripts"],
    ["missingKey", RejectCodes.InvalidFieldType, "phaseANativeScripts"],
    ["validKey", RejectCodes.NativeScriptInvalid, "phaseANativeScripts"],
    ["invalidChildren", RejectCodes.InvalidFieldType, "phaseANativeScripts"],
    [
      "invalidThresholdChildren",
      RejectCodes.InvalidFieldType,
      "phaseANativeScripts",
    ],
    ["missing", RejectCodes.InvalidFieldType, "phaseANativeScripts"],
    ["empty", RejectCodes.EmptyInputs, "inputSets"],
    ["signature", RejectCodes.InvalidSignature, "signatures"],
    ["earlierFalse", RejectCodes.NativeScriptInvalid, "phaseANativeScripts"],
  ] as const)("preserves %s first fault", async (shape, code, phase) => {
    const { trace, phaseA, statePatch } = await nativeFaultFixture(shape);
    expect(phaseA).toMatchObject({ code, consensusPhase: phase });
    expect(trace.rejectionCode).toBe(code);
    expect(trace.states.at(-2)?.phase).toBe(phase);
    expect(trace.states.at(-1)?.verdict).toBe("rejected");
    expect(statePatch).toEqual({ deletedOutRefs: [], upsertedOutRefs: [] });
    expect(trace.witnesses.some((w) => w.phase === "resolveInputs")).toBe(
      false,
    );
    const handoff = trace.witnesses.findIndex(
      (w, i) =>
        w.phase === "signatures" &&
        trace.states[i + 1]?.phase === "phaseANativeScripts",
    );
    if (shape !== "empty" && shape !== "signature") {
      expect(handoff).toBeGreaterThan(0);
      const control = Data.from(
        trace.witnesses[handoff + 1]!.cbor.toString("hex"),
      ) as unknown[];
      // Positions 6 and 7 are script_count and script_seen in the native scan.
      expect(control.slice(6, 8)).toEqual([-1n, 0n]);
    }
    for (const [i, witness] of trace.witnesses.entries()) {
      if (witness.phase === "terminal") continue;
      const argument = buildValidationOneStepArgument({ trace, stateIndex: i });
      expect(argument.semanticResolverIndex).toBeGreaterThanOrEqual(0);
    }
    if (process.env.MIDGARD_NATIVE_FAULT_VECTORS != null) {
      const steps: {
        phase: string;
        pre: string;
        transition: string;
        auxiliary: string;
        nativeStage?: number;
        signerProofKind?: string;
      }[] = trace.witnesses
        .filter((w) => w.phase !== "terminal")
        .map((_, i) => {
          const argument = buildValidationOneStepArgument({
            trace,
            stateIndex: i,
          });
          return {
            phase: trace.states[i]!.phase,
            pre: Data.to(validationMachineStateData(trace.states[i]!) as never),
            transition: argument.transitionCbor.toString("hex"),
            auxiliary: argument.auxiliaryCbor.toString("hex"),
            ...(trace.witnesses[i]!.auxiliary?.kind === "nativeScriptToken"
              ? {
                  nativeStage: Number(
                    (
                      Data.from(
                        trace.witnesses[i]!.cbor.toString("hex"),
                      ) as unknown[]
                    )[5],
                  ),
                  signerProofKind:
                    trace.witnesses[i]!.auxiliary.signerProof.kind,
                }
              : {}),
          };
        });
      // Reproduce the retired producer's already-seen native successor without
      // changing the authenticated pre-state or witness at the handoff.
      if (handoff >= 0) {
        const raw = Data.from(
          trace.witnesses[handoff + 1]!.cbor.toString("hex"),
        ) as unknown[];
        raw[6] = shape === "earlierFalse" ? 2n : 1n;
        raw[7] = raw[6];
        const forged = {
          ...trace,
          states: trace.states.map((s, i) =>
            i === handoff + 1
              ? {
                  ...s,
                  workRoot: hashMidgardValidationWorkWitness({
                    phase: s.phase,
                    programCounter: s.programCounter,
                    witnessCbor: Buffer.from(Data.to(raw as never), "hex"),
                  }),
                }
              : s,
          ),
        };
        const bad = buildValidationOneStepArgument({
          trace: forged,
          stateIndex: handoff,
        });
        steps.push({
          phase: "retiredHandoff",
          pre: Data.to(
            validationMachineStateData(trace.states[handoff]!) as never,
          ),
          transition: bad.transitionCbor.toString("hex"),
          auxiliary: bad.auxiliaryCbor.toString("hex"),
        });
      }
      const authenticItem = steps.find(
        (s) =>
          s.phase === "phaseANativeScripts" &&
          s.auxiliary.includes("8146820043820700"),
      );
      if (authenticItem != null)
        steps.push({
          ...authenticItem,
          phase: "wrongSource",
          auxiliary: authenticItem.auxiliary.replace(
            "8146820043820700",
            "8146820043820701",
          ),
        });
      await writeFile(
        `${process.env.MIDGARD_NATIVE_FAULT_VECTORS}/${shape}.json`,
        JSON.stringify(steps),
      );
    }
  });
});

it.each([false, true])(
  "scans malformed redeemer Data before missing script=%s",
  async (missingScript) => {
    const { trace } = await redeemerFaultFixture(missingScript);
    expect(trace.rejectionCode).toBe(RejectCodes.InvalidFieldType);
    expect(trace.states.at(-2)?.phase).toBe("scriptSources");
    if (process.env.MIDGARD_NATIVE_FAULT_VECTORS != null) {
      const steps = trace.witnesses.flatMap((w, i) => {
        if (w.phase === "terminal") return [];
        const arg = buildValidationOneStepArgument({ trace, stateIndex: i });
        return [
          {
            phase: w.phase,
            pre: Data.to(validationMachineStateData(trace.states[i]!) as never),
            transition: arg.transitionCbor.toString("hex"),
            auxiliary: arg.auxiliaryCbor.toString("hex"),
          },
        ];
      });
      await writeFile(
        `${process.env.MIDGARD_NATIVE_FAULT_VECTORS}/redeemer${missingScript ? "Missing" : "Present"}.json`,
        JSON.stringify(steps),
      );
    }
  },
);
