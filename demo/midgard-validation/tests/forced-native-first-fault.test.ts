import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { RejectCodes } from "../src/index.js";
import { buildValidationOneStepArgument } from "../src/validation-machine-data.js";
import { nativeFaultFixture } from "./forced-native-first-fault.fixture.js";

// The ledger's Ed25519 check is libsodium's, which initialises
// asynchronously; production entry points await it, and so does this file.
beforeAll(() => ledgerEd25519SignatureReady());

/** The raw envelope must retain prior Phase A faults, then use the same native
 * scan as canonical witnesses. */
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
  });
});
