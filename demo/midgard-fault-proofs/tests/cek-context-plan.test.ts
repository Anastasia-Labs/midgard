import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import { hashMidgardValidationWorkWitness } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import { buildValidationOneStepArgument } from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  deriveCekContextItemReturnPlan,
  deriveCekContextPlan,
} from "../src/validation-dispute/cek-context.js";
import {
  validateCekSubmissionEvidence,
  validationOneStepEvidenceHash,
} from "../src/validation-dispute/submit.js";
import { buildForgedOperatorSuccessorValidationDisputeFixture } from "./support/emulator/validation-dispute-fixtures.js";

const contextInput = async (cekContextStage: number) => {
  const fixture = await buildForgedOperatorSuccessorValidationDisputeFixture({
    operatorVkey: "11".repeat(32),
    now: 1_780_000_000_000,
    disputedPhase: "cek",
    plutusSelection: true,
    cekContextStage,
  });
  return inputAt(fixture, fixture.disputedLowIndex);
};
const inputAt = (
  fixture: Awaited<
    ReturnType<typeof buildForgedOperatorSuccessorValidationDisputeFixture>
  >,
  index: number,
) => {
  const argument = buildValidationOneStepArgument({
    trace: fixture.challengerTrace,
    stateIndex: index,
  });
  if (!("transitionCbor" in argument))
    throw new Error("Expected staged context evidence");
  const prepared = Data.from(
    Data.to(
      {
        version: 1n,
        resolution: {
          version: 1n,
          pre_state: SDK.validationMachineStateDataFromCore(
            fixture.challengerTrace.states[index]!,
          ),
          operator_successor_hash: hashMidgardValidationMachineState(
            fixture.operatorTrace.states[index + 1]!,
          ).toString("hex"),
          challenger_successor_hash: hashMidgardValidationMachineState(
            fixture.challengerTrace.states[index + 1]!,
          ).toString("hex"),
        },
        evidence_hash: validationOneStepEvidenceHash(argument),
      },
      SDK.PreparedValidationResolutionState,
    ),
  );
  return {
    prepared,
    transition: Data.from(Buffer.from(argument.transitionCbor).toString("hex")),
    auxiliary: Data.from(Buffer.from(argument.auxiliaryCbor).toString("hex")),
    successorWorkWitnessCbor: Buffer.from(
      argument.cekContextSuccessorWorkWitnessCbor!,
    ).toString("hex"),
  };
};

describe("CEK context retained successor planning", () => {
  it.each([0, 1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13])(
    "constructs the exact canonical stage %s plan",
    async (stage) => {
      const plan = deriveCekContextPlan(await contextInput(stage));
      expect(plan.route[0]).toBe("control");
      expect(plan.route.at(-1)).toBe("settle");
      expect(plan.states).toHaveLength(plan.route.length);
    },
    120_000,
  );
  it("retains a detached exact successor and refuses missing or substituted replay bytes", async () => {
    const fixture = await buildForgedOperatorSuccessorValidationDisputeFixture({
      operatorVkey: "11".repeat(32),
      now: 1_780_000_000_000,
      disputedPhase: "cek",
      plutusSelection: true,
      cekContextStage: 6,
    });
    const trace = fixture.challengerTrace;
    const stateIndex = fixture.disputedLowIndex;
    const argument = buildValidationOneStepArgument({ trace, stateIndex });
    expect(argument.cekContextSuccessorWorkWitnessCbor).toEqual(
      trace.witnesses[stateIndex + 1]!.cbor,
    );
    expect(
      validateCekSubmissionEvidence(argument)
        .cekContextSuccessorWorkWitnessCbor,
    ).toEqual(argument.cekContextSuccessorWorkWitnessCbor);
    expect(() =>
      validateCekSubmissionEvidence({ ...argument, semanticResolverIndex: 0 }),
    ).toThrow("context semantic resolver");
    expect(() =>
      validateCekSubmissionEvidence({
        ...argument,
        cekContextSuccessorWorkWitnessCbor: Buffer.from("80", "hex"),
      }),
    ).toThrow("frozen successor work root");
    expect(argument.cekContextSuccessorWorkWitnessCbor).not.toBe(
      trace.witnesses[stateIndex + 1]!.cbor,
    );
    expect(() =>
      buildValidationOneStepArgument({
        trace: {
          ...trace,
          witnesses: trace.witnesses.slice(0, stateIndex + 1),
        },
        stateIndex,
      }),
    ).toThrow("exact adjacent successor");
    const witnesses = [...trace.witnesses];
    witnesses[stateIndex + 1] = {
      ...witnesses[stateIndex + 1]!,
      cbor: Buffer.from("80", "hex"),
    };
    expect(() =>
      buildValidationOneStepArgument({
        trace: { ...trace, witnesses },
        stateIndex,
      }),
    ).toThrow("exact adjacent successor");
    const selectionIndex = trace.witnesses.findIndex(
      (witness) => witness.auxiliary?.kind === "nativeExecutionScan",
    );
    if (selectionIndex < 0) throw new Error("Missing selection fixture");
    expect(
      buildValidationOneStepArgument({ trace, stateIndex: selectionIndex })
        .cekContextSuccessorWorkWitnessCbor,
    ).toBeUndefined();
  });
  it.each(["80", "ff", "8900000000000000000000"])(
    "refuses malformed or trailing successor CBOR %s even when its hash is claimed",
    async (hex) => {
      const input = await contextInput(6);
      const transition = Data.from(
        Data.to(input.transition),
        SDK.ValidationOneStepWitness,
      );
      const successor = transition.claimed_successor;
      const bytes = Buffer.from(hex, "hex");
      const transitionCbor = Buffer.from(
        Data.to(
          {
            ...transition,
            claimed_successor: {
              ...successor,
              work_root: hashMidgardValidationWorkWitness({
                phase: "cek",
                programCounter: Number(successor.program_counter),
                witnessCbor: bytes,
              }).toString("hex"),
            },
          },
          SDK.ValidationOneStepWitness,
        ),
        "hex",
      );
      expect(() =>
        validateCekSubmissionEvidence({
          resolverIndex: 11,
          semanticResolverIndex: 2,
          transitionCbor,
          auxiliaryCbor: Buffer.from(Data.to(input.auxiliary), "hex"),
          cekContextSuccessorWorkWitnessCbor: bytes,
        }),
      ).toThrow();
    },
  );
  it("refuses a substituted adjacent replay witness", async () => {
    const input = await contextInput(6);
    const unrelated = await contextInput(12);
    expect(() =>
      deriveCekContextPlan({
        ...input,
        successorWorkWitnessCbor: unrelated.successorWorkWitnessCbor,
      }),
    ).toThrow("frozen successor work root");
  });
  it("refuses trailing bytes in the retained successor witness", async () => {
    const input = await contextInput(6);
    expect(() =>
      deriveCekContextPlan({
        ...input,
        successorWorkWitnessCbor: input.successorWorkWitnessCbor + "00",
      }),
    ).toThrow();
  });
  it("constructs every shared item handoff and terminal return from canonical replay", async () => {
    const fixture = await buildForgedOperatorSuccessorValidationDisputeFixture({
      operatorVkey: "11".repeat(32),
      now: 1_780_000_000_000,
      disputedPhase: "cek",
      plutusSelection: true,
      cekContextStage: 0,
    });
    const terminalKeys = new Set<string>();
    let count = 0;
    for (
      let index = 0;
      index < fixture.challengerTrace.witnesses.length;
      index++
    ) {
      const current = fixture.challengerTrace.witnesses[index]!;
      if (
        fixture.challengerTrace.states[index]!.phase !== "cek" ||
        current.auxiliary?.kind !== "redeemerItemStep"
      )
        continue;
      const input = inputAt(fixture, index);
      const transition = Data.from(
        Data.to(input.transition),
        SDK.ValidationOneStepWitness,
      );
      const prepared = Data.from(
        Data.to(input.prepared),
        SDK.PreparedValidationResolutionState,
      );
      const binding = SDK.deriveCekContextBinding({
        prepared: input.prepared,
        transactionId: prepared.resolution.pre_state.transaction_id,
        workWitnessCbor: transition.work_witness_cbor,
        auxiliary: input.auxiliary,
      });
      const next = SDK.deriveCekContextBinding({
        prepared: input.prepared,
        transactionId: prepared.resolution.pre_state.transaction_id,
        workWitnessCbor: input.successorWorkWitnessCbor,
        auxiliary: input.auxiliary,
      });
      const verifiedContext = new Constr(0, [
        binding.staged,
        new Constr(0, [next.context]),
      ]);
      const plan = deriveCekContextItemReturnPlan({
        staged: binding.staged,
        auxiliary: input.auxiliary,
        verifiedContext,
      });
      expect(plan.states).toHaveLength(plan.route.length);
      expect(plan.route[0]).toBe("itemReturn");
      terminalKeys.add(plan.route.at(-1)!);
      count++;
    }
    expect(count).toBeGreaterThan(5);
    expect(terminalKeys).toEqual(
      new Set([
        "itemSelectionContinue",
        "itemSelectionFinish",
        "itemDataContinue",
        "itemDataFinishValue",
      ]),
    );
  }, 120_000);
  it("preserves lexical execution mint order at the mixed-width 1,304-asset source maximum", async () => {
    const fixture = await buildForgedOperatorSuccessorValidationDisputeFixture({
      operatorVkey: "11".repeat(32),
      now: 1_780_000_000_000,
      disputedPhase: "cek",
      plutusSelection: true,
      cekSelection: true,
      assetCount: 1304,
      cekContextStage: 8,
      cekContextMintCursor: 1303,
    });
    const input = inputAt(fixture, fixture.disputedLowIndex);
    const plan = deriveCekContextPlan(input);
    expect(plan.route).toEqual(["control", "mintItem", "settle"]);
    const auxiliary =
      fixture.challengerTrace.witnesses[fixture.disputedLowIndex]!.auxiliary;
    if (auxiliary?.kind !== "cekMintContextItem")
      throw new Error("Missing mint context item");
    expect(auxiliary.mintIndex).toBe(0);
    expect(auxiliary.assetName.length).toBe(0);
    expect(auxiliary.previous).not.toBeNull();
  }, 180_000);
});
