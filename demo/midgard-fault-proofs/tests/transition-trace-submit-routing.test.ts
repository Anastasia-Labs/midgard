import type {
  InvalidOneStepTransitionWitness,
  TransitionFaultProof,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { transitionTraceFinalIndex } from "../src/transition-trace/submit.js";

const proofFor = (
  witness: InvalidOneStepTransitionWitness,
): Pick<TransitionFaultProof, "fault"> => ({
  fault: { InvalidOneStepTransition: { witness } },
});

const placeholder = {} as never;

describe("transition-trace final routing", () => {
  it.each([
    {
      name: "ValidWithdrawalTransition",
      witness: { ValidWithdrawalTransition: placeholder },
      expected: 2,
    },
    {
      name: "InvalidWithdrawalNoOpTransition",
      witness: { InvalidWithdrawalNoOpTransition: placeholder },
      expected: 2,
    },
    {
      name: "InvalidForcedTransactionNoOpTransition",
      witness: { InvalidForcedTransactionNoOpTransition: placeholder },
      expected: 3,
    },
    {
      name: "L2TransactionTransition",
      witness: { L2TransactionTransition: placeholder },
      expected: 4,
    },
    {
      name: "ValidDepositTransition",
      witness: { ValidDepositTransition: placeholder },
      expected: 5,
    },
  ] satisfies readonly {
    readonly name: string;
    readonly witness: InvalidOneStepTransitionWitness;
    readonly expected: number;
  }[])("routes $name to final validator $expected", ({ witness, expected }) => {
    expect(transitionTraceFinalIndex(proofFor(witness))).toBe(expected);
  });

  it("fails closed for an unknown one-step constructor", () => {
    expect(() =>
      transitionTraceFinalIndex({
        fault: {
          InvalidOneStepTransition: {
            witness: { FutureTransition: {} },
          },
        } as never,
      }),
    ).toThrow(/Unsupported transition-trace proof variant/);
  });
});
