import { describe, expect, it } from "vitest";

import {
  buildMidgardValidationTraceTree,
  canOpenMidgardValidationDisputeBeforeMaturity,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  openMidgardValidationDispute,
  revealMidgardValidationChallengerMidpoint,
  revealMidgardValidationOperatorMidpoint,
  selectMidgardValidationDisputeReveal,
  timeoutMidgardValidationDispute,
} from "../src/index.js";

const hash = (byte: number): Buffer => Buffer.alloc(32, byte);

const trees = () => ({
  operator: buildMidgardValidationTraceTree(
    [hash(1), hash(2), hash(3), hash(4)],
    "accepted",
  ),
  challenger: buildMidgardValidationTraceTree(
    [hash(1), hash(2), hash(5), hash(6)],
    "accepted",
  ),
});

describe("validation dispute orchestration", () => {
  it("bisects to the first differing transition with exact turns", () => {
    const { operator, challenger } = trees();
    const opened = openMidgardValidationDispute({
      operatorDescriptor: operator.descriptor,
      challengerDescriptor: challenger.descriptor,
      currentTime: 100,
    });
    expect(opened.turn).toEqual({ type: "awaitingOperator", midpoint: 1 });

    const operatorMove = selectMidgardValidationDisputeReveal({
      dispute: opened,
      role: "operator",
      proofs: operator.proofs,
    });
    expect(operatorMove.type).toBe("revealOperator");
    if (operatorMove.type !== "revealOperator") throw new Error("unreachable");
    const awaitingChallenger = revealMidgardValidationOperatorMidpoint({
      dispute: opened,
      proof: operatorMove.proof,
      currentTime: 101,
    });
    const challengerMove = selectMidgardValidationDisputeReveal({
      dispute: awaitingChallenger,
      role: "challenger",
      proofs: challenger.proofs,
    });
    expect(challengerMove.type).toBe("revealChallenger");
    if (challengerMove.type !== "revealChallenger") {
      throw new Error("unreachable");
    }
    const secondRound = revealMidgardValidationChallengerMidpoint({
      dispute: awaitingChallenger,
      proof: challengerMove.proof,
      currentTime: 102,
    });
    expect(secondRound).toMatchObject({
      lowIndex: 1,
      highIndex: 3,
      round: 1,
      turn: { type: "awaitingOperator", midpoint: 2 },
    });

    const withOperatorDifference = revealMidgardValidationOperatorMidpoint({
      dispute: secondRound,
      proof: operator.proofs[2]!,
      currentTime: 103,
    });
    const ready = revealMidgardValidationChallengerMidpoint({
      dispute: withOperatorDifference,
      proof: challenger.proofs[2]!,
      currentTime: 104,
    });
    expect(ready).toMatchObject({
      lowIndex: 1,
      highIndex: 2,
      round: 2,
      turn: { type: "readyForOneStep" },
    });
    expect(ready.agreedLowHash).toEqual(hash(2));
    expect(ready.operatorHighHash).toEqual(hash(3));
    expect(ready.challengerHighHash).toEqual(hash(5));
  });

  it("assigns timeout loss to the party that owes the move", () => {
    const { operator, challenger } = trees();
    const opened = openMidgardValidationDispute({
      operatorDescriptor: operator.descriptor,
      challengerDescriptor: challenger.descriptor,
      currentTime: 100,
    });
    expect(
      timeoutMidgardValidationDispute({
        dispute: opened,
        currentTime: opened.responseDeadline + 1,
      }),
    ).toBe("challenger");
    const awaitingChallenger = revealMidgardValidationOperatorMidpoint({
      dispute: opened,
      proof: operator.proofs[1]!,
      currentTime: 101,
    });
    expect(
      timeoutMidgardValidationDispute({
        dispute: awaitingChallenger,
        currentTime: awaitingChallenger.responseDeadline + 1,
      }),
    ).toBe("operator");
  });

  it("matches the inclusive L1 maturity boundary", () => {
    const blockEnd = 1_000_000;
    const maturity = MIDGARD_CONSENSUS_LIMITS_V1.blockMaturityMs;
    const finalOpen =
      blockEnd + maturity - MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS;
    expect(
      canOpenMidgardValidationDisputeBeforeMaturity({
        currentTimeUpper: finalOpen,
        challengedBlockEndTime: blockEnd,
        maturityDuration: maturity,
      }),
    ).toBe(true);
    expect(
      canOpenMidgardValidationDisputeBeforeMaturity({
        currentTimeUpper: finalOpen + 1,
        challengedBlockEndTime: blockEnd,
        maturityDuration: maturity,
      }),
    ).toBe(false);
  });

  it("fails closed for same claims, wrong roles, stale moves, and bad proofs", () => {
    const { operator, challenger } = trees();
    expect(() =>
      openMidgardValidationDispute({
        operatorDescriptor: operator.descriptor,
        challengerDescriptor: {
          ...operator.descriptor,
          rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
        },
        currentTime: 0,
      }),
    ).toThrow(/cannot be disputed/u);

    const opened = openMidgardValidationDispute({
      operatorDescriptor: operator.descriptor,
      challengerDescriptor: challenger.descriptor,
      currentTime: 100,
    });
    expect(() =>
      selectMidgardValidationDisputeReveal({
        dispute: opened,
        role: "challenger",
        proofs: challenger.proofs,
      }),
    ).toThrow(/not awaiting the challenger/u);
    expect(() =>
      revealMidgardValidationOperatorMidpoint({
        dispute: opened,
        proof: { ...operator.proofs[1]!, stateHash: hash(99) },
        currentTime: 101,
      }),
    ).toThrow(/Invalid operator midpoint proof/u);
    expect(() =>
      revealMidgardValidationOperatorMidpoint({
        dispute: opened,
        proof: operator.proofs[1]!,
        currentTime: opened.responseDeadline + 1,
      }),
    ).toThrow(/deadline has passed/u);
  });
});
