import { expect, it } from "vitest";

import {
  type DecisionPruneInputs,
  decisionsToPrune,
} from "../../src/committee-service.decision-pruning.js";
import type { Obligation } from "../../src/l1/follower/obligations.js";
export const registerCatchUpPruningCase = ({
  inputs,
  obligation,
  hash,
}: {
  readonly inputs: (
    obligations: readonly Obligation[],
    overrides?: Partial<DecisionPruneInputs>,
  ) => DecisionPruneInputs;
  readonly obligation: (
    n: number,
    state: Obligation["state"],
    decisionDeletable?: boolean,
  ) => Obligation;
  readonly hash: (n: number) => string;
}) => {
  it("retains a catch-up decision through its commit clock and holds an unknown clock", () => {
    const final = [obligation(1, "final")];
    expect(
      decisionsToPrune(
        inputs(final, {
          signed: [{ headerHash: hash(1), blockTimeMs: 20_000n }],
          finalBlockTimeMs: 10_000,
        }),
      ),
    ).toEqual([]);
    expect(
      decisionsToPrune(
        inputs(final, {
          signed: [{ headerHash: hash(1), blockTimeMs: 20_000n }],
          finalBlockTimeMs: 20_000,
        }),
      ),
    ).toEqual([]);
    expect(
      decisionsToPrune(
        inputs(final, {
          signed: [{ headerHash: hash(1), blockTimeMs: 20_000n }],
          finalBlockTimeMs: 20_001,
        }),
      ),
    ).toEqual([hash(1)]);
    expect(
      decisionsToPrune(
        inputs(final, {
          signed: [{ headerHash: hash(1), blockTimeMs: null }],
          finalBlockTimeMs: 20_001,
        }),
      ),
    ).toEqual([]);
  });
};
