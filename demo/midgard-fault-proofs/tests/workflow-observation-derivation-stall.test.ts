/** A family whose stage cannot be derived from the admitted L1 view stalls
 * that workflow alone: the run returns `stalled`, and the next run observes
 * again and proceeds once the view derives, sending nothing twice. Before
 * and after the workflow's first landing alike. An L1 wait still reaches
 * the caller unchanged. */
import { describe, expect, it } from "vitest";

import { FraudProofL1UnavailableError } from "../src/workflow/l1-source.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import { FraudProofRawL1DerivationError } from "../src/workflow/raw-l1-family-derivation.state-queue-topology.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  canonicalEvidence,
  chainOf,
  type FixtureChain,
  makeAdapter,
  PROOF_TX_HASH,
  REMOVAL_TX_HASH,
  run,
} from "./workflow.make-adapter.js";

/** `makeAdapter`'s family, whose observation fails with `failure` while
 * `failing` says so for the chain it reads. */
const failingWhile = (
  failing: (chain: FixtureChain) => boolean,
  failure: () => Error,
): FraudProofFamilyWorkflowAdapter => {
  const adapter = makeAdapter();
  const chain = chainOf(adapter);
  const observe = adapter.observe;
  adapter.observe = async (context) => {
    if (failing(chain)) throw failure();
    return await observe(context);
  };
  return adapter;
};

const derivationFailure = () =>
  new FraudProofRawL1DerivationError(
    "doubleSpend",
    new Error("raw L1 view holds a stage no derivation reads"),
  );

describe("workflow observation: a stage the family cannot derive", () => {
  for (const [when, before] of [
    ["before the first landing", true],
    ["after the first landing", false],
  ] as const)
    it(`stalls this workflow alone ${when}, and proceeds once the view derives`, async () => {
      const evidence = await canonicalEvidence();
      const process = createTestWorkflowProcess();
      // Before: the view fails while nothing has landed. After: the proof
      // lands and the view fails while the removal is still to come.
      let viewFails = true;
      const adapter = failingWhile(
        (chain) =>
          viewFails &&
          (before
            ? !chain.landed(PROOF_TX_HASH)
            : chain.landed(PROOF_TX_HASH) && !chain.landed(REMOVAL_TX_HASH)),
        derivationFailure,
      );
      const first = await run({ evidence, adapter, process });
      expect(first.kind).toBe("stalled");
      if (!("reason" in first)) throw new Error("no reason");
      expect(first.reason).toMatch(
        /^observation failed: .*raw L1 view holds a stage no derivation reads/u,
      );
      const again = await run({ evidence, adapter, process });
      expect(again).toMatchObject({ kind: "stalled", reason: first.reason });
      expect(chainOf(adapter).landed(PROOF_TX_HASH)).toBe(!before);

      viewFails = false;
      let outcome = await run({ evidence, adapter, process });
      for (
        let attempt = 0;
        attempt < 4 && outcome.kind !== "completed";
        attempt += 1
      )
        outcome = await run({ evidence, adapter, process });
      expect(outcome.kind).toBe("completed");
      expect(process.sent().map(({ actionId }) => actionId)).toEqual([
        "prove",
        "remove",
      ]);
    });

  it("hands an L1 wait to the caller unchanged, then proceeds", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const unavailable = new FraudProofL1UnavailableError("follower is down");
    let followerDown = true;
    const adapter = failingWhile(
      () => followerDown,
      () => unavailable,
    );
    await expect(run({ evidence, adapter, process })).rejects.toBe(unavailable);
    followerDown = false;
    const resumed = await run({ evidence, adapter, process });
    expect(resumed.kind).toBe("completed");
  });
});
