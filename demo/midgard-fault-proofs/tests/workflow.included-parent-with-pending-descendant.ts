import { describe, expect, it } from "vitest";

import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  canonicalEvidence,
  chainOf,
  makeAdapter,
  PROOF_TX_HASH,
  REMOVAL_TX_HASH,
  run,
  terminal,
} from "./workflow.make-adapter.js";

describe("canonical reobservation with pending descendants", () => {
  it("holds a landed, still reversible proof while the observation lags it, sending nothing again", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const adapter = makeAdapter();
    const chain = chainOf(adapter);
    chain.reversible.add(PROOF_TX_HASH);
    let lagging = true;
    adapter.observe = async () =>
      chain.landed(REMOVAL_TX_HASH)
        ? { kind: "completed", terminal: terminal(evidence.headerHash) }
        : {
            kind: "action_required",
            // The family's view of L1 has not yet caught up with the proof.
            action:
              chain.landed(PROOF_TX_HASH) && !lagging
                ? { actionId: "remove", input: { step: 1 } }
                : { actionId: "prove", input: { step: 0 } },
          };
    expect(await run({ evidence, adapter, process })).toMatchObject({
      kind: "pending",
      awaitingActionId: "prove",
    });
    expect(adapter.submit).toHaveBeenCalledTimes(1);
    lagging = false;
    expect(await run({ evidence, adapter, process })).toMatchObject({
      kind: "completed",
    });
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    expect(process.sent().map(({ actionId }) => actionId)).toEqual([
      "prove",
      "remove",
    ]);
  });
});
