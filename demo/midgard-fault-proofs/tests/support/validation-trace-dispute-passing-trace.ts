/** The honest trace of the Open scene's passing transaction, replayed as
 * accepted: Terminal Accepted, with the claim a challenger would open on it. */
import {
  buildTraceAtStepCountV1,
  buildValidationMachineLedgerInsertOp,
  openValidationTraceV1,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { expect } from "vitest";

import { buildValidationClaimWitness } from "../../src/validation-trace-dispute/claim-material.js";
import {
  acceptedLedgerMutation,
  plutusReplayInput,
  replayPlutusVerdict,
} from "../../src/validation-trace-dispute/replay.js";
import {
  eventAndLedger,
  l1SlotConfig,
} from "./validation-trace-claim-material.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

/** The honest trace of the passing transaction: Terminal Accepted. */
export const passingTrace = async (scene: Scene) => {
  const block = scene.passing;
  const { event, ledger } = await eventAndLedger(block);
  const replay = {
    header: block.evidence.header,
    event,
    ledger,
    l1SlotConfig,
  } as const;
  const verdict = await replayPlutusVerdict(
    plutusReplayInput({ ...replay, verdict: { verdict: "rejected" } }),
  );
  expect(verdict.verdict).toBe("accepted");
  const mutation = await acceptedLedgerMutation(ledger, verdict.statePatch);
  // The block's own effect: the seed spent, the created output inserted.
  expect(mutation.ledgerOps).toStrictEqual([
    { type: "delete", key: block.seed.outRef },
    buildValidationMachineLedgerInsertOp({
      key: block.created.outRef,
      outputCbor: block.created.output,
    }),
  ]);
  const leaves = await Effect.runPromise(
    openValidationTraceV1(
      plutusReplayInput({
        ...replay,
        verdict: { verdict: "accepted", mutation },
      }),
      { revealStates: [0] },
    ),
  );
  const terminal = leaves.states.get(leaves.stepCount)!;
  expect(terminal).toMatchObject({ phase: "terminal", verdict: "accepted" });
  const trace = buildTraceAtStepCountV1(leaves, leaves.stepCount);
  return {
    claim: {
      claimWitness: await buildValidationClaimWitness({
        reconstruction: block.evidence.reconstruction,
        eventKey: event.eventKey,
      }),
      challengerRoot: trace.descriptor.traceRoot.toString("hex"),
      stepCount: BigInt(leaves.stepCount),
    },
    terminal,
    terminalProof: trace.proof(leaves.stepCount),
    initialState: leaves.states.get(0)!,
  };
};
