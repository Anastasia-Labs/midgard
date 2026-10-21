/** The honest trace of the scene's passing transaction
 * (validation-trace-dispute-open-scene.ts), which ends Terminal Accepted. */
import {
  buildChallengerDescriptorV1,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  openValidationTraceV1,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { expect } from "vitest";

import { buildValidationClaimWitness } from "../../src/validation-trace-dispute/claim-material.js";
import { plutusFailureReplayInput } from "../../src/validation-trace-dispute/replay.js";
import {
  eventAndLedger,
  l1SlotConfig,
} from "./validation-trace-claim-material.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

/** The honest trace of the passing transaction: Terminal Accepted. */
export const passingTrace = async (scene: Scene) => {
  const block = scene.passing;
  const { event, ledger } = await eventAndLedger(block);
  const expectedLedgerOps = [
    { type: "delete" as const, key: block.seed.outRef },
    buildValidationMachineLedgerInsertOp({
      key: block.created.outRef,
      outputCbor: block.created.output,
    }),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: ledger.entries.map(({ outRef, output }) => ({
      outRef,
      output,
    })),
    operations: expectedLedgerOps,
  });
  const leaves = await Effect.runPromise(
    openValidationTraceV1(
      {
        ...plutusFailureReplayInput({
          header: block.evidence.header,
          event,
          ledger,
          l1SlotConfig,
        }),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      },
      { revealStates: [0] },
    ),
  );
  const terminal = leaves.states.get(leaves.stepCount)!;
  expect(terminal).toMatchObject({ phase: "terminal", verdict: "accepted" });
  const trace = buildChallengerDescriptorV1(leaves, leaves.stepCount);
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
