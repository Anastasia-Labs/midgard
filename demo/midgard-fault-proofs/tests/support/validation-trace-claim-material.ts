/** A committed Plutus-execution block's single L2 event, the ledger it
 * executes over, and the challenger's claim material for it. */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";

import { ledgerBeforeTransactionEvent } from "../../src/input-existence/event-ledger.js";
import type { CheckedLedger } from "../../src/ledger-at.js";
import {
  buildValidationTraceClaimMaterial,
  type ValidationTraceClaimMaterial,
} from "../../src/validation-trace-dispute/claim-material.js";
import type { CommittedL2Event } from "../../src/validation-trace-dispute/replay.js";
import { completeCanonicalReplayPredecessorLedger } from "../../src/workflow/complete-replay.js";
import type { plutusExecutionBlockFixture } from "../helpers/plutus-execution-block-fixture.js";

export const l1SlotConfig = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;

type Block = Pick<
  Awaited<ReturnType<typeof plutusExecutionBlockFixture>>,
  "evidence" | "context"
>;

/** The block's single L2 event and the ledger it executes over. */
export const eventAndLedger = async (
  block: Block,
): Promise<{ event: CommittedL2Event; ledger: CheckedLedger }> => {
  const { evidence, context } = block;
  const event = evidence.reconstruction.sourceEvents.find(
    (candidate) => candidate.phase === "L2Transaction",
  );
  if (event?.phase !== "L2Transaction") throw new Error("no L2 event");
  const opening = evidence.reconstruction.eventToStepByFingerprint.get(
    event.fingerprint,
  )!;
  const ledger = await ledgerBeforeTransactionEvent({
    block: evidence,
    eventKey: event.eventKey,
    stepIndex: opening.value.step_index,
    predecessor: completeCanonicalReplayPredecessorLedger({
      evidence,
      context,
    }),
  });
  return { event, ledger };
};

/** The claim material for the block's failing L2 event. */
export const claimMaterialOf = async (
  block: Block,
): Promise<ValidationTraceClaimMaterial> => {
  const { event, ledger } = await eventAndLedger(block);
  return await buildValidationTraceClaimMaterial({
    evidence: block.evidence,
    event,
    ledger,
    l1SlotConfig,
  });
};
