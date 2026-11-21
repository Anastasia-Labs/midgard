/**
 * Our case verified by another party and defended by nobody, on the Open
 * scene's emulator, and the case close the funded watcher builds for it.
 * The journey records the raw L1 view from before the scene, so the
 * convicted block's removal can observe it, and captures every close the
 * watcher builds.
 */
import type { TxSigned } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
} from "../../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../../src/validation-trace-dispute/adapter.js";
import {
  encodeValidationTraceDisputeArtifact,
  parseValidationTraceDisputeArtifact,
  type ValidationTraceDisputeArtifact,
} from "../../src/validation-trace-dispute/artifact.js";
import type { FraudProofWorkflowAction } from "../../src/workflow/orchestrator.js";
import { workflowPreflightTransaction } from "../../src/workflow/transaction-boundary.js";
import { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import { drainProver } from "./installed-prover-wallet.js";
import { verifiedByAnotherParty } from "./validation-trace-dispute-foreign-verify.js";
import { confirmedMoves } from "./validation-trace-dispute-funded-moves.js";
import {
  type AdapterWrap,
  type FundedDispute,
  openFundedDispute,
  turnUntil,
} from "./validation-trace-dispute-funded-runner.js";
import { FUNDED } from "./validation-trace-dispute-funded-verify-source.js";
import {
  failingMaterial,
  type Scene,
  setUpScene,
} from "./validation-trace-dispute-open-scene.js";
import {
  type RecordedRemoval,
  recordedRemoval,
} from "./validation-trace-dispute-removal-l1.js";

/** A close the watcher built, as its builder evaluated it. */
export type CapturedClose = Readonly<{
  signed: TxSigned;
  action: FraudProofWorkflowAction;
}>;

export type ForeignClose = Readonly<{
  recorder: ReturnType<typeof recordCrossBlockRawEmulator>;
  removal: RecordedRemoval;
  scene: Scene;
  dispute: FundedDispute;
  verified: Awaited<ReturnType<typeof verifiedByAnotherParty>>;
  /** The pending claim our confirmed Open left. */
  opener: string;
  artifact: ValidationTraceDisputeArtifact;
  closes: CapturedClose[];
  /** Captures each close the watcher builds into `closes`. */
  capture: AdapterWrap;
}>;

/**
 * A fresh scene whose funded watcher opened our claim, another party
 * verified it first, and the next process holds the case open.
 */
export const openForeignVerifiedCase = async (): Promise<ForeignClose> => {
  // Before the scene, so the raw L1 view has the whole history.
  const recorder = recordCrossBlockRawEmulator();
  const scene = await setUpScene();
  const removal = await recordedRemoval(scene, recorder);
  await drainProver(scene.h as never, FUNDED);
  const dispute = await openFundedDispute(scene, { removal: removal.removal });
  const verified = await verifiedByAnotherParty(scene, dispute);
  dispute.restart();
  const opened = await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN, {
    limit: 40,
  });
  expect(opened.result.kind, opened.history.join("\n")).toBe("pending");
  const stages = await confirmedMoves(dispute);
  expect(stages.map(({ stage }) => stage)).toStrictEqual(["init", "open"]);
  const closes: CapturedClose[] = [];
  return {
    recorder,
    removal,
    scene,
    dispute,
    verified,
    opener: stages[1]!.outputs[0]!,
    // The artifact the watcher prepares from the failing block's claim.
    artifact: parseValidationTraceDisputeArtifact(
      encodeValidationTraceDisputeArtifact({
        headerHash: scene.failing.evidence.headerHash,
        material: await failingMaterial(scene),
      }),
    ),
    closes,
    capture: (adapter) => ({
      ...adapter,
      preflight: async (context) => {
        const preflight = await adapter.preflight(context);
        const signed = workflowPreflightTransaction(preflight);
        if (
          signed !== undefined &&
          context.action.input.schemaVersion ===
            VALIDATION_TRACE_DISPUTE_ACTION &&
          parseValidationTraceAction(context.action).stage === "close"
        )
          closes.push({ signed, action: context.action });
        return preflight;
      },
    }),
  };
};
