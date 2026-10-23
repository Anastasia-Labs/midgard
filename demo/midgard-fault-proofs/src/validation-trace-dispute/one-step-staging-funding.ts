/** Our staging of a staged builtin step's operands under the prover
 * funding permit (workflow/funding-reservation-permit.validation-dispute-
 * staging-authority.ts): each staging, and each spend back, signed with the
 * authority the permit admits it by. A staging names what the rest of its
 * step needs from our wallet, the deposits of the runs still to stage and
 * the step's loop hops our wallet pays, and the case, the thread and the
 * deadline the operator's alarm names when our wallet is short of it; a
 * staging that does not build for want of our wallet's ada raises that
 * alarm too. */
import {
  buildUnsignedMidgardCekStagingProgram,
  buildUnsignedMidgardCekStagingReclaimProgram,
  midgardCekStagingOutRef,
  type MidgardCekStagingPins,
  type MidgardCekStagingRun,
} from "@al-ft/midgard-sdk";
import type {
  LucidEvolution,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";

import type { ResolvedProverSigner } from "../runtime.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { grantValidationDisputeStagingAuthority } from "../workflow/funding-reservation-permit.validation-dispute-staging-authority.js";
import { ValidationDisputeWalletMoveUnfundedError } from "../workflow/funding-reservation-permit.validation-dispute-wallet-move-reserve.js";
import { isWorkflowFundingShortfall } from "../workflow/funding-reservation-permit.workflow-funding-shortfall.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { confirmedValidationTraceStages } from "./actions.js";
import { parseValidationTraceDisputeArtifact } from "./artifact.js";
import { validationTraceCaseReads } from "./case-reads.js";
import { validationTraceCaseExpiryMs } from "./threads.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";
import { validationTraceMoveDeadlineMs } from "./wallet-move-unfunded.js";

/** A run to stage, with what the rest of its step needs. */
export type ValidationTraceStagingStep = Readonly<{
  run: MidgardCekStagingRun;
  /** The inline datums of the step's runs still to stage after `run`. */
  rest: readonly string[];
  /** The step's loop hops, each a move our wallet pays. */
  walletPaidHops: number;
  /** The thread whose PrepareSelected move waits on the step. */
  threadOutRef: string;
}>;

/**
 * The hops of a staged step's arm chain, `holders`, that our wallet pays:
 * each visit of a loop holder, which the chain visits more than once,
 * the thread re-output to its own script, its fee past the per-move cap.
 * A chain that visits each holder once has none.
 */
export const stagedStepWalletPaidHops = (holders: readonly string[]): number =>
  holders.filter(
    (holder) => holders.indexOf(holder) !== holders.lastIndexOf(holder),
  ).length;

/** The value of `program`, or its error thrown as itself. */
const run = async <A>(program: Effect.Effect<A, Error>): Promise<A> => {
  const result = await Effect.runPromise(Effect.either(program));
  if (Either.isLeft(result)) throw result.left;
  return result.right;
};

/** Signs our staging and our spend back of its outputs. */
export const validationTraceStagingSigner = ({
  lucid,
  signer,
  deployment,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
}) => {
  const { ourCase } = validationTraceCaseReads({ lucid, signer, deployment });
  const headerHashOf = (context: FraudProofWorkflowAdapterContext) =>
    parseValidationTraceDisputeArtifact(context.artifact).headerHash;

  /** The case of ours `threadOutRef` plays, and the deadline its move
   * must land by: what the operator's alarm names. */
  const alarmOf = async (
    context: FraudProofWorkflowAdapterContext,
    threadOutRef: string,
  ) => {
    const found = await ourCase(
      parseValidationTraceDisputeArtifact(context.artifact),
      confirmedValidationTraceStages(context.entries),
    );
    if (found === undefined)
      throw new WorkflowActionChangedError(
        "validation-trace staging has no case of ours to play",
      );
    return {
      caseUnit: found.caseUnit,
      threadOutRef,
      deadlineMs: validationTraceMoveDeadlineMs({
        stage: "resolve",
        caseExpiresAtMs: validationTraceCaseExpiryMs(found.caseDatum),
        thread: undefined,
      }),
    };
  };

  return {
    /** The signed staging of `step`'s run, and the index of its staging
     * output. */
    stage: async (
      context: FraudProofWorkflowAdapterContext,
      actionId: string,
      step: ValidationTraceStagingStep,
    ) => {
      const alarm = await alarmOf(context, step.threadOutRef);
      let unsigned: TxSignBuilder;
      try {
        unsigned = await run(
          buildUnsignedMidgardCekStagingProgram(lucid, { run: step.run }),
        );
      } catch (cause) {
        if (!isWorkflowFundingShortfall(cause)) throw cause;
        throw new ValidationDisputeWalletMoveUnfundedError({
          actionId,
          ...alarm,
          cause,
        });
      }
      const { outputIndex } = midgardCekStagingOutRef(unsigned, step.run);
      const signed = await unsigned.sign.withWallet().complete();
      grantValidationDisputeStagingAuthority({
        signed,
        deploymentFingerprint: deployment.deploymentFingerprint,
        headerHash: headerHashOf(context),
        shape: {
          datumCborHex: step.run.datumCbor,
          remainingDatumCborHexes: step.rest,
          walletPaidHops: step.walletPaidHops,
          ...alarm,
        },
      });
      return { signed, outputIndex };
    },

    /** The signed spend back of the staging output `output`, which no
     * thread in `pins` pins. */
    reclaim: async (
      context: FraudProofWorkflowAdapterContext,
      output: UTxO,
      pins: MidgardCekStagingPins,
    ) => {
      const unsigned = await run(
        buildUnsignedMidgardCekStagingReclaimProgram(lucid, {
          outputs: [output],
          pins,
        }),
      );
      const signed = await unsigned.sign.withWallet().complete();
      grantValidationDisputeStagingAuthority({
        signed,
        deploymentFingerprint: deployment.deploymentFingerprint,
        headerHash: headerHashOf(context),
        shape: { output },
      });
      return signed;
    },
  };
};
