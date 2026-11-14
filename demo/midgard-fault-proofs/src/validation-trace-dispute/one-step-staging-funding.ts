/** Our staging of a staged builtin step's operands under the prover
 * funding permit (workflow/funding-reservation-permit.validation-dispute-
 * staging-authority.ts): each staging, and each spend back, signed with the
 * authority the permit admits it by. A staging that does not build for want
 * of our wallet's ada raises the operator's alarm, naming the case, the
 * thread and the deadline, and what the rest of the step needs from our
 * wallet: the runs still to stage, the step's loop hops our wallet pays,
 * and about the ada they take. That sum is for the operator only; it holds
 * back no staging that builds. */
import {
  buildUnsignedMidgardCekStagingProgram,
  buildUnsignedMidgardCekStagingReclaimProgram,
  midgardCekStagingAddress,
  midgardCekStagingDeposit,
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

/** The ada `lovelace` is, rounded up. */
const adaOf = (lovelace: bigint): string =>
  ((lovelace + 999_999n) / 1_000_000n).toString();

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

  /** What `step`, its run included, still needs from our wallet, for the
   * operator's alarm: each run's deposit and the fee of a full-size
   * transaction for each run, and the fee of a full-size transaction at the
   * largest execution units for each loop hop our wallet pays. */
  const needsOf = (step: ValidationTraceStagingStep): string => {
    const parameters = lucid.config().protocolParameters!;
    const address = midgardCekStagingAddress(
      lucid.config().network!,
      signer.paymentKeyHash,
    );
    const runs = [step.run.datumCbor, ...step.rest];
    const fee =
      BigInt(parameters.minFeeA) * BigInt(parameters.maxTxSize) +
      BigInt(parameters.minFeeB);
    const scripts = BigInt(
      Math.ceil(
        parameters.priceMem * Number(parameters.maxTxExMem) +
          parameters.priceStep * Number(parameters.maxTxExSteps),
      ),
    );
    const lovelace =
      runs.reduce(
        (sum, datumCbor) =>
          sum +
          midgardCekStagingDeposit({
            address,
            datumCbor,
            coinsPerUtxoByte: parameters.coinsPerUtxoByte,
          }) +
          fee,
        0n,
      ) +
      BigInt(step.walletPaidHops) * (fee + scripts);
    return `${runs.length.toString()} runs / ${step.walletPaidHops.toString()} hops remain; about ${adaOf(lovelace)} ADA needed`;
  };

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
      let unsigned: TxSignBuilder;
      try {
        unsigned = await run(
          buildUnsignedMidgardCekStagingProgram(lucid, { run: step.run }),
        );
      } catch (cause) {
        if (!isWorkflowFundingShortfall(cause)) throw cause;
        throw new ValidationDisputeWalletMoveUnfundedError({
          actionId,
          ...(await alarmOf(context, step.threadOutRef)),
          cause: new Error(
            `${needsOf(step)}: ${cause instanceof Error ? cause.message : String(cause)}`,
            { cause },
          ),
        });
      }
      const { outputIndex } = midgardCekStagingOutRef(unsigned, step.run);
      const signed = await unsigned.sign.withWallet().complete();
      grantValidationDisputeStagingAuthority({
        signed,
        deploymentFingerprint: deployment.deploymentFingerprint,
        headerHash: headerHashOf(context),
        shape: { datumCborHex: step.run.datumCbor },
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
