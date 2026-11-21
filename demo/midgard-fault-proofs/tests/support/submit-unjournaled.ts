/**
 * Submits fault-proof transactions with no journal, the way the diagnostic
 * CLI does: the builder's own arguments, plus whether to wait for the step's
 * confirmation (by default it waits, as the CLI does) and a hook that sees
 * the step's signed transaction before it is submitted. The result is the
 * builder's fields with the submitted hash first and whether confirmation was
 * awaited last.
 */
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  type ContinuationTurn,
  type FaultProofTurn,
  isContinuationTurn,
  isPrerequisiteTurn,
  type PrerequisiteTurn,
} from "../../src/fault-proof-turn.js";
import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  buildProofChunkPublication,
  type PublishedProof,
  readPublishedProofChunks,
} from "../../src/publish-proof-chunks.js";
import {
  settleFaultProofTurns,
  submitFaultProofTurns,
  submitPrerequisiteTurn,
} from "../../src/submit-fault-proof-turns.js";
import type { OwnOutputJournal } from "../../src/workflow/own-outputs.js";
import {
  type LocallyEvaluatedTransaction,
  type OutsideWorkflowSubmission,
  submitOutsideWorkflow,
} from "../../src/workflow/transaction-boundary.js";

/**
 * What submitting `Build`'s step reports: its fields with the submitted hash
 * and no transaction, whichever helper settled the turns before it.
 */
export type SubmittedStep<
  Build extends (...args: never[]) => Promise<unknown>,
> = OutsideWorkflowSubmission<
  Exclude<
    Awaited<ReturnType<Build>>,
    PrerequisiteTurn | ContinuationTurn<unknown>
  >
>;

/** A builder's arguments carry the wallet's Lucid, directly or in its config. */
type Submitter =
  | { readonly lucid: LucidEvolution }
  | { readonly config: { readonly lucid: LucidEvolution } };

const lucidOf = (args: Submitter): LucidEvolution =>
  "lucid" in args ? args.lucid : args.config.lucid;

/**
 * Whether to wait for the step's confirmation (by default it waits), and a
 * hook that sees the step's signed transaction before it is submitted.
 */
type SubmitOptions = {
  readonly awaitConfirmation?: boolean;
  readonly beforeSubmit?: (
    transaction: LocallyEvaluatedTransaction,
  ) => Promise<void> | void;
};

/**
 * Builds one transaction and submits it. A builder that can return a
 * prerequisite or continuation turn goes through
 * {@link submitTurnsUnjournaled} instead, and a turn reaching this helper is
 * refused rather than submitted as the step.
 */
export const buildAndSubmitUnjournaled = async <
  Args extends Submitter,
  Built extends { readonly transaction: LocallyEvaluatedTransaction },
>(
  build: (args: Args) => Promise<Built>,
  { awaitConfirmation = true, beforeSubmit, ...args }: Args & SubmitOptions,
): Promise<OutsideWorkflowSubmission<Built>> => {
  const built = await build(args as unknown as Args);
  if (isPrerequisiteTurn(built) || isContinuationTurn(built))
    throw new Error(
      `${build.name} returned the turn ${built.label}; submit it with submitTurnsUnjournaled`,
    );
  const { transaction, ...fields } = built;
  await beforeSubmit?.(transaction);
  const txHash = await submitOutsideWorkflow(
    lucidOf(args as unknown as Submitter),
    transaction,
    {
      label: build.name,
      awaitConfirmation,
    },
  );
  return {
    txHash,
    ...fields,
    awaitedConfirmation: awaitConfirmation,
  } as unknown as OutsideWorkflowSubmission<Built>;
};

/**
 * What {@link unjournaled} binds the builder `Build` into, for an export whose
 * inferred type names a module the test package does not depend on.
 */
export type UnjournaledSubmitter<
  Build extends (
    ...args: never[]
  ) => Promise<{ readonly transaction: LocallyEvaluatedTransaction }>,
> = (
  args: Parameters<Build>[0] & SubmitOptions,
) => Promise<OutsideWorkflowSubmission<Awaited<ReturnType<Build>>>>;

/**
 * `build` bound into a function that builds its step and submits it, as
 * {@link buildAndSubmitUnjournaled} does.
 */
export const unjournaled =
  <
    Args extends Submitter,
    Built extends { readonly transaction: LocallyEvaluatedTransaction },
  >(
    build: (args: Args) => Promise<Built>,
  ) =>
  async (
    args: Args & SubmitOptions,
  ): Promise<OutsideWorkflowSubmission<Built>> =>
    await buildAndSubmitUnjournaled(build, args);

/**
 * Builds one signed transaction with no fields of its own, such as a linear
 * thread hop, and submits it, returning its hash.
 */
export const buildAndSubmitTransactionUnjournaled = async <
  Args extends Submitter,
>(
  build: (args: Args) => Promise<LocallyEvaluatedTransaction>,
  { awaitConfirmation = true, beforeSubmit, ...args }: Args & SubmitOptions,
): Promise<string> => {
  const transaction = await build(args as unknown as Args);
  await beforeSubmit?.(transaction);
  return await submitOutsideWorkflow(
    lucidOf(args as unknown as Submitter),
    transaction,
    { label: build.name, awaitConfirmation },
  );
};

/**
 * `build` bound into a function that builds its signed transaction and
 * submits it, as {@link buildAndSubmitTransactionUnjournaled} does.
 */
export const unjournaledTransaction =
  <Args extends Submitter>(
    build: (args: Args) => Promise<LocallyEvaluatedTransaction>,
  ) =>
  async (args: Args & SubmitOptions): Promise<string> =>
    await buildAndSubmitTransactionUnjournaled(build, args);

/**
 * Drives a builder that returns turns to its step: every prerequisite and
 * continuation it returns is submitted and confirmed, and the builder called
 * again with the same own-output journal, until it returns the step, which
 * is submitted last (see {@link submitFaultProofTurns}).
 */
export const submitTurnsUnjournaled = async <
  Args extends {
    readonly lucid: LucidEvolution;
    readonly ownOutputs?: OwnOutputJournal;
  },
  Step extends { readonly transaction: LocallyEvaluatedTransaction },
>(
  build: (
    args: Args,
  ) => Promise<FaultProofTurn<Step | ContinuationTurn<Partial<Args>>>>,
  {
    awaitConfirmation = true,
    beforeSubmit,
    ...args
  }: NoInfer<Args> & SubmitOptions,
): Promise<
  OutsideWorkflowSubmission<
    Exclude<Step, PrerequisiteTurn | ContinuationTurn<unknown>>
  >
> => {
  if (beforeSubmit === undefined)
    return (await submitFaultProofTurns(build, args as unknown as Args, {
      label: build.name,
      awaitConfirmation,
    })) as OutsideWorkflowSubmission<
      Exclude<Step, PrerequisiteTurn | ContinuationTurn<unknown>>
    >;
  const { transaction, ...fields } = await settleFaultProofTurns(
    build,
    args as unknown as Args,
  );
  await beforeSubmit(transaction);
  const txHash = await submitOutsideWorkflow(
    (args as unknown as Args).lucid,
    transaction,
    { label: build.name, awaitConfirmation },
  );
  return {
    txHash,
    ...fields,
    awaitedConfirmation: awaitConfirmation,
  } as unknown as OutsideWorkflowSubmission<
    Exclude<Step, PrerequisiteTurn | ContinuationTurn<unknown>>
  >;
};

/**
 * `build` bound into a function that drives it to its step and submits it,
 * as {@link submitTurnsUnjournaled} does.
 */
export const unjournaledTurns =
  <
    Args extends {
      readonly lucid: LucidEvolution;
      readonly ownOutputs?: OwnOutputJournal;
    },
    Step extends { readonly transaction: LocallyEvaluatedTransaction },
  >(
    build: (
      args: Args,
    ) => Promise<FaultProofTurn<Step | ContinuationTurn<Partial<Args>>>>,
  ) =>
  async (
    args: NoInfer<Args> & SubmitOptions,
  ): Promise<
    OutsideWorkflowSubmission<
      Exclude<Step, PrerequisiteTurn | ContinuationTurn<unknown>>
    >
  > =>
    await submitTurnsUnjournaled(build, args);

/**
 * Submits what `build` returns, settling its turns first, for a builder
 * already bound to its arguments, such as one action of a family's workflow
 * submission. `label` names the transactions in a refusal.
 */
export const submitBoundUnjournaled = async <
  Step extends { readonly transaction: LocallyEvaluatedTransaction },
>(
  lucid: LucidEvolution,
  label: string,
  build: () => Promise<FaultProofTurn<Step>>,
  options: SubmitOptions = {},
): Promise<OutsideWorkflowSubmission<Step>> => {
  const named = {
    [label]: async (_: { readonly lucid: LucidEvolution }) => await build(),
  }[label]!;
  return (await submitTurnsUnjournaled(named, {
    lucid,
    ...options,
  })) as OutsideWorkflowSubmission<Step>;
};

/**
 * Drives a carriage function to its result, such as the live chunks of a
 * plan: every prerequisite it returns is submitted and confirmed and the
 * function called again with the same own-output journal (see
 * {@link settleFaultProofTurns}).
 */
export const settleTurnsUnjournaled = settleFaultProofTurns;

/** Submits one prerequisite, such as a certification, and returns its output. */
export const submitPrerequisiteUnjournaled = submitPrerequisiteTurn;

/** A hook that sees each signed transaction before it is submitted. */
type BeforeSubmit = (
  transaction: LocallyEvaluatedTransaction,
) => Promise<void> | void;

/**
 * Publishes every missing chunk of a field carriage plan, one confirmed
 * transaction at a time, and returns the plan's live chunks in plan order.
 */
export const publishCarriageUnjournaled = async ({
  beforeSubmit,
  ...args
}: Parameters<typeof publishFaultProofFieldCarriage>[0] & {
  readonly beforeSubmit?: BeforeSubmit;
}): Promise<readonly UTxO[]> =>
  await settleFaultProofTurns(async (current: typeof args) => {
    const published = await publishFaultProofFieldCarriage(current);
    if (isPrerequisiteTurn(published))
      await beforeSubmit?.(published.transaction);
    return published;
  }, args);

/**
 * Builds the tier-3 certification of a published carriage, submits it and
 * returns its hash and confirmed certificate output.
 */
export const certifyCarriageUnjournaled = async ({
  beforeSubmit,
  ...args
}: Parameters<typeof certifyFaultProofFieldCarriage>[0] & {
  readonly beforeSubmit?: BeforeSubmit;
}): Promise<{ readonly txHash: string; readonly certificateUtxo: UTxO }> => {
  const turn = await certifyFaultProofFieldCarriage(args);
  await beforeSubmit?.(turn.transaction);
  const certificateUtxo = await submitPrerequisiteTurn(args.lucid, turn);
  return { txHash: turn.transaction.txHash, certificateUtxo };
};

/**
 * Publishes a proof's chunk outputs in one transaction and reads them back,
 * in proof order, once it is confirmed.
 */
export const publishProofChunks = async ({
  beforeSubmit,
  ...args
}: Parameters<typeof buildProofChunkPublication>[0] & {
  readonly beforeSubmit?: BeforeSubmit;
}): Promise<PublishedProof> => {
  const { transaction } = await buildProofChunkPublication(args);
  await beforeSubmit?.(transaction);
  const txHash = await submitOutsideWorkflow(args.lucid, transaction, {
    label: "publish-proof-chunks",
    awaitConfirmation: true,
  });
  return await readPublishedProofChunks({ ...args, txHash });
};

/**
 * Submits and confirms every prerequisite `build` returns, then returns the
 * next transaction it builds, a continuation or the step, unsubmitted, so a
 * test can inspect or measure it before submitting it itself.
 */
export const buildPastPrerequisitesUnjournaled = async <
  Args extends {
    readonly lucid: LucidEvolution;
    readonly ownOutputs?: OwnOutputJournal;
  },
  Built,
>(
  build: (args: Args) => Promise<FaultProofTurn<Built>>,
  args: NoInfer<Args>,
): Promise<Built> =>
  (
    await settleFaultProofTurns(async (current: Args) => {
      const built = await build(current);
      return isPrerequisiteTurn(built) ? built : { held: built };
    }, args)
  ).held;
