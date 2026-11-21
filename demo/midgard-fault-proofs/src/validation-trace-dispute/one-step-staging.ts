/** The staging of a staged builtin step's operands (arm 31, semantics
 * groups 7 and 8) for a thread of ours, and its reclaim. When our next move
 * is a thread's PrepareSelected move and our plan of its step is a staged
 * step not staged yet, each run of its staged operands is first staged as
 * one output at our staging address (`sig(<our payment key hash>)`), one
 * transaction a run; once every run's output is at confirmation depth, our
 * plan fills those outputs in and the move goes. The outputs are found by
 * our own view (own-view.ts) alone, by their content where no move of ours
 * read them yet, so a runner started again over it stages nothing twice: one
 * our node's mempool holds is live and waits for its depth, and an output no
 * longer live is staged again. Each output no
 * thread of ours needs any more is spent back by our key
 * (one-step-staging-sweep.ts), and each a thread of ours at the
 * PrepareSelected move's holder will read is held from every sweep
 * (one-step-staging-holds.ts). */
import { createHash } from "node:crypto";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  midgardCekStagingAddress,
  midgardCekStagingResult,
  planMidgardCekStaging,
} from "@al-ft/midgard-sdk/fraud-proof";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { isFraudProofL1WaitError } from "../workflow/l1-source.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "../workflow/orchestrator.js";
import { isWorkflowFundingShortfall } from "../workflow/workflow-funding-shortfall.js";
import {
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
} from "./actions.js";
import type { ValidationTraceCaseHistory } from "./case-history.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import { validationTraceOneStepHolderAt } from "./one-step-holders.js";
import { resolvingPlan } from "./one-step-resolving-plan.js";
import type { ValidationTraceStagingLookup } from "./one-step-staged.js";
import { validationTraceStagingDepth } from "./one-step-staging-depth.js";
import {
  stagedStepWalletPaidHops,
  validationTraceStagingSigner,
  type ValidationTraceStagingStep,
} from "./one-step-staging-funding.js";
import { validationTraceStagingHolds } from "./one-step-staging-holds.js";
import {
  type SignedValidationTraceStaging,
  VALIDATION_TRACE_STAGING,
  VALIDATION_TRACE_STAGING_RECLAIM,
  type ValidationTraceStagingJournal,
  type ValidationTraceStagingOutput,
} from "./one-step-staging-journal.js";
import { pinWorkflowValidationTraceStagings } from "./one-step-staging-pins.js";
import {
  stagedOutRef,
  VALIDATION_TRACE_RESOLVED_ELSEWHERE,
  type ValidationTraceStagingReclaim,
  validationTraceStagingSweep,
} from "./one-step-staging-sweep.js";
import { type OwnKeyActions, withOwnKeyActions } from "./own-key-actions.js";
import type { ValidationTraceOwnViewOf } from "./own-view.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

const CATEGORY = "validationTraceDispute" as const;

/** The alarm of a staging output whose spend back does not build. */
export const VALIDATION_TRACE_STAGING_UNRECLAIMED =
  "validation-trace-staging-unreclaimed" as const;

/** Why a staged step waits once each run is staged. */
export const VALIDATION_TRACE_STAGING_SHALLOW =
  "validation-trace staged operands wait for their staging outputs' confirmation depth" as const;

const digestOf = (datumCbor: string): string =>
  createHash("sha256").update(Buffer.from(datumCbor, "hex")).digest("hex");

/** The staging of `run`. */
const stageAction = (run: string) =>
  Object.freeze({
    actionId: `validation-trace-stage:${run}:0`,
    input: Object.freeze({
      schemaVersion: VALIDATION_TRACE_STAGING,
      category: CATEGORY,
      stage: "stage_operands",
      run,
    }),
  });

/**
 * Our staging of the staged steps of our threads: `wrap`, which stages a
 * step's operands before its PrepareSelected move and spends the outputs
 * back once no thread needs them, and `lookup`, where our own view shows
 * them staged at `confirmationDepth`.
 */
export const validationTraceStaging = ({
  lucid,
  signer,
  deployment,
  caseHistory,
  releaseDepth,
  materialOf,
  confirmationDepth = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
  ownView,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  /** Where each case's threads are read from. */
  readonly caseHistory: ValidationTraceCaseHistory;
  /** How deep the ledger holds a transaction of ours once it is final. */
  readonly releaseDepth: number;
  readonly materialOf: (
    headerHash: string,
  ) => ValidationTraceClaimMaterial | undefined;
  readonly confirmationDepth?: number;
  /** Where our staging is read from. */
  readonly ownView: ValidationTraceOwnViewOf;
}) => {
  const address = midgardCekStagingAddress(
    lucid.config().network!,
    signer.paymentKeyHash,
  );
  const signing = validationTraceStagingSigner({
    lucid,
    signer,
    deployment,
    caseHistory,
    ownView,
  });
  /** The runs and reclaims last observed, by action id, for their builds. */
  const runs = new Map<
    string,
    ValidationTraceStagingStep & Readonly<{ threadUnit: string | undefined }>
  >();
  const reclaims = new Map<string, ValidationTraceStagingReclaim>();

  /** The output `staging` left, while it is live as staged. */
  const live = async (
    staging: ValidationTraceStagingOutput,
  ): Promise<UTxO | undefined> => {
    const [utxo] = await lucid.utxosByOutRef([
      parseOutRefLabel(staging.outRef),
    ]);
    return utxo !== undefined &&
      utxo.address === address &&
      utxo.datum === staging.datumCbor &&
      utxo.scriptRef == null
      ? utxo
      : undefined;
  };

  const deep = validationTraceStagingDepth({
    lucid,
    confirmationDepth,
  });

  const stagingsOf = async (context: FraudProofWorkflowAdapterContext) =>
    await ownView(context).stagings();

  /**
   * The live staging output our own view shows for the run `datumCbor`,
   * and whether it is deep enough: the latest one a resolve move read, as
   * a thread's evidence names it, else the latest staged, else one found by
   * its content.
   */
  const usable = async (
    { stagings, readers, byContent }: ValidationTraceStagingJournal,
    datumCbor: string,
  ) => {
    const candidates = stagings
      .filter((staging) => staging.datumCbor === datumCbor)
      .reverse();
    const read = (staging: ValidationTraceStagingOutput) =>
      readers.some(({ reads }) => reads.has(staging.outRef));
    const ordered = [
      ...candidates.filter(read),
      ...candidates.filter((candidate) => !read(candidate)),
    ];
    for (const staging of [
      ...ordered,
      ...((await byContent?.(datumCbor)) ?? []),
    ])
      if ((await live(staging)) !== undefined)
        return { staging, ready: await deep(staging) };
    return undefined;
  };

  /** The latest staging output our own view shows for the run `datumCbor`
   * among those `reads` holds: one live and deep enough, or, when `onLedger`
   * is false, any. */
  const readBy = async (
    { stagings }: ValidationTraceStagingJournal,
    reads: ReadonlySet<string>,
    datumCbor: string,
    onLedger = true,
  ) => {
    for (const staging of [...stagings].reverse())
      if (
        staging.datumCbor === datumCbor &&
        reads.has(staging.outRef) &&
        (!onLedger ||
          ((await live(staging)) !== undefined && (await deep(staging))))
      )
        return staging.outRef;
    return undefined;
  };

  /**
   * Where our own view shows `operands` staged: the stagings `usable` names,
   * or, when `accepts` holds of other stagings one resolve move of ours read
   * (the latest such move first) and not of those, the other stagings: live
   * ones first, else ones no longer live. `accepts` holds only of the
   * staging a thread's committed evidence names, whose window is rebuilt
   * from it alone; a hop that reads the outputs still refuses one not live
   * (one-step-staged.ts).
   */
  const lookup =
    (context: FraudProofWorkflowAdapterContext): ValidationTraceStagingLookup =>
    async (operands, accepts) => {
      const known = await stagingsOf(context);
      const plan = planMidgardCekStaging(operands);
      const stagedBy = async (
        pick: (datumCbor: string) => Promise<string | undefined>,
      ) => {
        const refs = [];
        for (const { datumCbor } of plan) {
          const outRef = await pick(datumCbor);
          if (outRef === undefined) return undefined;
          refs.push(stagedOutRef(outRef));
        }
        return midgardCekStagingResult(operands, plan, refs);
      };
      const preferred = await stagedBy(async (datumCbor) => {
        const found = await usable(known, datumCbor);
        return found?.ready === true ? found.staging.outRef : undefined;
      });
      if (
        accepts === undefined ||
        (preferred !== undefined && accepts(preferred))
      )
        return preferred;
      for (const onLedger of [true, false])
        for (const { reads } of [...known.readers].reverse()) {
          const read = await stagedBy(
            async (datumCbor) =>
              await readBy(known, reads, datumCbor, onLedger),
          );
          if (read !== undefined && accepts(read)) return read;
        }
      return preferred;
    };

  /** The staging our next move, `observed`, waits on, if any. */
  const staging = async (
    context: FraudProofWorkflowAdapterContext,
    observed: FraudProofWorkflowObservation,
  ): Promise<FraudProofWorkflowObservation | undefined> => {
    if (
      observed.kind !== "action_required" ||
      observed.action.input.schemaVersion !== VALIDATION_TRACE_DISPUTE_ACTION
    )
      return undefined;
    const { stage, consumes } = parseValidationTraceAction(observed.action);
    if (stage !== "resolve" || consumes === null) return undefined;
    const [utxo] = await lucid.utxosByOutRef([parseOutRefLabel(consumes)]);
    if (
      utxo === undefined ||
      validationTraceOneStepHolderAt(deployment, utxo.address)?.holder !==
        "prepare"
    )
      return undefined;
    // A step the runner cannot plan is reported by its move's build.
    const plan = await resolvingPlan({
      deployment,
      material: materialOf(ownView(context).artifact().headerHash),
      utxo,
      holder: "prepare",
      caseThread: undefined,
      carried: undefined,
      staging: lookup(context),
    }).catch(() => undefined);
    if (
      plan?.kind !== "core" ||
      plan.core.staged === undefined ||
      plan.core.staged.refs !== undefined
    )
      return undefined;
    const known = await stagingsOf(context);
    let shallow = false;
    const missing = [];
    for (const planned of planMidgardCekStaging(plan.core.staged.operands)) {
      const found = await usable(known, planned.datumCbor);
      if (found === undefined) missing.push(planned);
      else shallow ||= !found.ready;
    }
    // The first run still to stage; when its staging does not build, the
    // operator's alarm names what the rest of the step needs from our
    // wallet: the other runs' deposits, and the step's loop hops.
    const [planned, ...rest] = missing;
    if (planned !== undefined) {
      const digest = digestOf(planned.datumCbor);
      const action = stageAction(digest);
      if (context.unbuiltActionIds?.has(action.actionId))
        return {
          kind: "pending",
          reason: `validation-trace staged operands are not staged: ${action.actionId} does not build`,
        };
      runs.set(action.actionId, {
        run: planned,
        rest: rest.map(({ datumCbor }) => datumCbor),
        walletPaidHops: stagedStepWalletPaidHops(plan.core.hops.holders),
        threadOutRef: consumes,
        threadUnit: Object.entries(utxo.assets).find(
          ([asset, quantity]) => asset !== "lovelace" && quantity === 1n,
        )?.[0],
      });
      return { kind: "action_required", action };
    }
    return shallow
      ? { kind: "pending", reason: VALIDATION_TRACE_STAGING_SHALLOW }
      : undefined;
  };

  const holds = validationTraceStagingHolds({
    lucid,
    signer,
    deployment,
    materialOf,
    ownView,
    stagings: stagingsOf,
    usable,
  });

  const sweep = validationTraceStagingSweep({
    lucid,
    deployment,
    releaseDepth,
    live,
    usable,
    stagings: stagingsOf,
    plans: holds.plans,
    traced: (context) =>
      materialOf(ownView(context).artifact().headerHash) !== undefined,
    select: (actionId, reclaim) => reclaims.set(actionId, reclaim),
  });

  // A staging being built pins its own output and run; each observation
  // pins the rest of what our threads hold (one-step-staging-holds.ts).
  const pin = (
    context: FraudProofWorkflowAdapterContext,
    signed: readonly SignedValidationTraceStaging[],
  ) => pinWorkflowValidationTraceStagings({ signer, lucid }, context, signed);

  const build = async (
    context: FraudProofWorkflowAdapterContext,
    action: FraudProofWorkflowAction,
  ) => {
    signer.selectWallet(lucid);
    if (action.input.schemaVersion === VALIDATION_TRACE_STAGING) {
      const step = runs.get(action.actionId);
      if (step === undefined)
        throw new WorkflowActionChangedError(
          `validation-trace ${action.actionId} has no run to stage`,
        );
      const { signed, outputIndex } = await signing.stage(
        context,
        action.actionId,
        step,
      );
      pin(context, [
        {
          outRef: `${signed.toHash().toLowerCase()}#${outputIndex.toString()}`,
          datumCbor: step.run.datumCbor,
        },
      ]);
      return { signed };
    }
    const found = reclaims.get(action.actionId);
    if (found === undefined)
      throw new WorkflowActionChangedError(
        `validation-trace ${action.actionId} has no output to spend back`,
      );
    try {
      return {
        signed: await signing.reclaim(context, found.utxo, found.pins),
      };
    } catch (cause) {
      if (
        cause instanceof WorkflowActionChangedError ||
        isWorkflowFundingShortfall(cause) ||
        isFraudProofL1WaitError(cause)
      )
        throw cause;
      const { txHash, outputIndex, assets } = found.utxo;
      throw new Error(
        `${VALIDATION_TRACE_STAGING_UNRECLAIMED}: staging output ${txHash}#${outputIndex.toString()} holds ${(assets.lovelace ?? 0n).toString()} lovelace of ours at our staging address, and its spend back to our wallet does not build; it is tried again every run: ${String(cause)}`,
        { cause },
      );
    }
  };

  const own: OwnKeyActions = {
    owns: ({ input }) =>
      input.schemaVersion === VALIDATION_TRACE_STAGING ||
      input.schemaVersion === VALIDATION_TRACE_STAGING_RECLAIM,
    next: async (context, observed) => {
      const staged = await staging(context, observed);
      if (staged?.kind === "action_required") return staged;
      return (await sweep(context, staged ?? observed)) ?? staged;
    },
    // A staging our wallet cannot fund names what its build would.
    unfunded: async (context, action, shortfall) => {
      const step =
        action.input.schemaVersion === VALIDATION_TRACE_STAGING
          ? runs.get(action.actionId)
          : undefined;
      return step === undefined
        ? undefined
        : await signing.unfunded(
            context,
            action.actionId,
            step,
            shortfall.cause,
          );
    },
    // Once the header rolls back, each staging output no thread of ours
    // needs is still spent back; nothing is staged.
    reclaim: async (context, observed) => await sweep(context, observed),
    // Resolved by another record, our workflow is done: each staging output
    // is spent back, and final at release depth, before it resolves.
    resolved: async (context) =>
      await sweep(context, VALIDATION_TRACE_RESOLVED_ELSEWHERE),
    build,
  };

  return {
    wrap: (base: FraudProofFamilyWorkflowAdapter) => {
      const wrapped = withOwnKeyActions({
        base,
        own,
        label: "staging",
      });
      return {
        ...wrapped,
        observe: async (context) => {
          await holds.register(context);
          return await wrapped.observe(context);
        },
      } satisfies FraudProofFamilyWorkflowAdapter;
    },
    lookup,
  };
};
