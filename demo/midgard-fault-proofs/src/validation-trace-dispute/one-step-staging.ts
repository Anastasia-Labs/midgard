/** The staging of a staged builtin step's operands (arm 31, semantics
 * groups 7 and 8) for a thread of ours, and its reclaim. When our next move
 * is a thread's PrepareSelected move and our plan of its step is a staged
 * step not staged yet, each run of its staged operands is first staged as
 * one output at our staging address (`sig(<our payment key hash>)`), one
 * transaction a run; once every run's output is at confirmation depth, our
 * plan fills those outputs in and the move goes. The outputs are found by
 * the journal alone, so a runner started again over it stages nothing
 * twice; an output no longer live is staged again. A thread pins the
 * outputs its moves read until its move to the settle, which read them, is
 * final at release depth; an output read and pinned by no thread, or any
 * once our workflow is done, is spent back by our key. */
import { createHash } from "node:crypto";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { depth, isSafe } from "@al-ft/midgard-l1-follower/heads";
import {
  buildUnsignedMidgardCekStagingProgram,
  buildUnsignedMidgardCekStagingReclaimProgram,
  midgardCekStagingAddress,
  midgardCekStagingOutRef,
  MidgardCekStagingPins,
  midgardCekStagingResult,
  type MidgardCekStagingRun,
  planMidgardCekStaging,
} from "@al-ft/midgard-sdk";
import type {
  LucidEvolution,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";

import type { ResolvedProverSigner } from "../runtime.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import type { FraudProofSignedTransactionRecovery } from "../workflow/l1-source.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "../workflow/orchestrator.js";
import {
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
} from "./actions.js";
import { parseValidationTraceDisputeArtifact } from "./artifact.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import {
  validationTraceOneStepHolder,
  validationTraceOneStepHolderAt,
} from "./one-step-holders.js";
import { resolvingPlan } from "./one-step-resolving-plan.js";
import type { ValidationTraceStagingLookup } from "./one-step-staged.js";
import {
  type JournaledStaging,
  journaledStagings,
  VALIDATION_TRACE_STAGING,
  VALIDATION_TRACE_STAGING_RECLAIM,
  type ValidationTraceResolveReader,
  validationTraceResolveReaders,
} from "./one-step-staging-journal.js";
import {
  finalAtReleaseDepth,
  type OwnKeyActions,
  withOwnKeyActions,
} from "./own-key-actions.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

const CATEGORY = "validationTraceDispute" as const;

/** Why a staged step waits once each run is staged. */
export const VALIDATION_TRACE_STAGING_SHALLOW =
  "validation-trace staged operands wait for their staging outputs' confirmation depth" as const;

const digestOf = (datumCbor: string): string =>
  createHash("sha256").update(Buffer.from(datumCbor, "hex")).digest("hex");

const stageAction = (run: string, generation: number) =>
  Object.freeze({
    actionId: `validation-trace-stage:${run}:${generation.toString()}`,
    input: Object.freeze({
      schemaVersion: VALIDATION_TRACE_STAGING,
      category: CATEGORY,
      stage: "stage_operands",
      run,
    }),
  });

const reclaimAction = (outRef: string) =>
  Object.freeze({
    actionId: `validation-trace-reclaim-staging:${outRef}`,
    input: Object.freeze({
      schemaVersion: VALIDATION_TRACE_STAGING_RECLAIM,
      category: CATEGORY,
      stage: "reclaim_staging",
      consumes: outRef,
    }),
  });

const stagedOutRef = (outRef: string) => {
  const { txHash, outputIndex } = parseOutRefLabel(outRef);
  return { transactionId: Buffer.from(txHash, "hex"), outputIndex };
};

/** The value of `program`, or its error thrown as itself. */
const run = async <A>(program: Effect.Effect<A, Error>): Promise<A> => {
  const result = await Effect.runPromise(Effect.either(program));
  if (Either.isLeft(result)) throw result.left;
  return result.right;
};

/**
 * Our staging of the staged steps of our threads: `wrap`, which stages a
 * step's operands before its PrepareSelected move and spends the outputs
 * back once no thread needs them, and `lookup`, where our journal shows
 * them staged at `confirmationDepth`.
 */
export const validationTraceStaging = ({
  lucid,
  signer,
  deployment,
  signedRecovery,
  materialOf,
  confirmationDepth = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly signedRecovery: FraudProofSignedTransactionRecovery;
  readonly materialOf: (
    headerHash: string,
  ) => ValidationTraceClaimMaterial | undefined;
  readonly confirmationDepth?: number;
}) => {
  const address = midgardCekStagingAddress(
    lucid.config().network!,
    signer.paymentKeyHash,
  );
  /** The runs and reclaims last observed, by action id, for their builds. */
  const runs = new Map<string, MidgardCekStagingRun>();
  const reclaims = new Map<
    string,
    Readonly<{ utxo: UTxO; pins: MidgardCekStagingPins }>
  >();

  /** The output `staging` left, while it is live as staged. */
  const live = async (staging: JournaledStaging): Promise<UTxO | undefined> => {
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

  /** Whether `staging`'s transaction is safe: at depth `confirmationDepth`
   * or deeper, counted by the heads module. */
  const deep = async (staging: JournaledStaging): Promise<boolean> => {
    try {
      const observed = await signedRecovery.observeSignedTransaction({
        transactionHash: staging.txHash,
        signedTransactionCborHex: staging.signed,
      });
      return (
        observed.status === "included" &&
        observed.inclusionPoint !== undefined &&
        isSafe(
          depth(
            Number(observed.canonicalPoint.blockNo),
            Number(observed.inclusionPoint.blockNo),
          ),
          { confirmationDepth },
        )
      );
    } catch {
      return false;
    }
  };

  const journal = (context: FraudProofWorkflowAdapterContext) => ({
    ...journaledStagings(context.entries),
    readers: validationTraceResolveReaders(context.entries),
  });
  type Journal = ReturnType<typeof journal>;

  /**
   * The live staging output our journal shows for the run `datumCbor`,
   * and whether it is deep enough: the latest one a resolve move read, as
   * a thread's evidence names it, else the latest staged.
   */
  const usable = async (
    { stagings, reclaimed, readers }: Journal,
    datumCbor: string,
  ) => {
    const candidates = stagings
      .filter(
        (staging) =>
          staging.datumCbor === datumCbor && !reclaimed.has(staging.outRef),
      )
      .reverse();
    const read = (staging: JournaledStaging) =>
      readers.some(({ reads }) => reads.has(staging.outRef));
    for (const staging of [
      ...candidates.filter(read),
      ...candidates.filter((candidate) => !read(candidate)),
    ])
      if ((await live(staging)) !== undefined)
        return { staging, ready: await deep(staging) };
    return undefined;
  };

  const lookup =
    (context: FraudProofWorkflowAdapterContext): ValidationTraceStagingLookup =>
    async (operands) => {
      const known = journal(context);
      const plan = planMidgardCekStaging(operands);
      const refs = [];
      for (const { datumCbor } of plan) {
        const found = await usable(known, datumCbor);
        if (found?.ready !== true) return undefined;
        refs.push(stagedOutRef(found.staging.outRef));
      }
      return midgardCekStagingResult(operands, plan, refs);
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
      material: materialOf(
        parseValidationTraceDisputeArtifact(context.artifact).headerHash,
      ),
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
    const known = journal(context);
    let shallow = false;
    for (const planned of planMidgardCekStaging(plan.core.staged.operands)) {
      const found = await usable(known, planned.datumCbor);
      if (found !== undefined) {
        shallow ||= !found.ready;
        continue;
      }
      const digest = digestOf(planned.datumCbor);
      const action = stageAction(
        digest,
        new Set(
          known.stagings
            .filter((staged) => staged.run === digest)
            .map(({ actionId }) => actionId),
        ).size,
      );
      if (context.unbuiltActionIds?.has(action.actionId))
        return {
          kind: "pending",
          reason: `validation-trace staged operands are not staged: ${action.actionId} does not build`,
        };
      runs.set(action.actionId, planned);
      return { kind: "action_required", action };
    }
    return shallow
      ? { kind: "pending", reason: VALIDATION_TRACE_STAGING_SHALLOW }
      : undefined;
  };

  /** Each thread's pins of the staging outputs its moves read, released
   * once its move to the settle, which read them, is final. */
  const pinsOf = async (
    readers: readonly ValidationTraceResolveReader[],
    staged: ReadonlySet<string>,
  ): Promise<MidgardCekStagingPins> => {
    const pins = new MidgardCekStagingPins();
    const settle = validationTraceOneStepHolder(deployment, "settle").address;
    const released: string[] = [];
    for (const reader of readers) {
      const reads = [...reader.reads].filter((outRef) => staged.has(outRef));
      const thread = reader.outputs.find(
        (output) =>
          validationTraceOneStepHolderAt(deployment, output.address) !==
          undefined,
      );
      const unit = Object.entries(thread?.assets ?? {}).find(
        ([asset, quantity]) => asset !== "lovelace" && quantity === 1n,
      )?.[0];
      if (reads.length === 0 || unit === undefined) continue;
      pins.pin(unit, reads.map(stagedOutRef));
      if (
        thread!.address === settle &&
        (await finalAtReleaseDepth(
          signedRecovery,
          reader.txHash,
          reader.signed,
        ))
      )
        released.push(unit);
    }
    for (const unit of released) pins.release(unit);
    return pins;
  };

  /** The staging output of ours to spend back now, while our workflow
   * waits or once it is done. */
  const reclaim = async (
    context: FraudProofWorkflowAdapterContext,
    observed: FraudProofWorkflowObservation,
  ): Promise<FraudProofWorkflowObservation | undefined> => {
    if (observed.kind !== "pending" && observed.kind !== "completed")
      return undefined;
    const { stagings, reclaimed, readers } = journal(context);
    const open = stagings.filter(
      ({ outRef }) =>
        !reclaimed.has(outRef) &&
        context.unbuiltActionIds?.has(reclaimAction(outRef).actionId) !== true,
    );
    if (open.length === 0) return undefined;
    const pins =
      observed.kind === "completed"
        ? new MidgardCekStagingPins()
        : await pinsOf(readers, new Set(stagings.map(({ outRef }) => outRef)));
    for (const staging of open) {
      const ref = stagedOutRef(staging.outRef);
      if (
        observed.kind === "pending" &&
        (pins.pinnedBy(ref).length > 0 ||
          !readers.some(({ reads }) => reads.has(staging.outRef)))
      )
        continue;
      const utxo = await live(staging);
      if (utxo === undefined) continue;
      const action = reclaimAction(staging.outRef);
      reclaims.set(action.actionId, { utxo, pins });
      return { kind: "action_required", action };
    }
    return undefined;
  };

  const build = async (
    _context: FraudProofWorkflowAdapterContext,
    action: FraudProofWorkflowAction,
  ) => {
    signer.selectWallet(lucid);
    if (action.input.schemaVersion === VALIDATION_TRACE_STAGING) {
      const planned = runs.get(action.actionId);
      if (planned === undefined)
        throw new WorkflowActionChangedError(
          `validation-trace ${action.actionId} has no run to stage`,
        );
      const unsigned: TxSignBuilder = await run(
        buildUnsignedMidgardCekStagingProgram(lucid, { run: planned }),
      );
      const { outputIndex } = midgardCekStagingOutRef(unsigned, planned);
      const signed = await unsigned.sign.withWallet().complete();
      return {
        signed,
        recovery: {
          outRef: `${signed.toHash().toLowerCase()}#${outputIndex.toString()}`,
        },
      };
    }
    const found = reclaims.get(action.actionId);
    if (found === undefined)
      throw new WorkflowActionChangedError(
        `validation-trace ${action.actionId} has no output to spend back`,
      );
    const unsigned = await run(
      buildUnsignedMidgardCekStagingReclaimProgram(lucid, {
        outputs: [found.utxo],
        pins: found.pins,
      }),
    );
    return { signed: await unsigned.sign.withWallet().complete() };
  };

  const own: OwnKeyActions = {
    owns: ({ input }) =>
      input.schemaVersion === VALIDATION_TRACE_STAGING ||
      input.schemaVersion === VALIDATION_TRACE_STAGING_RECLAIM,
    next: async (context, observed) =>
      (await staging(context, observed)) ?? (await reclaim(context, observed)),
    build,
  };

  return {
    wrap: (base: FraudProofFamilyWorkflowAdapter) =>
      withOwnKeyActions({ base, own, signedRecovery, label: "staging" }),
    lookup,
  };
};
