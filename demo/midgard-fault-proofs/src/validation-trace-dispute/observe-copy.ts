/** Our published copy of a field the observe move of a canonical-decode item
 * reads when the field is too long to carry inline. The copy is tier-2
 * bytes-only carriage at our key address, published through the shared
 * raw-datum prerequisite, which publishes it again should it go missing
 * before the move lands. The move reads it as a reference input
 * (`RawUtxo`). Once that move is final at release depth, or our workflow is
 * done, the copy is spent back by our key: only while our funding
 * reservation still holds it, when funding signs. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { CML, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { RAW_DATUM_PREIMAGE_PREREQUISITE } from "../workflow/field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { recordedCarriageRecovery } from "../workflow/field-carriage-prerequisite.recorded-carriage-recovery.js";
import type {
  FraudProofWorkflowJournalEntry,
  JournalJsonObject,
} from "../workflow/journal.js";
import type { FraudProofSignedTransactionRecovery } from "../workflow/l1-source.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { latestSubmissionIntent } from "../workflow/orchestrator.fraud-proof-workflow-run-result.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "../workflow/orchestrator.js";
import { WorkflowActionUnbuiltError } from "../workflow/orchestrator.unbuilt-actions.js";
import {
  createAuthenticatedRawDatumPreimagePrerequisitePort,
  createRawDatumPreimageRequirement,
  withRawDatumPreimagePrerequisite,
} from "../workflow/raw-datum-preimage-prerequisite.js";
import type { FraudProofAuthenticatedPublicationObserver } from "../workflow/raw-l1-publication-observation.js";
import { reconcileSignedWorkflowTransaction } from "../workflow/signed-transaction-reconciliation.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  submitCapturedTransaction,
} from "../workflow/transaction-boundary.js";
import {
  parseValidationTraceAction,
  recordedValidationTraceMoveBytes,
  VALIDATION_TRACE_DISPUTE_ACTION,
  validationTraceAction,
} from "./actions.js";
import { parseValidationTraceDisputeArtifact } from "./artifact.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import { validationTraceObserveCopyPreimage } from "./one-step-moves.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

const CATEGORY = "validationTraceDispute" as const;

/** The action that spends a copy of ours back by our key. */
export const VALIDATION_TRACE_COPY_RECLAIM =
  "midgard-validation-trace-copy-reclaim-v1" as const;

const reclaimAction = (outRef: string): FraudProofWorkflowAction =>
  Object.freeze({
    actionId: `validation-trace-reclaim-copy:${outRef}`,
    input: Object.freeze({
      schemaVersion: VALIDATION_TRACE_COPY_RECLAIM,
      category: CATEGORY,
      stage: "reclaim_field_copy",
      consumes: outRef,
    }),
  });

const isReclaim = (action: FraudProofWorkflowAction): boolean =>
  action.input.schemaVersion === VALIDATION_TRACE_COPY_RECLAIM;

const label = ({ txHash, outputIndex }: UTxO): string =>
  `${txHash}#${outputIndex.toString()}`;

type Copy = Readonly<{ outRef: string; datumCbor: string }>;

/**
 * From the journal's confirmed actions: each copy our raw publications
 * left, and each confirmed resolve move with the out-refs it read.
 */
const journaled = (entries: readonly FraudProofWorkflowJournalEntry[]) => {
  const copies = new Map<string, Copy>();
  const readers: Readonly<{
    txHash: string;
    signed: string;
    reads: Set<string>;
  }>[] = [];
  for (const { event } of entries) {
    if (event.kind !== "confirmed") continue;
    const intent = latestSubmissionIntent(entries, event.actionId);
    if (intent === undefined || intent.txHash !== event.txHash) continue;
    const { schemaVersion, category, stage } = intent.actionInput;
    if (
      schemaVersion === RAW_DATUM_PREIMAGE_PREREQUISITE &&
      category === CATEGORY &&
      stage === "publish_field_carriage"
    ) {
      const { outRef, datumCbor } = recordedCarriageRecovery({
        category: CATEGORY,
        action: { actionId: intent.actionId, input: intent.actionInput },
        txHash: intent.txHash,
        value: intent.durableRecovery,
      });
      copies.set(outRef, { outRef, datumCbor });
    } else if (
      schemaVersion === VALIDATION_TRACE_DISPUTE_ACTION &&
      stage === "resolve"
    ) {
      const signed = recordedValidationTraceMoveBytes({
        value: intent.durableRecovery,
        txHash: intent.txHash,
      });
      const references = CML.Transaction.from_cbor_hex(signed)
        .body()
        .reference_inputs();
      const reads = new Set<string>();
      for (let index = 0; index < (references?.len() ?? 0); index += 1) {
        const input = references!.get(index);
        reads.add(
          `${input.transaction_id().to_hex()}#${input.index().toString()}`,
        );
      }
      readers.push({ txHash: intent.txHash, signed, reads });
    }
  }
  return { copies: [...copies.values()], readers };
};

/**
 * The copy of a field the observe move of our thread reads, and `wrap`,
 * which publishes it before that move and spends it back after.
 */
export const validationTraceObserveCopy = ({
  lucid,
  signer,
  deployment,
  publications,
  signedRecovery,
  materialOf,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  /** Where a publication of ours stands; without one, no copy is made. */
  readonly publications: FraudProofAuthenticatedPublicationObserver | undefined;
  readonly signedRecovery: FraudProofSignedTransactionRecovery;
  readonly materialOf: (
    headerHash: string,
  ) => ValidationTraceClaimMaterial | undefined;
}) => {
  const port =
    publications === undefined
      ? undefined
      : createAuthenticatedRawDatumPreimagePrerequisitePort({
          category: CATEGORY,
          lucid,
          network: lucid.config().network!,
          signer,
          publications,
          requirementForAction: async ({ action, artifact }) => {
            if (action.input.schemaVersion !== VALIDATION_TRACE_DISPUTE_ACTION)
              return null;
            const { stage, consumes } = parseValidationTraceAction(action);
            if (stage !== "resolve" || consumes === null) return null;
            const [utxo] = await lucid.utxosByOutRef([
              parseOutRefLabel(consumes),
            ]);
            if (utxo === undefined) return null;
            // A step the runner cannot plan is reported by its move's build.
            const preimage = await validationTraceObserveCopyPreimage({
              deployment,
              material: materialOf(
                parseValidationTraceDisputeArtifact(artifact).headerHash,
              ),
              utxo,
            }).catch(() => undefined);
            return preimage === undefined
              ? null
              : createRawDatumPreimageRequirement({ preimage });
          },
        });

  /** The live copy of ours `copy`, unless funding no longer holds it. */
  const spendable = async (copy: Copy): Promise<UTxO | undefined> => {
    if (signer.reservedFunding?.().includes(copy.outRef) === false)
      return undefined;
    const [utxo] = await lucid.utxosByOutRef([parseOutRefLabel(copy.outRef)]);
    return utxo !== undefined &&
      label(utxo) === copy.outRef &&
      utxo.address === signer.address &&
      utxo.datum === copy.datumCbor &&
      utxo.scriptRef == null
      ? utxo
      : undefined;
  };

  /** Whether the transaction `txHash` is final at release depth. */
  const final = async (txHash: string, signed: string): Promise<boolean> => {
    const observed = await signedRecovery.observeSignedTransaction({
      transactionHash: txHash,
      signedTransactionCborHex: signed,
    });
    return (
      observed.status === "included" &&
      observed.inclusionPoint !== undefined &&
      BigInt(observed.inclusionPoint.blockNo) <=
        BigInt(observed.releaseFinalPoint.blockNo)
    );
  };

  /**
   * The copy of ours to spend back now, while the workflow waits or once it
   * is done: one a resolve move read that is final at release depth, or,
   * once done, any.
   */
  const reclaimable = async (
    context: FraudProofWorkflowAdapterContext,
    inner: FraudProofWorkflowObservation,
  ): Promise<UTxO | undefined> => {
    if (inner.kind !== "pending" && inner.kind !== "completed")
      return undefined;
    const { copies, readers } = journaled(context.entries);
    for (const copy of copies) {
      if (context.unbuiltActionIds?.has(reclaimAction(copy.outRef).actionId))
        continue;
      const utxo = await spendable(copy);
      if (utxo === undefined) continue;
      if (inner.kind === "completed") return utxo;
      for (const reader of readers)
        if (
          reader.reads.has(copy.outRef) &&
          (await final(reader.txHash, reader.signed))
        )
          return utxo;
    }
    return undefined;
  };

  const wrap = (
    base: FraudProofFamilyWorkflowAdapter,
  ): FraudProofFamilyWorkflowAdapter => {
    if (port === undefined) return base;
    const inner = withRawDatumPreimagePrerequisite({
      category: CATEGORY,
      base,
      prerequisite: port,
    });
    const prepared = new Map<string, LocallyEvaluatedTransaction>();
    const observe = async (
      context: FraudProofWorkflowAdapterContext,
    ): Promise<FraudProofWorkflowObservation> => {
      const observed = await inner.observe(context);
      if (context.reconciliationOnly === true) return observed;
      const copy = await reclaimable(context, observed);
      return copy === undefined
        ? observed
        : { kind: "action_required", action: reclaimAction(label(copy)) };
    };
    const adapter: FraudProofFamilyWorkflowAdapter = {
      ...inner,
      observe,
      preflight: async (context) => {
        if (!isReclaim(context.action)) return await inner.preflight(context);
        const { actionId } = context.action;
        const current = await observe(context);
        if (
          current.kind !== "action_required" ||
          current.action.actionId !== actionId
        )
          throw new WorkflowActionChangedError(
            `validation-trace ${actionId} is no longer the next action`,
          );
        const key = `${context.workflowId}\u0000${actionId}`;
        prepared.delete(key);
        const [copy] = await lucid.utxosByOutRef([
          parseOutRefLabel(context.action.input.consumes as string),
        ]);
        let transaction: LocallyEvaluatedTransaction;
        try {
          signer.selectWallet(lucid);
          const unsigned = await lucid
            .newTx()
            .collectFrom([copy!])
            .complete({ coinSelection: false });
          const signed = await unsigned.sign.withWallet().complete();
          transaction = {
            txHash: signed.toHash().toLowerCase(),
            signed,
            referenceScripts: [],
          };
        } catch (cause) {
          throw new WorkflowActionUnbuiltError(
            actionId,
            `validation-trace ${actionId} does not build: ${String(cause)}`,
          );
        }
        prepared.set(key, transaction);
        return bindWorkflowPreflightTransaction(
          {
            actionId,
            txHash: transaction.txHash,
            scriptExecution: "none" as const,
            localUplcEvaluation: {
              status: "passed" as const,
              evaluator: LOCAL_UPLC_EVALUATOR,
            },
            referenceScripts: [],
            durableRecovery: {
              signedTransactionCborHex: transaction.signed.toCBOR(),
            },
          },
          transaction.signed,
        );
      },
      submit: async (context) => {
        if (!isReclaim(context.action)) return await inner.submit(context);
        const key = `${context.workflowId}\u0000${context.action.actionId}`;
        const transaction = prepared.get(key);
        if (
          transaction === undefined ||
          transaction.txHash !== context.preflight.txHash
        )
          throw new Error(
            `validation-trace ${context.action.actionId} has no captured body to submit`,
          );
        try {
          return {
            kind: "submitted",
            txHash: await submitCapturedTransaction(transaction),
          };
        } finally {
          prepared.delete(key);
        }
      },
      reconcile: async (context) => {
        if (!isReclaim(context.action)) return await inner.reconcile(context);
        const { txHash, durableRecovery } = context;
        if (txHash === undefined)
          return {
            kind: "conflict",
            reason:
              "validation-trace copy reclaim omitted its transaction hash",
          };
        const recorded = (durableRecovery as JournalJsonObject | undefined)
          ?.signedTransactionCborHex;
        const signed =
          context.signedTransactionCborHex ??
          (typeof recorded === "string" ? recorded : undefined);
        const reconciled = await reconcileSignedWorkflowTransaction({
          transactionHash: txHash,
          ...(signed === undefined ? {} : { signedTransactionCborHex: signed }),
          observe: signedRecovery.observeSignedTransaction,
          rebroadcast: signedRecovery.rebroadcastSignedTransaction,
          ...(context.authorizeResubmission === undefined
            ? {}
            : { authorizeResubmission: context.authorizeResubmission }),
          reportInclusion: true,
        });
        return reconciled.kind === "pending" &&
          reconciled.retirement?.reason === "included"
          ? { kind: "confirmed", txHash }
          : reconciled;
      },
    };
    return Object.freeze(adapter);
  };

  return {
    wrap,
    /** The copy the observe move of `action` reads, as our journal shows it
     * published. */
    of: async (
      context: FraudProofWorkflowAdapterContext,
      action: Parameters<typeof validationTraceAction>[0],
    ): Promise<UTxO | undefined> =>
      port === undefined
        ? undefined
        : (
            await port.resolveAuthenticated({
              headerHash: parseValidationTraceDisputeArtifact(context.artifact)
                .headerHash,
              action: validationTraceAction(action),
              artifact: context.artifact,
              entries: context.entries,
            })
          ).publications[0],
  };
};
