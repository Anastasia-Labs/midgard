/** Our published copy of a field the observe move of a canonical-decode item
 * reads when the field is too long to carry inline. The copy is tier-2
 * bytes-only carriage at our key address, published through the shared
 * raw-datum prerequisite, which publishes it again should it go missing
 * before the move lands. The move reads it as a reference input
 * (`RawUtxo`). Once that move is final at release depth, or our workflow is
 * done, the copy is spent back by our key: only while our funding
 * reservation still holds it, when funding signs. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { RAW_DATUM_PREIMAGE_PREREQUISITE } from "../workflow/field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { recordedCarriageRecovery } from "../workflow/field-carriage-prerequisite.recorded-carriage-recovery.js";
import type { FraudProofWorkflowJournalEntry } from "../workflow/journal.js";
import type { FraudProofSignedTransactionRecovery } from "../workflow/l1-source.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "../workflow/orchestrator.js";
import {
  createAuthenticatedRawDatumPreimagePrerequisitePort,
  createRawDatumPreimageRequirement,
  withRawDatumPreimagePrerequisite,
} from "../workflow/raw-datum-preimage-prerequisite.js";
import type { FraudProofAuthenticatedPublicationObserver } from "../workflow/raw-l1-publication-observation.js";
import {
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
  validationTraceAction,
} from "./actions.js";
import { parseValidationTraceDisputeArtifact } from "./artifact.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import { validationTraceObserveCopyPreimage } from "./one-step-moves.js";
import {
  confirmedIntents,
  validationTraceResolveReaders,
} from "./one-step-staging-journal.js";
import {
  finalAtReleaseDepth,
  type OwnKeyActions,
  withOwnKeyActions,
} from "./own-key-actions.js";
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

const label = ({ txHash, outputIndex }: UTxO): string =>
  `${txHash}#${outputIndex.toString()}`;

type Copy = Readonly<{ outRef: string; datumCbor: string }>;

/** From the journal's confirmed actions: each copy our raw publications
 * left. */
const journaled = (entries: readonly FraudProofWorkflowJournalEntry[]) => {
  const copies = new Map<string, Copy>();
  for (const intent of confirmedIntents(
    entries,
    RAW_DATUM_PREIMAGE_PREREQUISITE,
  )) {
    const { category, stage } = intent.actionInput;
    if (category !== CATEGORY || stage !== "publish_field_carriage") continue;
    const { outRef, datumCbor } = recordedCarriageRecovery({
      category: CATEGORY,
      action: { actionId: intent.actionId, input: intent.actionInput },
      txHash: intent.txHash,
      value: intent.durableRecovery,
    });
    copies.set(outRef, { outRef, datumCbor });
  }
  return [...copies.values()];
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
    const copies = journaled(context.entries);
    if (copies.length === 0) return undefined;
    const readers = validationTraceResolveReaders(context.entries);
    for (const copy of copies) {
      if (context.unbuiltActionIds?.has(reclaimAction(copy.outRef).actionId))
        continue;
      const utxo = await spendable(copy);
      if (utxo === undefined) continue;
      if (inner.kind === "completed") return utxo;
      for (const reader of readers)
        if (
          reader.reads.has(copy.outRef) &&
          (await finalAtReleaseDepth(
            signedRecovery,
            reader.txHash,
            reader.signed,
          ))
        )
          return utxo;
    }
    return undefined;
  };

  const own: OwnKeyActions = {
    owns: (action) =>
      action.input.schemaVersion === VALIDATION_TRACE_COPY_RECLAIM,
    next: async (context, observed) => {
      const copy = await reclaimable(context, observed);
      return copy === undefined
        ? undefined
        : { kind: "action_required", action: reclaimAction(label(copy)) };
    },
    build: async (_context, action) => {
      const [copy] = await lucid.utxosByOutRef([
        parseOutRefLabel(action.input.consumes as string),
      ]);
      signer.selectWallet(lucid);
      const unsigned = await lucid
        .newTx()
        .collectFrom([copy!])
        .complete({ coinSelection: false });
      return { signed: await unsigned.sign.withWallet().complete() };
    },
  };

  const wrap = (
    base: FraudProofFamilyWorkflowAdapter,
  ): FraudProofFamilyWorkflowAdapter =>
    port === undefined
      ? base
      : withOwnKeyActions({
          base: withRawDatumPreimagePrerequisite({
            category: CATEGORY,
            base,
            prerequisite: port,
          }),
          own,
          signedRecovery,
          label: "copy reclaim",
        });

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
