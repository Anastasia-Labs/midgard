/** Our publications of a field a move of our thread reads by reference
 * (one-step-carried-moves.ts). A field one publication of §8.4's raw tier
 * holds is published bytes-only at our proof-materials address through the
 * shared raw-datum prerequisite; a longer one through the shared
 * field-carriage prerequisite, which certifies its publications under the
 * dispute's field-preimage certificate policy past one publication's bytes.
 * Either
 * publishes it again should it go missing before the move that needs it
 * lands. Threads of one case at steps carrying the same field read the
 * same publications. A publication is spent back by our key once our
 * workflow is done (a resolution by another record goes through once each
 * live publication is spent back), or once a move that reads it
 * as a reference input, and leaves its thread past every move that reads
 * it, is final at release depth while no thread of the case past the
 * boundary has a move reading it. A certificate stays where its policy sends
 * it. In front of them, the field grammar certificates our steps read an
 * item through (one-step-field-grammar-certify.ts). */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { spendProofMaterials } from "../funding/proof-materials-address.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { createAuthenticatedFieldCarriagePrerequisitePort } from "../workflow/field-carriage-prerequisite.create-authenticated-field-carriage-prerequisite-port.js";
import {
  createRawCommittedFieldCarriagePlan,
  type PreimageCarriageRequirement,
} from "../workflow/field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { withFieldCarriagePrerequisite } from "../workflow/field-carriage-prerequisite.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import type { FraudProofSignedTransactionRecovery } from "../workflow/l1-source.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "../workflow/orchestrator.js";
import { workflowProofMaterialsHolder } from "../workflow/proof-materials-view.holder.js";
import { createRawDatumPreimageRequirement } from "../workflow/raw-datum-preimage-prerequisite.js";
import type { FraudProofAuthenticatedPublicationObserver } from "../workflow/raw-l1-publication-observation.js";
import {
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
  validationTraceAction,
} from "./actions.js";
import { parseValidationTraceDisputeArtifact } from "./artifact.js";
import type { ValidationTraceCaseHistory } from "./case-history.js";
import { validationTraceCaseReads } from "./case-reads.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import {
  carriageReads,
  type ValidationTraceFieldCarriage,
} from "./one-step-carriage.js";
import { validationTraceFieldGrammarCertification } from "./one-step-field-grammar-certify.js";
import { validationTraceOneStepHolder } from "./one-step-holders.js";
import { validationTraceCarriedField } from "./one-step-moves.js";
import {
  finalAtReleaseDepth,
  type OwnKeyActions,
  withOwnKeyActions,
} from "./own-key-actions.js";
import type {
  ValidationTraceOwnPublication,
  ValidationTraceOwnViewOf,
} from "./own-view.js";
import { validationTraceResolvingOutRefs } from "./thread-observation.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

const CATEGORY = "validationTraceDispute" as const;

/** The longest field the raw-datum prerequisite publishes as one output. */
const RAW_PUBLICATION_MAX_BYTES = 15_000;

/** The action that spends a publication of ours back by our key. */
export const VALIDATION_TRACE_COPY_RECLAIM =
  "midgard-validation-trace-copy-reclaim-v1" as const;

/** A resolution by another record waits on our publications' spend back. */
export const VALIDATION_TRACE_COPY_OUTSTANDING =
  "validation-trace field publications of ours are not yet spent back";

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

/**
 * Our publications of the field each move of our threads reads by
 * reference, and `wrap`, which publishes them before those moves and spends
 * them back after.
 */
export const validationTraceCarriedFields = ({
  lucid,
  signer,
  deployment,
  caseHistory,
  publications,
  signedRecovery,
  materialOf,
  ownView,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly caseHistory: ValidationTraceCaseHistory;
  /** Where a publication of ours stands; without one, none is made. */
  readonly publications: FraudProofAuthenticatedPublicationObserver | undefined;
  readonly signedRecovery: FraudProofSignedTransactionRecovery;
  readonly materialOf: (
    headerHash: string,
  ) => ValidationTraceClaimMaterial | undefined;
  /** Where our publications and our case are read from. */
  readonly ownView: ValidationTraceOwnViewOf;
}) => {
  const { fieldPreimageCertificate: certificate } = deployment;
  /** The requirement of the move `action` names, if it reads a field by
   * reference whose length `raw` admits. */
  const requirementFor =
    (raw: boolean) =>
    async ({
      action,
      artifact,
    }: {
      readonly action: FraudProofWorkflowAction;
      readonly artifact: JournalJsonObject;
    }): Promise<PreimageCarriageRequirement | null> => {
      if (action.input.schemaVersion !== VALIDATION_TRACE_DISPUTE_ACTION)
        return null;
      const { stage, consumes } = parseValidationTraceAction(action);
      if (stage !== "resolve" || consumes === null) return null;
      const [utxo] = await lucid.utxosByOutRef([parseOutRefLabel(consumes)]);
      if (utxo === undefined) return null;
      const material = materialOf(
        parseValidationTraceDisputeArtifact(artifact).headerHash,
      );
      // A step the runner cannot plan is reported by its move's build.
      const field = await validationTraceCarriedField({
        deployment,
        material,
        utxo,
      }).catch(() => undefined);
      if (
        field === undefined ||
        material === undefined ||
        raw !== field.preimage.length <= RAW_PUBLICATION_MAX_BYTES
      )
        return null;
      if (raw) return createRawDatumPreimageRequirement(field);
      // Without the certificate policy the move does not build, and says so.
      if (certificate === undefined) return null;
      const source = material.fieldSource();
      return {
        planned: createRawCommittedFieldCarriagePlan({
          sourceKind: source.sourceKind,
          owner: signer.paymentKeyHash,
          nativeTxId: source.nativeTxId,
          fieldIndex: field.fieldIndex,
          preimage: field.preimage,
        }),
        compactCbor: source.compactCbor,
        witnessSetCompactCbor: source.witnessSetCompactCbor,
        certificate: {
          policyId: certificate.policyId,
          mintingScript: certificate.mintingScript,
          referenceScriptUtxo: certificate.reference,
        },
      };
    };
  const port = (raw: boolean) =>
    publications === undefined
      ? undefined
      : createAuthenticatedFieldCarriagePrerequisitePort({
          category: CATEGORY,
          lucid,
          network: lucid.config().network!,
          signer,
          publications,
          requirementForAction: requirementFor(raw),
        });
  const ports = [
    { port: port(true), raw: true },
    { port: port(false), raw: false },
  ] as const;

  /** Our publications of the field the move of `action` reads, as the
   * field-carriage prerequisite shows them published. */
  const of = async (
    context: FraudProofWorkflowAdapterContext,
    action: Parameters<typeof validationTraceAction>[0],
  ): Promise<ValidationTraceFieldCarriage | undefined> => {
    for (const { port } of ports) {
      if (port === undefined) continue;
      const resolved = await port.resolveAuthenticated({
        headerHash: ownView(context).artifact().headerHash,
        holder: workflowProofMaterialsHolder(context),
        action: validationTraceAction(action),
        artifact: context.artifact,
      });
      if (resolved.requirement === null) continue;
      return Object.freeze({
        publications: resolved.publications,
        ...(resolved.certificate === undefined
          ? {}
          : { certificate: resolved.certificate }),
      });
    }
    return undefined;
  };

  const certification = validationTraceFieldGrammarCertification({
    ...{ lucid, signer, deployment, caseHistory, materialOf },
    ownView,
    carriageOf: async (context, consumes) =>
      await of(context, { stage: "resolve", consumes }),
  });

  const { ourCases } = validationTraceCaseReads({
    lucid,
    signer,
    deployment,
    caseHistory,
  });
  /** The outputs a move of a thread of any case of ours past the boundary
   * reads to carry its field; undefined while that cannot be told. */
  const stillNamed = async (
    context: FraudProofWorkflowAdapterContext,
  ): Promise<ReadonlySet<string> | undefined> => {
    const view = ownView(context);
    const artifact = view.artifact();
    const named = new Set<string>();
    for (const found of await ourCases(view)) {
      const outRefs = await validationTraceResolvingOutRefs({
        lucid,
        signer,
        deployment,
        caseHistory,
        headerHash: artifact.headerHash,
        caseUnit: found.caseUnit,
        caseDatum: found.caseDatum,
      });
      if (outRefs === undefined) return undefined;
      for (const consumes of outRefs) {
        const carriage = await of(context, {
          stage: "resolve",
          consumes,
        }).catch(() => null);
        if (carriage === null) return undefined;
        for (const utxo of carriage === undefined
          ? []
          : carriageReads(carriage))
          named.add(label(utxo));
      }
    }
    return named;
  };

  /** The live publication of ours `publication`. */
  const spendable = async (
    publication: ValidationTraceOwnPublication,
  ): Promise<UTxO | undefined> => {
    const [utxo] = await lucid.utxosByOutRef([
      parseOutRefLabel(publication.outRef),
    ]);
    return utxo !== undefined &&
      label(utxo) === publication.outRef &&
      utxo.address === signer.proofMaterialsAddress &&
      utxo.datum === publication.datumCbor &&
      utxo.scriptRef == null
      ? utxo
      : undefined;
  };

  /** The holders a thread still has a move to make past that reads our
   * publications of its field. */
  const stillRead = new Set(
    (["contextObserverFold", "contextSettle"] as const).flatMap((holder) => {
      try {
        return [validationTraceOneStepHolder(deployment, holder).address];
      } catch {
        return [];
      }
    }),
  );

  /**
   * The publication of ours to spend back now, while the workflow waits or
   * once it is `done`: one a resolve move read, final at release depth, that
   * left no thread where a later move reads it again, which no move of a
   * thread of our case past the boundary reads; or, once done, any.
   */
  const reclaimable = async (
    context: FraudProofWorkflowAdapterContext,
    done: boolean,
  ): Promise<UTxO | undefined> => {
    const view = ownView(context);
    const left = await view.publications();
    if (left.length === 0) return undefined;
    const readers = (await view.resolveReads()).filter(
      ({ outputs }) => !outputs.some(({ address }) => stillRead.has(address)),
    );
    let named: Promise<ReadonlySet<string> | undefined> | undefined;
    const held = async (outRef: string) => {
      named ??= stillNamed(context);
      const read = await named;
      return read === undefined || read.has(outRef);
    };
    for (const publication of left) {
      if (
        context.unbuiltActionIds?.has(
          reclaimAction(publication.outRef).actionId,
        )
      )
        continue;
      const utxo = await spendable(publication);
      if (utxo === undefined) continue;
      if (done) return utxo;
      for (const reader of readers)
        if (
          reader.reads.has(publication.outRef) &&
          (await finalAtReleaseDepth(
            signedRecovery,
            reader.txHash,
            reader.signed,
          ))
        ) {
          if (!(await held(publication.outRef))) return utxo;
          break;
        }
    }
    return undefined;
  };

  const spendBack = (
    publication: UTxO | undefined,
  ): FraudProofWorkflowObservation | undefined =>
    publication === undefined
      ? undefined
      : { kind: "action_required", action: reclaimAction(label(publication)) };

  const own: OwnKeyActions = {
    owns: (action) =>
      action.input.schemaVersion === VALIDATION_TRACE_COPY_RECLAIM,
    next: async (context, observed) =>
      observed.kind === "pending" || observed.kind === "completed"
        ? spendBack(await reclaimable(context, observed.kind === "completed"))
        : undefined,
    // Resolved by another record, our workflow is done, and no thread of
    // our case is left to read a publication: that resolution is read only
    // before the case opens or once it has closed. Each publication the
    // chain shows live is spent back before it resolves; the resolution
    // waits on no spend back's finality, since a spend back a rollback
    // drops leaves a plain output a later proof-materials sweep takes.
    resolved: async (context) => {
      const next = spendBack(await reclaimable(context, true));
      if (next !== undefined) return next;
      const outstanding: string[] = [];
      for (const publication of await ownView(context).publications())
        if ((await spendable(publication)) !== undefined)
          outstanding.push(`${publication.outRef} does not spend back`);
      return outstanding.length === 0
        ? undefined
        : {
            kind: "pending",
            reason: `${VALIDATION_TRACE_COPY_OUTSTANDING}: ${outstanding.join("; ")}`,
          };
    },
    build: async (_context, action) => {
      const [publication] = await lucid.utxosByOutRef([
        parseOutRefLabel(action.input.consumes as string),
      ]);
      signer.selectWallet(lucid);
      // The publication sits at our proof-materials address: spend it by
      // its sig(key) lock, its coin going back to our funding address.
      const unsigned = await spendProofMaterials(
        lucid.newTx().collectFrom([publication!]),
        signer.paymentKeyHash,
      ).complete({ coinSelection: false });
      return { signed: await unsigned.sign.withWallet().complete() };
    },
  };

  const wrap = (
    base: FraudProofFamilyWorkflowAdapter,
  ): FraudProofFamilyWorkflowAdapter => {
    if (ports.every(({ port }) => port === undefined))
      return certification.wrap(base);
    let wrapped = base;
    for (const { port, raw } of ports)
      if (port !== undefined)
        wrapped = withFieldCarriagePrerequisite({
          category: CATEGORY,
          base: wrapped,
          prerequisite: port,
          rawDatum: raw,
        });
    return certification.wrap(
      withOwnKeyActions({
        base: wrapped,
        own,
        label: "copy reclaim",
      }),
    );
  };

  return {
    wrap,
    of,
  };
};
