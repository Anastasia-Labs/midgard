/** The field grammar certificate a step of ours reads its item through,
 * certified before the step: a transaction our key signs opens it, or
 * advances our `InProgress` one, batch by batch to `Complete`, the
 * certificate validator running from its published reference script alone.
 * The field is the disputed transaction's, so the certificate can be built
 * any time after the case opens: while a thread of ours is in its game, as
 * soon as its range is within `VALIDATION_TRACE_GRAMMAR_LOOKAHEAD` steps,
 * for each step left in it whose resolution reads a field through the
 * certificate (a field one of our moves carries inline), and at the latest
 * at the move that needs it, beside our publications of the field when they
 * carry it. A certificate stays where its validator keeps it. */
import {
  hashMidgardValidationMachineState,
  type MidgardFieldCarriagePlan,
} from "@al-ft/midgard-core";
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  fieldGrammarCertificateAddress,
  reuseOrCertifyFaultProofFieldGrammar,
} from "../field-grammar-certificate.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import type { FraudProofSignedTransactionRecovery } from "../workflow/l1-source.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "../workflow/orchestrator.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  confirmedValidationTraceStages,
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
} from "./actions.js";
import { validationTraceArtifactOf } from "./artifact.js";
import type { ValidationTraceCaseHistory } from "./case-history.js";
import { validationTraceCaseReads } from "./case-reads.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import {
  carriageReads,
  type ValidationTraceFieldCarriage,
} from "./one-step-carriage.js";
import {
  findValidationTraceFieldGrammarCertificate,
  validationTraceFieldGrammarPlan,
  type ValidationTraceFieldItemRead,
  validationTraceFieldItemRead,
  validationTraceFieldItemRoute,
} from "./one-step-field-item-route.js";
import { validationTraceOneStepDrives } from "./one-step-holders.js";
import { validationTraceReaderPlan } from "./one-step-moves.js";
import { planValidationTraceOneStep } from "./one-step-plan.js";
import { type OwnKeyActions, withOwnKeyActions } from "./own-key-actions.js";
import {
  discoverValidationTraceThreads,
  validationTraceRecordedThreads,
} from "./thread-observation.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

const CATEGORY = "validationTraceDispute" as const;

/** The action that opens or advances a field grammar certificate of ours. */
export const VALIDATION_TRACE_GRAMMAR_CERTIFY =
  "midgard-validation-trace-grammar-certify-v1" as const;

/** The most steps a thread of ours in its game may still dispute for the
 * runner to certify, ahead of the step, the fields their resolutions read
 * through a certificate. */
export const VALIDATION_TRACE_GRAMMAR_LOOKAHEAD = 16;

/** The moves in place of which a certificate is certified. */
const MOVES: ReadonlySet<string> = new Set([
  "reveal",
  "enter_resolution",
  "resolve",
]);

type Certify = Readonly<{
  fieldIndex: number;
  /** The thread whose move's publications carry the field; null for a
   * field the certificate transaction carries inline. */
  consumes: string | null;
  /** Our `InProgress` certificate the transaction advances, or "open". */
  progress: string;
}>;

const certifyAction = (certify: Certify): FraudProofWorkflowAction =>
  Object.freeze({
    actionId: `validation-trace-certify-field-grammar:${certify.fieldIndex.toString()}:${certify.progress}`,
    input: Object.freeze({
      schemaVersion: VALIDATION_TRACE_GRAMMAR_CERTIFY,
      category: CATEGORY,
      stage: "certify_field_grammar",
      ...certify,
    }),
  });

const parseCertify = (action: FraudProofWorkflowAction): Certify => {
  const { fieldIndex, consumes, progress } = action.input;
  if (
    typeof fieldIndex !== "number" ||
    !(typeof consumes === "string" || consumes === null) ||
    typeof progress !== "string"
  )
    throw new Error(
      "validation-trace field grammar certification is malformed",
    );
  return { fieldIndex, consumes, progress };
};

const label = ({ txHash, outputIndex }: UTxO): string =>
  `${txHash}#${outputIndex.toString()}`;

/** Our `InProgress` certificate of the field `plan` names, if one is live. */
const inProgress = async (
  lucid: LucidEvolution,
  deployment: ValidationTraceDisputeDeployment,
  plan: MidgardFieldCarriagePlan,
  owner: string,
): Promise<UTxO | undefined> => {
  const policyId =
    deployment.fieldGrammarCertificate!.validator.spendingScriptHash;
  const unit = SDK.fieldGrammarCertificateUnit(policyId);
  const live = await lucid.utxosAt(
    fieldGrammarCertificateAddress({
      network: deployment.network,
      certificatePolicyId: policyId,
    }),
  );
  return live.find((utxo) => {
    if (utxo.assets[unit] !== 1n || typeof utxo.datum !== "string")
      return false;
    try {
      const datum = Data.from(utxo.datum, SDK.FieldGrammarCertificate);
      if (!("InProgress" in datum)) return false;
      const held = datum.InProgress;
      return (
        held.owner === owner &&
        held.tx_id === plan.txId.toString("hex") &&
        held.field_index === BigInt(plan.fieldIndex) &&
        held.field_hash === plan.commitment.toString("hex") &&
        held.total_length === BigInt(plan.totalLength)
      );
    } catch {
      return false;
    }
  });
};

/**
 * The certification of the field grammar certificates our steps read
 * through, and `wrap`, which puts it in front of the adapter: in place of a
 * wait or a move of ours, the certificate transaction a step of ours still
 * needs.
 */
export const validationTraceFieldGrammarCertification = ({
  lucid,
  signer,
  deployment,
  caseHistory,
  signedRecovery,
  materialOf,
  carriageOf,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly caseHistory: ValidationTraceCaseHistory;
  readonly signedRecovery: FraudProofSignedTransactionRecovery;
  readonly materialOf: (
    headerHash: string,
  ) => ValidationTraceClaimMaterial | undefined;
  /** Our publications of the field the resolve move of `consumes` reads. */
  readonly carriageOf: (
    context: FraudProofWorkflowAdapterContext,
    consumes: string,
  ) => Promise<ValidationTraceFieldCarriage | undefined>;
}) => {
  const owner = signer.paymentKeyHash;
  const drives = validationTraceOneStepDrives(deployment);
  const { ourCase } = validationTraceCaseReads({ lucid, signer, deployment });
  /** Each step's routed item read, by state index, per trace. */
  const planned = new WeakMap<
    ValidationTraceClaimMaterial,
    Map<number, ValidationTraceFieldItemRead | null>
  >();

  /** The routed item reads of the steps from `stateIndices` of our trace;
   * a step the runner does not plan makes none. */
  const readsOf = async (
    material: ValidationTraceClaimMaterial,
    stateIndices: readonly number[],
  ): Promise<readonly ValidationTraceFieldItemRead[]> => {
    const known = planned.get(material) ?? new Map();
    planned.set(material, known);
    const fresh = stateIndices.filter((index) => !known.has(index));
    const steps = await material.stepsAt(fresh);
    for (const [at, stateIndex] of fresh.entries()) {
      const successor = steps[at]?.states[stateIndex + 1];
      const planning =
        successor === undefined
          ? undefined
          : await planValidationTraceOneStep({
              material,
              stateIndex,
              challengerHighHash:
                hashMidgardValidationMachineState(successor).toString("hex"),
              ...drives,
            }).catch(() => undefined);
      known.set(
        stateIndex,
        planning?.kind === "plan"
          ? (validationTraceFieldItemRead(planning.plan) ?? null)
          : null,
      );
    }
    return stateIndices.flatMap((index) => known.get(index) ?? []);
  };

  /** The reads the steps left to the threads of our case in their games
   * make, once within the lookahead. */
  const ahead = async (
    context: FraudProofWorkflowAdapterContext,
    material: ValidationTraceClaimMaterial,
  ): Promise<readonly ValidationTraceFieldItemRead[]> => {
    const artifact = validationTraceArtifactOf(context);
    const found = await ourCase(
      artifact,
      confirmedValidationTraceStages(context.entries),
    );
    if (found === undefined) return [];
    const { threads } = await discoverValidationTraceThreads({
      lucid,
      deployment,
      recorded: await validationTraceRecordedThreads({
        caseHistory,
        headerHash: artifact.headerHash,
        caseUnit: found.caseUnit,
      }),
      caseUnit: found.caseUnit,
      prover: owner,
    });
    const indices = new Set<number>();
    for (const { at, thread } of threads) {
      const [low, high] = [Number(thread.low_index), Number(thread.high_index)];
      if (at === "timeout" || high - low > VALIDATION_TRACE_GRAMMAR_LOOKAHEAD)
        continue;
      for (let index = low; index < high; index += 1) indices.add(index);
    }
    return await readsOf(
      material,
      [...indices].sort((l, r) => l - r),
    );
  };

  /** The certificate transaction to take in place of `observed`, if a step
   * of ours still needs one. */
  const due = async (
    context: FraudProofWorkflowAdapterContext,
    observed: FraudProofWorkflowObservation,
  ): Promise<FraudProofWorkflowAction | undefined> => {
    if (deployment.fieldGrammarCertificate === undefined) return undefined;
    const move =
      observed.kind === "action_required" &&
      observed.action.input.schemaVersion === VALIDATION_TRACE_DISPUTE_ACTION
        ? parseValidationTraceAction(observed.action)
        : undefined;
    if (
      observed.kind !== "pending" &&
      (move === undefined || !MOVES.has(move.stage))
    )
      return undefined;
    const material = materialOf(validationTraceArtifactOf(context).headerHash);
    if (material === undefined) return undefined;
    const reads: { read: ValidationTraceFieldItemRead; consumes: string }[] =
      [];
    if (move?.stage === "resolve" && move.consumes !== null) {
      const [utxo] = await lucid.utxosByOutRef([
        parseOutRefLabel(move.consumes),
      ]);
      // A step the runner cannot plan is reported by its move's build.
      const reader =
        utxo === undefined
          ? undefined
          : await validationTraceReaderPlan({
              deployment,
              material,
              utxo,
            }).catch(() => undefined);
      const read = reader && validationTraceFieldItemRead(reader.plan);
      if (read !== undefined) reads.push({ read, consumes: move.consumes });
    }
    const candidates = [
      ...reads,
      ...(await ahead(context, material)).map((read) => ({
        read,
        consumes: null,
      })),
    ];
    for (const { read, consumes } of candidates) {
      const { fieldIndex } = read;
      if (
        validationTraceFieldItemRoute(
          read,
          material.fieldPreimage(fieldIndex),
        ) === "walk"
      )
        continue;
      const plan = validationTraceFieldGrammarPlan(material, fieldIndex, owner);
      const inline = plan.tier === "Inline";
      // A field our publications carry waits for the move they serve.
      if (
        !inline &&
        (consumes === null ||
          (await carriageOf(context, consumes)) === undefined)
      )
        continue;
      if (
        (await findValidationTraceFieldGrammarCertificate({
          ...{ lucid, deployment, material, fieldIndex, owner },
        })) !== undefined
      )
        continue;
      const held = await inProgress(lucid, deployment, plan, owner);
      const action = certifyAction({
        fieldIndex,
        consumes: inline ? null : consumes,
        progress: held === undefined ? "open" : label(held),
      });
      if (!context.unbuiltActionIds?.has(action.actionId)) return action;
    }
    return undefined;
  };

  const own: OwnKeyActions = {
    owns: (action) =>
      action.input.schemaVersion === VALIDATION_TRACE_GRAMMAR_CERTIFY,
    next: async (context, observed) => {
      const action = await due(context, observed);
      return action === undefined
        ? undefined
        : { kind: "action_required", action };
    },
    build: async (context, action) => {
      const { fieldIndex, consumes } = parseCertify(action);
      const material = materialOf(
        validationTraceArtifactOf(context).headerHash,
      );
      const certificate = deployment.fieldGrammarCertificate;
      if (material === undefined || certificate === undefined)
        throw new Error(
          "validation-trace field grammar certification has no trace or certificate validator",
        );
      if (
        (await findValidationTraceFieldGrammarCertificate({
          ...{ lucid, deployment, material, fieldIndex, owner },
        })) !== undefined
      )
        throw new WorkflowActionChangedError(
          `validation-trace field ${fieldIndex.toString()} already has a Complete field grammar certificate`,
        );
      const carriage =
        consumes === null ? undefined : await carriageOf(context, consumes);
      if (consumes !== null && carriage === undefined)
        throw new WorkflowActionChangedError(
          `validation-trace field ${fieldIndex.toString()} has no publication of ours to certify from`,
        );
      const { sourceKind, compactCbor, witnessSetCompactCbor } =
        material.fieldSource();
      const policyId = deployment.fieldPreimageCertificate?.policyId;
      const captured = await captureLocallyEvaluatedTransaction(
        async (boundary) =>
          await reuseOrCertifyFaultProofFieldGrammar({
            lucid,
            network: deployment.network,
            signer,
            plan: validationTraceFieldGrammarPlan(material, fieldIndex, owner),
            source: { sourceKind, compactCbor, witnessSetCompactCbor },
            certificateValidator: certificate.validator,
            certificateReferenceScriptUtxo: certificate.reference,
            carriageUtxos:
              carriage === undefined ? [] : carriageReads(carriage),
            ...(policyId === undefined
              ? {}
              : { fieldPreimageCertificatePolicyId: policyId }),
            preSubmitBoundary: boundary,
          }),
      );
      return {
        signed: captured.signed,
        referenceScripts: captured.referenceScripts,
      };
    },
  };

  const wrap = (
    base: FraudProofFamilyWorkflowAdapter,
  ): FraudProofFamilyWorkflowAdapter =>
    deployment.fieldGrammarCertificate === undefined
      ? base
      : withOwnKeyActions({
          base,
          own,
          signedRecovery,
          label: "field grammar certification",
        });

  return { wrap };
};
