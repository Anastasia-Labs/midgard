import { midgardFieldCommitment } from "@al-ft/midgard-core";
import {
  deriveValidationProofItemPublication,
  PreparedValidationResolutionDatum,
  resolveMidgardFieldCarriageAgainstReferenceInputs,
  ValidationResolutionDatum,
} from "@al-ft/midgard-sdk";
import { validationSemanticResolverIndex } from "@al-ft/midgard-validation";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  fetchUtxoByOutRef,
  parseOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import { requireManifestBoundReferenceScriptUtxo } from "../workflow/deployment-manifest-binding.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  createBoundDataPublicationRequirement,
  createRawCommittedFieldCarriagePlan,
  type FieldCarriagePrerequisitePort,
} from "../workflow/field-carriage-prerequisite.js";
import {
  journalJsonDigest,
  normalizeJournalJson,
} from "../workflow/journal.js";
import { type FraudProofWorkflowAction } from "../workflow/orchestrator.js";
import { validationSemanticResolverGlobalIndex } from "./submit/reference-scripts.js";
import { selectValidationCompleteItemCarriage } from "./submit/validity.js";
import { type ValidationTraceDisputeWorkflowDeploymentBinding } from "./workflow-binding.js";
import { readCanonicalCheckpoint } from "./workflow-canonical-checkpoint.js";
import {
  type ValidationTraceDisputeActuationMaterial,
  type ValidationTraceDisputeActuatorAction,
  type ValidationTraceDisputeFieldCarriageBinding,
} from "./workflow-engine.plan-validation-trace-dispute-move.js";
import { recoverValidationTraceStateIndex } from "./workflow-engine.recover-validation-trace-state-index.js";

export const validationTraceFieldCarriageAction = (
  action: ValidationTraceDisputeActuatorAction,
): FraudProofWorkflowAction => {
  const input = normalizeJournalJson({
    ...action,
    category: "validationTraceDispute",
    ...(action.stage === "remove"
      ? {
          requiresMutationLease:
            action.nextRemovalOutRef !== action.stateQueueBlockOutRef,
        }
      : {}),
  }) as FraudProofWorkflowAction["input"];
  return { actionId: `${action.stage}:${journalJsonDigest(input)}`, input };
};

export const validationResolutionFromUtxo = (utxo: UTxO) => {
  if (utxo.datum == null)
    throw new Error("validation resolution thread lost its datum");
  try {
    const prepared = Data.from(utxo.datum, PreparedValidationResolutionDatum);
    if (prepared.data !== null) return prepared.data.resolution;
  } catch {
    // An unprepared resolution uses the other exact onchain schema.
  }
  const datum = Data.from(utxo.datum, ValidationResolutionDatum);
  if (datum.data === null)
    throw new Error("validation resolution thread carries a null state");
  return datum.data;
};

/** Reuses the raw-L1, intent-journaled publication/certification protocol. */
export const createValidationTraceFieldCarriageProvider = ({
  binding,
  lucid,
  signer,
  l1,
  material,
}: {
  readonly binding: ValidationTraceDisputeWorkflowDeploymentBinding;
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly l1: FraudProofFamilyL1ObservationPort<"validationTraceDispute">;
  readonly material: ValidationTraceDisputeActuationMaterial | undefined;
}) => {
  const reference = async (name: string) => {
    const deployed = binding.referenceScriptsByContract[name];
    if (deployed === undefined)
      throw new Error(`validation field carriage omitted published ${name}`);
    return requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo: await fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(deployed.outRef, name),
        label: name,
      }),
    });
  };
  const requirementForAction = async ({
    action,
  }: {
    readonly action: FraudProofWorkflowAction;
  }) => {
    if (
      action.input.stage !== "prepare_selected" &&
      action.input.stage !== "semantic_resolution"
    )
      return null;
    if (material === undefined)
      throw new Error(
        "validation field carriage requires an admitted challenge",
      );
    const thread = action.input.threadOutRef;
    if (typeof thread !== "string")
      throw new Error("validation field carriage action omitted thread");
    const utxo = await fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(thread, "resolution thread"),
      label: "resolution thread",
    });
    const canonical = readCanonicalCheckpoint(
      utxo,
      binding.resolvedContracts.contracts.validationTraceDispute,
    );
    if (canonical !== undefined && canonical.role !== "observe") return null;
    const index = recoverValidationTraceStateIndex({
      trace: material.challengerTrace,
      resolution: canonical?.resolution ?? validationResolutionFromUtxo(utxo),
    });
    const witness = material.challengerTrace.witnesses[index]!;
    const source = witness.auxiliary;
    if (
      source?.kind !== "transactionFieldItem" &&
      source?.kind !== "transactionFieldChunk" &&
      source?.kind !== "requiredSignerItem"
    )
      return null;
    const pre = material.challengerTrace.states[index]!;
    if (
      pre.phase === "signatures" &&
      source.fieldIndex !== (source.kind === "requiredSignerItem" ? 4 : 7)
    )
      throw new Error(
        "validation signature source carries the wrong field index",
      );
    const planned = createRawCommittedFieldCarriagePlan({
      sourceKind: pre.sourceKind === "forced" ? 1n : 0n,
      owner: signer.paymentKeyHash,
      nativeTxId: pre.transactionId.toString("hex"),
      fieldIndex: source.fieldIndex,
      preimage: source.fieldPreimage,
    });
    if (planned.plan.tier === "Inline") {
      if (
        canonical?.role !== "observe" ||
        pre.phase !== "canonicalDecode" ||
        source.kind !== "transactionFieldItem" ||
        source.fieldIndex !== 2 ||
        selectValidationCompleteItemCarriage(source.fieldPreimage.length) ===
          "direct"
      )
        return null;
      const publication = deriveValidationProofItemPublication({
        transactionId: pre.transactionId.toString("hex"),
        transactionCommitment: pre.transactionCommitment.toString("hex"),
        fieldPreimage: source.fieldPreimage.toString("hex"),
      });
      return createBoundDataPublicationRequirement({
        publicationAddress:
          binding.resolvedContracts.contracts.validationTraceDispute.proofItem
            .spendingScriptAddress,
        datumCbor: publication.datumCbor,
        sourceIdentity: {
          headerHash: binding.definition.headerHash,
          action,
          sourceKind: planned.sourceKind.toString(),
          fieldIndex: planned.fieldIndex,
          transactionId: planned.nativeTxId,
          transactionCommitment: pre.transactionCommitment.toString("hex"),
          fieldCommitment: planned.commitment,
        },
      });
    }
    // Signatures resolve at the semantic script; complete output items resolve
    // at observation. Each route derives its complete actual reference set.
    if (
      pre.phase !== "signatures" &&
      !(
        pre.phase === "canonicalDecode" &&
        source.kind === "transactionFieldItem" &&
        source.fieldIndex === 2 &&
        canonical?.role === "observe"
      )
    )
      throw new Error(
        "Autonomous reference carriage is not installed for this validation phase",
      );
    const member = material.claim.source_membership;
    const entry =
      "ForcedValidationSource" in member
        ? member.ForcedValidationSource.membership.value
        : member.NormalValidationSource.membership.value;
    const submitted =
      "submitted_source" in entry ? entry.submitted_source : entry.source;
    const certificate = binding.fieldPreimageCertificate;
    if (certificate === null)
      throw new Error("validation field carriage omitted certificate contract");
    return {
      planned,
      compactCbor: submitted.compact_cbor,
      witnessSetCompactCbor: submitted.witness_set_compact_cbor,
      certificate: {
        ...certificate,
        referenceScriptUtxo: await reference("fieldPreimageCertificateMint"),
      },
    };
  };
  const prerequisite: FieldCarriagePrerequisitePort<"validationTraceDispute"> =
    createAuthenticatedFieldCarriagePrerequisitePort({
      category: "validationTraceDispute",
      lucid,
      signer,
      network: binding.network,
      publications: l1.publications,
      requirementForAction,
      transactionConfirmed: async (input) =>
        await l1.transactionConfirmed(input),
    });
  return Object.freeze({
    prerequisite,
    resolve: async (
      action: ValidationTraceDisputeActuatorAction,
      stateIndex: number,
    ) => {
      const authenticated = await prerequisite.resolveAuthenticated({
        headerHash: binding.definition.headerHash,
        action: validationTraceFieldCarriageAction(action),
        artifact: {},
      });
      if (authenticated.requirement === null) return undefined;
      if (
        "kind" in authenticated.requirement &&
        authenticated.requirement.kind === "bound_data_publication"
      ) {
        const witness = material!.challengerTrace.witnesses[stateIndex]!;
        const pre = material!.challengerTrace.states[stateIndex]!;
        if (
          pre.phase !== "canonicalDecode" ||
          witness.auxiliary?.kind !== "transactionFieldItem" ||
          witness.auxiliary.fieldIndex !== 2
        )
          throw new Error(
            "bound proof-item delivery changed its complete source",
          );
        const fieldPreimage = witness.auxiliary.fieldPreimage;
        const publication = deriveValidationProofItemPublication({
          transactionId: pre.transactionId.toString("hex"),
          transactionCommitment: pre.transactionCommitment.toString("hex"),
          fieldPreimage: fieldPreimage.toString("hex"),
        });
        const proofItemReference = authenticated.publications[0];
        if (
          authenticated.publications.length !== 1 ||
          proofItemReference === undefined ||
          proofItemReference.address !==
            binding.resolvedContracts.contracts.validationTraceDispute.proofItem
              .spendingScriptAddress ||
          proofItemReference.datum !== publication.datumCbor
        )
          throw new Error(
            "canonical observation changed its exact typed proof-item publication",
          );
        const semantic =
          binding.resolvedContracts.contracts.validationTraceDispute
            .canonicalDecodeItemStages.observe;
        const names = Object.entries(binding.contractEntries).filter(
          ([, entry]) =>
            entry?.scriptHash === semantic.spendingScriptHash &&
            entry.refScriptUTxO != null,
        );
        if (names.length !== 1)
          throw new Error(
            "canonical observation publication is ambiguous or missing",
          );
        const semanticReference = await reference(names[0]![0]);
        const referenceOutRefs = [proofItemReference, semanticReference]
          .map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`)
          .sort();
        const planned = createRawCommittedFieldCarriagePlan({
          sourceKind: pre.sourceKind === "forced" ? 1n : 0n,
          owner: signer.paymentKeyHash,
          nativeTxId: pre.transactionId.toString("hex"),
          fieldIndex: 2,
          preimage: fieldPreimage,
        });
        const durableBinding: ValidationTraceDisputeFieldCarriageBinding = {
          stateIndex,
          fieldIndex: 2,
          transactionId: planned.nativeTxId,
          fieldCommitment: planned.commitment,
          proofItemPublication: {
            address: proofItemReference.address,
            datumCbor: publication.datumCbor,
            outRef: `${proofItemReference.txHash}#${proofItemReference.outputIndex}`,
          },
          referenceOutRefs,
        };
        return {
          durableBinding,
          semanticReference,
          proofItemReference,
          material: {
            plan: planned.plan,
            referenceUtxos: [proofItemReference],
          },
          resolveFieldCarriage: (input: {
            readonly fieldIndex: number;
            readonly fieldPreimage: Buffer;
          }) => {
            if (
              input.fieldIndex !== 2 ||
              !input.fieldPreimage.equals(fieldPreimage)
            )
              throw new Error(
                "canonical proof-item source changed during encoding",
              );
            return resolveMidgardFieldCarriageAgainstReferenceInputs({
              plan: planned.plan,
              referenceInputs: [proofItemReference, semanticReference],
            });
          },
        };
      }
      if (!("certificate" in authenticated.requirement))
        throw new Error("validation field carriage requires a field source");
      const { planned, certificate } = authenticated.requirement;
      const witness = material!.challengerTrace.witnesses[stateIndex]!;
      const semanticIndex = validationSemanticResolverIndex(witness);
      const pre = material!.challengerTrace.states[stateIndex]!;
      const semantic =
        pre.phase === "canonicalDecode"
          ? binding.resolvedContracts.contracts.validationTraceDispute
              .canonicalDecodeItemStages.observe
          : binding.resolvedContracts.contracts.validationTraceDispute
              .semanticResolvers[
              validationSemanticResolverGlobalIndex(4, semanticIndex)
            ];
      if (semantic === undefined)
        throw new Error(
          "validation field carriage semantic contract is missing",
        );
      const names = Object.entries(binding.contractEntries).filter(
        ([, entry]) =>
          entry?.scriptHash === semantic.spendingScriptHash &&
          entry.refScriptUTxO != null,
      );
      if (names.length !== 1)
        throw new Error(
          "validation field carriage semantic publication is ambiguous or missing",
        );
      const semanticReference = await reference(names[0]![0]);
      const fieldReferences = [
        ...authenticated.publications,
        ...(authenticated.certificate === undefined
          ? []
          : [authenticated.certificate]),
      ];
      const referenceInputs = [...fieldReferences, semanticReference];
      const outRefs = [
        ...new Set(
          referenceInputs.map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`),
        ),
      ].sort();
      const durableBinding: ValidationTraceDisputeFieldCarriageBinding = {
        stateIndex,
        fieldIndex: planned.fieldIndex,
        transactionId: planned.nativeTxId,
        fieldCommitment: midgardFieldCommitment(planned.preimage).toString(
          "hex",
        ),
        certificatePolicyId: certificate.policyId,
        referenceOutRefs: outRefs,
      };
      return {
        durableBinding,
        semanticReference,
        material: {
          plan: planned.plan,
          referenceUtxos: fieldReferences,
          certificatePolicyId: certificate.policyId,
        },
        resolveFieldCarriage: (input: {
          readonly fieldIndex: number;
          readonly fieldPreimage: Buffer;
        }) => {
          if (
            input.fieldIndex !== planned.fieldIndex ||
            !input.fieldPreimage.equals(planned.preimage)
          )
            throw new Error(
              "validation field carriage source changed during one-step encoding",
            );
          return resolveMidgardFieldCarriageAgainstReferenceInputs({
            plan: planned.plan,
            referenceInputs,
            certificatePolicyId: certificate.policyId,
          });
        },
      };
    },
  });
};
