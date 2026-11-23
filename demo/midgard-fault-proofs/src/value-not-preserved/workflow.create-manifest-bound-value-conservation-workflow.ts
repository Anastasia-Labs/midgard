import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { Data, type UTxO, validatorToScriptHash } from "@lucid-evolution/lucid";

import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import { buildInit } from "../submit-init.submit-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { completeCanonicalReplayPredecessorStoreLedger } from "../workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { captureCursorRemoval } from "../workflow/cursor-family-runtime.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { withFieldCarriagePrerequisite } from "../workflow/field-carriage-prerequisite.js";
import {
  workflowJsonDigest,
  type WorkflowJsonObject,
} from "../workflow/identity.js";
import { nextViewOf } from "../workflow/next-state.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "../workflow/orchestrator.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  withProofChunkPrerequisite,
} from "../workflow/proof-chunk-prerequisite.js";
import { resolveDirectFirstProofChunks } from "../workflow/proof-chunk-prerequisite.route-action-identity.js";
import { workflowProofMaterialsHolder } from "../workflow/proof-materials-view.holder.js";
import {
  deriveFraudProofRawL1FamilyStage,
  fraudProofRawL1SnapshotRequestForFamily,
} from "../workflow/raw-l1-family-derivation.js";
import type { FraudProofRawL1ComputationStepRole } from "../workflow/raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";
import { admitFraudProofRawL1Snapshot } from "../workflow/raw-l1-snapshot.js";
import { workflowStepTransaction } from "../workflow/step-transaction.js";
import { createValueConservationAdapter } from "./adapter.js";
import { admitValueConservationArtifact } from "./artifact.js";
import {
  CONSERVATION_POSITIONS,
  conservationManifestName,
  type ValueNotPreservedContracts,
} from "./contracts.js";
import { createValueConservationFieldPrerequisite } from "./field-prerequisite.js";
import {
  prepareValueConservationArtifact,
  VALUE_NOT_PRESERVED_REJECTION_VIOLATION,
  VALUE_NOT_PRESERVED_VIOLATION,
} from "./replay.js";
import {
  ValueNotPreservedStep02Datum,
  ValueNotPreservedStep03Datum,
  ValueNotPreservedStep04Datum,
} from "./schemas.js";
import {
  buildConservationAcceptedSource,
  buildConservationAction,
  conservationDatum,
  ConservationRawDatumSchema,
  conservationStep,
} from "./submit-union.js";
import {
  fromRaw,
  type ManifestBoundValueConservationWorkflowConfig,
  text,
} from "./workflow.value-conservation-references.js";
/** Fixed manifest, raw-L1 and retained-DA authority; runtime config admits no callbacks. */
export const createManifestBoundValueConservationWorkflow = async (
  config: ManifestBoundValueConservationWorkflowConfig,
) => {
  const allowed = [
    "manifest",
    "blueprintJson",
    "deploymentInfo",
    "headerHash",
    "lucid",
    "signer",
    "referenceScripts",
    "l1Source",
    "stateQueueMutationLeaseCoordinator",
    ...(config.replayContext === undefined ? [] : ["replayContext"]),
  ];
  if (Object.keys(config).sort().join() !== allowed.sort().join())
    throw new Error(
      "value conservation: production configuration contains callback authority",
    );
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "valueNotPreserved",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      ConservationRawDatumSchema as never,
      ValueNotPreservedStep02Datum,
      ValueNotPreservedStep03Datum,
      ValueNotPreservedStep04Datum,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    proofMaterialsAddress: config.signer.proofMaterialsAddress,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const resolved = binding.resolvedContracts;
  const chain = resolved.contracts.valueNotPreserved;
  if (chain === undefined || resolved.stateQueuePolicyId === undefined)
    throw new Error(
      "value conservation: deployment omitted required contracts",
    );
  const certificateReference = requireManifestBoundReferenceScriptUtxo({
    binding,
    contractName: "fieldPreimageCertificateMint",
    utxo: config.referenceScripts.fieldPreimageCertificateMint,
  });
  const contracts: ValueNotPreservedContracts = {
    ...chain,
    computationThread: resolved.contracts.computationThread,
    fraudProof: resolved.contracts.fraudProof,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: validatorToScriptHash(
      certificateReference.scriptRef!,
    ),
  };
  const bind = (contractName: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({ binding, contractName, utxo });
  const legacyNames = [
    "fraudProofValueNotPreserved",
    "fraudProofValueNotPreservedStep02",
    "fraudProofValueNotPreservedStep03",
    "fraudProofValueNotPreservedStep04",
  ];
  for (const [index, name] of legacyNames.entries())
    bind(name, config.referenceScripts.steps[index]!);
  for (const position of CONSERVATION_POSITIONS)
    bind(
      conservationManifestName(position),
      config.referenceScripts.union[position],
    );
  for (const [name, utxo] of Object.entries(config.referenceScripts.removal))
    bind(name, utxo);
  const witnessNames = {
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
    pexcludesWithdraw: "pexcludesWithdraw",
  };
  for (const [name, manifestName] of Object.entries(witnessNames))
    bind(
      manifestName,
      config.referenceScripts.witnesses[
        name as keyof FaultProofWitnessReferenceScripts
      ],
    );
  const certificate = {
    policyId: contracts.fieldPreimageCertificatePolicyId,
    mintingScript: certificateReference.scriptRef!,
    referenceScriptUtxo: certificateReference,
  };
  const definition = {
    ...binding.definition,
    computationThread: {
      ...binding.definition.computationThread,
      steps: [
        ...binding.definition.computationThread.steps,
        ...CONSERVATION_POSITIONS.map((position, index) => ({
          role: `computation_thread_step_${(index + 5).toString().padStart(2, "0")}` as FraudProofRawL1ComputationStepRole,
          address: contracts[position].spendingScriptAddress,
          datumSchema: ConservationRawDatumSchema as never,
        })),
      ],
    },
  };
  const l1 = createFraudProofFamilyL1ObservationPort({
    l1: config.l1Source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition,
  });
  const request = fraudProofRawL1SnapshotRequestForFamily({
    definition,
    releaseFinality: binding.releaseFinality,
  });
  const snapshot = async () => {
    if (l1.rawL1 === undefined)
      throw new Error("value conservation: raw L1 authority absent");
    const { snapshot: value, pending } = await l1.rawL1.capture(request);
    return {
      snapshot: admitFraudProofRawL1Snapshot({
        value,
        request,
        releaseFinality: binding.releaseFinality,
        observationDepth: "inclusion",
      }),
      pending,
    };
  };
  const cache = new Map<
    string,
    ReturnType<typeof admitValueConservationArtifact>
  >();
  const admit = (artifact: WorkflowJsonObject) => {
    const digest = workflowJsonDigest(artifact);
    let admitted = cache.get(digest);
    if (admitted === undefined) {
      admitted = admitValueConservationArtifact(artifact, contracts);
      cache.set(digest, admitted);
    }
    return admitted;
  };
  const actionAt = async (
    action: FraudProofWorkflowAction,
    artifact: WorkflowJsonObject,
  ) => {
    const admitted = await admit(artifact);
    const index = action.input.index;
    if (
      typeof index !== "number" ||
      !Number.isSafeInteger(index) ||
      index < 0 ||
      admitted.actions[index] === undefined
    )
      throw new Error(
        "value conservation: action cursor is outside authenticated replay",
      );
    return { admitted, selected: admitted.actions[index]!, index };
  };
  const fieldPrerequisite = createValueConservationFieldPrerequisite({
    contracts,
    certificate,
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePort({
    category: "valueNotPreserved",
    lucid: config.lucid,
    signer: config.signer,
    publications: l1.publications,
    maximumTransactionBytes: binding.cardanoProtocolParameters.maxTxSize,
    proofCborForAction: async ({ action, artifact }) => {
      if (action.input.stage !== "fold") return null;
      const { admitted, selected } = await actionAt(action, artifact);
      return selected.position === "unionAcceptedSource"
        ? admitted.artifact.acceptedProofCbor
        : null;
    },
  });
  const current = async (
    context: Parameters<FraudProofFamilyWorkflowAdapter["observe"]>[0],
  ): Promise<FraudProofWorkflowObservation> => {
    const admitted = await admit(context.artifact);
    if (
      context.identity.category !== "valueNotPreserved" ||
      context.identity.target.kind !== "state_queue_header" ||
      context.identity.target.headerHash !== config.headerHash ||
      admitted.headerHash !== config.headerHash
    )
      throw new Error("value conservation: workflow identity changed");
    // The pending fold may be included between captures. Its stage and datum
    // must therefore come from the same authenticated chain boundary.
    const observed = await snapshot();
    const stage = await deriveFraudProofRawL1FamilyStage({
      ...observed,
      definition,
      releaseEconomics: binding.releaseEconomics,
    });
    if (stage.kind === "removed")
      return { kind: "completed", terminal: stage.terminal };
    if (stage.kind === "pending")
      return { kind: "pending", reason: stage.reason };
    if (stage.kind === "not_started")
      return {
        kind: "action_required",
        action: {
          actionId: "init",
          input: {
            stage: "init",
            stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
          },
        },
      };
    if (stage.kind === "proof_token")
      return {
        kind: "action_required",
        action: {
          actionId: `remove:${stage.nextRemovalOutRef}`,
          input: {
            stage: "remove",
            stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
            nextRemovalOutRef: stage.nextRemovalOutRef,
            fraudProofOutRef: stage.fraudProofOutRef,
          },
        },
      };
    const raw = nextViewOf(observed)
      .scopes.flatMap((scope) => scope.utxos)
      .find((utxo) => utxo.outRef === stage.threadOutRef);
    if (raw === undefined)
      throw new Error(
        "value conservation: live thread omitted from raw L1 snapshot",
      );
    const utxo = fromRaw(raw);
    const matches = admitted.actions.flatMap((action, index) =>
      conservationStep(contracts, action.position).spendingScriptAddress ===
        utxo.address &&
      conservationDatum(config.signer.paymentKeyHash, action.inputState) ===
        Data.to(Data.from(utxo.datum!))
        ? [index]
        : [],
    );
    if (matches.length !== 1)
      throw new Error(
        "value conservation: live checkpoint absent or ambiguous in authenticated replay",
      );
    return {
      kind: "action_required",
      action: {
        actionId: `fold:${matches[0]}:${stage.threadOutRef}`,
        input: {
          stage: "fold",
          index: matches[0]!,
          threadOutRef: stage.threadOutRef,
          stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
        },
      },
    };
  };
  let adapter = createValueConservationAdapter({
    prepare: async ({ evidence, replayContext, classification }) => {
      const forced =
        classification.selected.violationId ===
        VALUE_NOT_PRESERVED_REJECTION_VIOLATION;
      const prefix = forced
        ? VALUE_NOT_PRESERVED_REJECTION_VIOLATION
        : VALUE_NOT_PRESERVED_VIOLATION;
      if (
        classification.category !== "valueNotPreserved" ||
        !classification.selected.detectionId.startsWith(`${prefix}:`)
      )
        throw new Error("value conservation: selected classification changed");
      const sourceIndex = Number(
        classification.selected.detectionId.slice(prefix.length + 1),
      );
      if (
        !Number.isSafeInteger(sourceIndex) ||
        sourceIndex < 0 ||
        `${prefix}:${sourceIndex}` !== classification.selected.detectionId
      )
        throw new Error("value conservation: noncanonical source coordinate");
      const artifact = await prepareValueConservationArtifact({
        block: evidence,
        predecessor: await completeCanonicalReplayPredecessorStoreLedger({
          evidence,
          context: replayContext,
        }),
        sourceIndex,
        forced,
      });
      if (artifact === null)
        throw new Error(
          "value conservation: classification has no authenticated contradiction",
        );
      await admit(artifact);
      return artifact;
    },
    current,
    capture: async (context) => {
      const admitted = await admit(context.artifact);
      if (context.action.input.stage === "remove")
        return await captureCursorRemoval({
          category: "valueNotPreserved",
          lucid: config.lucid,
          blueprint: binding.blueprint,
          deploymentInfo: binding.deploymentInfo,
          network: binding.network,
          signer: config.signer,
          headerHash: config.headerHash,
          input: { ...context.action.input, stage: "remove" },
          stateQueueMutationLeaseCoordinator:
            config.stateQueueMutationLeaseCoordinator,
          fraudProverRewardLovelace: BigInt(
            binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
        });
      return {
        transaction: (
          await workflowStepTransaction(
            await (async () => {
              if (context.action.input.stage === "init") {
                return await buildInit({
                  lucid: config.lucid,
                  blueprint: binding.blueprint,
                  deploymentInfo: binding.deploymentInfo,
                  network: binding.network,
                  signer: config.signer,
                  fraudCategory: "valueNotPreserved",
                  fraudulentBlockOutRef: text(
                    context.action.input,
                    "stateQueueBlockOutRef",
                  ),
                  fraudulentHeaderHash: config.headerHash,
                  witnessReferenceScripts: config.referenceScripts.witnesses,
                });
              }
              const { selected } = await actionAt(
                context.action,
                context.artifact,
              );
              const threadOutRef = text(context.action.input, "threadOutRef");
              const referenceScriptUtxo =
                selected.position === "entry"
                  ? config.referenceScripts.steps[0]
                  : config.referenceScripts.union[selected.position];
              if (selected.position === "unionAcceptedSource") {
                if (admitted.accepted === undefined)
                  throw new Error("value conservation: accepted source absent");
                const raw = nextViewOf(await snapshot())
                  .scopes.flatMap((scope) => scope.utxos)
                  .find((utxo) => utxo.outRef === threadOutRef);
                if (raw === undefined)
                  throw new Error(
                    "value conservation: authenticated source thread absent",
                  );
                const publishedProofChunks =
                  await resolveDirectFirstProofChunks({
                    action: context.action,
                    address: config.signer.proofMaterialsAddress,
                    proofCbor: admitted.artifact.acceptedProofCbor!,
                  });
                return await buildConservationAcceptedSource({
                  lucid: config.lucid,
                  blueprint: binding.blueprint,
                  network: binding.network,
                  contracts,
                  source: admitted.source,
                  signer: config.signer,
                  threadUtxo: fromRaw(raw),
                  threadToken: {
                    unit:
                      contracts.computationThread.policyId +
                      binding.definition.categoryId +
                      config.headerHash,
                    fraudulentHeaderHash: config.headerHash,
                  },
                  stateQueueBlockOutRef: text(
                    context.action.input,
                    "stateQueueBlockOutRef",
                  ),
                  txInclusion: admitted.accepted,
                  publishedProofChunks,
                  referenceScriptUtxo,
                  witnessReferenceScripts: config.referenceScripts.witnesses,
                });
              }
              const fieldIndex =
                selected.fieldIndex ??
                (selected.position === "unionOutputScan" ? 2 : undefined);
              const resolvedField =
                await fieldPrerequisite.resolveAuthenticated({
                  headerHash: config.headerHash,
                  holder: workflowProofMaterialsHolder(context),
                  action: context.action,
                  artifact: context.artifact,
                });
              const field =
                fieldIndex === undefined
                  ? undefined
                  : {
                      planned: planFaultProofFieldOpening({
                        anchorSourceKind:
                          admitted.source.claim === "ForcedConservation"
                            ? 1n
                            : 0n,
                        fieldIndex,
                        anchorTxId: admitted.source.transaction_id,
                        nativeTxCompactCbor: admitted.nativeTxCompactCbor,
                        itemCbors: decodeMidgardFieldPreimage(
                          Buffer.from(admitted.fields[fieldIndex], "hex"),
                        ),
                        owner: config.signer.paymentKeyHash,
                        publish: true,
                        label: "value conservation",
                      }),
                      carriageUtxos: resolvedField.publications,
                      ...(resolvedField.certificate === undefined
                        ? {}
                        : { certificateUtxo: resolvedField.certificate }),
                    };
              return await buildConservationAction({
                lucid: config.lucid,
                contracts,
                categoryId: binding.definition.categoryId,
                headerHash: config.headerHash,
                signer: config.signer,
                threadOutRef,
                action: selected,
                referenceScriptUtxo,
                ...(field === undefined ? {} : { field }),
                ...(selected.position === "unionOutputScan"
                  ? { chunkUtxos: resolvedField.publications }
                  : {}),
                witnessReferenceScripts: config.referenceScripts.witnesses,
              });
            })(),
            `value-not-preserved ${context.action.actionId}`,
          )
        ).transaction,
      };
    },
  });
  adapter = withFieldCarriagePrerequisite({
    category: "valueNotPreserved",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  adapter = withProofChunkPrerequisite({
    category: "valueNotPreserved",
    base: adapter,
    prerequisite: proofPrerequisite,
  });
  return {
    binding,
    contracts,
    l1,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
    ...(config.replayContext === undefined
      ? {}
      : { replayContext: config.replayContext }),
  };
};
