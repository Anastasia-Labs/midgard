import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  type LucidEvolution,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { planFaultProofFieldOpening } from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  type CompleteCanonicalReplayContext,
  completeCanonicalReplayPredecessorEvidence,
} from "../workflow/complete-replay.js";
import { captureCursorRemoval } from "../workflow/cursor-family-runtime.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { withFieldCarriagePrerequisite } from "../workflow/field-carriage-prerequisite.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "../workflow/orchestrator.js";
import {
  createFraudProofWorkflowRegistry,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  resolveDirectFirstProofChunks,
  withProofChunkPrerequisite,
} from "../workflow/proof-chunk-prerequisite.js";
import {
  deriveFraudProofRawL1FamilyStage,
  fraudProofRawL1SnapshotRequestForFamily,
} from "../workflow/raw-l1-family-derivation.js";
import {
  admitFraudProofRawL1Snapshot,
  type FraudProofRawL1ComputationStepRole,
  type FraudProofRawL1Utxo,
} from "../workflow/raw-l1-snapshot.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
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
  conservationDatum,
  ConservationRawDatumSchema,
  conservationStep,
  submitConservationAcceptedSource,
  submitConservationAction,
} from "./submit-union.js";
import type { ConservationPosition } from "./union-plan.js";

export type ValueConservationReferences = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  union: Readonly<Record<ConservationPosition, UTxO>>;
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
  removal: Readonly<
    Record<
      | "correctionLockSpend"
      | "stateQueueSpend"
      | "stateQueueMint"
      | "stateQueueFraudRemovalWithdraw"
      | "activeOperatorsSpend"
      | "activeOperatorsMint"
      | "retiredOperatorsSpend"
      | "retiredOperatorsMint"
      | "schedulerSpend",
      UTxO
    >
  >;
}>;
export type ManifestBoundValueConservationWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: ValueConservationReferences;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
const fromRaw = (raw: FraudProofRawL1Utxo): UTxO => {
  const [txHash, outputIndex] = raw.outRef.split("#");
  return {
    txHash: txHash!,
    outputIndex: Number(outputIndex),
    ...coreToTxOutput(CML.TransactionOutput.from_cbor_hex(raw.outputCbor)),
  };
};
const text = (input: JournalJsonObject, field: string): string => {
  const value = input[field];
  if (typeof value !== "string" || value.length === 0)
    throw new Error(`value conservation: missing ${field}`);
  return value;
};

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
    "source",
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
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
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
    return admitFraudProofRawL1Snapshot({
      value: await l1.rawL1.capture(request),
      request,
      releaseFinality: binding.releaseFinality,
      observationDepth: "inclusion",
    });
  };
  const cache = new Map<
    string,
    ReturnType<typeof admitValueConservationArtifact>
  >();
  const admit = (artifact: JournalJsonObject) => {
    const digest = journalJsonDigest(artifact);
    let admitted = cache.get(digest);
    if (admitted === undefined) {
      admitted = admitValueConservationArtifact(artifact, contracts);
      cache.set(digest, admitted);
    }
    return admitted;
  };
  const actionAt = async (
    action: FraudProofWorkflowAction,
    artifact: JournalJsonObject,
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
    transactionConfirmed: (input) => l1.transactionConfirmed(input),
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePort({
    category: "valueNotPreserved",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    maximumTransactionBytes: binding.cardanoProtocolParameters.maxTxSize,
    transactionConfirmed: (input) => l1.transactionConfirmed(input),
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
      snapshot: observed,
      definition,
      releaseEconomics: binding.releaseEconomics,
    });
    if (stage.kind === "removed")
      return { kind: "completed", terminal: stage.terminal };
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
            nextRemovalOutRef: stage.nextRemovalOutRef,
            fraudProofOutRef: stage.fraudProofOutRef,
          },
        },
      };
    const raw = observed.scopes
      .flatMap((scope) => scope.utxos)
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
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
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
        predecessor: completeCanonicalReplayPredecessorEvidence({
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
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            if (context.action.input.stage === "init") {
              await submitInit({
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
                preSubmitBoundary,
                awaitConfirmation: false,
              });
              return;
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
              const raw = (await snapshot()).scopes
                .flatMap((scope) => scope.utxos)
                .find((utxo) => utxo.outRef === threadOutRef);
              if (raw === undefined)
                throw new Error(
                  "value conservation: authenticated source thread absent",
                );
              const publishedProofChunks = await resolveDirectFirstProofChunks({
                action: context.action,
                lucid: config.lucid,
                address: config.signer.address,
                proofCbor: admitted.artifact.acceptedProofCbor!,
              });
              await submitConservationAcceptedSource({
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
                preSubmitBoundary,
                awaitConfirmation: false,
              });
              return;
            }
            const fieldIndex =
              selected.fieldIndex ??
              (selected.position === "unionOutputScan" ? 2 : undefined);
            const resolvedField = await fieldPrerequisite.resolveAuthenticated({
              headerHash: config.headerHash,
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
            await submitConservationAction({
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
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      };
    },
    confirmed: async (context) => {
      const admitted = await admit(context.artifact);
      const transaction = (await snapshot()).transactions.find(
        (tx) => tx.txHash === context.txHash,
      );
      if (transaction === undefined) return false;
      const outputs = CML.TransactionBody.from_cbor_hex(
        transaction.bodyCbor,
      ).outputs();
      const lucidOutputs = Array.from({ length: outputs.len() }, (_, index) =>
        coreToTxOutput(outputs.get(index)),
      );
      const threadUnit =
        contracts.computationThread.policyId +
        binding.definition.categoryId +
        config.headerHash;
      const proofUnit =
        contracts.fraudProof.policyId +
        binding.definition.categoryId +
        config.headerHash;
      if (context.action.input.stage === "remove") {
        if (
          !transaction.resolvedInputs.some(
            (input) => input.outRef === context.action.input.nextRemovalOutRef,
          ) ||
          !transaction.resolvedReferenceInputs.some(
            (input) => input.outRef === context.action.input.fraudProofOutRef,
          )
        )
          throw new Error(
            "value conservation: confirmed removal changed exact inputs",
          );
        return true;
      }
      if (context.action.input.stage === "init") {
        if (
          !lucidOutputs.some(
            (output) =>
              output.address === contracts.steps[0].spendingScriptAddress &&
              output.assets[threadUnit] === 1n &&
              output.datum != null &&
              Data.to(Data.from(output.datum)) ===
                conservationDatum(config.signer.paymentKeyHash, null),
          )
        )
          throw new Error(
            "value conservation: confirmed initialization differs",
          );
        return true;
      }
      const { selected } = await actionAt(context.action, context.artifact);
      const input = transaction.resolvedInputs.find(
        (input) => input.outRef === context.action.input.threadOutRef,
      );
      if (
        input === undefined ||
        fromRaw(input).address !==
          conservationStep(contracts, selected.position)
            .spendingScriptAddress ||
        fromRaw(input).assets[threadUnit] !== 1n ||
        Data.to(Data.from(fromRaw(input).datum!)) !==
          conservationDatum(config.signer.paymentKeyHash, selected.inputState)
      )
        throw new Error(
          "value conservation: confirmed input checkpoint differs",
        );
      const terminal = selected.nextPosition === null;
      const expectedAddress = terminal
        ? contracts.fraudProof.spendingScriptAddress
        : contracts[selected.nextPosition!].spendingScriptAddress;
      if (
        !lucidOutputs.some(
          (output) =>
            output.address === expectedAddress &&
            output.assets[terminal ? proofUnit : threadUnit] === 1n &&
            output.datum != null &&
            Data.to(Data.from(output.datum)) ===
              (terminal
                ? Data.to(
                    { fraud_prover: config.signer.paymentKeyHash },
                    FraudProofTokenDatum,
                  )
                : conservationDatum(
                    config.signer.paymentKeyHash,
                    selected.outputState,
                  )),
        )
      )
        throw new Error(
          "value conservation: confirmed output checkpoint differs",
        );
      if (admitted.headerHash !== config.headerHash)
        throw new Error(
          "value conservation: confirmed artifact targets another header",
        );
      return true;
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
export type ManifestBoundValueConservationWorkflow = Awaited<
  ReturnType<typeof createManifestBoundValueConservationWorkflow>
>;

export const runOrResumeManifestBoundValueConservationWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundValueConservationWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) =>
  runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY,
    ...(workflow.replayContext === undefined
      ? {}
      : { replayContext: workflow.replayContext }),
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["valueNotPreserved"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
