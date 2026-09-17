import {
  deriveMidgardNativeTxProofSource,
  encodeMidgardFieldPreimage,
} from "@al-ft/midgard-core";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { MintAuthorizationClaimEvidence } from "@al-ft/midgard-sdk";
import {
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX,
  MintAuthorizationEvaluateDatum,
  MintAuthorizationStep02ThreadDatum,
  MintAuthorizationStep03Datum,
  MintAuthorizationStep04Datum,
  MintAuthorizationStep05Datum,
  MintAuthorizationWitnessScanDatum,
} from "@al-ft/midgard-sdk";
import { type MintAuthorizationStep04State, Proof } from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Data } from "@lucid-evolution/lucid";

import {
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { parseSubmitStep01TxInclusion } from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { keyValuePhasMembershipProofs } from "../transition-trace/phas.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
import { MINT_AUTHORIZATION_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { MINT_AUTHORIZATION_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { defineFamily } from "../workflow/family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import {
  type FieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
} from "../workflow/field-carriage-prerequisite.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import { createStructuredDataPreimageRequirement } from "../workflow/raw-datum-preimage.js";
import { createRawDatumPreimageRequirement } from "../workflow/raw-datum-preimage-prerequisite.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  admitMintAuthorizationWorkflowArtifact,
  prepareMintAuthorizationWorkflowArtifact,
} from "./artifact.js";
import type { MintAuthorizationContracts } from "./contracts.js";
import { mintAuthorizationEvaluationPreimage } from "./evaluate.js";
import { buildMintAuthorizationStep02Evidence } from "./evidence.js";
import {
  requireMintAuthorizationStepState,
  requireMintAuthorizationThreadUtxo,
} from "./submit-common.js";
import { submitMintAuthorizationEvaluate } from "./submit-mint-authorization-evaluate.js";
import { submitMintAuthorizationStep01 } from "./submit-mint-authorization-step-01.js";
import { submitMintAuthorizationStep02 } from "./submit-mint-authorization-step-02.js";
import {
  submitMintAuthorizationStep03EvaluateUnsatisfied,
  submitMintAuthorizationStep03WitnessAbsence,
} from "./submit-mint-authorization-step-03.js";
import {
  submitMintAuthorizationStep04AdvanceComplete,
  submitMintAuthorizationStep04ResolveNext,
} from "./submit-mint-authorization-step-04.js";
import { submitMintAuthorizationStep05 } from "./submit-mint-authorization-step-05.js";
import { submitMintAuthorizationWitnessScan } from "./submit-mint-authorization-witness-scan.js";

export type MintAuthorizationWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfig = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"mintAuthorization">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MintAuthorizationContracts;
  references: MintAuthorizationWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

type Prepared = Awaited<
  ReturnType<typeof admitMintAuthorizationWorkflowArtifact>
>;
const witnessSet = (admitted: Prepared) => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(admitted.nativeTxCanonicalCbor, "hex"),
    ).witnessSet,
  );
  return {
    addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
  };
};
export const planMintAuthorizationWorkflowField = (
  admitted: Prepared,
  owner: string,
  stage: unknown,
) => {
  const fieldIndex =
    stage === "step_02"
      ? MIDGARD_FIELD_INDEX.mint
      : stage === "step_03"
        ? admitted.finding.direction === 0n
          ? MIDGARD_FIELD_INDEX.scriptWitnesses
          : MIDGARD_FIELD_INDEX.addressWitnesses
        : stage === "step_04"
          ? MIDGARD_FIELD_INDEX.referenceInputs
          : null;
  if (fieldIndex === null) return null;
  const items =
    fieldIndex === MIDGARD_FIELD_INDEX.mint
      ? admitted.mintItemCbors
      : fieldIndex === MIDGARD_FIELD_INDEX.scriptWitnesses
        ? admitted.scriptWitnessItemCbors
        : fieldIndex === MIDGARD_FIELD_INDEX.addressWitnesses
          ? admitted.addrWitnessItemCbors
          : admitted.referenceInputItemCbors;
  return planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex,
    anchorTxId: admitted.txInclusion.nativeTxId,
    nativeTxCompactCbor: admitted.nativeTxCompactCbor,
    itemCbors: items.map((item) => Buffer.from(item, "hex")),
    owner,
    ...(fieldIndex >= MIDGARD_FIELD_INDEX.scriptWitnesses
      ? {
          witnessSet: witnessSet(admitted),
          anchorWitnessSetHash: admitted.witnessSet,
        }
      : {}),
    label: `mint authorization field ${fieldIndex}`,
  });
};

export const mintAuthorizationWorkflowFieldRequirement = (
  admitted: Prepared,
  owner: string,
  stage: unknown,
  certificate: FieldCarriageRequirement["certificate"],
): FieldCarriageRequirement | null => {
  const planned = planMintAuthorizationWorkflowField(admitted, owner, stage);
  if (planned === null) return null;
  return {
    planned,
    compactCbor: admitted.nativeTxCompactCbor,
    witnessSetCompactCbor: deriveMidgardNativeTxProofSource(
      decodeMidgardNativeTxFullFromCanonicalCbor(
        Buffer.from(admitted.nativeTxCanonicalCbor, "hex"),
      ),
    ).witnessSetCompactCbor.toString("hex"),
    certificate,
  };
};

export const mintAuthorizationWorkflowRawRequirement = async (
  admitted: Prepared,
  stage: unknown,
) => {
  if (stage === "step_02") {
    const evidence = await buildMintAuthorizationStep02Evidence({
      reconstruction: admitted.current,
      eventKey: {
        L2TransactionEventKey: { tx_id: admitted.txInclusion.nativeTxId },
      },
    });
    const preimageHex = aikenSerialisedPlutusDataCborPreservingMapOrder(
      Data.to(
        {
          header: admitted.current.header,
          event_to_step_membership: evidence.eventToStepMembership,
          transition_step_membership: evidence.transitionStepMembership,
          policy_index: admitted.finding.policyIndex,
          direction: admitted.finding.direction,
        },
        MintAuthorizationClaimEvidence,
      ),
    );
    return preimageHex.length <= 6000 * 2
      ? null
      : createStructuredDataPreimageRequirement({ preimageHex });
  }
  if (stage !== "step_03" && stage !== "step_06" && stage !== "step_07")
    return null;
  const preimage =
    admitted.finding.direction === 0n
      ? encodeMidgardFieldPreimage(
          admitted.scriptWitnessItemCbors.map((item) =>
            Buffer.from(item, "hex"),
          ),
        )
      : mintAuthorizationEvaluationPreimage(
          Buffer.from(admitted.finding.scriptBytesHex!, "hex"),
          encodeMidgardFieldPreimage(
            admitted.addrWitnessItemCbors.map((item) =>
              Buffer.from(item, "hex"),
            ),
          ),
        );
  if (
    preimage.length === 0 ||
    preimage.length > (admitted.finding.direction === 0n ? 32_768 : 65_536)
  )
    throw new Error(
      "mint authorization preimage exceeds the canonical 32768-byte domain",
    );
  return createRawDatumPreimageRequirement({ preimage });
};

const resolveField = async ({
  config,
  plan,
}: {
  readonly config: BoundConfig;
  readonly plan: NonNullable<
    ReturnType<typeof planMintAuthorizationWorkflowField>
  >;
}) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("mint-authorization field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.binding.network,
    planned: plan,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (plan.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("mint-authorization field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

export const createMintAuthorizationTransactionPort = (
  config: BoundConfig,
  rawPrerequisite: FieldCarriagePrerequisitePort<"mintAuthorization">,
): CursorFamilyTransactionPort<"mintAuthorization"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "mintAuthorization",
  prepare: async ({ evidence, classification }) =>
    await prepareMintAuthorizationWorkflowArtifact({
      evidence,
      classification,
      replayContext: config.replayContext,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitMintAuthorizationWorkflowArtifact(artifact);
    if (admitted.current.headerHash !== config.binding.definition.headerHash)
      throw new Error("mint authorization: bound header changed");
    const input = cursorFamilyActionInput({
      category: "mintAuthorization",
      action,
    });
    const stage = input.stage;
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    if (stage === "remove")
      return await captureCursorRemoval({
        category: "mintAuthorization",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: admitted.current.headerHash,
        input,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    const planned = planMintAuthorizationWorkflowField(
      admitted,
      config.signer.paymentKeyHash,
      stage,
    );
    const carriage =
      planned === null ? null : await resolveField({ config, plan: planned });
    const rawPreimageUtxos =
      (await mintAuthorizationWorkflowRawRequirement(admitted, stage)) !== null
        ? (
            await rawPrerequisite.resolveAuthenticated({
              headerHash: admitted.current.headerHash,
              action,
              artifact,
            })
          ).publications
        : undefined;
    const common = {
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId,
      signer: config.signer,
      network: config.binding.network,
      blueprint: config.binding.blueprint,
      nativeTxCompactCbor: admitted.nativeTxCompactCbor,
      witnessSet: witnessSet(admitted),
      publishedCarriageUtxos: carriage?.publications,
      certificateUtxo: carriage?.certificate,
      awaitConfirmation: false,
    } as const;
    return {
      transaction: await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          if (stage === "init") {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudCategory: "mintAuthorization",
              fraudulentBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: admitted.current.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
            return;
          }
          const threadOutRef = cursorStringField(input, "threadOutRef");
          if (stage === "step_01") {
            await submitMintAuthorizationStep01({
              ...common,
              threadOutRef,
              stateQueueBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: parseSubmitStep01TxInclusion(admitted.txInclusion),
              referenceScriptUtxo: config.references.steps[0],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
            });
          } else if (stage === "step_02") {
            await submitMintAuthorizationStep02({
              ...common,
              threadOutRef,
              reconstruction: admitted.current,
              evidenceReferences: rawPreimageUtxos,
              policyIndex: admitted.finding.policyIndex,
              direction: admitted.finding.direction,
              mintItemCbors: admitted.mintItemCbors,
              referenceScriptUtxo: config.references.steps[1],
              preSubmitBoundary,
            });
          } else if (stage === "step_03") {
            if (admitted.finding.direction === 0n)
              await submitMintAuthorizationStep03WitnessAbsence({
                ...common,
                threadOutRef,
                scriptTxWitsItemCbors: admitted.scriptWitnessItemCbors,
                rawPreimageUtxos,
                referenceScriptUtxo: config.references.steps[2],
                preSubmitBoundary,
              });
            else {
              if (admitted.finding.scriptBytesHex === null)
                throw new Error("mint authorization: native policy absent");
              await submitMintAuthorizationStep03EvaluateUnsatisfied({
                ...common,
                threadOutRef,
                scriptBytesHex: admitted.finding.scriptBytesHex,
                rawPreimageUtxos,
                addrTxWitsItemCbors: admitted.addrWitnessItemCbors,
                referenceScriptUtxo: config.references.steps[2],
                preSubmitBoundary,
              });
            }
          } else if (stage === "step_04") {
            const { threadUtxo } = await requireMintAuthorizationThreadUtxo({
              ...common,
              stepIndex: 3,
              threadOutRef,
            });
            const state =
              requireMintAuthorizationStepState<MintAuthorizationStep04State>({
                threadUtxo,
                signer: config.signer,
                schema: MintAuthorizationStep04Datum,
                stepIndex: 3,
              });
            const reference = admitted.references[Number(state.ref_cursor)];
            const args = {
              ...common,
              threadOutRef,
              referenceInputsItemCbors: admitted.referenceInputItemCbors,
              referenceScriptUtxo: config.references.steps[3],
              preSubmitBoundary,
            };
            if (reference === undefined)
              await submitMintAuthorizationStep04AdvanceComplete(args);
            else
              await submitMintAuthorizationStep04ResolveNext({
                ...args,
                descriptorCborHex: reference.descriptorCbor,
                trie: {
                  rootHex: state.prior_ledger_root,
                  prove: async (key) => {
                    if (key.toString("hex") !== reference.keyHex)
                      throw new Error(
                        "mint authorization: reference coordinate changed",
                      );
                    return Buffer.from(
                      Data.to(
                        (
                          await keyValuePhasMembershipProofs(
                            admitted.referenceLedger,
                            [
                              {
                                key,
                                value: Buffer.from(
                                  reference.descriptorCbor,
                                  "hex",
                                ),
                              },
                            ],
                          )
                        )[0]!,
                        Proof,
                      ),
                      "hex",
                    );
                  },
                },
              });
          } else if (stage === "step_05")
            await submitMintAuthorizationStep05({
              ...common,
              threadOutRef,
              referenceScriptUtxo: config.references.steps[4],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
            });
          else if (stage === "step_06") {
            if (
              admitted.finding.scriptBytesHex === null ||
              rawPreimageUtxos === undefined
            )
              throw new Error(
                "mint authorization native evaluator omitted retained policy",
              );
            await submitMintAuthorizationEvaluate({
              ...common,
              threadOutRef,
              scriptBytesHex: admitted.finding.scriptBytesHex,
              addrWitnessItemCbors: admitted.addrWitnessItemCbors,
              rawPreimageUtxos,
              referenceScriptUtxo: config.references.steps[5],
              preSubmitBoundary,
            });
          } else if (stage === "step_07") {
            if (rawPreimageUtxos === undefined)
              throw new Error("mint witness scan omitted retained field");
            await submitMintAuthorizationWitnessScan({
              ...common,
              threadOutRef,
              fieldBytesHex: encodeMidgardFieldPreimage(
                admitted.scriptWitnessItemCbors.map((item) =>
                  Buffer.from(item, "hex"),
                ),
              ).toString("hex"),
              rawPreimageUtxos,
              referenceScriptUtxo: config.references.steps[6],
              preSubmitBoundary,
            });
          } else
            throw new Error(`mint authorization: unsupported stage ${stage}`);
        },
      ),
    };
  },
});

export type ManifestBoundMintAuthorizationWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MintAuthorizationWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMintAuthorizationWorkflow = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"mintAuthorization">;
  l1: FraudProofFamilyL1ObservationPort<"mintAuthorization">;
  transactions: CursorFamilyTransactionPort<"mintAuthorization">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const MINT_AUTHORIZATION_FAMILY_DEFINITION = defineFamily({
  category: "mintAuthorization",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    MintAuthorizationStep02ThreadDatum,
    MintAuthorizationStep03Datum,
    MintAuthorizationStep04Datum,
    MintAuthorizationStep05Datum,
    MintAuthorizationEvaluateDatum,
    MintAuthorizationWitnessScanDatum,
  ],
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
    "chunkedVerifyWithdraw",
    "pexcludesWithdraw",
  ],
  fieldPreimageCertificate: true,
  replayer: () => MINT_AUTHORIZATION_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: MINT_AUTHORIZATION_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofMintAuthorization",
      "fraudProofMintAuthorizationStep02",
      "fraudProofMintAuthorizationStep03",
      "fraudProofMintAuthorizationStep04",
      "fraudProofMintAuthorizationStep05",
      "fraudProofMintAuthorizationStep06",
      "fraudProofMintAuthorizationStep07",
    ],
    transactionPort: (context) => {
      const { binding, certificate } = context;
      const chain = binding.resolvedContracts.contracts.mintAuthorization;
      const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
      if (
        chain === undefined ||
        stateQueuePolicyId === undefined ||
        certificate === null
      ) {
        throw new Error(
          "mint-authorization manifest omitted required contracts",
        );
      }

      const contracts: MintAuthorizationContracts = Object.freeze({
        steps: chain.steps,
        computationThread:
          binding.resolvedContracts.contracts.computationThread,
        fraudProof: {
          policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
          mintingScript:
            binding.resolvedContracts.contracts.fraudProof.mintingScript,
          spendingScriptAddress:
            binding.resolvedContracts.contracts.fraudProof
              .spendingScriptAddress,
        },
        hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
        stateQueuePolicyId,
        fieldPreimageCertificatePolicyId: certificate.policyId,
      });

      return createMintAuthorizationTransactionPort(
        { ...context, contracts },
        context.fieldCarriagePrerequisites[1]!,
      );
    },
  },
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        const { certificate, references } = context;

        if (
          typeof action.input.stage !== "string" ||
          !["step_02", "step_03", "step_04"].includes(action.input.stage)
        )
          return null;
        const input = cursorFamilyActionInput({
          category: "mintAuthorization",
          action,
        });
        const admitted = await admitMintAuthorizationWorkflowArtifact(artifact);
        return mintAuthorizationWorkflowFieldRequirement(
          admitted,
          context.signer.paymentKeyHash,
          input.stage,
          {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: references.fieldPreimageCertificateMint,
          },
        );
      },
    },
    {
      rawDatum: true,
      requirementForAction: async (_context, { action, artifact }) => {
        if (
          typeof action.input.stage !== "string" ||
          !["step_02", "step_03", "step_06", "step_07"].includes(
            action.input.stage,
          )
        )
          return null;
        return mintAuthorizationWorkflowRawRequirement(
          await admitMintAuthorizationWorkflowArtifact(artifact),
          action.input.stage,
        );
      },
    },
  ],
  proofChunk: async (_context, { action, artifact }) => {
    if (action.input.stage !== "step_01") return null;
    const admitted = await admitMintAuthorizationWorkflowArtifact(artifact);
    return admitted.txInclusion.txMembershipProofCbor ?? null;
  },
});

export const createManifestBoundMintAuthorizationWorkflow = (
  config: ManifestBoundMintAuthorizationWorkflowConfig,
): Promise<ManifestBoundMintAuthorizationWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    MINT_AUTHORIZATION_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundMintAuthorizationWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMintAuthorizationWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> =>
  await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: MINT_AUTHORIZATION_COMPLETE_CANONICAL_REPLAY,
    replayContext: workflow.replayContext,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["mintAuthorization"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
