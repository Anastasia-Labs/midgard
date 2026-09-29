import { decodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core";
import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  AuthenticatedCanonicalDecodeItemDatum,
  buildUnsignedValidationProofItemPublicationProgram,
  CEK_CONTEXT_STAGE_REFERENCES,
  CEK_CORE_STAGE_REFERENCES,
  CEK_MATERIAL_TASK_YIELD_ROLES,
  CEK_SELECTION_YIELD_ROLES,
  type CekContextStages,
  type CekCoreStages,
  CekMaterialTraversalDatum,
  deriveCekProgramMaterialPublications,
  deriveCekSelectionFacts,
  deriveCekSinglePublication,
  deriveValidationProofItemPublication,
  deriveValidationTraceDeploymentId,
  ObservedCanonicalDecodeItemDatum,
  PreparedCanonicalDecodeItemDatum,
  PreparedValidationResolutionState,
  resolveMidgardFieldCarriageAgainstReferenceInputs,
  sharedRedeemerItemReferenceScripts,
  type ValidationCekMaterialRoute,
  ValidationProofItemDatum,
  VerifiedCanonicalDecodeItemDatum,
  WinningValidationResolutionDatum,
} from "@al-ft/midgard-sdk";
import {
  Constr,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  type TxSigned,
  type UTxO,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type ContractDeploymentInfo } from "../../inspect-contracts.js";
import {
  deriveLedgerOutputProofFinalizePlan,
  deriveLedgerOutputProofStepPlan,
} from "../../ledger-output-proof-plan.js";
import { type RedeemerItemStageKey } from "../../redeemer-item-plan.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
  resolveValidationTraceDisputeDeploymentContracts,
} from "../../runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../../step-support.js";
import { witnessSpendingValidatorCarriage } from "../../witness-reference-scripts.js";
import { type FraudProofPreSubmitBoundary } from "../../workflow/transaction-boundary.js";
import {
  deriveCekContextPlan,
  submitCekContextChain,
} from ".././cek-context.js";
import { deriveCekCorePlan, submitCekCoreChain } from ".././cek-core.js";
import {
  initialCekMaterialTraversal,
  submitCekMaterialTraversal,
} from ".././cek-material-traversal.js";
import {
  LEDGER_OUTPUT_DESCRIPTOR_YIELD_ROLES,
  LEDGER_OUTPUT_PROOF_ATTESTATION_YIELD_ROLES,
  LEDGER_OUTPUT_PROOF_STAGE_YIELD_ROLES,
} from ".././ledger-output-proof-yields.js";
import { scriptSourcesDescriptorClaim } from ".././script-sources-descriptor.js";
import {
  SCRIPT_SOURCES_DESCRIPTOR_YIELD_ROLE,
  SCRIPT_SOURCES_MIDDLE_YIELD_ROLES,
  SCRIPT_SOURCES_OBSERVER_YIELD_ROLES,
  scriptSourcesMiddleYieldIndex,
} from ".././script-sources-yields.js";
import {
  deriveCanonicalDecodeItemStageData,
  errorMessage,
  isDeterministicLocalCekFitFailure,
  requireConfirmedCekMaterialReferenceUtxo,
  type SubmitValidationDisputeSemanticResolutionResult,
  type ValidationCekProgramMaterialReferenceOutRefs,
  type ValidationCekRejectedLocalRouteAttempt,
  type ValidationCekSelectedRoute,
  type ValidationDisputeStageReferenceScriptUtxos,
} from "./cek-route.js";
import { resolveCekContextItemReferences } from "./cek-session.js";
import {
  midgardFieldCarriageFromData,
  midgardFieldCarriageToData,
  type PlutusDataValue,
  type ValidationFieldCarriageMaterial,
  type ValidationOneStepSubmissionArgument,
} from "./evidence.js";
import { type ContinueLayout } from "./redeemers.js";
import {
  deriveScriptSourcesItemSubmissionPlan,
  hasValidationAuxiliaryShape,
  requirePublishedValidationSemanticReferenceScriptUtxo,
  requireStagedOneStepArgument,
  requireValidationCekSemanticReferenceScriptUtxo,
  requireValidationDisputeReferenceScript,
  requireValidationItemObserveReferenceScriptUtxo,
  requireValidationItemSemanticReferenceScriptUtxo,
  requireValidationValueAndMintSemanticReferenceScriptUtxo,
  scriptSourcesItemResumeIndex,
  VALIDATION_AUXILIARY_SHAPES,
  VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX,
  validationCekSemanticReferenceScriptDeploymentEntry,
  validationPhaseASemanticReferenceScriptDeploymentEntry,
  validationResolveInputsSemanticReferenceScriptDeploymentEntry,
  validationScriptSourcesSemanticReferenceScriptDeploymentEntry,
  validationValueAndMintSemanticReferenceScriptDeploymentEntry,
} from "./reference-scripts.js";
import {
  requirePreparedResolutionDatum,
  validationResolverIndex,
} from "./resolution.js";
import {
  isLedgerOutputProofFinalizeResolver,
  isLedgerOutputProofStepResolver,
  makeIndexedValidationStageRedeemer,
  makeSemanticResolutionRedeemer,
  type SemanticResolutionLayout,
} from "./semantic-redeemers.js";
import {
  findUniqueInlineDatumOutputIndex,
  projectSignedL1ProofTransactionBytes,
  requireL1ProofEnvelope,
  resolveValidationProofItemDeliveryRoute,
  threadAssets,
  ValidationInlineDeliveryEnvelopeRefusedError,
  type ValidationProofItemDelivery,
} from "./transaction-material.js";
import {
  MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES,
  reachOptionalPreSubmitBoundary,
  refreshExpiredValidationDisputeValidityRange,
  requireValidityRange,
  type ValidationDisputeValidityRange,
  validationDisputeValidityRange,
} from "./validity.js";

export const submitValidationDisputeSemanticResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  oneStepArgument,
  scriptSourcesItemPreparedCbor,
  proofItemReferenceOutRef,
  proofItemDelivery,
  carriageMaterial,
  phaseANativeItemYieldKind,
  cekProgramMaterialReferenceOutRefs,
  cekMaterialTraversalBatchSize,
  referenceScriptUtxo,
  stageReferenceScriptUtxos,
  validityRange,
  awaitConfirmation = true,
  preSubmitBoundary,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly oneStepArgument: ValidationOneStepSubmissionArgument;
  /** Exact retained preparation for resuming a shared ScriptSources item checkpoint. */
  readonly scriptSourcesItemPreparedCbor?: string;
  /**
   * Already-confirmed CEK program-material publications for the
   * execution-selection route ladder (resolver 11, semantic resolver 1):
   * consulted only after the direct-proof route is refused for size.
   */
  readonly cekProgramMaterialReferenceOutRefs?: ValidationCekProgramMaterialReferenceOutRefs;
  /** Optional bounded submission batches; each batch reconstructs the next checkpoint from retained evidence. */
  readonly cekMaterialTraversalBatchSize?: number;
  readonly proofItemReferenceOutRef?: string;
  /**
   * Tier-1 complete-item delivery preference (#621): "inline" carries the
   * §5.1 preimage in the observe redeemer, "reference" routes it through a
   * §8 proof-item publication. Omitted, the builder routes by
   * {@link selectValidationCompleteItemCarriage}'s measured cost heuristic
   * (a supplied `proofItemReferenceOutRef` implies "reference"). A
   * preference steers cost, never liveness: an inline build over the L1
   * envelope is refused pre-sign and falls back to the reference route.
   */
  readonly proofItemDelivery?: ValidationProofItemDelivery;
  /** Required when the staged carriage is tier 2 or tier 3 (#600). */
  readonly carriageMaterial?: ValidationFieldCarriageMaterial;
  /** Structural item branch; its language tag is authenticated by the rewarding validator. */
  readonly phaseANativeItemYieldKind?: "native" | "foreign";
  /** Explicit semantic-resolver reference; otherwise resolved from deployment info. */
  readonly referenceScriptUtxo?: UTxO;
  /** Published scripts for any multi-stage semantic route selected. */
  readonly stageReferenceScriptUtxos?: ValidationDisputeStageReferenceScriptUtxos;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
  /**
   * Optional Q51 pre-submit boundary (workflow ruling R5). Invoked once per
   * transaction the resolution route submits, immediately before each
   * provider submission, including staged multi-transaction routes and the
   * proof-item publication.
   */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitValidationDisputeSemanticResolutionResult> => {
  const {
    deploymentInfo: parsedDeploymentInfo,
    referenceScriptAuthPolicyId,
    validationTraceDisputeCategory,
    fraudProofCataloguePolicyId,
    contracts,
  } = await resolveValidationTraceDisputeDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
  });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "prepared validation semantic-resolver UTxO",
  });
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  if (
    scriptSourcesItemPreparedCbor !== undefined &&
    (oneStepArgument.resolverIndex !== 8 ||
      oneStepArgument.semanticResolverIndex !== 28)
  )
    throw new Error(
      "Retained ScriptSources item preparation is only valid for its shared item route",
    );
  const inputDatum = requirePreparedResolutionDatum(
    scriptSourcesItemPreparedCbor === undefined
      ? threadUtxo
      : { ...threadUtxo, datum: scriptSourcesItemPreparedCbor },
  );
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation semantic resolution requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const resolverIndex = validationResolverIndex(
    inputDatum.data.resolution.pre_state.phase,
  );
  if (resolverIndex !== oneStepArgument.resolverIndex) {
    throw new Error(
      "Validation one-step argument does not match the prepared phase resolver",
    );
  }
  const staged = requireStagedOneStepArgument(oneStepArgument);
  if (staged.evidenceHash !== inputDatum.data.evidence_hash) {
    throw new Error(
      "Validation one-step argument does not match the prepared evidence hash",
    );
  }
  const semanticContract =
    contracts.validationTraceDispute.semanticResolvers[
      staged.semanticResolverGlobalIndex
    ];
  if (semanticContract === undefined) {
    throw new Error("Validation semantic resolver deployment is incomplete");
  }
  if (
    scriptSourcesItemPreparedCbor === undefined &&
    threadUtxo.address !== semanticContract.spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at semantic resolver ${staged.semanticResolverGlobalIndex.toString()}`,
    );
  }
  // The CEK execution-selection, context-step and core-step semantic bodies
  // can never fit the L1 proof envelope, so their resolutions consume the
  // published reference script instead of attaching the validator. Resolved
  // up front so a missing deployment entry fails fast.
  const cekSemanticReferenceScriptUtxo =
    referenceScriptUtxo === undefined &&
    resolverIndex === 11 &&
    validationCekSemanticReferenceScriptDeploymentEntry(
      staged.semanticResolverIndex,
    ) !== undefined
      ? await requireValidationCekSemanticReferenceScriptUtxo({
          lucid,
          deploymentInfo: parsedDeploymentInfo,
          semanticResolverIndex: staged.semanticResolverIndex,
          expectedScriptHash: semanticContract.spendingScriptHash,
        })
      : undefined;
  // #634. The ValueAndMint semantics get the same reference-script deployment
  // role, but their route is chosen by what the deployment info carries rather
  // than by a frozen sub-roster: eight of the eleven applied bodies are over
  // the envelope today and three are not, and which is which moves with every
  // regeneration. So a published entry is consumed by reference, an absent one
  // attaches inline as before — except when the applied body alone already
  // exceeds the envelope, where no redeemer can make the transaction fit and
  // the honest failure is a precise "publish it" instead of Lucid's
  // "Max transaction size of 16384 exceeded" from deep inside `complete()`.
  const valueAndMintSemanticReferenceEntryName =
    resolverIndex === VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX
      ? validationValueAndMintSemanticReferenceScriptDeploymentEntry(
          staged.semanticResolverIndex,
        )
      : undefined;
  if (
    referenceScriptUtxo === undefined &&
    valueAndMintSemanticReferenceEntryName !== undefined &&
    parsedDeploymentInfo[valueAndMintSemanticReferenceEntryName] ===
      undefined &&
    semanticContract.spendingScript.script.length / 2 >
      MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES
  ) {
    throw new Error(
      `Applied ValueAndMint semantic resolver ${staged.semanticResolverIndex.toString()} is ${(
        semanticContract.spendingScript.script.length / 2
      ).toString()} bytes and cannot ride inline inside the ${MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES.toString()}-byte L1 proof envelope; publish it as "${valueAndMintSemanticReferenceEntryName}" and regenerate deployment info before submitting this semantic resolution`,
    );
  }
  const valueAndMintSemanticReferenceScriptUtxo =
    referenceScriptUtxo === undefined &&
    valueAndMintSemanticReferenceEntryName !== undefined &&
    parsedDeploymentInfo[valueAndMintSemanticReferenceEntryName] !== undefined
      ? await requireValidationValueAndMintSemanticReferenceScriptUtxo({
          lucid,
          deploymentInfo: parsedDeploymentInfo,
          semanticResolverIndex: staged.semanticResolverIndex,
          expectedScriptHash: semanticContract.spendingScriptHash,
        })
      : undefined;
  // At most one of the two can be set: the two rosters are keyed by disjoint
  // resolver indices (CEK 11, ValueAndMint 12).
  const publishedSemanticEntryName =
    validationResolveInputsSemanticReferenceScriptDeploymentEntry(
      resolverIndex,
      staged.semanticResolverIndex,
    ) ??
    validationPhaseASemanticReferenceScriptDeploymentEntry(
      resolverIndex,
      staged.semanticResolverIndex,
    ) ??
    validationScriptSourcesSemanticReferenceScriptDeploymentEntry(
      resolverIndex,
      staged.semanticResolverIndex,
    );
  const publishedSemanticReferenceScriptUtxo =
    referenceScriptUtxo === undefined &&
    publishedSemanticEntryName !== undefined &&
    parsedDeploymentInfo[publishedSemanticEntryName] !== undefined
      ? await requirePublishedValidationSemanticReferenceScriptUtxo({
          lucid,
          deploymentInfo: parsedDeploymentInfo,
          entryName: publishedSemanticEntryName,
          expectedScriptHash: semanticContract.spendingScriptHash,
        })
      : undefined;
  if (
    referenceScriptUtxo === undefined &&
    publishedSemanticEntryName !== undefined &&
    publishedSemanticReferenceScriptUtxo === undefined &&
    semanticContract.spendingScript.script.length / 2 >
      MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES
  )
    throw new Error(
      `Publish the validation semantic resolver as "${publishedSemanticEntryName}" before submitting`,
    );
  const semanticValidatorReferenceScriptUtxo =
    referenceScriptUtxo ??
    cekSemanticReferenceScriptUtxo ??
    valueAndMintSemanticReferenceScriptUtxo ??
    publishedSemanticReferenceScriptUtxo;
  const assetFoldYield =
    resolverIndex === 12 && [3, 6, 8].includes(staged.semanticResolverIndex)
      ? contracts.validationTraceDispute.yields.valueAndMintAssetFold
      : undefined;
  let assetFoldYieldReferenceUtxo: UTxO | undefined;
  if (assetFoldYield !== undefined) {
    const entry =
      parsedDeploymentInfo.validationTraceDisputeValueAndMintAssetFoldWithdraw;
    if (entry?.refScriptUTxO == null)
      throw new Error(
        "Missing authenticated ValueAndMint asset-fold yield publication",
      );
    assetFoldYieldReferenceUtxo = await fetchUtxoByOutRef({
      lucid,
      outRef: entry.refScriptUTxO,
      label: "asset fold yield",
    });
    requireValidationDisputeReferenceScript({
      utxo: assetFoldYieldReferenceUtxo,
      deployedScriptHash: entry.scriptHash,
      expectedScriptHash: assetFoldYield.withdrawalScriptHash,
      authPolicyId: referenceScriptAuthPolicyId,
      role: "V1 validation-trace value-and-mint asset-fold yield",
    });
  }
  const scriptSourcesDescriptorYield = await (async () => {
    if (
      resolverIndex !== 8 ||
      ![19, 21, 22].includes(staged.semanticResolverIndex) ||
      staged.auxiliary.index !== 18
    )
      return undefined;
    const spec = SCRIPT_SOURCES_DESCRIPTOR_YIELD_ROLE;
    const contract = contracts.validationTraceDispute.yields[spec.contract];
    const entry = parsedDeploymentInfo[spec.deployment];
    if (entry?.refScriptUTxO == null)
      throw new Error("Missing authenticated redeemer descriptor yield");
    const utxo = await fetchUtxoByOutRef({
      lucid,
      outRef: entry.refScriptUTxO,
      label: spec.role,
    });
    requireValidationDisputeReferenceScript({
      utxo,
      deployedScriptHash: entry.scriptHash,
      expectedScriptHash: contract.withdrawalScriptHash,
      authPolicyId: referenceScriptAuthPolicyId,
      role: spec.role,
    });
    return {
      contract,
      utxo,
      claim: scriptSourcesDescriptorClaim(staged.auxiliary),
    };
  })();
  const scriptSourcesObserver = await (async () => {
    if (resolverIndex !== 8 || staged.semanticResolverIndex !== 25)
      return undefined;
    const carriage = midgardFieldCarriageFromData(
      staged.auxiliary.fields[2]!,
      "observer field carriage",
    );
    const bytes =
      carriage.carriage === "Inline"
        ? carriage.preimage
        : carriageMaterial === undefined
          ? undefined
          : Buffer.concat(
              carriageMaterial.plan.publications.map((p) => p.bytes),
            );
    if (bytes === undefined)
      throw new Error(
        "Observer reference carriage requires its publication material",
      );
    const items = decodeMidgardFieldPreimage(bytes);
    const itemIndex = staged.auxiliary.fields[1];
    if (
      typeof itemIndex !== "bigint" ||
      itemIndex < 0n ||
      itemIndex >= BigInt(items.length)
    )
      throw new Error("Observer item index is outside its field");
    const observerHash = Buffer.from(items[Number(itemIndex)]!).toString("hex");
    if (observerHash.length !== 56)
      throw new Error("Observer field item must be a 28-byte hash");
    const yields = await Promise.all(
      SCRIPT_SOURCES_OBSERVER_YIELD_ROLES.map(async (spec) => {
        const contract = contracts.validationTraceDispute.yields[spec.contract];
        const entry = parsedDeploymentInfo[spec.deployment];
        if (entry?.refScriptUTxO == null)
          throw new Error(
            `Missing authenticated observer yield ${spec.deployment}`,
          );
        const utxo = await fetchUtxoByOutRef({
          lucid,
          outRef: entry.refScriptUTxO,
          label: spec.role,
        });
        requireValidationDisputeReferenceScript({
          utxo,
          deployedScriptHash: entry.scriptHash,
          expectedScriptHash: contract.withdrawalScriptHash,
          authPolicyId: referenceScriptAuthPolicyId,
          role: spec.role,
        });
        return { contract, utxo };
      }),
    );
    return { yields, observerHash, activeCount: BigInt(items.length) };
  })();
  const scriptSourcesMiddleYield = await (async () => {
    if (resolverIndex !== 8 || staged.semanticResolverIndex !== 0)
      return undefined;
    const kind = scriptSourcesMiddleYieldIndex(
      staged.transition.work_witness_cbor,
      staged.auxiliary,
    );
    const spec = SCRIPT_SOURCES_MIDDLE_YIELD_ROLES[kind]!;
    const contract = contracts.validationTraceDispute.yields[spec.contract];
    const entry = parsedDeploymentInfo[spec.deployment];
    if (entry?.refScriptUTxO == null)
      throw new Error(
        `Missing authenticated ScriptSources yield ${spec.deployment}`,
      );
    const utxo = await fetchUtxoByOutRef({
      lucid,
      outRef: entry.refScriptUTxO,
      label: spec.role,
    });
    requireValidationDisputeReferenceScript({
      utxo,
      deployedScriptHash: entry.scriptHash,
      expectedScriptHash: contract.withdrawalScriptHash,
      authPolicyId: referenceScriptAuthPolicyId,
      role: spec.role,
    });
    return { contract, utxo, kind };
  })();
  const fetchLedgerOutputProofYield = async (spec: {
    readonly contract: keyof typeof contracts.validationTraceDispute.yields;
    readonly deployment: keyof ContractDeploymentInfo;
    readonly role: string;
  }) => {
    const contract = contracts.validationTraceDispute.yields[spec.contract];
    const entry = parsedDeploymentInfo[spec.deployment];
    if (entry?.refScriptUTxO == null)
      throw new Error(
        `Missing authenticated ledger-output-proof yield ${spec.deployment}`,
      );
    const utxo = await fetchUtxoByOutRef({
      lucid,
      outRef: entry.refScriptUTxO,
      label: spec.role,
    });
    requireValidationDisputeReferenceScript({
      utxo,
      deployedScriptHash: entry.scriptHash,
      expectedScriptHash: contract.withdrawalScriptHash,
      authPolicyId: referenceScriptAuthPolicyId,
      role: spec.role,
    });
    return { contract, utxo };
  };
  const ledgerOutputProofStepYields = await (async () => {
    if (
      !isLedgerOutputProofStepResolver(
        resolverIndex,
        staged.semanticResolverIndex,
      )
    )
      return undefined;
    const plan = deriveLedgerOutputProofStepPlan({
      resolverIndex,
      semanticResolverIndex: staged.semanticResolverIndex,
      transitionCbor: oneStepArgument.transitionCbor,
      auxiliaryCbor: oneStepArgument.auxiliaryCbor,
      ...(oneStepArgument.ledgerOutputProofSuccessorWorkWitnessCbor ===
      undefined
        ? {}
        : {
            ledgerOutputProofSuccessorWorkWitnessCbor:
              oneStepArgument.ledgerOutputProofSuccessorWorkWitnessCbor,
          }),
    });
    const stageSpec = LEDGER_OUTPUT_PROOF_STAGE_YIELD_ROLES[plan.roleIndex];
    if (stageSpec === undefined)
      throw new Error(
        "Ledger output proof stage role is outside the roles table",
      );
    const yields = await Promise.all(
      [
        stageSpec,
        ...plan.attestationRoles.map(
          (role) => LEDGER_OUTPUT_PROOF_ATTESTATION_YIELD_ROLES[role],
        ),
      ].map(fetchLedgerOutputProofYield),
    );
    return { plan, yields };
  })();
  const ledgerOutputProofFinalizeYields = await (async () => {
    if (
      !isLedgerOutputProofFinalizeResolver(
        resolverIndex,
        staged.semanticResolverIndex,
      )
    )
      return undefined;
    const plan = deriveLedgerOutputProofFinalizePlan({
      resolverIndex,
      semanticResolverIndex: staged.semanticResolverIndex,
      transitionCbor: oneStepArgument.transitionCbor,
    });
    // A fact-attach step references exactly its attach group's descriptor
    // yields; the thin terminal (all four facts recorded) references none.
    const yields = await Promise.all(
      plan.attachRoles
        .map((role) => {
          const spec = LEDGER_OUTPUT_DESCRIPTOR_YIELD_ROLES[role];
          if (spec === undefined)
            throw new Error(
              "Ledger output descriptor role is outside the roles table",
            );
          return spec;
        })
        .map(fetchLedgerOutputProofYield),
    );
    return { plan, yields };
  })();
  const phaseANativeItemYield = await (async () => {
    if (resolverIndex !== 5 || staged.semanticResolverIndex !== 1)
      return undefined;
    if (phaseANativeItemYieldKind === undefined)
      throw new Error(
        "Phase-A native item resolution requires the selected item language branch",
      );
    const native = phaseANativeItemYieldKind === "native";
    const contract = native
      ? contracts.validationTraceDispute.yields.phaseANativeItemNative
      : contracts.validationTraceDispute.yields.phaseANativeItemForeign;
    const entry = native
      ? parsedDeploymentInfo.validationTraceDisputePhaseANativeItemNativeWithdraw
      : parsedDeploymentInfo.validationTraceDisputePhaseANativeItemForeignWithdraw;
    if (entry?.refScriptUTxO == null)
      throw new Error("Missing authenticated phase-A item yield publication");
    const utxo = await fetchUtxoByOutRef({
      lucid,
      outRef: entry.refScriptUTxO,
      label: "phase-A item yield",
    });
    requireValidationDisputeReferenceScript({
      utxo,
      deployedScriptHash: entry.scriptHash,
      expectedScriptHash: contract.withdrawalScriptHash,
      authPolicyId: referenceScriptAuthPolicyId,
      role: native
        ? "V1 validation-trace phase-A native item native yield"
        : "V1 validation-trace phase-A native item foreign yield",
    });
    return { contract, utxo, kind: native ? (0 as const) : (1 as const) };
  })();
  const semanticFieldCarriageData =
    resolverIndex === 8 && staged.semanticResolverIndex === 15
      ? staged.auxiliary.fields[0]!
      : ((resolverIndex === 5 || resolverIndex === 6) &&
            staged.semanticResolverIndex === 1) ||
          (resolverIndex === 8 && staged.auxiliary.index === 1)
        ? staged.auxiliary.fields[2]!
        : undefined;
  const semanticFieldCarriage =
    semanticFieldCarriageData === undefined
      ? undefined
      : midgardFieldCarriageFromData(
          semanticFieldCarriageData,
          "semantic field item carriage",
        );
  const semanticFieldCarriageMaterial =
    semanticFieldCarriage === undefined ||
    semanticFieldCarriage.carriage === "Inline"
      ? undefined
      : carriageMaterial;
  if (
    semanticFieldCarriage !== undefined &&
    semanticFieldCarriage.carriage !== "Inline" &&
    semanticFieldCarriageMaterial === undefined
  )
    throw new Error(
      "Semantic field item reference carriage requires its authenticated publication material",
    );
  const isCekExecutionSelection =
    resolverIndex === 11 && staged.semanticResolverIndex === 1;
  const cekSelectionRoles = isCekExecutionSelection
    ? CEK_SELECTION_YIELD_ROLES.slice(
        0,
        staged.auxiliary.fields[1] === 0n ? 2 : 4,
      )
    : [];
  const cekSelectionYields = await Promise.all(
    cekSelectionRoles.map(async (spec) => {
      const contract = contracts.validationTraceDispute.yields[spec.contract];
      const entry = parsedDeploymentInfo[spec.deployment];
      if (entry?.refScriptUTxO == null)
        throw new Error(
          `Missing authenticated CEK selection yield publication: ${spec.deployment}`,
        );
      const utxo = await fetchUtxoByOutRef({
        lucid,
        outRef: entry.refScriptUTxO,
        label: spec.role,
      });
      requireValidationDisputeReferenceScript({
        utxo,
        deployedScriptHash: entry.scriptHash,
        expectedScriptHash: contract.withdrawalScriptHash,
        authPolicyId: referenceScriptAuthPolicyId,
        role: spec.role,
      });
      return { contract, utxo };
    }),
  );
  const cekSelectionFacts = isCekExecutionSelection
    ? deriveCekSelectionFacts(staged.cekRouteMaterial)
    : undefined;

  if (
    !isCekExecutionSelection &&
    cekProgramMaterialReferenceOutRefs !== undefined
  ) {
    throw new Error(
      "CEK program-material publication outrefs are permitted only for the CEK execution-selection semantic resolver",
    );
  }
  const isCompleteCanonicalItem =
    oneStepArgument.resolverIndex === 0 &&
    staged.semanticResolverIndex === 1 &&
    hasValidationAuxiliaryShape(
      staged.auxiliary,
      VALIDATION_AUXILIARY_SHAPES.transactionFieldItem,
    );
  // #597/#600. `TransactionFieldItemWitness` carries one field — a
  // `FieldCarriageV1` — and the bytes it stands for are the field's whole §5.1
  // preimage rather than one item with an opening into it. #597 could only read
  // tier-1 `Inline`; #600 made the producer tier-free, so all three §8.4 rungs
  // reach here and the tier is decided by the preimage's own length, never by
  // this builder.
  //
  // Tier 1 is self-contained: the preimage is inside the auxiliary. Tiers 2-3
  // carry positional reference-input indices and no bytes at all, so the
  // submitter must supply the material those indices name — that is what
  // `carriageMaterial` is, and its absence is a refusal rather than a
  // transaction that references nothing.
  const stagedItemCarriage = isCompleteCanonicalItem
    ? midgardFieldCarriageFromData(
        staged.auxiliary.fields[0]!,
        "Validation complete proof-item §8 carriage",
      )
    : undefined;
  const completeItemCarriageMaterial = (():
    | ValidationFieldCarriageMaterial
    | undefined => {
    if (
      stagedItemCarriage === undefined ||
      stagedItemCarriage.carriage === "Inline"
    ) {
      return undefined;
    }
    if (carriageMaterial === undefined) {
      throw new Error(
        `Validation complete proof-item carriage is tier-${
          stagedItemCarriage.carriage === "RawUtxo" ? "2" : "3"
        } \`${stagedItemCarriage.carriage}\`, which names reference inputs and carries no ` +
          "bytes, so the submission must supply the §8 carriage material those indices name (#600)",
      );
    }
    if (carriageMaterial.plan.tier !== stagedItemCarriage.carriage) {
      throw new Error(
        `Validation complete proof-item carriage material plans tier \`${carriageMaterial.plan.tier}\` ` +
          `while the staged auxiliary names \`${stagedItemCarriage.carriage}\``,
      );
    }
    return carriageMaterial;
  })();
  const completeFieldPreimage = ((): string | undefined => {
    if (stagedItemCarriage === undefined) {
      return undefined;
    }
    if (stagedItemCarriage.carriage === "Inline") {
      return stagedItemCarriage.preimage.toString("hex");
    }
    // §8.4's split is positional and exhaustive, so the plan's own publications
    // concatenate back to exactly the preimage the door will materialise. Taking
    // the bytes from the producer's plan rather than from a caller-supplied copy
    // is what keeps the staged datums below derived from the same bytes the door
    // authenticates.
    return Buffer.concat(
      completeItemCarriageMaterial!.plan.publications.map(
        (publication) => publication.bytes,
      ),
    ).toString("hex");
  })();
  // The complete-item proof transaction must source the semantic validator
  // from the published reference script: embedding the validator body would
  // consume the 16,384-byte envelope the measured complete-item redeemer
  // needs. Resolve it before publishing anything so a missing deployment
  // entry fails fast.
  const semanticReferenceScriptUtxo = isCompleteCanonicalItem
    ? (referenceScriptUtxo ??
      (await requireValidationItemSemanticReferenceScriptUtxo({
        lucid,
        deploymentInfo: parsedDeploymentInfo,
        expectedScriptHash: semanticContract.spendingScriptHash,
      })))
    : undefined;
  // The observe stage — the §8.8 door — must source its validator from the
  // published reference script the same way: embedding the applied observe
  // body in the door transaction spends the envelope the carriage bytes need
  // (#597 ruling a / #617). Resolved up front, beside the semantic entry, so
  // a missing deployment entry fails fast.
  const observeReferenceScriptUtxo = isCompleteCanonicalItem
    ? await requireValidationItemObserveReferenceScriptUtxo({
        lucid,
        deploymentInfo: parsedDeploymentInfo,
        expectedScriptHash:
          contracts.validationTraceDispute.canonicalDecodeItemStages.observe
            .spendingScriptHash,
      })
    : undefined;
  // #619/#621: how a tier-1 complete item's preimage reaches the §8.8 door is
  // decided here, at build time — explicit request, then a supplied
  // publication out-ref, then the measured cost heuristic. The committed
  // evidence is transition-only, so this is a routing decision and nothing
  // staged on chain can disagree with it.
  const proofItemDeliveryRoute = resolveValidationProofItemDeliveryRoute({
    requestedDelivery: proofItemDelivery,
    hasProofItemReferenceOutRef: proofItemReferenceOutRef !== undefined,
    committedCarriage: stagedItemCarriage?.carriage,
    ...(stagedItemCarriage?.carriage === "Inline"
      ? {
          preimageByteLength: Buffer.from(
            completeFieldPreimage as string,
            "hex",
          ).length,
        }
      : {}),
  });
  let resolvedProofItemReferenceOutRef = proofItemReferenceOutRef;
  let proofItemReferenceUtxo: UTxO | undefined;
  if (proofItemReferenceOutRef !== undefined) {
    try {
      proofItemReferenceUtxo = await fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          proofItemReferenceOutRef,
          "--proof-item-reference-out-ref",
        ),
        label: "validation complete proof-item reference UTxO",
      });
    } catch (cause) {
      const detail = cause instanceof Error ? cause.message : String(cause);
      // A spent or missing publication is a routing setback, never a loss of
      // the dispute: the §8 publication is content-addressed (§8.7), so any
      // fresh copy of the same bytes serves, and tier-1 bytes also fit the
      // observe redeemer directly. Refuse here, before any stage transaction
      // exists, and name both recoveries (#621).
      throw new Error(
        `Validation complete proof-item publication ${proofItemReferenceOutRef} is spent or ` +
          `missing on chain (${detail}). The publication is content-addressed, so recover by ` +
          "either (a) re-publishing: omit `proofItemReferenceOutRef` and the builder publishes " +
          "a fresh publication and routes by reference, or (b) inline delivery: pass " +
          '`proofItemDelivery: "inline"` to carry the preimage in the observe redeemer when ' +
          "it fits the L1 envelope.",
      );
    }
  }
  let proofItemPublication:
    | SubmitValidationDisputeSemanticResolutionResult["proofItemPublication"]
    | undefined;
  // The publication route reconstructs tier-1 `Inline` from a datum, so it
  // exists only inside tier 1 — as a redeemer-size optimisation, never as a
  // fourth rung (#600 Ruling 1, Q4). Above the cap the carriage already names
  // reference inputs of its own and a second one would be an unbound copy of
  // the same bytes. Factored into a function because two call sites route
  // through it: the up-front reference route, and the observe stage's
  // pre-sign fallback when an inline build outgrows the L1 envelope (#621).
  const publishProofItemPublication = async (): Promise<UTxO> => {
    const publication = deriveValidationProofItemPublication({
      transactionId: inputDatum.data.resolution.pre_state.transaction_id,
      transactionCommitment:
        inputDatum.data.resolution.pre_state.transaction_commitment,
      fieldPreimage: completeFieldPreimage as string,
    });
    signer.selectWallet(lucid);
    const publicationUnsigned = await Effect.runPromise(
      buildUnsignedValidationProofItemPublicationProgram(
        lucid,
        contracts,
        publication,
      ),
    );
    const publicationSigned = await publicationUnsigned.sign
      .withWallet()
      .complete();
    const publicationCbor = publicationSigned.toCBOR();
    requireL1ProofEnvelope(
      publicationCbor,
      "Validation complete proof-item publication",
    );
    const publicationOutputIndex = findUniqueInlineDatumOutputIndex({
      transactionCbor: publicationCbor,
      address: contracts.validationTraceDispute.proofItem.spendingScriptAddress,
      datum: publication.datumCbor,
      label: "Validation complete proof-item publication",
    });
    await reachOptionalPreSubmitBoundary({
      signed: publicationSigned,
      boundary: preSubmitBoundary,
    });
    const publicationTxHash = await publicationSigned.submit();
    // A reference input cannot be consumed until its creating transaction is
    // visible, even when the caller elects not to await the later resolution.
    await lucid.awaitTx(publicationTxHash, DEFAULT_CONFIRMATION_POLL_MS);
    resolvedProofItemReferenceOutRef = `${publicationTxHash}#${publicationOutputIndex.toString()}`;
    const publishedUtxo = await fetchUtxoByOutRef({
      lucid,
      outRef: {
        txHash: publicationTxHash,
        outputIndex: publicationOutputIndex,
      },
      label: "published validation complete proof-item reference UTxO",
    });
    proofItemPublication = {
      txHash: publicationTxHash,
      outRef: resolvedProofItemReferenceOutRef,
      outputIndex: publicationOutputIndex,
      completeSignedBytes: publicationCbor.length / 2,
      lovelace: publishedUtxo.assets.lovelace ?? 0n,
      awaitedConfirmation: true,
    };
    return publishedUtxo;
  };
  if (
    proofItemReferenceUtxo === undefined &&
    proofItemDeliveryRoute === "reference"
  ) {
    proofItemReferenceUtxo = await publishProofItemPublication();
  }
  if (proofItemReferenceUtxo !== undefined) {
    if (!isCompleteCanonicalItem) {
      throw new Error(
        "Validation complete proof-item reference is only valid for a CanonicalDecode complete item",
      );
    }
    if (stagedItemCarriage?.carriage !== "Inline") {
      throw new Error(
        "Validation complete proof-item reference reconstructs tier-1 `Inline` carriage and is not available above §8.3's tier-1 cap",
      );
    }
    if (
      proofItemReferenceUtxo.address !==
        contracts.validationTraceDispute.proofItem.spendingScriptAddress ||
      proofItemReferenceUtxo.datum == null ||
      proofItemReferenceUtxo.scriptRef !== undefined
    ) {
      throw new Error(
        "Validation complete proof-item reference is not locked by the deployed proof-item validator with only an inline datum",
      );
    }
    const expectedDatum = {
      version: 1n,
      transaction_id: inputDatum.data.resolution.pre_state.transaction_id,
      transaction_commitment:
        inputDatum.data.resolution.pre_state.transaction_commitment,
      field_preimage: completeFieldPreimage as string,
    };
    if (
      Data.to(expectedDatum, ValidationProofItemDatum) !==
      proofItemReferenceUtxo.datum
    ) {
      throw new Error(
        "Validation complete proof-item reference datum does not match the prepared evidence",
      );
    }
  }
  const range = requireValidityRange(
    validityRange ?? validationDisputeValidityRange(Date.now()),
  );
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: { version: 1n },
    },
    WinningValidationResolutionDatum,
  );
  if (resolverIndex === 11 && staged.semanticResolverIndex === 2) {
    if (semanticValidatorReferenceScriptUtxo === undefined)
      throw new Error("Missing CEK context binder publication");
    const prepared = Data.from(
      Data.to(inputDatum.data, PreparedValidationResolutionState),
    );
    if (staged.cekContextSuccessorWorkWitnessCbor === undefined)
      throw new Error(
        "CEK context requires retained canonical successor bytes",
      );
    const successorWorkWitnessCbor = Buffer.from(
      staged.cekContextSuccessorWorkWitnessCbor,
    ).toString("hex");
    const plan = deriveCekContextPlan({
      prepared,
      transition: staged.transitionData,
      auxiliary: staged.auxiliaryData,
      successorWorkWitnessCbor,
    });
    const stageReferences: Partial<Record<keyof CekContextStages, UTxO>> = {};
    for (const key of plan.route) {
      const spec = CEK_CONTEXT_STAGE_REFERENCES[key];
      const contract = contracts.validationTraceDispute.cekContextStages[key];
      const entry = parsedDeploymentInfo[spec.deployment];
      if (entry?.refScriptUTxO == null)
        throw new Error(`Missing CEK context publication: ${spec.deployment}`);
      const utxo = await fetchUtxoByOutRef({
        lucid,
        outRef: entry.refScriptUTxO,
        label: spec.role,
      });
      requireValidationDisputeReferenceScript({
        utxo,
        deployedScriptHash: entry.scriptHash,
        expectedScriptHash: contract.spendingScriptHash,
        authPolicyId: referenceScriptAuthPolicyId,
        role: spec.role,
      });
      stageReferences[key] = utxo;
    }
    const result = await submitCekContextChain({
      lucid,
      signer,
      contracts: contracts.validationTraceDispute,
      binder: semanticContract,
      binderReference: semanticValidatorReferenceScriptUtxo,
      stageReferences,
      sharedItem:
        plan.item === undefined
          ? undefined
          : {
              stages: contracts.validationTraceDispute.cekContextItemStages,
              deploymentId: deriveValidationTraceDeploymentId(
                fraudProofCataloguePolicyId,
              ),
              references: await resolveCekContextItemReferences({
                lucid,
                deployed: parsedDeploymentInfo,
                authPolicyId: referenceScriptAuthPolicyId,
                stages: contracts.validationTraceDispute.cekContextItemStages,
              }),
            },
      threadUtxo,
      threadUnit: token.unit,
      prepared,
      transition: staged.transitionData,
      auxiliary: staged.auxiliaryData,
      successorWorkWitnessCbor,
      awardDatum: outputDatum,
      getValidityRange: () =>
        refreshExpiredValidationDisputeValidityRange({
          range,
          currentLedgerTime: lucid.slotToUnixTime(lucid.currentSlot()),
        }),
    });
    const last = result.transactions.at(-1)!;
    return {
      txHash: last.txHash,
      threadOutRef,
      nextThreadOutRef: last.nextThreadOutRef,
      proofItemCarriage: "direct",
      resolverIndex,
      semanticResolverIndex: staged.semanticResolverIndex,
      semanticResolverGlobalIndex: staged.semanticResolverGlobalIndex,
      inputIndex: last.inputIndex,
      outputIndex: last.outputIndex,
      awaitedConfirmation: true,
      stageTransactions: result.transactions,
    };
  }
  if (resolverIndex === 11 && staged.semanticResolverIndex === 3) {
    if (semanticValidatorReferenceScriptUtxo === undefined)
      throw new Error("Missing CEK core binder publication");
    const prepared = Data.from(
      Data.to(inputDatum.data, PreparedValidationResolutionState),
    );
    const step = staged.auxiliary.fields[0]!;
    const plan = deriveCekCorePlan(prepared, step);
    const stageReferences: Partial<Record<keyof CekCoreStages, UTxO>> = {};
    for (const key of [...plan.route, "settle"] as const) {
      const spec = CEK_CORE_STAGE_REFERENCES[key];
      const contract = contracts.validationTraceDispute.cekCoreStages[key];
      const entry = parsedDeploymentInfo[spec.deployment];
      if (entry?.refScriptUTxO == null)
        throw new Error(`Missing CEK core publication: ${spec.deployment}`);
      const utxo = await fetchUtxoByOutRef({
        lucid,
        outRef: entry.refScriptUTxO,
        label: spec.role,
      });
      requireValidationDisputeReferenceScript({
        utxo,
        deployedScriptHash: entry.scriptHash,
        expectedScriptHash: contract.spendingScriptHash,
        authPolicyId: referenceScriptAuthPolicyId,
        role: spec.role,
      });
      stageReferences[key] = utxo;
    }
    const result = await submitCekCoreChain({
      lucid,
      signer,
      contracts: contracts.validationTraceDispute,
      binder: semanticContract,
      binderReference: semanticValidatorReferenceScriptUtxo,
      stageReferences,
      threadUtxo,
      threadUnit: token.unit,
      prepared,
      transition: staged.transitionData,
      step,
      awardDatum: outputDatum,
      getValidityRange: () =>
        refreshExpiredValidationDisputeValidityRange({
          range,
          currentLedgerTime: lucid.slotToUnixTime(lucid.currentSlot()),
        }),
    });
    const last = result.transactions.at(-1)!;
    return {
      txHash: last.txHash,
      threadOutRef,
      nextThreadOutRef: last.nextThreadOutRef,
      proofItemCarriage: "direct",
      resolverIndex,
      semanticResolverIndex: staged.semanticResolverIndex,
      semanticResolverGlobalIndex: staged.semanticResolverGlobalIndex,
      inputIndex: last.inputIndex,
      outputIndex: last.outputIndex,
      awaitedConfirmation: true,
      stageTransactions: result.transactions,
    };
  }
  const isSplitScriptSourcesStageOne =
    resolverIndex === 8 &&
    staged.semanticResolverIndex === 28 &&
    hasValidationAuxiliaryShape(
      staged.auxiliary,
      VALIDATION_AUXILIARY_SHAPES.redeemerItemStep,
    );
  if (isSplitScriptSourcesStageOne) {
    if (proofItemReferenceUtxo !== undefined) {
      throw new Error(
        "ScriptSources split stage-one route does not accept a proof-item reference",
      );
    }
    const stages =
      contracts.validationTraceDispute.scriptSourcesStageOneRedeemerStages;
    if (
      semanticContract.spendingScriptHash !==
        stages.envelope.spendingScriptHash ||
      semanticContract.spendingScriptAddress !==
        stages.envelope.spendingScriptAddress
    ) {
      throw new Error(
        "ScriptSources split stage-one semantic resolver is not the deployed envelope validator",
      );
    }
    const submissionPlan = deriveScriptSourcesItemSubmissionPlan({
      preparedCbor: scriptSourcesItemPreparedCbor ?? threadUtxo.datum!,
      oneStepArgument,
      deploymentId: deriveValidationTraceDeploymentId(
        fraudProofCataloguePolicyId,
      ),
      stages: { ...stages, entry: stages.envelope },
    });
    const firstStageIndex = scriptSourcesItemResumeIndex({
      plan: submissionPlan,
      thread: threadUtxo,
    });
    const plannedStages = submissionPlan.bindings;
    const sharedReferences = sharedRedeemerItemReferenceScripts(stages);
    type SplitStageContract = {
      readonly spendingScriptAddress: string;
      readonly spendingScript: Script;
    };
    type SplitStageResult = {
      readonly txHash: string;
      readonly nextThreadOutRef: string;
      readonly completeSignedBytes: number;
      readonly layout: ContinueLayout;
      readonly nextThreadUtxo?: UTxO;
    };
    const submitSplitStage = async ({
      inputUtxo,
      inputContract,
      outputContract,
      stageOutputDatum,
      label,
      scriptReference,
      awaitStage,
      encode,
    }: {
      readonly inputUtxo: UTxO;
      readonly inputContract: SplitStageContract;
      readonly outputContract: SplitStageContract;
      readonly stageOutputDatum: string;
      readonly label: string;
      readonly scriptReference?: UTxO;
      readonly awaitStage: boolean;
      readonly encode: (layout: {
        readonly inputIndex: bigint;
        readonly outputIndex: bigint;
      }) => string;
    }): Promise<SplitStageResult> => {
      let stageLayout: ContinueLayout | undefined;
      signer.selectWallet(lucid);
      const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
      const currentLedgerTime = lucid.slotToUnixTime(lucid.currentSlot());
      const stageRange =
        validityRange === undefined
          ? requireValidityRange(
              validationDisputeValidityRange(currentLedgerTime),
            )
          : refreshExpiredValidationDisputeValidityRange({
              range,
              currentLedgerTime,
            });
      const scriptCarriage = witnessSpendingValidatorCarriage({
        script: inputContract.spendingScript,
        referenceUtxo: scriptReference,
        label: `${label} spending validator`,
      });
      const base = lucid
        .newTx()
        .collectFrom([feeInput])
        .collectFrom(
          [inputUtxo],
          makeIndexedValidationStageRedeemer({
            threadUtxo: inputUtxo,
            outputAddress: outputContract.spendingScriptAddress,
            outputDatum: stageOutputDatum,
            threadUnit: token.unit,
            label,
            encode,
            onLayout: (resolvedLayout) => {
              stageLayout = resolvedLayout;
            },
          }),
        )
        .pay.ToContract(
          outputContract.spendingScriptAddress,
          { kind: "inline", value: stageOutputDatum },
          threadAssets(inputUtxo, token.unit),
        )
        .validFrom(stageRange.validFrom)
        .validTo(stageRange.validTo)
        .addSignerKey(signer.paymentKeyHash);
      const withReferenceScript =
        scriptCarriage.referenceInputs.length === 0
          ? base
          : base.readFrom([...scriptCarriage.referenceInputs]);
      const stageTx = scriptCarriage.attach(withReferenceScript);
      let unsigned: Awaited<ReturnType<typeof stageTx.complete>>;
      try {
        unsigned = await stageTx.complete({ localUPLCEval: true });
      } catch (cause) {
        const detail = cause instanceof Error ? cause.message : String(cause);
        throw new Error(`${label} local evaluation failed: ${detail}`);
      }
      if (stageLayout === undefined) {
        throw new Error(`BuildTxWithRedeemer did not resolve ${label} layout`);
      }
      const resolvedLayout = stageLayout as ContinueLayout;
      const signed = await unsigned.sign.withWallet().complete();
      const signedCbor = signed.toCBOR();
      requireL1ProofEnvelope(signedCbor, label);
      await reachOptionalPreSubmitBoundary({
        signed,
        boundary: preSubmitBoundary,
        referenceScriptCandidates: [
          { role: `${label} spending validator`, utxo: scriptReference },
        ],
      });
      const txHash = await signed.submit();
      const nextThreadOutRef = `${txHash}#${resolvedLayout.outputIndex.toString()}`;
      let nextThreadUtxo: UTxO | undefined;
      if (awaitStage) {
        await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
        nextThreadUtxo = await fetchUtxoByOutRef({
          lucid,
          outRef: {
            txHash,
            outputIndex: Number(resolvedLayout.outputIndex),
          },
          label: `${label} output`,
        });
      }
      return {
        txHash,
        nextThreadOutRef,
        completeSignedBytes: signedCbor.length / 2,
        layout: resolvedLayout,
        ...(nextThreadUtxo === undefined ? {} : { nextThreadUtxo }),
      };
    };
    const stageTransactions: {
      kind: RedeemerItemStageKey;
      txHash: string;
      nextThreadOutRef: string;
      completeSignedBytes: number;
    }[] = [];
    let currentThread = threadUtxo;
    let settle: SplitStageResult | undefined;
    for (let index = firstStageIndex; index < plannedStages.length; index++) {
      const binding = plannedStages[index]!;
      const outputContract =
        plannedStages[index + 1]?.validator ??
        contracts.validationTraceDispute.award;
      const metadata = sharedReferences.find(
        (reference) =>
          reference.validator.spendingScriptHash ===
          binding.validator.spendingScriptHash,
      );
      const explicit =
        binding.key === "entry"
          ? (referenceScriptUtxo ??
            stageReferenceScriptUtxos?.scriptSourcesEnvelope)
          : binding.key === "settle"
            ? stageReferenceScriptUtxos?.scriptSourcesSettlement
            : stageReferenceScriptUtxos?.sharedRedeemerItem?.get(
                binding.validator.spendingScriptHash,
              );
      const entryName =
        metadata?.deploymentEntry ??
        (binding.key === "entry"
          ? "validationTraceDisputeScriptSourcesRedeemerNormalizationSemantic"
          : "validationTraceDisputeRedeemerItemSettlement");
      const reference =
        explicit ??
        (await requirePublishedValidationSemanticReferenceScriptUtxo({
          lucid,
          deploymentInfo: parsedDeploymentInfo,
          entryName,
          expectedScriptHash: binding.validator.spendingScriptHash,
        }));
      if (explicit === undefined && metadata !== undefined)
        requireValidationDisputeReferenceScript({
          utxo: reference,
          deployedScriptHash: binding.validator.spendingScriptHash,
          expectedScriptHash: binding.validator.spendingScriptHash,
          authPolicyId: referenceScriptAuthPolicyId,
          role: metadata.role,
        });
      const stageOutputDatum = Data.to(
        new Constr(0, [
          inputDatum.fraud_prover,
          new Constr(0, [binding.outputState]),
        ]),
      );
      settle = await submitSplitStage({
        inputUtxo: currentThread,
        inputContract: binding.validator,
        outputContract,
        stageOutputDatum,
        label: `Validation ScriptSources item ${binding.key}`,
        scriptReference: reference,
        awaitStage: index < plannedStages.length - 1 || awaitConfirmation,
        encode: ({ inputIndex, outputIndex }) =>
          Data.to(binding.spendRedeemer(inputIndex, outputIndex)),
      });
      stageTransactions.push({
        kind: binding.key,
        txHash: settle.txHash,
        nextThreadOutRef: settle.nextThreadOutRef,
        completeSignedBytes: settle.completeSignedBytes,
      });
      if (settle.nextThreadUtxo !== undefined)
        currentThread = settle.nextThreadUtxo;
    }
    if (settle === undefined)
      throw new Error("ScriptSources item plan has no stages");

    return {
      txHash: settle.txHash,
      threadOutRef,
      nextThreadOutRef: settle.nextThreadOutRef,
      proofItemCarriage: "direct",
      resolverIndex,
      semanticResolverIndex: staged.semanticResolverIndex,
      semanticResolverGlobalIndex: staged.semanticResolverGlobalIndex,
      inputIndex: Number(settle.layout.inputIndex),
      outputIndex: Number(settle.layout.outputIndex),
      awaitedConfirmation: awaitConfirmation,
      stageTransactions,
    };
  }
  if (isCompleteCanonicalItem) {
    const stageData = deriveCanonicalDecodeItemStageData({
      preparedResolution: inputDatum.data,
      transition: staged.transition,
      fieldPreimage: completeFieldPreimage as string,
    });
    const authenticatedDatum = Data.to(
      {
        fraud_prover: inputDatum.fraud_prover,
        data: stageData.authenticated,
      },
      AuthenticatedCanonicalDecodeItemDatum,
    );
    const preparedDatum = Data.to(
      {
        fraud_prover: inputDatum.fraud_prover,
        data: stageData.prepared,
      },
      PreparedCanonicalDecodeItemDatum,
    );
    const observedDatum = Data.to(
      {
        fraud_prover: inputDatum.fraud_prover,
        data: stageData.observed,
      },
      ObservedCanonicalDecodeItemDatum,
    );
    const verifiedDatum = Data.to(
      {
        fraud_prover: inputDatum.fraud_prover,
        data: stageData.verified,
      },
      VerifiedCanonicalDecodeItemDatum,
    );
    type StageContract = {
      readonly spendingScriptAddress: string;
      readonly spendingScript: Script;
    };
    type SubmittedStage = {
      readonly txHash: string;
      readonly nextThreadOutRef: string;
      readonly completeSignedBytes: number;
      readonly layout: ContinueLayout;
      readonly nextThreadUtxo?: UTxO;
      readonly projectedSignedBytes?: number;
    };
    const submitStage = async ({
      inputUtxo,
      inputContract,
      outputContract,
      stageOutputDatum,
      label,
      proofReference,
      scriptReference,
      carriageReferences,
      awaitStage,
      projectEnvelopePreSign = false,
      encode,
    }: {
      readonly inputUtxo: UTxO;
      readonly inputContract: StageContract;
      readonly outputContract: StageContract;
      readonly stageOutputDatum: string;
      readonly label: string;
      readonly proofReference?: UTxO;
      readonly scriptReference?: UTxO;
      /** §8 tiers 2-3: the carriage UTxOs the resolved indices name. */
      readonly carriageReferences?: readonly UTxO[];
      readonly awaitStage: boolean;
      /**
       * Inline delivery's pre-sign envelope gate (#621): project the signed
       * byte length before signing and throw
       * {@link ValidationInlineDeliveryEnvelopeRefusedError} — signing and
       * submitting nothing — when it exceeds the L1 proof envelope, so the
       * caller can fall back to the reference route.
       */
      readonly projectEnvelopePreSign?: boolean;
      readonly encode: (layout: {
        readonly inputIndex: bigint;
        readonly outputIndex: bigint;
        readonly referenceInputIndex?: bigint;
      }) => string;
    }): Promise<SubmittedStage> => {
      let stageLayout: ContinueLayout | undefined;
      signer.selectWallet(lucid);
      const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
      const scriptCarriage = witnessSpendingValidatorCarriage({
        script: inputContract.spendingScript,
        referenceUtxo: scriptReference,
        label: `${label} spending validator`,
      });
      const referenceInputs = [
        ...(proofReference === undefined ? [] : [proofReference]),
        ...scriptCarriage.referenceInputs,
        ...(carriageReferences ?? []),
      ];
      let stageTx = lucid
        .newTx()
        .collectFrom([feeInput])
        .collectFrom(
          [inputUtxo],
          makeIndexedValidationStageRedeemer({
            threadUtxo: inputUtxo,
            outputAddress: outputContract.spendingScriptAddress,
            outputDatum: stageOutputDatum,
            threadUnit: token.unit,
            proofItemReferenceUtxo: proofReference,
            label,
            encode,
            onLayout: (resolvedLayout) => {
              stageLayout = resolvedLayout;
            },
          }),
        );
      if (referenceInputs.length > 0) {
        stageTx = stageTx.readFrom(referenceInputs);
      }
      const currentLedgerTime = lucid.slotToUnixTime(lucid.currentSlot());
      const stageRange =
        validityRange === undefined
          ? requireValidityRange(
              validationDisputeValidityRange(currentLedgerTime),
            )
          : refreshExpiredValidationDisputeValidityRange({
              range,
              currentLedgerTime,
            });
      stageTx = stageTx.pay
        .ToContract(
          outputContract.spendingScriptAddress,
          { kind: "inline", value: stageOutputDatum },
          threadAssets(inputUtxo, token.unit),
        )
        .validFrom(stageRange.validFrom)
        .validTo(stageRange.validTo)
        .addSignerKey(signer.paymentKeyHash);
      stageTx = scriptCarriage.attach(stageTx);
      let unsigned: Awaited<ReturnType<typeof stageTx.complete>>;
      try {
        unsigned = await stageTx.complete({ localUPLCEval: true });
      } catch (cause) {
        const detail = cause instanceof Error ? cause.message : String(cause);
        // On any chain whose protocol `maxTxSize` sits at or under the
        // Midgard envelope — today's L1 parameters exactly — an
        // over-envelope inline build never reaches the dummy-witness
        // projection below: CML's fee-calculation build refuses it first,
        // with its own size message (the same signature midgard-node's
        // reference-script publisher pins). That is still a pre-sign
        // refusal of the same build for the same reason, so on the
        // envelope-projection path it converts to the routing refusal and
        // the caller's publication fallback, not a hard failure (#621).
        const builderCeiling = projectEnvelopePreSign
          ? /Max transaction size of (\d+) exceeded\. Found: (\d+)/iu.exec(
              detail,
            )
          : null;
        if (builderCeiling !== null) {
          throw new ValidationInlineDeliveryEnvelopeRefusedError({
            label,
            projectedSignedBytes: Number(builderCeiling[2]),
            maxTransactionBytes: Number(builderCeiling[1]),
          });
        }
        throw new Error(`${label} local evaluation failed: ${detail}`);
      }
      if (stageLayout === undefined) {
        throw new Error(`BuildTxWithRedeemer did not resolve ${label} layout`);
      }
      const resolvedLayout = stageLayout as ContinueLayout;
      let projectedSignedBytes: number | undefined;
      if (projectEnvelopePreSign) {
        projectedSignedBytes = projectSignedL1ProofTransactionBytes(
          unsigned.toCBOR(),
        );
        if (projectedSignedBytes > MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES) {
          throw new ValidationInlineDeliveryEnvelopeRefusedError({
            label,
            projectedSignedBytes,
            maxTransactionBytes: MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES,
          });
        }
      }
      const signed = await unsigned.sign.withWallet().complete();
      const signedCbor = signed.toCBOR();
      requireL1ProofEnvelope(signedCbor, label);
      await reachOptionalPreSubmitBoundary({
        signed,
        boundary: preSubmitBoundary,
        referenceScriptCandidates: [
          { role: `${label} spending validator`, utxo: scriptReference },
        ],
      });
      const txHash = await signed.submit();
      const nextThreadOutRef = `${txHash}#${resolvedLayout.outputIndex.toString()}`;
      let nextThreadUtxo: UTxO | undefined;
      if (awaitStage) {
        await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
        nextThreadUtxo = await fetchUtxoByOutRef({
          lucid,
          outRef: {
            txHash,
            outputIndex: Number(resolvedLayout.outputIndex),
          },
          label: `${label} output`,
        });
      }
      return {
        txHash,
        nextThreadOutRef,
        completeSignedBytes: signedCbor.length / 2,
        layout: resolvedLayout,
        ...(nextThreadUtxo === undefined ? {} : { nextThreadUtxo }),
        ...(projectedSignedBytes === undefined ? {} : { projectedSignedBytes }),
      };
    };
    const stages = contracts.validationTraceDispute.canonicalDecodeItemStages;
    // Build-time carriage resolution (#619/#621, re-scoping #600 ruling
    // D3-A's committed-index re-check). Since Option B no index is frozen
    // into `evidence_hash`, so there is nothing committed left to re-check:
    // tiers 2-3 resolve the producer's plan by content (§8.7) against the
    // complete reference-input set the door transaction will read — the
    // published observe validator rides that same canonically-sorted list —
    // and the resolved carriage is what goes on the observe wire. Tier 1
    // carries the delivered preimage itself. Either way, material whose
    // content the door will not see refuses here, before any stage
    // transaction exists, where re-staging is still free.
    const observeCarriageReferences =
      completeItemCarriageMaterial === undefined
        ? undefined
        : completeItemCarriageMaterial.referenceUtxos;
    const observeCarriageData: PlutusDataValue =
      completeItemCarriageMaterial !== undefined
        ? midgardFieldCarriageToData(
            resolveMidgardFieldCarriageAgainstReferenceInputs({
              plan: completeItemCarriageMaterial.plan,
              referenceInputs: [
                ...(observeReferenceScriptUtxo === undefined
                  ? []
                  : [observeReferenceScriptUtxo]),
                ...completeItemCarriageMaterial.referenceUtxos,
              ],
              ...(completeItemCarriageMaterial.certificatePolicyId === undefined
                ? {}
                : {
                    certificatePolicyId:
                      completeItemCarriageMaterial.certificatePolicyId,
                  }),
            }),
          )
        : new Constr(0, [completeFieldPreimage as string]);
    const authenticate = await submitStage({
      inputUtxo: threadUtxo,
      inputContract: semanticContract,
      outputContract: stages.source,
      stageOutputDatum: authenticatedDatum,
      label: "Validation canonical item authentication",
      scriptReference: semanticReferenceScriptUtxo,
      awaitStage: true,
      // Option B (#620): `Verify` is `(input_index, output_index, transition)`.
      // The stage re-checks the transition-only commitment — the carriage is
      // neither forwarded nor referenced here; it is dereferenced once, at the
      // observe stage's §8.8 door, whichever route delivers it there.
      encode: ({ inputIndex, outputIndex }) =>
        Data.to(
          new Constr(1, [
            new Constr(0, [inputIndex, outputIndex, staged.transitionData]),
          ]),
        ),
    });
    const source = await submitStage({
      inputUtxo: authenticate.nextThreadUtxo!,
      inputContract: stages.source,
      outputContract: stages.observe,
      stageOutputDatum: preparedDatum,
      label: "Validation canonical item source binding",
      scriptReference: stageReferenceScriptUtxos?.canonicalDecodeItemSource,
      awaitStage: true,
      encode: ({ inputIndex, outputIndex }) =>
        Data.to(new Constr(1, [new Constr(0, [inputIndex, outputIndex])])),
    });
    // **The §8.8 door's own transaction (#600 ruling D3-A, #619/#621).** The
    // observe stage is the one stage that dereferences the carriage — every
    // earlier stage only re-checks the transition-only commitment — so it is
    // the one that reads the carriage UTxOs, and the sole content gate on
    // this path. The carriage on its wire is the one resolved at build time
    // above, never a replay of the staged auxiliary; on the tier-1 inline
    // route the redeemer carries the preimage itself, and a build whose
    // projected signed bytes outgrow the L1 envelope is refused pre-sign and
    // falls back to the §8 publication route — no routing input can strand
    // the staged thread (#621).
    let proofItemInlineEnvelopeRefusal:
      | SubmitValidationDisputeSemanticResolutionResult["proofItemInlineEnvelopeRefusal"]
      | undefined;
    const observeByReference = (
      proofReference: UTxO,
    ): Promise<SubmittedStage> =>
      submitStage({
        inputUtxo: source.nextThreadUtxo!,
        inputContract: stages.observe,
        outputContract: stages.proof,
        stageOutputDatum: observedDatum,
        label: "Validation canonical item observation",
        proofReference,
        scriptReference: observeReferenceScriptUtxo,
        awaitStage: true,
        encode: ({ inputIndex, outputIndex, referenceInputIndex }) =>
          Data.to(
            new Constr(1, [
              new Constr(1, [inputIndex, outputIndex, referenceInputIndex!]),
            ]),
          ),
      });
    let observe: SubmittedStage;
    if (proofItemReferenceUtxo !== undefined) {
      observe = await observeByReference(proofItemReferenceUtxo);
    } else {
      try {
        observe = await submitStage({
          inputUtxo: source.nextThreadUtxo!,
          inputContract: stages.observe,
          outputContract: stages.proof,
          stageOutputDatum: observedDatum,
          label: "Validation canonical item observation",
          scriptReference: observeReferenceScriptUtxo,
          ...(observeCarriageReferences === undefined
            ? {}
            : { carriageReferences: observeCarriageReferences }),
          awaitStage: true,
          projectEnvelopePreSign: proofItemDeliveryRoute === "inline",
          // #592's `Observe` is `(input_index, output_index, carriage)`.
          encode: ({ inputIndex, outputIndex }) =>
            Data.to(
              new Constr(1, [
                new Constr(0, [inputIndex, outputIndex, observeCarriageData]),
              ]),
            ),
        });
      } catch (cause) {
        if (!(cause instanceof ValidationInlineDeliveryEnvelopeRefusedError)) {
          throw cause;
        }
        // The refused build was never signed; the same staged thread and
        // datums serve the reference route unchanged, because the route
        // decides only how the door's bytes travel (#621).
        proofItemInlineEnvelopeRefusal = {
          projectedSignedBytes: cause.projectedSignedBytes,
          maxTransactionBytes: cause.maxTransactionBytes,
        };
        proofItemReferenceUtxo = await publishProofItemPublication();
        observe = await observeByReference(proofItemReferenceUtxo);
      }
    }
    const proof = await submitStage({
      inputUtxo: observe.nextThreadUtxo!,
      inputContract: stages.proof,
      outputContract: stages.settlement,
      stageOutputDatum: verifiedDatum,
      label: "Validation canonical item proof verification",
      scriptReference: stageReferenceScriptUtxos?.canonicalDecodeItemProof,
      awaitStage: true,
      encode: ({ inputIndex, outputIndex }) =>
        Data.to(new Constr(1, [new Constr(0, [inputIndex, outputIndex])])),
    });
    const settle = await submitStage({
      inputUtxo: proof.nextThreadUtxo!,
      inputContract: stages.settlement,
      outputContract: contracts.validationTraceDispute.award,
      stageOutputDatum: outputDatum,
      label: "Validation canonical item successor settlement",
      scriptReference: stageReferenceScriptUtxos?.canonicalDecodeItemSettlement,
      awaitStage: awaitConfirmation,
      encode: ({ inputIndex, outputIndex }) =>
        Data.to(new Constr(1, [new Constr(0, [inputIndex, outputIndex])])),
    });
    const stageTransactions = [
      { kind: "authenticate" as const, ...authenticate },
      { kind: "source" as const, ...source },
      { kind: "observe" as const, ...observe },
      { kind: "proof" as const, ...proof },
      { kind: "settle" as const, ...settle },
    ].map(
      ({
        kind,
        txHash,
        nextThreadOutRef,
        completeSignedBytes,
        projectedSignedBytes,
      }) => ({
        kind,
        txHash,
        nextThreadOutRef,
        completeSignedBytes,
        ...(projectedSignedBytes === undefined ? {} : { projectedSignedBytes }),
      }),
    );
    return {
      txHash: settle.txHash,
      threadOutRef,
      nextThreadOutRef: settle.nextThreadOutRef,
      proofItemCarriage:
        proofItemReferenceUtxo === undefined ? "direct" : "reference",
      ...(resolvedProofItemReferenceOutRef === undefined
        ? {}
        : { proofItemReferenceOutRef: resolvedProofItemReferenceOutRef }),
      ...(proofItemInlineEnvelopeRefusal === undefined
        ? {}
        : { proofItemInlineEnvelopeRefusal }),
      ...(proofItemPublication === undefined ? {} : { proofItemPublication }),
      resolverIndex,
      semanticResolverIndex: staged.semanticResolverIndex,
      semanticResolverGlobalIndex: staged.semanticResolverGlobalIndex,
      inputIndex: Number(settle.layout.inputIndex),
      outputIndex: Number(settle.layout.outputIndex),
      awaitedConfirmation: awaitConfirmation,
      stageTransactions,
    };
  }
  // One semantic-resolution transaction, signed and envelope-checked but not
  // yet submitted. Factored so the CEK execution selection can ladder through
  // its program-material routes — each a complete build refused pre-sign on a
  // deterministic fit failure before the next is tried — while every other
  // resolver builds exactly once.
  const prepareSemanticResolution = async ({
    label,
    materialReferenceUtxos = [],
    materialRoute,
    beginTraversal = false,
  }: {
    readonly label: string;
    readonly beginTraversal?: boolean;
    readonly materialReferenceUtxos?: readonly UTxO[];
    readonly materialRoute?: (
      layout: SemanticResolutionLayout,
    ) => ValidationCekMaterialRoute;
  }): Promise<{
    readonly signed: TxSigned;
    readonly layout: SemanticResolutionLayout;
  }> => {
    const stageContract = beginTraversal
      ? contracts.validationTraceDispute.cekMaterialTraversal
      : contracts.validationTraceDispute.award;
    const stageDatum = beginTraversal
      ? Data.to(
          {
            fraud_prover: inputDatum.fraud_prover,
            data: initialCekMaterialTraversal(staged.cekRouteMaterial!),
          },
          CekMaterialTraversalDatum,
        )
      : outputDatum;
    const activeYields = beginTraversal
      ? cekSelectionYields.slice(0, 2)
      : cekSelectionYields;
    let layout: SemanticResolutionLayout | undefined;
    signer.selectWallet(lucid);
    const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
    const semanticScriptCarriage = witnessSpendingValidatorCarriage({
      script: semanticContract.spendingScript,
      referenceUtxo: semanticValidatorReferenceScriptUtxo,
      label: "validation-dispute semantic-resolver validator",
    });
    const referenceInputs = [
      ...(proofItemReferenceUtxo === undefined ? [] : [proofItemReferenceUtxo]),
      ...materialReferenceUtxos,
      ...(assetFoldYieldReferenceUtxo === undefined
        ? []
        : [assetFoldYieldReferenceUtxo]),
      ...semanticScriptCarriage.referenceInputs,
      ...activeYields.map(({ utxo }) => utxo),
      ...(phaseANativeItemYield === undefined
        ? []
        : [phaseANativeItemYield.utxo]),
      ...(scriptSourcesMiddleYield === undefined
        ? []
        : [scriptSourcesMiddleYield.utxo]),
      ...(scriptSourcesObserver?.yields.map((y) => y.utxo) ?? []),
      ...(scriptSourcesDescriptorYield === undefined
        ? []
        : [scriptSourcesDescriptorYield.utxo]),
      ...(ledgerOutputProofStepYields?.yields.map((y) => y.utxo) ?? []),
      ...(ledgerOutputProofFinalizeYields?.yields.map((y) => y.utxo) ?? []),
      ...(semanticFieldCarriageMaterial?.referenceUtxos ?? []),
    ];
    if (semanticFieldCarriageMaterial !== undefined) {
      const resolved = resolveMidgardFieldCarriageAgainstReferenceInputs({
        plan: semanticFieldCarriageMaterial.plan,
        referenceInputs,
        ...(semanticFieldCarriageMaterial.certificatePolicyId === undefined
          ? {}
          : {
              certificatePolicyId:
                semanticFieldCarriageMaterial.certificatePolicyId,
            }),
      });
      if (
        Data.to(midgardFieldCarriageToData(resolved)) !==
        Data.to(semanticFieldCarriageData!)
      )
        throw new Error(
          "Semantic field item carriage indices differ from the committed evidence",
        );
    }
    let tx = lucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom(
        [threadUtxo],
        makeSemanticResolutionRedeemer({
          threadUtxo,
          outputAddress: stageContract.spendingScriptAddress,
          outputDatum: stageDatum,
          threadUnit: token.unit,
          resolverIndex,
          semanticResolverIndex: staged.semanticResolverIndex,
          transition: staged.transitionData,
          auxiliary: staged.auxiliary,
          materialReferenceUtxos,
          ...(cekSelectionFacts === undefined
            ? {}
            : {
                cekSelection: {
                  referenceUtxos: activeYields.map(({ utxo }) => utxo),
                  facts: beginTraversal
                    ? {
                        envelope: cekSelectionFacts.envelope,
                        material: deriveCekSelectionFacts().material,
                      }
                    : cekSelectionFacts,
                  beginTraversal,
                },
              }),
          ...(phaseANativeItemYield === undefined
            ? {}
            : { phaseANativeItemYield }),
          ...(scriptSourcesMiddleYield === undefined
            ? {}
            : { scriptSourcesMiddleYield }),
          ...(scriptSourcesDescriptorYield === undefined
            ? {}
            : { scriptSourcesDescriptorYield }),
          ...(ledgerOutputProofStepYields === undefined
            ? {}
            : {
                ledgerOutputProofStepYield: {
                  plan: ledgerOutputProofStepYields.plan,
                  utxos: ledgerOutputProofStepYields.yields.map((y) => y.utxo),
                },
              }),
          ...(ledgerOutputProofFinalizeYields === undefined
            ? {}
            : {
                ledgerOutputProofFinalizeYield: {
                  plan: ledgerOutputProofFinalizeYields.plan,
                  utxos: ledgerOutputProofFinalizeYields.yields.map(
                    (y) => y.utxo,
                  ),
                },
              }),
          ...(scriptSourcesObserver === undefined
            ? {}
            : {
                scriptSourcesObserverYields: {
                  utxos: scriptSourcesObserver.yields.map((y) => y.utxo),
                  observerHash: scriptSourcesObserver.observerHash,
                  activeCount: scriptSourcesObserver.activeCount,
                },
              }),
          ...(assetFoldYieldReferenceUtxo === undefined
            ? {}
            : { assetFoldYieldReferenceUtxo }),
          ...(materialRoute === undefined ? {} : { materialRoute }),
          onLayout: (resolvedLayout) => {
            layout = resolvedLayout;
          },
        }),
      );
    if (referenceInputs.length > 0) {
      tx = tx.readFrom(referenceInputs);
    }
    tx = tx.pay
      .ToContract(
        stageContract.spendingScriptAddress,
        { kind: "inline", value: stageDatum },
        threadAssets(threadUtxo, token.unit),
      )
      .validFrom(range.validFrom)
      .validTo(range.validTo)
      .addSignerKey(signer.paymentKeyHash);
    if (scriptSourcesDescriptorYield !== undefined)
      tx = tx.withdraw(
        validatorToRewardAddress(
          network,
          scriptSourcesDescriptorYield.contract.withdrawalScript,
        ),
        0n,
        Data.void(),
      );
    for (const observerYield of scriptSourcesObserver?.yields ?? [])
      tx = tx.withdraw(
        validatorToRewardAddress(
          network,
          observerYield.contract.withdrawalScript,
        ),
        0n,
        Data.void(),
      );
    if (scriptSourcesMiddleYield !== undefined)
      tx = tx.withdraw(
        validatorToRewardAddress(
          network,
          scriptSourcesMiddleYield.contract.withdrawalScript,
        ),
        0n,
        Data.void(),
      );
    for (const proofYield of [
      ...(ledgerOutputProofStepYields?.yields ?? []),
      ...(ledgerOutputProofFinalizeYields?.yields ?? []),
    ])
      tx = tx.withdraw(
        validatorToRewardAddress(network, proofYield.contract.withdrawalScript),
        0n,
        Data.void(),
      );
    if (phaseANativeItemYield !== undefined)
      tx = tx.withdraw(
        validatorToRewardAddress(
          network,
          phaseANativeItemYield.contract.withdrawalScript,
        ),
        0n,
        Data.void(),
      );
    if (assetFoldYield !== undefined)
      tx = tx.withdraw(
        validatorToRewardAddress(network, assetFoldYield.withdrawalScript),
        0n,
        Data.void(),
      );
    for (const { contract } of activeYields)
      tx = tx.withdraw(
        validatorToRewardAddress(network, contract.withdrawalScript),
        0n,
        Data.void(),
      );
    const readiedTx = semanticScriptCarriage.attach(tx);
    const unsigned = await readiedTx.complete({ localUPLCEval: true });
    if (layout === undefined) {
      throw new Error(
        "BuildTxWithRedeemer did not resolve validation semantic resolution layout",
      );
    }
    const signed = await unsigned.sign.withWallet().complete();
    requireL1ProofEnvelope(signed.toCBOR(), label);
    return { signed, layout };
  };
  const submitPreparedSemanticResolution = async (
    prepared: Awaited<ReturnType<typeof prepareSemanticResolution>>,
    cekRoute?: {
      readonly route: ValidationCekSelectedRoute;
      readonly materialReferenceUtxos: readonly UTxO[];
      readonly rejectedLocalRouteAttempts: readonly ValidationCekRejectedLocalRouteAttempt[];
    },
  ): Promise<SubmitValidationDisputeSemanticResolutionResult> => {
    await reachOptionalPreSubmitBoundary({
      signed: prepared.signed,
      boundary: preSubmitBoundary,
      referenceScriptCandidates: [
        {
          role: "validation-dispute semantic-resolver validator",
          utxo: semanticValidatorReferenceScriptUtxo,
        },
      ],
    });
    const txHash = await prepared.signed.submit();
    if (awaitConfirmation) {
      await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    }
    return {
      txHash,
      threadOutRef,
      nextThreadOutRef: `${txHash}#${prepared.layout.outputIndex.toString()}`,
      proofItemCarriage:
        proofItemReferenceUtxo === undefined ? "direct" : "reference",
      semanticValidatorCarriage:
        semanticValidatorReferenceScriptUtxo === undefined
          ? ("inline" as const)
          : ("reference" as const),
      ...(resolvedProofItemReferenceOutRef === undefined
        ? {}
        : { proofItemReferenceOutRef: resolvedProofItemReferenceOutRef }),
      ...(proofItemPublication === undefined ? {} : { proofItemPublication }),
      resolverIndex,
      semanticResolverIndex: staged.semanticResolverIndex,
      semanticResolverGlobalIndex: staged.semanticResolverGlobalIndex,
      inputIndex: Number(prepared.layout.inputIndex),
      outputIndex: Number(prepared.layout.outputIndex),
      awaitedConfirmation: awaitConfirmation,
      ...(cekRoute === undefined
        ? {}
        : {
            cekRoute: cekRoute.route,
            cekMaterialReferenceInputOutRefs:
              cekRoute.materialReferenceUtxos.map(outRefLabel),
            cekMaterialReferenceInputIndices:
              prepared.layout.materialReferenceInputIndices.map(Number),
            cekRejectedLocalRouteAttempts: cekRoute.rejectedLocalRouteAttempts,
          }),
    };
  };
  if (!isCekExecutionSelection) {
    return await submitPreparedSemanticResolution(
      await prepareSemanticResolution({
        label: "Validation semantic resolution",
      }),
    );
  }
  // CEK execution selection: `VerifyExecutionSelection` carries the
  // program-material route (`CekMaterialRouteV1`) beside the committed
  // evidence, and `material_entries_for_route` (validation-resolver-v1.ak)
  // authenticates the material it names against the immutable CEK
  // program-material publications. A native-script
  // selection carries no material; a Plutus/Midgard selection ladders
  // direct proof → single publication → minimum multi-output, each refused
  // pre-sign on a deterministic fit failure, exactly as the retired direct
  // resolver did.
  const routeMaterial = staged.cekRouteMaterial;
  if (routeMaterial === undefined) {
    return await submitPreparedSemanticResolution(
      await prepareSemanticResolution({
        label: "Validation-dispute CEK execution selection (no material)",
        materialRoute: () => "NoCekMaterial",
      }),
      {
        route: "noCekMaterial",
        materialReferenceUtxos: [],
        rejectedLocalRouteAttempts: [],
      },
    );
  }
  const rejectedLocalRouteAttempts: ValidationCekRejectedLocalRouteAttempt[] =
    [];
  const prepareCekRoute = async ({
    route,
    materialReferenceUtxos = [],
    materialRoute,
  }: {
    readonly route: ValidationCekRejectedLocalRouteAttempt["route"];
    readonly materialReferenceUtxos?: readonly UTxO[];
    readonly materialRoute: (
      layout: SemanticResolutionLayout,
    ) => ValidationCekMaterialRoute;
  }): Promise<
    Awaited<ReturnType<typeof prepareSemanticResolution>> | undefined
  > => {
    try {
      return await prepareSemanticResolution({
        label: `Validation-dispute CEK ${route}`,
        materialReferenceUtxos,
        materialRoute,
      });
    } catch (cause) {
      if (!isDeterministicLocalCekFitFailure(cause)) {
        throw cause;
      }
      rejectedLocalRouteAttempts.push({
        route,
        failure: errorMessage(cause),
      });
      return undefined;
    }
  };
  const submitSelectedRoute = (
    prepared: Awaited<ReturnType<typeof prepareSemanticResolution>>,
    route: ValidationCekSelectedRoute,
    materialReferenceUtxos: readonly UTxO[],
  ): Promise<SubmitValidationDisputeSemanticResolutionResult> =>
    submitPreparedSemanticResolution(prepared, {
      route,
      materialReferenceUtxos,
      rejectedLocalRouteAttempts,
    });

  const directPrepared = await prepareCekRoute({
    route: "directProof",
    materialRoute: () => ({
      DirectCekMaterial: {
        envelope_cbor: routeMaterial.envelopeCbor.toString("hex"),
        sidecar_cbor: routeMaterial.programMaterialSidecarCbor.toString("hex"),
      },
    }),
  });
  if (directPrepared !== undefined) {
    return await submitSelectedRoute(directPrepared, "directProof", []);
  }

  const traverseMaterial =
    async (): Promise<SubmitValidationDisputeSemanticResolutionResult> => {
      const traversalContract =
        contracts.validationTraceDispute.cekMaterialTraversal;
      const traversalEntry =
        parsedDeploymentInfo.validationTraceDisputeCekMaterialTraversal;
      if (traversalEntry?.refScriptUTxO == null)
        throw new Error("Missing CEK material traversal publication");
      const traversalReference = await fetchUtxoByOutRef({
        lucid,
        outRef: traversalEntry.refScriptUTxO,
        label: "CEK material traversal",
      });
      requireValidationDisputeReferenceScript({
        utxo: traversalReference,
        deployedScriptHash: traversalEntry.scriptHash,
        expectedScriptHash: traversalContract.spendingScriptHash,
        authPolicyId: referenceScriptAuthPolicyId,
        role: "V1 validation-trace CEK material traversal",
      });
      const taskReferences = await Promise.all(
        CEK_MATERIAL_TASK_YIELD_ROLES.map(async (spec) => {
          const entry = parsedDeploymentInfo[spec.deployment];
          if (entry?.refScriptUTxO == null)
            throw new Error(`Missing ${spec.role} publication`);
          const utxo = await fetchUtxoByOutRef({
            lucid,
            outRef: entry.refScriptUTxO,
            label: spec.role,
          });
          requireValidationDisputeReferenceScript({
            utxo,
            deployedScriptHash: entry.scriptHash,
            expectedScriptHash:
              contracts.validationTraceDispute.yields[spec.contract]
                .withdrawalScriptHash,
            authPolicyId: referenceScriptAuthPolicyId,
            role: spec.role,
          });
          return utxo;
        }),
      );
      const prepared = await prepareSemanticResolution({
        label: "CEK material traversal admission",
        beginTraversal: true,
        materialRoute: () => ({
          DirectCekMaterial: {
            envelope_cbor: routeMaterial.envelopeCbor.toString("hex"),
            sidecar_cbor: "",
          },
        }),
      });
      const begun = await submitPreparedSemanticResolution(prepared);
      await lucid.awaitTx(begun.txHash, DEFAULT_CONFIRMATION_POLL_MS);
      const initialThread = await fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(begun.nextThreadOutRef, "CEK traversal checkpoint"),
        label: "CEK traversal checkpoint",
      });
      let checkpointThread = initialThread;
      const traversalTransactions: Awaited<
        ReturnType<typeof submitCekMaterialTraversal>
      >["transactions"] = [];
      for (;;) {
        const batch = await submitCekMaterialTraversal({
          lucid,
          network,
          signer,
          contracts: contracts.validationTraceDispute,
          threadUtxo: checkpointThread,
          maxTransactions: cekMaterialTraversalBatchSize,
          threadUnit: token.unit,
          material: routeMaterial,
          traversalReference,
          taskReferences: [taskReferences[0]!, taskReferences[1]!],
          awardDatum: outputDatum,
          getValidityRange: () =>
            refreshExpiredValidationDisputeValidityRange({
              range,
              currentLedgerTime: lucid.slotToUnixTime(lucid.currentSlot()),
            }),
        });
        traversalTransactions.push(...batch.transactions);
        if (batch.completed) break;
        checkpointThread = await fetchUtxoByOutRef({
          lucid,
          outRef: parseOutRef(
            outRefLabel(batch.threadUtxo),
            "CEK traversal restart",
          ),
          label: "CEK traversal restart",
        });
      }
      const last = traversalTransactions.at(-1)!;
      return {
        ...begun,
        txHash: last.txHash,
        nextThreadOutRef: last.nextThreadOutRef,
        inputIndex: last.inputIndex,
        outputIndex: last.outputIndex,
        cekRoute: "authenticatedMaterialTraversal",
        cekMaterialReferenceInputOutRefs: [],
        cekMaterialReferenceInputIndices: [],
        cekRejectedLocalRouteAttempts: rejectedLocalRouteAttempts,
        stageTransactions: [
          {
            kind: "authenticate",
            txHash: begun.txHash,
            nextThreadOutRef: begun.nextThreadOutRef,
            completeSignedBytes: prepared.signed.toCBOR().length / 2,
          },
          ...traversalTransactions,
        ],
      };
    };
  if (cekProgramMaterialReferenceOutRefs === undefined)
    return await traverseMaterial();

  const materialAddress =
    contracts.validationTraceDispute.cekProgramMaterial.spendingScriptAddress;
  const singlePublication = deriveCekSinglePublication({
    envelopeCbor: routeMaterial.envelopeCbor,
    sidecarCbor: routeMaterial.programMaterialSidecarCbor,
  });
  const singleOutRef = cekProgramMaterialReferenceOutRefs?.singlePublication;
  if (singleOutRef === undefined) {
    throw new Error(
      "CEK direct proof did not fit; provide an already-confirmed exact single-publication material outref before selecting a more complex route",
    );
  }
  const singleReferenceUtxo = await requireConfirmedCekMaterialReferenceUtxo({
    lucid,
    outRef: singleOutRef,
    expectedAddress: materialAddress,
    expectedDatum: singlePublication.datumCbor,
    label: "CEK single-publication reference outref",
  });
  const singlePrepared = await prepareCekRoute({
    route: "completeSinglePublicationReference",
    materialReferenceUtxos: [singleReferenceUtxo],
    materialRoute: (layout) => {
      const reference_input_index = layout.materialReferenceInputIndices[0];
      if (reference_input_index === undefined) {
        throw new Error(
          "CEK single-publication reference is missing from final layout",
        );
      }
      return {
        SinglePublicationCekMaterial: {
          envelope_cbor: routeMaterial.envelopeCbor.toString("hex"),
          reference_input_index,
        },
      };
    },
  });
  if (singlePrepared !== undefined) {
    return await submitSelectedRoute(
      singlePrepared,
      "completeSinglePublicationReference",
      [singleReferenceUtxo],
    );
  }

  const entries = decodeMidgardCekProgramMaterialSidecar(
    routeMaterial.programMaterialSidecarCbor,
  );
  const expectedMultiPublications =
    deriveCekProgramMaterialPublications(entries);
  const multiOutRefs = cekProgramMaterialReferenceOutRefs?.minimumMultiOutput;
  if (multiOutRefs === undefined) {
    throw new Error(
      "CEK single-publication route did not fit; provide already-confirmed exact multi-output material outrefs in root order before selecting incremental traversal",
    );
  }
  if (multiOutRefs.length !== expectedMultiPublications.length) {
    throw new Error(
      `CEK minimum-multi route requires exactly ${expectedMultiPublications.length.toString()} root-ordered material outrefs, got ${multiOutRefs.length.toString()}`,
    );
  }
  if (new Set(multiOutRefs).size !== multiOutRefs.length) {
    throw new Error("CEK minimum-multi material outrefs must be unique");
  }
  const multiReferenceUtxos = await Promise.all(
    expectedMultiPublications.map((publication, index) =>
      requireConfirmedCekMaterialReferenceUtxo({
        lucid,
        outRef: multiOutRefs[index]!,
        expectedAddress: materialAddress,
        expectedDatum: publication.datumCbor,
        label: `CEK minimum-multi root-order outref ${index.toString()}`,
      }),
    ),
  );
  const multiPrepared = await prepareCekRoute({
    route: "minimumMultiOutputReconstruction",
    materialReferenceUtxos: multiReferenceUtxos,
    materialRoute: (layout) => ({
      MinimumMultiOutputCekMaterial: {
        envelope_cbor: routeMaterial.envelopeCbor.toString("hex"),
        reference_input_indices: [...layout.materialReferenceInputIndices],
      },
    }),
  });
  if (multiPrepared !== undefined) {
    return await submitSelectedRoute(
      multiPrepared,
      "minimumMultiOutputReconstruction",
      multiReferenceUtxos,
    );
  }

  if (staged.cekIncrementalNecessityReceiptSet === undefined) {
    throw new Error(
      "CEK direct, single-publication, and minimum-multi routes did not fit; incremental traversal requires an exact receipt-bound necessity set",
    );
  }
  // The legacy IncrementalCekMaterial redeemer remains inadmissible. After
  // authenticating these route receipts, use the bounded computation-thread
  // traversal, which must finish before the award can be spent.
  return await traverseMaterial();
};
