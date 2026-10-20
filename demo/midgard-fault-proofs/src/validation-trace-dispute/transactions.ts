/** The open step's three transactions, each built, locally evaluated and
 * signed from the prover wallet, and captured at the pre-submit boundary
 * with the durable recovery that names the outputs of ours it leaves. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { ResolvedProverSigner } from "../runtime.js";
import { requireSingletonUtxo } from "../runtime.js";
import type {
  ResolvedInitCatalogueCategory,
  ResolvedInitContracts,
} from "../submit-init.resolve-non-existent-input-no-index-init.js";
import { submitResolvedInit } from "../submit-init.submit-resolved-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import { ownOutRefLabel, parseOwnOutRef } from "../workflow/own-outputs.js";
import {
  captureLocallyEvaluatedTransaction,
  type LocallyEvaluatedTransaction,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import { validationTraceRecovery } from "./actions.js";
import type { ValidationTraceDisputeArtifact } from "./artifact.js";

/** How long a signed Open stays valid, so one that never lands retires. */
export const VALIDATION_TRACE_OPEN_VALIDITY_MS = 15 * 60_000;

/** L_d: the longest validity range VerifySource admits. */
const MAX_RANGE_MS =
  SELECTED_DEPLOYMENT_PROFILE.timing.dispute_max_validity_range_ms;

type DisputeChain =
  SDK.ValidationTraceDisputeFaultProofContracts["validationTraceDispute"];

/** The deployment the open step builds against. */
export type ValidationTraceDisputeDeployment = Readonly<{
  network: Network;
  blueprint: unknown;
  /** Init's contracts; the first step is the dispute opener. */
  init: Readonly<{
    contracts: ResolvedInitContracts;
    category: ResolvedInitCatalogueCategory;
    catalogue: Readonly<{
      policyId: string;
      spendingScriptAddress: string;
      root: string;
    }>;
    witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  }>;
  stateQueue: Readonly<{ policyId: string; spendingScriptAddress: string }>;
  chain: Pick<
    DisputeChain,
    "opener" | "source" | "case" | "disputeBond" | "caseConfig"
  >;
  /** The published reference scripts of the opener, source and case. */
  references: Readonly<{ opener: UTxO; source: UTxO; case: UTxO }>;
  /** The bond source-v1 is deployed with. */
  bondLovelace: bigint;
}>;

export type CapturedValidationTraceStage = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  durableRecovery: JournalJsonObject;
}>;

type StageInput = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  deployment: ValidationTraceDisputeDeployment;
  artifact: ValidationTraceDisputeArtifact;
}>;

/** The unit of the thread Init mints for `headerHash`. */
export const validationTraceThreadUnit = (
  deployment: ValidationTraceDisputeDeployment,
  headerHash: string,
): string =>
  toUnit(
    deployment.init.contracts.computationThread.policyId,
    deployment.init.category.categoryId + headerHash,
  );

/** The challenged block's state-queue node, by its policy-unique token. */
export const challengedBlockUtxo = async (
  lucid: LucidEvolution,
  deployment: ValidationTraceDisputeDeployment,
  headerHash: string,
): Promise<UTxO> => {
  const unit = toUnit(
    deployment.stateQueue.policyId,
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  );
  const nodes = (
    await lucid.utxosAtWithUnit(
      deployment.stateQueue.spendingScriptAddress,
      unit,
    )
  ).filter((utxo) => utxo.assets[unit] === 1n);
  if (nodes.length !== 1)
    throw new WorkflowActionChangedError(
      `validation-trace challenged block ${headerHash} is not on chain`,
    );
  return nodes[0]!;
};

const ours = async (
  lucid: LucidEvolution,
  outRef: string,
  label: string,
): Promise<UTxO> => {
  const [utxo] = await lucid.utxosByOutRef([parseOwnOutRef(outRef)]);
  if (utxo === undefined || ownOutRefLabel(utxo) !== outRef)
    throw new WorkflowActionChangedError(
      `validation-trace ${label} ${outRef} is no longer on chain`,
    );
  return utxo;
};

const captured = (
  signed: Parameters<
    typeof workflowReferenceScriptsUsedByTransaction
  >[0]["signed"],
  candidates: Parameters<
    typeof workflowReferenceScriptsUsedByTransaction
  >[0]["candidates"],
): LocallyEvaluatedTransaction =>
  Object.freeze({
    txHash: signed.toHash().toLowerCase(),
    signed,
    referenceScripts: Object.freeze(
      workflowReferenceScriptsUsedByTransaction({ signed, candidates }),
    ),
  });

/** Init: mints our thread on the challenged block, at the opener. */
export const captureValidationTraceInit = async ({
  lucid,
  signer,
  deployment,
  artifact,
}: StageInput): Promise<CapturedValidationTraceStage> => {
  const block = await challengedBlockUtxo(
    lucid,
    deployment,
    artifact.headerHash,
  );
  const transaction = await captureLocallyEvaluatedTransaction(
    async (boundary) =>
      await submitResolvedInit({
        lucid,
        blueprint: deployment.blueprint,
        network: deployment.network,
        label: "validation-trace init",
        contracts: deployment.init.contracts,
        category: deployment.init.category,
        catalogue: deployment.init.catalogue,
        signer,
        fraudulentBlockOutRef: ownOutRefLabel(block),
        ...(deployment.init.witnessReferenceScripts === undefined
          ? {}
          : {
              witnessReferenceScripts: deployment.init.witnessReferenceScripts,
            }),
        preSubmitBoundary: boundary,
        awaitConfirmation: false,
      }),
  );
  return {
    transaction,
    durableRecovery: validationTraceRecovery({
      stage: "init",
      transaction,
      slots: [
        {
          address: deployment.chain.opener.spendingScriptAddress,
          unit: validationTraceThreadUnit(deployment, artifact.headerHash),
        },
      ],
    }),
  };
};

/** Open: spends our thread into the pending claim at the source validator. */
export const captureValidationTraceOpen = async ({
  lucid,
  signer,
  deployment,
  artifact,
  threadOutRef,
}: StageInput & {
  readonly threadOutRef: string;
}): Promise<CapturedValidationTraceStage> => {
  const { chain, references } = deployment;
  const thread = await ours(lucid, threadOutRef, "thread");
  const block = await challengedBlockUtxo(
    lucid,
    deployment,
    artifact.headerHash,
  );
  const challenged = await Effect.runPromise(
    SDK.validationDisputeChallengedBlock(block, deployment.stateQueue.policyId),
  );
  const hubOraclePolicyId = deployment.init.contracts.hubOraclePolicyId;
  const hubOracle = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      deployment.network,
      scriptHashToCredential(hubOraclePolicyId),
    ),
    unit: toUnit(hubOraclePolicyId, SDK.HUB_ORACLE_ASSET_NAME),
    label: "validation-trace open hub oracle",
  });
  signer.selectWallet(lucid);
  const validTo =
    lucid.slotToUnixTime(lucid.currentSlot()) +
    VALIDATION_TRACE_OPEN_VALIDITY_MS;
  const unsigned = await SDK.appendValidationDisputeOpen(lucid.newTx(), {
    contracts: { opener: chain.opener, source: chain.source },
    computationThreadPolicyId:
      deployment.init.contracts.computationThread.policyId,
    openerReferenceScript: references.opener,
    thread,
    fraudProver: signer.paymentKeyHash,
    hubOracle,
    challenged,
    claim: artifact.claim,
  })
    .validTo(validTo)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const transaction = captured(signed, [
    { role: "validation-trace-opener", utxo: references.opener },
  ]);
  return {
    transaction,
    durableRecovery: validationTraceRecovery({
      stage: "open",
      transaction,
      slots: [
        {
          address: chain.source.spendingScriptAddress,
          unit: validationTraceThreadUnit(deployment, artifact.headerHash),
        },
      ],
    }),
  };
};

/** VerifySource: spends our pending claim into the case and its bond. */
export const captureValidationTraceVerifySource = async ({
  lucid,
  signer,
  deployment,
  artifact,
  openerOutRef,
}: StageInput & {
  readonly openerOutRef: string;
}): Promise<CapturedValidationTraceStage> => {
  const { chain, references } = deployment;
  const opener = await ours(lucid, openerOutRef, "pending claim");
  const caseConfig = await requireSingletonUtxo({
    lucid,
    address: SDK.validationCaseConfigAddress(
      deployment.network,
      chain.caseConfig,
    ),
    unit: toUnit(
      chain.caseConfig.policyId,
      SDK.VALIDATION_CASE_CONFIG_ASSET_NAME,
    ),
    label: "validation case configuration",
  });
  signer.selectWallet(lucid);
  const validFrom = lucid.slotToUnixTime(lucid.currentSlot());
  const { tx, opening } = SDK.appendValidationSourceVerification(
    lucid.newTx(),
    {
      contracts: {
        source: chain.source,
        case: chain.case,
        disputeBond: chain.disputeBond,
      },
      computationThreadPolicyId:
        deployment.init.contracts.computationThread.policyId,
      sourceReferenceScript: references.source,
      caseReferenceScript: references.case,
      caseConfig,
      opener,
      challengerTerminal: artifact.terminal,
      challengerTerminalProof: artifact.terminalProof,
      agreedInitialState: artifact.initialState,
      bondLovelace: deployment.bondLovelace,
      validFrom,
      validTo: validFrom + MAX_RANGE_MS,
      slots: lucid,
    },
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const transaction = captured(signed, [
    { role: "validation-trace-source", utxo: references.source },
    { role: "validation-case", utxo: references.case },
  ]);
  return {
    transaction,
    durableRecovery: validationTraceRecovery({
      stage: "verify_source",
      transaction,
      slots: [
        { address: chain.case.spendingScriptAddress, unit: opening.caseUnit },
        {
          address: chain.disputeBond.spendingScriptAddress,
          unit: opening.bondMarkerUnit,
        },
      ],
    }),
  };
};
