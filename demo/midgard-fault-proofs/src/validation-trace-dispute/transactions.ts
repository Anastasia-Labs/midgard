/** The open step's three transactions, each built, locally evaluated and
 * signed from the prover wallet, and captured at the pre-submit boundary
 * with the durable recovery that names the outputs of ours it leaves; and
 * what the close (close.ts) shares with them. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { ResolvedProverSigner } from "../runtime.js";
import { requireSingletonUtxo } from "../runtime.js";
import type { ScriptDecodeDeployment } from "../script-decode/submit.js";
import type {
  ResolvedInitCatalogueCategory,
  ResolvedInitContracts,
} from "../submit-init.resolve-non-existent-input-no-index-init.js";
import { submitResolvedInit } from "../submit-init.submit-resolved-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { grantValidationDisputeFundingAuthority } from "../workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import { ownOutRefLabel, parseOwnOutRef } from "../workflow/own-outputs.js";
import {
  captureLocallyEvaluatedTransaction,
  type LocallyEvaluatedTransaction,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import { validationTraceRecovery } from "./actions.js";
import type { ValidationTraceDisputeArtifact } from "./artifact.js";
import type { ValidationTraceCanonicalDecodeHolder } from "./one-step-canonical-decode.js";
import type { DrivenCekContextStage } from "./one-step-context.js";
import type { ValidationTraceContextFollowerHolder } from "./one-step-context-followers.js";
import type { ValidationTraceContextItemHolder } from "./one-step-context-item-holders.js";
import type { ValidationTracePhaseHolder } from "./one-step-phase.js";

/** How long a signed Open stays valid, so one that never lands retires. */
export const VALIDATION_TRACE_OPEN_VALIDITY_MS = 15 * 60_000;

/** L_d: the longest validity range VerifySource admits. */
const MAX_RANGE_MS =
  SELECTED_DEPLOYMENT_PROFILE.timing.dispute_max_validity_range_ms;

type DisputeChain =
  SDK.ValidationTraceDisputeFaultProofContracts["validationTraceDispute"];

/**
 * The published reference scripts of the one-step resolution of a CEK core
 * step: the boundary, the CEK phase resolver, the CEK core step binder, the
 * arms that settle in one hop, and the settle; and of a CEK script
 * selection, when the deployment drives it.
 */
export type ValidationTraceOneStepReferences = Readonly<{
  boundary: UTxO;
  cekPrepare: UTxO;
  cekCore: UTxO;
  settle: UTxO;
  arms: Readonly<{
    compute: UTxO;
    machine: UTxO;
    mapConversion: UTxO;
    caseConstant: UTxO;
  }>;
  /**
   * The selection resolver, and its two yields' references, each carrying
   * its role's reference-script authentication token; without them a thread
   * at a CEK script selection waits.
   */
  cekSelection?: Readonly<{
    semantic: UTxO;
    authenticate: UTxO;
    successor: UTxO;
  }>;
  /**
   * The context step resolver, the context control and settle, and each
   * context stage the runner drives; without them a thread at a CEK context
   * step waits.
   */
  cekContext?: Readonly<{
    semantic: UTxO;
    control: UTxO;
    settle: UTxO;
    stages: Readonly<Record<DrivenCekContextStage, UTxO>>;
    /** The holders of the redeemer item step the item bind stage binds. */
    items: Readonly<Record<ValidationTraceContextItemHolder, UTxO>>;
    /** The holders past a multi-hop stage's first. */
    followers: Readonly<Record<ValidationTraceContextFollowerHolder, UTxO>>;
  }>;
  /**
   * The canonical-decode resolver, its empty and item semantic resolvers,
   * and the item's stages; without them a thread at a canonical-decode step
   * waits.
   */
  canonicalDecode?: Readonly<
    Record<ValidationTraceCanonicalDecodeHolder, UTxO>
  >;
  /** The phase resolvers and their semantic resolvers the deployment
   * publishes; a thread at a step of a phase without them waits. */
  phases?: Readonly<Partial<Record<ValidationTracePhaseHolder, UTxO>>>;
}>;

/** The deployment the open step builds against. */
export type ValidationTraceDisputeDeployment = Readonly<{
  /** The deployment VerifySource's funding authority is bound to. */
  deploymentFingerprint: string;
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
    | "opener"
    | "source"
    | "case"
    | "disputeBond"
    | "caseConfig"
    | "game"
    | "timeout"
    | "award"
    | "boundary"
    | "prepareResolvers"
    | "semanticResolvers"
    | "cekCoreStages"
    | "cekContextStages"
    | "cekContextItemStages"
    | "canonicalDecodeItemStages"
    | "yields"
  >;
  /** The hub's fraud-proof policy and address, where a conviction records. */
  fraudProof: Readonly<{
    policyId: string;
    mintingScript: Script;
    spendingScriptAddress: string;
  }>;
  /**
   * The published reference scripts of the opener, source, case and bond,
   * and of the game, timeout and award the thread moves run.
   */
  references: Readonly<{
    opener: UTxO;
    source: UTxO;
    case: UTxO;
    bond: UTxO;
    game: UTxO;
    timeout: UTxO;
    award: UTxO;
    /**
     * The one-step resolution's scripts the runner drives (one-step-moves.ts);
     * without them a thread at ReadyForOneStep waits.
     */
    oneStep?: ValidationTraceOneStepReferences;
  }>;
  /** The dispute bond the case validator is applied with. */
  bondLovelace: bigint;
  /**
   * The script-decode deployment whose records a CEK script selection reads,
   * the one the selection's successor yield is applied with; without it a
   * thread at a Plutus script selection waits.
   */
  scriptDecode?: ScriptDecodeDeployment;
}>;

export type CapturedValidationTraceStage = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  durableRecovery: JournalJsonObject;
}>;

export type StageInput = Readonly<{
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

/** Our output at `outRef`, or the action changed. */
export const ours = async (
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

/** The signed body with the published reference scripts it reads. */
export const captured = (
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

/** The deployment's hub oracle, by its unique token. */
export const hubOracleUtxo = async (
  lucid: LucidEvolution,
  deployment: ValidationTraceDisputeDeployment,
): Promise<UTxO> => {
  const hubOraclePolicyId = deployment.init.contracts.hubOraclePolicyId;
  return await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      deployment.network,
      scriptHashToCredential(hubOraclePolicyId),
    ),
    unit: toUnit(hubOraclePolicyId, SDK.HUB_ORACLE_ASSET_NAME),
    label: "validation-trace hub oracle",
  });
};

/** The validation case configuration, by its unique token. */
export const caseConfigUtxo = async (
  lucid: LucidEvolution,
  deployment: ValidationTraceDisputeDeployment,
): Promise<UTxO> =>
  await requireSingletonUtxo({
    lucid,
    address: SDK.validationCaseConfigAddress(
      deployment.network,
      deployment.chain.caseConfig,
    ),
    unit: toUnit(
      deployment.chain.caseConfig.policyId,
      SDK.VALIDATION_CASE_CONFIG_ASSET_NAME,
    ),
    label: "validation case configuration",
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
  const hubOracle = await hubOracleUtxo(lucid, deployment);
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
  const caseConfig = await caseConfigUtxo(lucid, deployment);
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
  const openerLovelace = opener.assets.lovelace ?? 0n;
  grantValidationDisputeFundingAuthority({
    signed,
    deploymentFingerprint: deployment.deploymentFingerprint,
    headerHash: artifact.headerHash,
    opener,
    bond: {
      address: chain.disputeBond.spendingScriptAddress,
      unit: opening.bondMarkerUnit,
      lovelace: deployment.bondLovelace,
      datumCborHex: Data.to(opening.bond, SDK.DisputeBond),
    },
    caseOutput: {
      address: chain.case.spendingScriptAddress,
      unit: opening.caseUnit,
      floorLovelace: SDK.VALIDATION_CASE_FLOOR_LOVELACE,
      lovelace:
        openerLovelace > SDK.VALIDATION_CASE_FLOOR_LOVELACE
          ? openerLovelace
          : SDK.VALIDATION_CASE_FLOOR_LOVELACE,
    },
  });
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
