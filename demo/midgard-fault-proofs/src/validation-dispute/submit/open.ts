import {
  canOpenMidgardValidationDisputeBeforeMaturity,
  openMidgardValidationDispute,
} from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  hashBlockHeader,
  HUB_ORACLE_ASSET_NAME,
  PendingValidationClaimDatum,
  type ValidationClaimWitness,
  type ValidationTraceDescriptor,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  type TxSigned,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveValidationTraceDisputeDeploymentContracts,
} from "../../runtime.js";
import {
  requireComputationThreadToken,
  requireInitialStepDatum,
  selectFeeInput,
} from "../../step-support.js";
import { type FraudProofPreSubmitBoundary } from "../../workflow/transaction-boundary.js";
import { makeOpenRedeemer, type OpenLayout } from "./redeemers.js";
import { requireValidationDisputeReferenceScript } from "./reference-scripts.js";
import {
  requireL1ProofEnvelope,
  threadAssets,
} from "./transaction-material.js";
import {
  inclusiveValidityUpperBound,
  ledgerPresentedValidationDisputeValidityRange,
  reachOptionalPreSubmitBoundary,
  requireValidityRange,
  safeUnsignedNumber,
  type ValidationDisputeValidityRange,
  validationDisputeValidityRange,
} from "./validity.js";

export const openValidationDisputeAfterSourceVerification = ({
  operatorDescriptor,
  challengerDescriptor,
  openTimeUpper,
  challengedBlockEndTime,
  sourceValidityRange,
}: {
  readonly operatorDescriptor: Parameters<
    typeof openMidgardValidationDispute
  >[0]["operatorDescriptor"];
  readonly challengerDescriptor: Parameters<
    typeof openMidgardValidationDispute
  >[0]["challengerDescriptor"];
  readonly openTimeUpper: bigint;
  readonly challengedBlockEndTime: bigint;
  readonly sourceValidityRange: ValidationDisputeValidityRange;
}): ReturnType<typeof openMidgardValidationDispute> => {
  const range = requireValidityRange(sourceValidityRange);
  const authenticatedOpenTimeUpper = safeUnsignedNumber(
    openTimeUpper,
    "pending.open_time_upper",
  );
  const sourceTimeUpper = inclusiveValidityUpperBound(range);
  if (sourceTimeUpper < authenticatedOpenTimeUpper) {
    throw new Error(
      "Validation-dispute source verification cannot precede the authenticated open transaction",
    );
  }
  const authenticatedBlockEndTime = safeUnsignedNumber(
    challengedBlockEndTime,
    "pending.challenged_header.endTime",
  );
  if (
    !canOpenMidgardValidationDisputeBeforeMaturity({
      currentTimeUpper: sourceTimeUpper,
      challengedBlockEndTime: authenticatedBlockEndTime,
      maturityDuration: MIDGARD_CONSENSUS_LIMITS.blockMaturityMs,
    })
  ) {
    throw new Error(
      "Validation dispute cannot complete before the challenged block matures after source verification",
    );
  }
  return openMidgardValidationDispute({
    operatorDescriptor,
    challengerDescriptor,
    currentTime: sourceTimeUpper,
  });
};

export type SubmitValidationDisputeOpenResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export type BuildValidationDisputeOpenParams = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly claim: ValidationClaimWitness;
  readonly challengerDescriptor: ValidationTraceDescriptor;
  readonly validityRange?: ValidationDisputeValidityRange;
};

export type BuildValidationDisputeOpenResult = {
  readonly signed: TxSigned;
  readonly threadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
};

export const buildValidationDisputeOpen = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  claim,
  challengerDescriptor,
  validityRange = validationDisputeValidityRange(Date.now()),
}: BuildValidationDisputeOpenParams): Promise<BuildValidationDisputeOpenResult> => {
  const range = ledgerPresentedValidationDisputeValidityRange(
    lucid,
    validityRange,
  );
  const resolved = await resolveValidationTraceDisputeDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireStateQueueMint: true,
  });
  const {
    deploymentInfo: parsedDeploymentInfo,
    referenceScriptAuthPolicyId,
    validationTraceDisputeCategory,
    hubOraclePolicyId,
    contracts,
  } = resolved;
  const stateQueuePolicyId = resolved.stateQueuePolicyId!;
  const disputeContract = contracts.validationTraceDispute.firstStep;
  const disputeDeploymentEntry = parsedDeploymentInfo.validationTraceDispute;
  if (disputeDeploymentEntry === undefined) {
    throw new Error('Deployment info is missing "validationTraceDispute"');
  }
  if (disputeDeploymentEntry.refScriptUTxO == null) {
    throw new Error(
      'Deployment info entry "validationTraceDispute" is missing refScriptUTxO; publish the authenticated V1 validation-trace dispute reference script and regenerate deployment info before opening a dispute',
    );
  }
  const [threadUtxo, hubOracleUtxo, stateQueueBlockUtxo, disputeReferenceUtxo] =
    await Promise.all([
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
        label: "validation-dispute computation-thread UTxO",
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(hubOraclePolicyId),
        ),
        unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: "hub oracle",
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          stateQueueBlockOutRef,
          "--state-queue-block-out-ref",
        ),
        label: "validation-dispute state-queue block UTxO",
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: disputeDeploymentEntry.refScriptUTxO,
        label: "validation-dispute authenticated reference-script UTxO",
      }),
    ]);
  requireValidationDisputeReferenceScript({
    utxo: disputeReferenceUtxo,
    deployedScriptHash: disputeDeploymentEntry.scriptHash,
    expectedScriptHash: disputeContract.spendingScriptHash,
    authPolicyId: referenceScriptAuthPolicyId,
  });
  if (threadUtxo.address !== disputeContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  requireInitialStepDatum({ threadUtxo, signer });
  const fraudulentHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (fraudulentHeaderHash !== token.fraudulentHeaderHash) {
    throw new Error(
      `State-queue block header hash ${fraudulentHeaderHash} does not match computation-thread header hash ${token.fraudulentHeaderHash}`,
    );
  }
  const stateQueueNodeView = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
  );
  const header = await Effect.runPromise(
    getHeaderFromStateQueueDatum(stateQueueNodeView),
  );
  const computedHeaderHash = await Effect.runPromise(hashBlockHeader(header));
  if (computedHeaderHash !== fraudulentHeaderHash) {
    throw new Error(
      `State-queue datum header hashes to ${computedHeaderHash}, expected ${fraudulentHeaderHash}`,
    );
  }
  // Opening authenticates committed structure. A game is constructed only
  // after source verification establishes valid normative endpoints.
  const currentTimeUpper = inclusiveValidityUpperBound(range);
  if (
    !canOpenMidgardValidationDisputeBeforeMaturity({
      currentTimeUpper,
      challengedBlockEndTime: safeUnsignedNumber(
        header.endTime,
        "header.endTime",
      ),
      maturityDuration: MIDGARD_CONSENSUS_LIMITS.blockMaturityMs,
    })
  ) {
    throw new Error(
      "Validation dispute cannot complete before the challenged block matures",
    );
  }
  const outputDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        challenged_header_hash: fraudulentHeaderHash,
        challenged_header: header,
        claim,
        challenger_descriptor: challengerDescriptor,
        open_time_upper: BigInt(currentTimeUpper),
      },
    },
    PendingValidationClaimDatum,
  );
  let layout: OpenLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeOpenRedeemer({
        threadUtxo,
        hubOracleUtxo,
        stateQueueBlockUtxo,
        outputAddress:
          contracts.validationTraceDispute.source.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        claim,
        challengerDescriptor,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .readFrom([hubOracleUtxo, stateQueueBlockUtxo, disputeReferenceUtxo])
    .pay.ToContract(
      contracts.validationTraceDispute.source.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute open layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  return {
    signed,
    threadOutRef,
    fraudulentHeaderHash,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    hubOracleRefInputIndex: Number(layout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(layout.stateQueueNodeRefInputIndex),
  };
};

export const submitValidationDisputeOpen = async ({
  awaitConfirmation = true,
  preSubmitBoundary,
  ...params
}: BuildValidationDisputeOpenParams & {
  readonly awaitConfirmation?: boolean;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitValidationDisputeOpenResult> => {
  const built = await buildValidationDisputeOpen(params);
  requireL1ProofEnvelope(built.signed.toCBOR(), "Validation-dispute open");
  await reachOptionalPreSubmitBoundary({
    signed: built.signed,
    boundary: preSubmitBoundary,
  });
  const txHash = await built.signed.submit();
  if (awaitConfirmation) {
    await params.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef: built.threadOutRef,
    nextThreadOutRef: `${txHash}#${built.outputIndex.toString()}`,
    fraudulentHeaderHash: built.fraudulentHeaderHash,
    inputIndex: built.inputIndex,
    outputIndex: built.outputIndex,
    hubOracleRefInputIndex: built.hubOracleRefInputIndex,
    stateQueueNodeRefInputIndex: built.stateQueueNodeRefInputIndex,
    awaitedConfirmation: awaitConfirmation,
  };
};
