/** Bind the disputed transaction and header fee schedule into step-02 state. */
import {
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  MinFeeStep01SpendRedeemer,
  MinFeeStep02Datum,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { MinFeeContracts } from "./min-fee-contracts-v1.js";
import {
  minFeeStepLabel,
  minFeeSubmitError,
  requireMinFeeReferenceScript,
  requireMinFeeThreadUtxo,
} from "./min-fee-submit-common-v1.js";
import { prepareNativeTxInclusionCarriage } from "./native-inclusion-carriage-v1.js";
import {
  type PublishedProofChunk,
  walletInputsExcludingChunks,
} from "./proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "./runtime.js";
import {
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "./witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "./workflow/transaction-boundary-v1.js";

const STEP_LABEL = minFeeStepLabel(0);

export type SubmitMinFeeStep01Result = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly nativeTxId: string;
  readonly fee: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMinFeeStep01 = async ({
  lucid,
  blueprint,
  contracts,
  categoryId,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  publishedProofChunks,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: MinFeeContracts;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  /** Mandatory: min-fee validators are reference-script-only. */
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMinFeeStep01Result> => {
  const { threadUtxo, threadToken } = await requireMinFeeThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  requireNativeTxMatchesCompactCbor(txInclusion);
  const [hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: `${STEP_LABEL} hub oracle`,
    }),
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: `${STEP_LABEL} state-queue block`,
    }),
  ]);
  const headerHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (headerHash !== threadToken.fraudulentHeaderHash) {
    throw minFeeSubmitError(
      `state-queue header ${headerHash} does not match thread header ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  const header = await Effect.runPromise(
    getHeaderFromStateQueueDatum(
      await Effect.runPromise(
        getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
      ),
    ),
  );
  const state = {
    bad_tx: txInclusion.nativeTx,
    bad_tx_body_fee: txInclusion.nativeTx.body.fee,
    bad_tx_id: txInclusion.nativeTxId,
    min_fee_a: header.minFeeA,
    min_fee_b: header.minFeeB,
  };
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state },
    MinFeeStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  const stepReference = requireMinFeeReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  const chunks = publishedProofChunks ?? [];
  const inclusionCarriage = prepareNativeTxInclusionCarriage({
    blueprint,
    network,
    txInclusion,
    publishedProofChunks: chunks,
    witnessReferenceScripts,
    label: STEP_LABEL,
    baseReferenceInputs: [hubOracleUtxo, stateQueueBlockUtxo, stepReference],
  });
  let resolved:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const inputIndex = requireInputIndex(ctx, threadUtxo, STEP_LABEL);
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${STEP_LABEL} output`,
    );
    resolved = { inputIndex, outputIndex };
    return Data.to(
      {
        Continue: [
          inclusionCarriage.redeemer(ctx, {
            input_index: inputIndex,
            output_index: outputIndex,
            hub_ref_input_index: requireReferenceInputIndex(
              ctx,
              hubOracleUtxo,
              `${STEP_LABEL} hub oracle`,
            ),
            state_queue_node_ref_input_index: requireReferenceInputIndex(
              ctx,
              stateQueueBlockUtxo,
              `${STEP_LABEL} state-queue block`,
            ),
          }),
        ],
      },
      MinFeeStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks,
    }),
  );
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(inclusionCarriage.referenceInputs)
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = inclusionCarriage.attachWithdrawal(base);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolved === undefined) {
    throw minFeeSubmitError("step-01 layout was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof min-fee step-01",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[0].spendingScript,
        },
        ...inclusionCarriage.referenceScriptCandidates,
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw minFeeSubmitError(
      `step-01 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${resolved.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    nativeTxId: txInclusion.nativeTxId,
    fee: state.bad_tx_body_fee,
    minFeeA: state.min_fee_a,
    minFeeB: state.min_fee_b,
    inputIndex: Number(resolved.inputIndex),
    outputIndex: Number(resolved.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
