import {
  HUB_ORACLE_ASSET_NAME,
  type NativeTxInclusionCarriage,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  chunkedMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunkV1,
  requireBuiltChunkReferenceIndices,
  walletInputsExcludingChunks,
} from "../proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { L2TxMistagContractsV1 } from "./contracts-v1.js";
import {
  L2TxMistagStep01SpendRedeemer,
  L2TxMistagStep02Datum,
  type L2TxMistagStep02State,
} from "./schemas-v1.js";
import {
  l2TxMistagStepLabelV1,
  l2TxMistagSubmitError,
  requireL2TxMistagReferenceScriptV1,
  requireL2TxMistagThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = l2TxMistagStepLabelV1(0);

export type SubmitL2TxMistagStep01Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  readonly state: L2TxMistagStep02State;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitL2TxMistagStep01 = async ({
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
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: L2TxMistagContractsV1;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly publishedProofChunks?: readonly PublishedProofChunkV1[];
  /** Mandatory published step-01 reference script. */
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitL2TxMistagStep01Result> => {
  const { threadUtxo, threadToken } = await requireL2TxMistagThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  const verifiedReferenceScript = requireL2TxMistagReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  const stateQueueBlockUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
    label: `${STEP_LABEL} state-queue block`,
  });
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOraclePolicyId),
    ),
    unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
    label: `${STEP_LABEL} hub oracle`,
  });
  const stateQueueHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (stateQueueHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw l2TxMistagSubmitError(
      `state-queue header ${stateQueueHeaderHash} does not match thread header ${threadToken.fraudulentHeaderHash}.`,
    );
  }

  requireNativeTxMatchesCompactCbor(txInclusion);
  if (txInclusion.nativeTx.validity_code === 0n) {
    throw l2TxMistagSubmitError(
      "a code-0 leaf is an honest acceptance; a valid block cannot be challenged.",
    );
  }
  if (txInclusion.nativeTx.validity_code !== 1n) {
    throw l2TxMistagSubmitError(
      `native-V1 validity code must be 0 or 1; received ${txInclusion.nativeTx.validity_code.toString()}.`,
    );
  }
  const state: L2TxMistagStep02State = {
    bad_tx_id: txInclusion.nativeTxId,
    committed_validity_code: txInclusion.nativeTx.validity_code,
  };

  signer.selectWallet(lucid);
  const chunks = publishedProofChunks ?? [];
  const carriedByChunks = chunks.length > 0;
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks,
    }),
  );
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    ...chunks.map((chunk) => chunk.utxo),
    verifiedReferenceScript,
  ];
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const chunkedVerifyScript = chunkedVerifyWithdrawalScript(blueprint);
  const chunkedVerifyRewardAddress = phasMembershipRewardAddress(
    network,
    chunkedVerifyScript,
  );
  const resolvedChunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks,
    label: STEP_LABEL,
  });
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state },
    L2TxMistagStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let layout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks,
      derived: resolvedChunkIndices,
      label: STEP_LABEL,
    });
    const common = {
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      hub_ref_input_index: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        `${STEP_LABEL} hub oracle`,
      ),
      state_queue_node_ref_input_index: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        `${STEP_LABEL} state-queue node`,
      ),
      native_tx_id: txInclusion.nativeTxId,
      native_tx_compact_cbor: txInclusion.nativeTxCompactCbor,
      transactions_phas_root: txInclusion.transactionsPhasRoot,
    };
    const carriage: NativeTxInclusionCarriage = carriedByChunks
      ? {
          PublishedChunkInclusion: [
            {
              ...common,
              ordered_chunk_reference_input_indices: resolvedChunkIndices,
            },
          ],
        }
      : {
          RedeemerCarriedInclusion: [
            {
              ...common,
              tx_membership_proof: txInclusion.txMembershipProof,
              inclusion_proof_script_withdraw_redeemer_index:
                requireWithdrawalRedeemerIndex(
                  ctx,
                  phasRewardAddress,
                  `${STEP_LABEL} membership`,
                ),
            },
          ],
        };
    return Data.to({ Continue: [carriage] }, L2TxMistagStep01SpendRedeemer);
  }) satisfies BuildTxWithRedeemer;

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom(referenceInputs);
  const withMembership = carriedByChunks
    ? base.withdraw(chunkedVerifyRewardAddress, 0n, ((_ctx) =>
        chunkedMembershipClaimRedeemer({
          merkleRoot: txInclusion.transactionsPhasRoot,
          keyBytes: txInclusion.nativeTxId,
          valueBytes: txInclusion.nativeTxCompactCbor,
          orderedChunkReferenceInputIndices: resolvedChunkIndices,
        })) satisfies BuildTxWithRedeemer)
    : base.withdraw(
        phasRewardAddress,
        0n,
        encodeRawPhasMembershipProofRedeemer({
          root: txInclusion.transactionsPhasRoot,
          keyBytes: txInclusion.nativeTxId,
          valueBytes: txInclusion.nativeTxCompactCbor,
          membershipProofCbor: txInclusion.txMembershipProofCbor,
        }),
      );
  const completedTx = withMembership.pay
    .ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.WithdrawalValidator(
      carriedByChunks ? chunkedVerifyScript : phasMembershipScript,
    );
  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw l2TxMistagSubmitError("step-01 layout was not resolved.");
  }
  const txHash = await (await unsigned.sign.withWallet().complete()).submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    state,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
