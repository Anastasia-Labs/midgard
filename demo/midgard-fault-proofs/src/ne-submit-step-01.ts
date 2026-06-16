import {
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  NonExistentInputStep01SpendRedeemer,
  NonExistentInputStep02Datum,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  compareUtxoOutRefs,
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  phasMembershipRewardAddress,
  readJsonFile,
  referenceInputIndex,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveNonExistentInputDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  inputIndex,
  parseSubmitStep01TxInclusion,
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  PHAS_WITHDRAW_REDEEMER_INDEX,
  requireComputationThreadToken,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "./submit-step-01.js";

const STEP_01_OUTPUT_INDEX = 0n;

// The non-existent-input proof commits the bad transaction by the node's native
// transaction root (same inclusion path as double-spend), so the tx-inclusion
// material is identical to `submit-step-01`'s.
export type NeStep01TxInclusion = SubmitStep01TxInclusion;
export const parseNeStep01TxInclusion = parseSubmitStep01TxInclusion;

export type NeSubmitStep01CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusionPath: string;
  readonly awaitConfirmation?: boolean;
};

export type NeSubmitStep01Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly firstStepAddress: string;
  readonly secondStepAddress: string;
  readonly nativeTxId: string;
  readonly badTxInputsHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const neSubmitStep01 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: NeStep01TxInclusion;
  readonly awaitConfirmation?: boolean;
}): Promise<NeSubmitStep01Result> => {
  requireNativeTxMatchesCompactCbor(txInclusion);
  const resolved = await resolveNonExistentInputDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireStateQueueMint: true,
  });
  const { nonExistentInputCategory, hubOraclePolicyId, contracts } = resolved;
  const stateQueuePolicyId = resolved.stateQueuePolicyId!;
  const steps = contracts.nonExistentInput.steps;

  const [threadUtxo, hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
      label: "step-01 computation-thread UTxO",
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
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: "state-queue block UTxO",
    }),
  ]);
  if (threadUtxo.address !== steps[0].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at non-existent-input step 01.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    doubleSpendCategoryId: nonExistentInputCategory.categoryId,
  });
  const stateQueueHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (stateQueueHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw new Error(
      `State-queue block header hash ${stateQueueHeaderHash} does not match computation-thread header hash ${threadToken.fraudulentHeaderHash}.`,
    );
  }

  const stateQueueNodeView = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
  );
  const header = await Effect.runPromise(
    getHeaderFromStateQueueDatum(stateQueueNodeView),
  );

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const sortedReferenceInputs = [hubOracleUtxo, stateQueueBlockUtxo].sort(
    compareUtxoOutRefs,
  );
  const spendInputIndex = inputIndex(threadUtxo, feeInput);
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const redeemer = Data.to(
    {
      Continue: [
        {
          input_index: spendInputIndex,
          output_index: STEP_01_OUTPUT_INDEX,
          hub_ref_input_index: referenceInputIndex(
            sortedReferenceInputs,
            hubOracleUtxo,
          ),
          state_queue_node_ref_input_index: referenceInputIndex(
            sortedReferenceInputs,
            stateQueueBlockUtxo,
          ),
          native_tx_id: txInclusion.nativeTxId,
          native_tx_compact_cbor: txInclusion.nativeTxCompactCbor,
          tx_membership_proof: txInclusion.txMembershipProof,
          inclusion_proof_script_withdraw_redeemer_index:
            PHAS_WITHDRAW_REDEEMER_INDEX,
        },
      ],
    },
    NonExistentInputStep01SpendRedeemer,
  );
  const step02Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bad_tx_inputs_hash: txInclusion.nativeTx.body.spend_inputs_hash,
        blocks_prev_utxos_root: header.prevUtxosRoot,
        blocks_transactions_root: header.transactionsRoot,
      },
    },
    NonExistentInputStep02Datum,
  );
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(sortedReferenceInputs)
    .withdraw(
      phasMembershipRewardAddress(network, phasMembershipScript),
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: header.transactionsRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.nativeTxCompactCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(steps[0].spendingScript)
    .attach.WithdrawalValidator(phasMembershipScript);

  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${STEP_01_OUTPUT_INDEX.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    firstStepAddress: steps[0].spendingScriptAddress,
    secondStepAddress: steps[1].spendingScriptAddress,
    nativeTxId: txInclusion.nativeTxId,
    badTxInputsHash: txInclusion.nativeTx.body.spend_inputs_hash,
    inputIndex: Number(spendInputIndex),
    outputIndex: Number(STEP_01_OUTPUT_INDEX),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const neSubmitStep01FromFiles = async (
  config: NeSubmitStep01CliConfig,
): Promise<NeSubmitStep01Result> => {
  const [blueprint, deploymentInfo, txInclusionJson, lucid] = await Promise.all(
    [
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.txInclusionPath),
      makeLucidForSubmit(config),
    ],
  );
  const signer = resolveProverSigner(config);
  return await neSubmitStep01({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    txInclusion: parseNeStep01TxInclusion(txInclusionJson),
    awaitConfirmation: config.awaitConfirmation,
  });
};
