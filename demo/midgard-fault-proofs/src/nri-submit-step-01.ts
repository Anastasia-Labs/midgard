import {
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  NoReferenceInputStep01SpendRedeemer,
  NoReferenceInputStep02Datum,
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
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  phasMembershipRewardAddress,
  readJsonFile,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveNoReferenceInputDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  parseSubmitStep01TxInclusion,
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireComputationThreadToken,
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";

// The no-reference-input proof commits the bad transaction by the node's native
// transaction root (the same inclusion path as double-spend / non-existent-
// input), so the tx-inclusion material is identical to `submit-step-01`'s. Only
// the field lifted into step 02 differs: the reference-inputs hash, not the
// spend-inputs hash.
export type NriStep01TxInclusion = SubmitStep01TxInclusion;
export const parseNriStep01TxInclusion = parseSubmitStep01TxInclusion;

export type NriSubmitStep01CliConfig = SubmitProviderConfig & {
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

export type NriSubmitStep01Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly firstStepAddress: string;
  readonly secondStepAddress: string;
  readonly nativeTxId: string;
  readonly badTxReferenceInputsHash: string;
  readonly blocksPrevUtxosRoot: string;
  readonly blocksTransactionsRoot: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type NriStep01Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

export const nriSubmitStep01 = async ({
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
  readonly txInclusion: NriStep01TxInclusion;
  readonly awaitConfirmation?: boolean;
}): Promise<NriSubmitStep01Result> => {
  requireNativeTxMatchesCompactCbor(txInclusion);
  const resolvedDeployment = await resolveNoReferenceInputDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireStateQueueMint: true,
  });
  const { noReferenceInputCategory, hubOraclePolicyId, contracts } =
    resolvedDeployment;
  const stateQueuePolicyId = resolvedDeployment.stateQueuePolicyId!;
  const steps = contracts.noReferenceInput.steps;

  const [threadUtxo, hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
      label: "no-reference-input step-01 computation-thread UTxO",
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
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at no-reference-input step 01.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: noReferenceInputCategory.categoryId,
    categoryLabel: "no-reference-input",
  });
  requireInitialStepDatum({ threadUtxo, signer });
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
  const referenceInputs = [hubOracleUtxo, stateQueueBlockUtxo];
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const step02Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bad_tx_reference_inputs_hash:
          txInclusion.nativeTx.body.reference_inputs_hash,
        blocks_prev_utxos_root: header.prevUtxosRoot,
        blocks_transactions_root: txInclusion.transactionsPhasRoot,
      },
    },
    NoReferenceInputStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: NriStep01Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "no-reference-input step 01");
    const layout: NriStep01Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "no-reference-input step 01",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        "no-reference-input step 01 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "no-reference-input step 01 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "no-reference-input step 01 state-queue node",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            hub_ref_input_index: layout.hubOracleRefInputIndex,
            state_queue_node_ref_input_index:
              layout.stateQueueNodeRefInputIndex,
            native_tx_id: txInclusion.nativeTxId,
            native_tx_compact_cbor: txInclusion.nativeTxCompactCbor,
            transactions_phas_root: txInclusion.transactionsPhasRoot,
            tx_membership_proof: txInclusion.txMembershipProof,
            inclusion_proof_script_withdraw_redeemer_index:
              requireWithdrawalRedeemerIndex(
                ctx,
                phasRewardAddress,
                "no-reference-input step 01 PHAS membership",
              ),
          },
        ],
      },
      NoReferenceInputStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs)
    .withdraw(
      phasRewardAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
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
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve no-reference-input step 01 layout.",
    );
  }
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
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    stateQueueBlockOutRef,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    firstStepAddress: steps[0].spendingScriptAddress,
    secondStepAddress: steps[1].spendingScriptAddress,
    nativeTxId: txInclusion.nativeTxId,
    badTxReferenceInputsHash: txInclusion.nativeTx.body.reference_inputs_hash,
    blocksPrevUtxosRoot: header.prevUtxosRoot,
    blocksTransactionsRoot: txInclusion.transactionsPhasRoot,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    hubOracleRefInputIndex: Number(resolvedLayout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(
      resolvedLayout.stateQueueNodeRefInputIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const nriSubmitStep01FromFiles = async (
  config: NriSubmitStep01CliConfig,
): Promise<NriSubmitStep01Result> => {
  const [blueprint, deploymentInfo, txInclusionJson, lucid] = await Promise.all(
    [
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.txInclusionPath),
      makeLucidForSubmit(config),
    ],
  );
  const signer = resolveProverSigner(config);
  return await nriSubmitStep01({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    txInclusion: parseNriStep01TxInclusion(txInclusionJson),
    awaitConfirmation: config.awaitConfirmation,
  });
};
