import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  NonExistentInputStep04Datum,
  NonExistentInputStep04SpendRedeemer,
  Proof,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { PEXCLUDES_EXCLUSION_WITHDRAW_TITLE } from "./ne-submit-step-03.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPexcludesProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  phasMembershipRewardAddress,
  readJsonFile,
  type ResolvedProverSigner,
  resolveNonExistentInputDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";

export type NeSubmitStep04CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly txsNonMembershipProofPath: string;
  readonly awaitConfirmation?: boolean;
};

export type NeSubmitStep04Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAssetName: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly fourthStepAddress: string;
  readonly missingInputTxId: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly nonMembershipProofScriptRedeemerIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step04DatumWithState = NonExistentInputStep04Datum & {
  readonly data: NonNullable<NonExistentInputStep04Datum["data"]>;
};

const requireStep04Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step04DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, NonExistentInputStep04Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Step 04 input datum must carry missing-input-tx-id state data.",
    );
  }
  return datum as Step04DatumWithState;
};

export const neSubmitStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  txsNonMembershipProofCbor,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly txsNonMembershipProofCbor: string;
  readonly awaitConfirmation?: boolean;
}): Promise<NeSubmitStep04Result> => {
  const { nonExistentInputCategory, contracts } =
    await resolveNonExistentInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const steps = contracts.nonExistentInput.steps;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "non-existent-input step-04 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[3].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at non-existent-input step 04.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: nonExistentInputCategory.categoryId,
    categoryLabel: "non-existent-input",
  });
  const inputDatum = requireStep04Datum({ threadUtxo, signer });
  const proof = Data.from(txsNonMembershipProofCbor, Proof);

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const pexcludesScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PEXCLUDES_EXCLUSION_WITHDRAW_TITLE),
  };
  const pexcludesRewardAddress = phasMembershipRewardAddress(
    network,
    pexcludesScript,
  );
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const fraudProofAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [fraudProofUnit]: 1n,
  };
  const fraudProofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });

  let spendLayout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        fraudProofMintRedeemerIndex: bigint;
        nonMembershipProofScriptRedeemerIndex: bigint;
      }
    | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "non-existent-input step 04");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "non-existent-input step 04",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputMatches,
        "non-existent-input step 04 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        "non-existent-input step 04 fraud-proof",
      ),
      nonMembershipProofScriptRedeemerIndex: requireWithdrawalRedeemerIndex(
        ctx,
        pexcludesRewardAddress,
        "non-existent-input step 04 txs non-membership",
      ),
    };
    spendLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            non_membership_proof_in_txs: proof,
            non_membership_proof_script_redeemer_index:
              layout.nonMembershipProofScriptRedeemerIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
          },
        ],
      },
      NonExistentInputStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const fraudProofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      "non-existent-input step 04 fraud-proof mint",
    );
    const ctIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      "non-existent-input step 04 computation-thread burn",
    );
    computationThreadMintRedeemerIndex = ctIndex;
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: ctIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const computationThreadSuccessRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "non-existent-input step 04 computation-thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .withdraw(
      pexcludesRewardAddress,
      0n,
      encodeRawPexcludesProofRedeemer({
        root: inputDatum.data.blocks_transactions_root,
        keyBytes: inputDatum.data.missing_input_tx_id,
        nonMembershipProofCbor: txsNonMembershipProofCbor,
      }),
    )
    .mintAssets({ [threadToken.unit]: -1n }, computationThreadSuccessRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      fraudProofAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(steps[3].spendingScript)
    .attach.WithdrawalValidator(pexcludesScript)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript)
    .complete({ localUPLCEval: true });
  if (spendLayout === undefined || computationThreadMintRedeemerIndex === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve non-existent-input step 04 layout.",
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
    fraudProofOutRef: `${txHash}#${spendLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    fourthStepAddress: steps[3].spendingScriptAddress,
    missingInputTxId: inputDatum.data.missing_input_tx_id,
    inputIndex: Number(spendLayout.inputIndex),
    outputIndex: Number(spendLayout.outputIndex),
    nonMembershipProofScriptRedeemerIndex: Number(
      spendLayout.nonMembershipProofScriptRedeemerIndex,
    ),
    computationThreadMintRedeemerIndex: Number(
      computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(spendLayout.fraudProofMintRedeemerIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const neSubmitStep04FromFiles = async (
  config: NeSubmitStep04CliConfig,
): Promise<NeSubmitStep04Result> => {
  const [blueprint, deploymentInfo, proofJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.txsNonMembershipProofPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await neSubmitStep04({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    txsNonMembershipProofCbor: proofJson as string,
    awaitConfirmation: config.awaitConfirmation,
  });
};
