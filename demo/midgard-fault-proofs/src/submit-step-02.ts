import {
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  type UTxO,
  credentialToAddress,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import {
  DoubleSpendStep02Datum,
  DoubleSpendStep02SpendRedeemer,
  DoubleSpendStep03Datum,
  HUB_ORACLE_ASSET_NAME,
  buildDoubleSpendFaultProofContracts,
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { parseContractDeploymentInfo } from "./inspect-contracts.js";
import {
  compareOutRefs,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  makeLucidForSubmitInit,
  outRefLabel,
  parseOutRef,
  phasMembershipRewardAddress,
  readJsonFile,
  referenceInputIndex,
  requireDeploymentScriptHash,
  requireSingletonUtxo,
  resolveFraudulentHeaderHash,
  resolveProverSigner,
  type ResolvedProverSigner,
  type SubmitProviderConfig,
} from "./submit-init.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  PHAS_WITHDRAW_REDEEMER_INDEX,
  inputIndex,
  parseSubmitStep01TxInclusion,
  parsedOutRefFromUtxo,
  requireComputationThreadToken,
  requireMatchingScriptHash,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "./submit-step-01.js";

const STEP_02_OUTPUT_INDEX = 0n;

export type SubmitStep02CliConfig = SubmitProviderConfig & {
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

export type SubmitStep02TxInclusion = SubmitStep01TxInclusion;

export type SubmitStep02Result = {
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
  readonly secondStepAddress: string;
  readonly thirdStepAddress: string;
  readonly verifiedTx1Id: string;
  readonly nativeTx2Id: string;
  readonly verifiedTx1SpendInputsHash: string;
  readonly verifiedTx2SpendInputsHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const parseSubmitStep02TxInclusion = parseSubmitStep01TxInclusion;

type Step02DatumWithState = DoubleSpendStep02Datum & {
  readonly data: NonNullable<DoubleSpendStep02Datum["data"]>;
};

const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, DoubleSpendStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error("Step 02 input datum must carry verified tx1 state data.");
  }
  return datum as Step02DatumWithState;
};

export const submitStep02 = async ({
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
  readonly txInclusion: SubmitStep02TxInclusion;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitStep02Result> => {
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const catalogue = parsedDeploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;
  const doubleSpendCategory = catalogue?.categories.doubleSpend;
  if (doubleSpendCategory === undefined) {
    throw new Error(
      "Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.categories.doubleSpend.",
    );
  }

  const stateQueuePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "stateQueueMint",
  );
  const fraudProofCataloguePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofCatalogueMint",
  );
  const hubOraclePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "hubOracleMint",
  );
  const deployedFraudProofPolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofMint",
  );
  const deployedDoubleSpendHash = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofDoubleSpend",
  );

  const contracts = await Effect.runPromise(
    buildDoubleSpendFaultProofContracts({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId,
      fraudProofCataloguePolicyId,
    }),
  );
  requireMatchingScriptHash({
    label: "fraudProofMint policy",
    deployed: deployedFraudProofPolicyId,
    derived: contracts.fraudProof.policyId,
  });
  requireMatchingScriptHash({
    label: "fraudProofDoubleSpend step-01 script",
    deployed: deployedDoubleSpendHash,
    derived: contracts.doubleSpend.firstStep.spendingScriptHash,
  });

  const parsedThreadOutRef = parseOutRef(threadOutRef, "--thread-out-ref");
  const parsedStateQueueBlockOutRef = parseOutRef(
    stateQueueBlockOutRef,
    "--state-queue-block-out-ref",
  );
  const [threadUtxo, hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parsedThreadOutRef,
      label: "step-02 computation-thread UTxO",
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
      outRef: parsedStateQueueBlockOutRef,
      label: "state-queue block UTxO",
    }),
  ]);
  if (threadUtxo.address !== contracts.doubleSpend.steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at double-spend step 02.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    doubleSpendCategoryId: doubleSpendCategory.categoryId,
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });
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
  requireNativeTxMatchesCompactCbor(txInclusion);
  if (inputDatum.data.verified_tx1_id === txInclusion.nativeTxId) {
    throw new Error(
      "--tx-inclusion.nativeTxId must differ from the verified tx1 id in the step-02 datum.",
    );
  }

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const sortedReferenceInputs = [hubOracleUtxo, stateQueueBlockUtxo].sort(
    (left, right) =>
      compareOutRefs(parsedOutRefFromUtxo(left), parsedOutRefFromUtxo(right)),
  );
  const spendInputIndex = inputIndex(threadUtxo, feeInput);
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const continueArgs = {
    input_index: spendInputIndex,
    output_index: STEP_02_OUTPUT_INDEX,
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
  };
  const redeemer = Data.to(
    {
      Continue: [continueArgs],
    },
    DoubleSpendStep02SpendRedeemer,
  );
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        verified_tx1_spend_inputs_hash:
          inputDatum.data.verified_tx1_spend_inputs_hash,
        verified_tx2_spend_inputs_hash:
          txInclusion.nativeTx.body.spend_inputs_hash,
      },
    },
    DoubleSpendStep03Datum,
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
      contracts.doubleSpend.steps[2].spendingScriptAddress,
      {
        kind: "inline",
        value: step03Datum,
      },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(contracts.doubleSpend.steps[1].spendingScript)
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
    nextThreadOutRef: `${txHash}#${STEP_02_OUTPUT_INDEX.toString()}`,
    stateQueueBlockOutRef,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: contracts.doubleSpend.steps[1].spendingScriptAddress,
    thirdStepAddress: contracts.doubleSpend.steps[2].spendingScriptAddress,
    verifiedTx1Id: inputDatum.data.verified_tx1_id,
    nativeTx2Id: txInclusion.nativeTxId,
    verifiedTx1SpendInputsHash:
      inputDatum.data.verified_tx1_spend_inputs_hash,
    verifiedTx2SpendInputsHash: txInclusion.nativeTx.body.spend_inputs_hash,
    inputIndex: Number(spendInputIndex),
    outputIndex: Number(STEP_02_OUTPUT_INDEX),
    hubOracleRefInputIndex: Number(
      referenceInputIndex(sortedReferenceInputs, hubOracleUtxo),
    ),
    stateQueueNodeRefInputIndex: Number(
      referenceInputIndex(sortedReferenceInputs, stateQueueBlockUtxo),
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitStep02FromFiles = async (
  config: SubmitStep02CliConfig,
): Promise<SubmitStep02Result> => {
  const [blueprint, deploymentInfo, txInclusionJson, lucid] =
    await Promise.all([
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.txInclusionPath),
      makeLucidForSubmitInit(config),
    ]);
  const signer = resolveProverSigner(config);
  return await submitStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    txInclusion: parseSubmitStep02TxInclusion(txInclusionJson),
    awaitConfirmation: config.awaitConfirmation,
  });
};
