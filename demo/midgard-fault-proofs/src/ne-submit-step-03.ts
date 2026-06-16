import {
  NonExistentInputStep03Datum,
  NonExistentInputStep03SpendRedeemer,
  NonExistentInputStep04Datum,
  Proof,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import { nativeInputKeyHex } from "./ne-inputs.js";
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
  inputIndex,
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";

const STEP_03_OUTPUT_INDEX = 0n;
export const PEXCLUDES_WITHDRAW_TITLE = "pexcludes.exclusion.withdraw";
export const PEXCLUDES_REDEEMER_INDEX = 0n;

export type NeSubmitStep03CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly nonMembershipProofPath: string;
  readonly awaitConfirmation?: boolean;
};

export type NeSubmitStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  readonly fourthStepAddress: string;
  readonly missingInputTxId: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step03DatumWithState = NonExistentInputStep03Datum & {
  readonly data: NonNullable<NonExistentInputStep03Datum["data"]>;
};

const requireStep03Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step03DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, NonExistentInputStep03Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Step 03 input datum must carry the missing-output-reference state.",
    );
  }
  return datum as Step03DatumWithState;
};

export const neSubmitStep03 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  nonMembershipProofCbor,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nonMembershipProofCbor: string;
  readonly awaitConfirmation?: boolean;
}): Promise<NeSubmitStep03Result> => {
  const { nonExistentInputCategory, contracts } =
    await resolveNonExistentInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const steps = contracts.nonExistentInput.steps;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "step-03 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[2].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at non-existent-input step 03.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    doubleSpendCategoryId: nonExistentInputCategory.categoryId,
  });
  const inputDatum = requireStep03Datum({ threadUtxo, signer });
  const missingInput = inputDatum.data.missing_input;
  const missingInputTxId = missingInput.tx_id;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const spendInputIndex = inputIndex(threadUtxo, feeInput);
  const pexcludesScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PEXCLUDES_WITHDRAW_TITLE),
  };
  const redeemer = Data.to(
    {
      Continue: [
        {
          input_index: spendInputIndex,
          output_index: STEP_03_OUTPUT_INDEX,
          non_membership_proof_in_ledger: Data.from(
            nonMembershipProofCbor,
            Proof,
          ),
          non_membership_proof_script_redeemer_index: PEXCLUDES_REDEEMER_INDEX,
        },
      ],
    },
    NonExistentInputStep03SpendRedeemer,
  );
  const step04Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        missing_input_tx_id: missingInputTxId,
        blocks_transactions_root: inputDatum.data.blocks_transactions_root,
      },
    },
    NonExistentInputStep04Datum,
  );
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .withdraw(
      phasMembershipRewardAddress(network, pexcludesScript),
      0n,
      encodeRawPexcludesProofRedeemer({
        root: inputDatum.data.blocks_prev_utxos_root,
        keyBytes: nativeInputKeyHex(missingInput),
        nonMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      steps[3].spendingScriptAddress,
      { kind: "inline", value: step04Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(steps[2].spendingScript)
    .attach.WithdrawalValidator(pexcludesScript);

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
    nextThreadOutRef: `${txHash}#${STEP_03_OUTPUT_INDEX.toString()}`,
    computationThreadUnit: threadToken.unit,
    thirdStepAddress: steps[2].spendingScriptAddress,
    fourthStepAddress: steps[3].spendingScriptAddress,
    missingInputTxId,
    inputIndex: Number(spendInputIndex),
    outputIndex: Number(STEP_03_OUTPUT_INDEX),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const neSubmitStep03FromFiles = async (
  config: NeSubmitStep03CliConfig,
): Promise<NeSubmitStep03Result> => {
  const [blueprint, deploymentInfo, proofJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.nonMembershipProofPath),
    makeLucidForSubmit(config),
  ]);
  if (typeof proofJson !== "string") {
    throw new Error(
      "--non-membership-proof file must contain the proof CBOR hex string.",
    );
  }
  const signer = resolveProverSigner(config);
  return await neSubmitStep03({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    nonMembershipProofCbor: proofJson,
    awaitConfirmation: config.awaitConfirmation,
  });
};
