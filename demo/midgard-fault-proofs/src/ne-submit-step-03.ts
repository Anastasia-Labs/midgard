import {
  type MidgardTxInput,
  NonExistentInputStep03Datum,
  NonExistentInputStep03SpendRedeemer,
  NonExistentInputStep04Datum,
  Proof,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  CML,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

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
import { computationThreadOutputPredicate } from "./tx-layout.js";

export const PEXCLUDES_EXCLUSION_WITHDRAW_TITLE = "pexcludes.exclusion.withdraw";

/**
 * Encodes a `MidgardTxInput` as the node's ledger MPF key: the Cardano
 * `TransactionInput` CBOR (`0x82 ‖ txid ‖ index`), matching the on-chain
 * `encode_midgard_tx_input`. This is NOT `cbor.serialise(OutputReference)`.
 */
export const ledgerKeyBytesHex = (input: MidgardTxInput): string =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(input.tx_id),
      input.output_index,
    ).to_cbor_bytes(),
  ).toString("hex");

export type NeSubmitStep03CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly ledgerNonMembershipProofPath: string;
  readonly awaitConfirmation?: boolean;
};

export type NeSubmitStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  readonly fourthStepAddress: string;
  readonly missingInput: MidgardTxInput;
  readonly missingInputTxId: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly nonMembershipProofScriptRedeemerIndex: number;
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
    throw new Error("Step 03 input datum must carry missing-input state data.");
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
  ledgerNonMembershipProofCbor,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly ledgerNonMembershipProofCbor: string;
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
    label: "non-existent-input step-03 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[2].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at non-existent-input step 03.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: nonExistentInputCategory.categoryId,
    categoryLabel: "non-existent-input",
  });
  const inputDatum = requireStep03Datum({ threadUtxo, signer });
  const missingInput = inputDatum.data.missing_input;
  const keyBytes = ledgerKeyBytesHex(missingInput);
  const proof = Data.from(ledgerNonMembershipProofCbor, Proof);

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
  const step04Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        missing_input_tx_id: missingInput.tx_id,
        blocks_transactions_root: inputDatum.data.blocks_transactions_root,
      },
    },
    NonExistentInputStep04Datum,
  );
  const step04OutputMatches = computationThreadOutputPredicate({
    address: steps[3].spendingScriptAddress,
    datum: step04Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        nonMembershipProofScriptRedeemerIndex: bigint;
      }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "non-existent-input step 03");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "non-existent-input step 03",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step04OutputMatches,
        "non-existent-input step 03 output",
      ),
      nonMembershipProofScriptRedeemerIndex: requireWithdrawalRedeemerIndex(
        ctx,
        pexcludesRewardAddress,
        "non-existent-input step 03 ledger non-membership",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            non_membership_proof_in_ledger: proof,
            non_membership_proof_script_redeemer_index:
              layout.nonMembershipProofScriptRedeemerIndex,
          },
        ],
      },
      NonExistentInputStep03SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .withdraw(
      pexcludesRewardAddress,
      0n,
      encodeRawPexcludesProofRedeemer({
        root: inputDatum.data.blocks_prev_utxos_root,
        keyBytes,
        nonMembershipProofCbor: ledgerNonMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      steps[3].spendingScriptAddress,
      { kind: "inline", value: step04Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(steps[2].spendingScript)
    .attach.WithdrawalValidator(pexcludesScript)
    .complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve non-existent-input step 03 layout.",
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
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    thirdStepAddress: steps[2].spendingScriptAddress,
    fourthStepAddress: steps[3].spendingScriptAddress,
    missingInput,
    missingInputTxId: missingInput.tx_id,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    nonMembershipProofScriptRedeemerIndex: Number(
      resolvedLayout.nonMembershipProofScriptRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const neSubmitStep03FromFiles = async (
  config: NeSubmitStep03CliConfig,
): Promise<NeSubmitStep03Result> => {
  const [blueprint, deploymentInfo, proofJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.ledgerNonMembershipProofPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await neSubmitStep03({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    ledgerNonMembershipProofCbor: proofJson as string,
    awaitConfirmation: config.awaitConfirmation,
  });
};
