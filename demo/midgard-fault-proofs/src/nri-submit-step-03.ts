import {
  type MidgardTxInput,
  NoReferenceInputStep03Datum,
  NoReferenceInputStep03SpendRedeemer,
  NoReferenceInputStep04Datum,
  Proof,
  requireInputIndex,
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
  type UTxO,
} from "@lucid-evolution/lucid";

// The ledger MPF key encoding and the pexcludes reward-address title are generic
// across native fault proofs; reuse the non-existent-input definitions rather
// than duplicating them (a duplicate re-export would collide in the barrel).
import {
  ledgerKeyBytesHex,
  PEXCLUDES_EXCLUSION_WITHDRAW_TITLE,
} from "./ne-submit-step-03.js";
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
  resolveNoReferenceInputDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";

export type NriSubmitStep03CliConfig = SubmitProviderConfig & {
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

export type NriSubmitStep03Result = {
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
  readonly missingReferenceInput: MidgardTxInput;
  readonly missingReferenceInputTxId: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly nonMembershipProofScriptRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step03DatumWithState = NoReferenceInputStep03Datum & {
  readonly data: NonNullable<NoReferenceInputStep03Datum["data"]>;
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
  const datum = Data.from(threadUtxo.datum, NoReferenceInputStep03Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Step 03 input datum must carry missing-reference-input state data.",
    );
  }
  return datum as Step03DatumWithState;
};

export const nriSubmitStep03 = async ({
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
}): Promise<NriSubmitStep03Result> => {
  const { noReferenceInputCategory, contracts } =
    await resolveNoReferenceInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const steps = contracts.noReferenceInput.steps;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "no-reference-input step-03 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[2].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at no-reference-input step 03.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: noReferenceInputCategory.categoryId,
    categoryLabel: "no-reference-input",
  });
  const inputDatum = requireStep03Datum({ threadUtxo, signer });
  const missingReferenceInput = inputDatum.data.missing_reference_input;
  const keyBytes = ledgerKeyBytesHex(missingReferenceInput);
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
        missing_reference_input_tx_id: missingReferenceInput.tx_id,
        blocks_transactions_root: inputDatum.data.blocks_transactions_root,
      },
    },
    NoReferenceInputStep04Datum,
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
    requireOwnSpendPurpose(ctx, threadUtxo, "no-reference-input step 03");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "no-reference-input step 03",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step04OutputMatches,
        "no-reference-input step 03 output",
      ),
      nonMembershipProofScriptRedeemerIndex: requireWithdrawalRedeemerIndex(
        ctx,
        pexcludesRewardAddress,
        "no-reference-input step 03 ledger non-membership",
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
      NoReferenceInputStep03SpendRedeemer,
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
      "BuildTxWithRedeemer did not resolve no-reference-input step 03 layout.",
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
    missingReferenceInput,
    missingReferenceInputTxId: missingReferenceInput.tx_id,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    nonMembershipProofScriptRedeemerIndex: Number(
      resolvedLayout.nonMembershipProofScriptRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const nriSubmitStep03FromFiles = async (
  config: NriSubmitStep03CliConfig,
): Promise<NriSubmitStep03Result> => {
  const [blueprint, deploymentInfo, proofJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.ledgerNonMembershipProofPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await nriSubmitStep03({
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
