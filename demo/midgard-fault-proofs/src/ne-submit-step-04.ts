import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  NonExistentInputStep04Datum,
  NonExistentInputStep04SpendRedeemer,
  Proof,
  resolveMintPolicyRedeemerTxInfoIndex,
  resolveMintPolicyTxInfoRedeemerIndexFromPolicySet,
} from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  type LucidEvolution,
  type Network,
  type RedeemerBuilder,
  type Script,
  toUnit,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  PEXCLUDES_REDEEMER_INDEX,
  PEXCLUDES_WITHDRAW_TITLE,
} from "./ne-submit-step-03.js";
import {
  compareOutRefs,
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPexcludesProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  makeLucidForSubmit,
  outRefLabel,
  outRefsEqual,
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

const STEP_04_INITIAL_OUTPUT_INDEX = 0n;
const STEP_04_SCRIPT_SPEND_REDEEMER_COUNT = 1;

export type NeSubmitStep04CliConfig = SubmitProviderConfig & {
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

export type NeSubmitStep04Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly computationThreadUnit: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly missingInputTxId: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step04DatumWithState = NonExistentInputStep04Datum & {
  readonly data: NonNullable<NonExistentInputStep04Datum["data"]>;
};

type Step04RedeemerLayout = {
  readonly outputIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

type Step04ResolvedLayout = Step04RedeemerLayout & {
  readonly inputIndex: bigint;
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
      "Step 04 input datum must carry the missing-input-tx-id state.",
    );
  }
  return datum as Step04DatumWithState;
};

const findInputIndex = (tx: CML.Transaction, target: UTxO): bigint => {
  const inputs = tx.body().inputs();
  const orderedInputs = Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  }).sort(compareOutRefs);
  const idx = orderedInputs.findIndex((input) => outRefsEqual(input, target));
  if (idx < 0) {
    throw new Error(`Draft transaction does not spend ${outRefLabel(target)}.`);
  }
  return BigInt(idx);
};

const findFraudProofOutputIndex = ({
  tx,
  fraudProofAddress,
  fraudProofUnit,
}: {
  readonly tx: CML.Transaction;
  readonly fraudProofAddress: string;
  readonly fraudProofUnit: string;
}): bigint => {
  const outputs = tx.body().outputs();
  // The fraud-proof token is a unique NFT, so address + unit identifies the
  // output unambiguously. (The inline datum is re-encoded to canonical
  // definite-length form by CML, so a raw string compare is unreliable.)
  const matches: number[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address === fraudProofAddress &&
      (output.assets[fraudProofUnit] ?? 0n) === 1n
    ) {
      matches.push(index);
    }
  }
  if (matches.length !== 1) {
    throw new Error(
      `Draft transaction must contain exactly one fraud-proof output for ${fraudProofUnit}; found ${matches.length.toString()}.`,
    );
  }
  return BigInt(matches[0]!);
};

const makeStep04SpendRedeemer = ({
  outputIndex,
  fraudProofMintRedeemerIndex,
  nonMembershipProofCbor,
}: {
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
  readonly nonMembershipProofCbor: string;
}): RedeemerBuilder => ({
  kind: "self",
  makeRedeemer: (inputIndex) =>
    Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            non_membership_proof_in_txs: Data.from(nonMembershipProofCbor, Proof),
            non_membership_proof_script_redeemer_index: PEXCLUDES_REDEEMER_INDEX,
            fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
          },
        ],
      },
      NonExistentInputStep04SpendRedeemer,
    ),
});

const makeFraudProofMintRedeemer = ({
  threadUtxo,
  computationThreadAssetName,
  computationThreadMintRedeemerIndex,
}: {
  readonly threadUtxo: UTxO;
  readonly computationThreadAssetName: string;
  readonly computationThreadMintRedeemerIndex: bigint;
}): RedeemerBuilder => ({
  kind: "selected",
  inputs: [threadUtxo],
  makeRedeemer: () =>
    Data.to(
      {
        computation_thread_token_asset_name: computationThreadAssetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    ),
});

const makeComputationThreadSuccessRedeemer = (
  computationThreadAssetName: string,
): string =>
  Data.to(
    { Success: { burning_token_asset_name: computationThreadAssetName } },
    FraudProofComputationThreadRedeemer,
  );

export const neSubmitStep04 = async ({
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
    label: "step-04 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[3].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at non-existent-input step 04.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    doubleSpendCategoryId: nonExistentInputCategory.categoryId,
  });
  const inputDatum = requireStep04Datum({ threadUtxo, signer });
  const missingInputTxId = inputDatum.data.missing_input_tx_id;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
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
  const mintPolicyIds = [
    contracts.computationThread.policyId,
    contracts.fraudProof.policyId,
  ];
  const computationThreadSuccessRedeemer = makeComputationThreadSuccessRedeemer(
    threadToken.assetName,
  );
  const pexcludesScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PEXCLUDES_WITHDRAW_TITLE),
  };
  const pexcludesRedeemer = encodeRawPexcludesProofRedeemer({
    root: inputDatum.data.blocks_transactions_root,
    keyBytes: missingInputTxId,
    nonMembershipProofCbor,
  });

  const initialLayout: Step04RedeemerLayout = {
    outputIndex: STEP_04_INITIAL_OUTPUT_INDEX,
    computationThreadMintRedeemerIndex:
      resolveMintPolicyTxInfoRedeemerIndexFromPolicySet({
        policyIds: mintPolicyIds,
        targetPolicyId: contracts.computationThread.policyId,
        precedingSpendRedeemerCount: STEP_04_SCRIPT_SPEND_REDEEMER_COUNT,
      }),
    fraudProofMintRedeemerIndex:
      resolveMintPolicyTxInfoRedeemerIndexFromPolicySet({
        policyIds: mintPolicyIds,
        targetPolicyId: contracts.fraudProof.policyId,
        precedingSpendRedeemerCount: STEP_04_SCRIPT_SPEND_REDEEMER_COUNT,
      }),
  };

  const makeStep04Tx = (layout: Step04RedeemerLayout): TxBuilder =>
    lucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom(
        [threadUtxo],
        makeStep04SpendRedeemer({
          outputIndex: layout.outputIndex,
          fraudProofMintRedeemerIndex: layout.fraudProofMintRedeemerIndex,
          nonMembershipProofCbor,
        }),
      )
      .withdraw(
        phasMembershipRewardAddress(network, pexcludesScript),
        0n,
        pexcludesRedeemer,
      )
      .mintAssets({ [threadToken.unit]: -1n }, computationThreadSuccessRedeemer)
      .mintAssets(
        { [fraudProofUnit]: 1n },
        makeFraudProofMintRedeemer({
          threadUtxo,
          computationThreadAssetName: threadToken.assetName,
          computationThreadMintRedeemerIndex:
            layout.computationThreadMintRedeemerIndex,
        }),
      )
      .pay.ToContract(
        contracts.fraudProof.spendingScriptAddress,
        { kind: "inline", value: fraudProofDatum },
        fraudProofAssets,
      )
      .addSignerKey(signer.paymentKeyHash)
      .attach.SpendingValidator(steps[3].spendingScript)
      .attach.MintingPolicy(contracts.computationThread.mintingScript)
      .attach.MintingPolicy(contracts.fraudProof.mintingScript)
      .attach.WithdrawalValidator(pexcludesScript);

  const draft = await makeStep04Tx(initialLayout).complete({
    localUPLCEval: true,
  });
  const draftTx = draft.toTransaction();
  const resolvedLayout: Step04ResolvedLayout = {
    inputIndex: findInputIndex(draftTx, threadUtxo),
    outputIndex: findFraudProofOutputIndex({
      tx: draftTx,
      fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
      fraudProofUnit,
    }),
    computationThreadMintRedeemerIndex: resolveMintPolicyRedeemerTxInfoIndex({
      tx: draftTx,
      policyIds: mintPolicyIds,
      targetPolicyId: contracts.computationThread.policyId,
    }),
    fraudProofMintRedeemerIndex: resolveMintPolicyRedeemerTxInfoIndex({
      tx: draftTx,
      policyIds: mintPolicyIds,
      targetPolicyId: contracts.fraudProof.policyId,
    }),
  };

  const unsigned = await makeStep04Tx(resolvedLayout).complete({
    localUPLCEval: true,
  });
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
    fraudProofOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    missingInputTxId,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      resolvedLayout.computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      resolvedLayout.fraudProofMintRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const neSubmitStep04FromFiles = async (
  config: NeSubmitStep04CliConfig,
): Promise<NeSubmitStep04Result> => {
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
  return await neSubmitStep04({
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
