import {
  CML,
  Data,
  coreToTxOutput,
  toUnit,
  type LucidEvolution,
  type Network,
  type RedeemerBuilder,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import {
  DoubleSpendStep04Datum,
  DoubleSpendStep04SpendRedeemer,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  buildDoubleSpendFaultProofContracts,
  parseFaultProofBlueprint,
  resolveMintPolicyRedeemerTxInfoIndex,
  resolveMintPolicyTxInfoRedeemerIndexFromPolicySet,
  type MidgardTxInput,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { parseContractDeploymentInfo } from "./inspect-contracts.js";
import {
  compareOutRefs as compareParsedOutRefs,
  fetchUtxoByOutRef,
  makeLucidForSubmitInit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  referenceInputIndex,
  requireDeploymentScriptHash,
  resolveProverSigner,
  type ResolvedProverSigner,
  type SubmitProviderConfig,
} from "./submit-init.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  parsedOutRefFromUtxo,
  parseInteger,
  requireComputationThreadToken,
  requireMatchingScriptHash,
  selectFeeInput,
} from "./submit-step-01.js";
import {
  hashSpendInputCbors,
  parseSpendInputCbors,
} from "./submit-step-03.js";
import {
  ensureSpendInputsReferenceWitness,
  excludeUtxo,
  spendInputsWitnessFromCbors,
} from "./spend-input-witness.js";

const STEP_04_INITIAL_OUTPUT_INDEX = 0n;
const STEP_04_SCRIPT_SPEND_REDEEMER_COUNT = 1;

export type SubmitStep04CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly tx2InputsPath: string;
  readonly doubleSpentInputIndex: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitStep04Result = {
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
  readonly verifiedTx2SpendInputsHash: string;
  readonly doubleSpentInputIndex: number;
  readonly doubleSpentInput: MidgardTxInput;
  readonly doubleSpentInputCbor: string;
  readonly tx2SpendInputsWitnessOutRef: string;
  readonly tx2SpendInputsWitnessCreated: boolean;
  readonly tx2SpendInputsRefInputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const parseSubmitStep04Tx2Inputs = (
  value: unknown,
): readonly string[] => parseSpendInputCbors(value, "--tx2-inputs");

const parseDoubleSpentInputIndex = (
  value: string,
  inputCount: number,
): bigint => {
  const index = parseInteger(value, "--double-spent-input-index");
  if (index >= BigInt(inputCount)) {
    throw new Error(
      `--double-spent-input-index ${index.toString()} is out of bounds for ${inputCount.toString()} tx2 inputs.`,
    );
  }
  if (index > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error("--double-spent-input-index exceeds the safe integer range.");
  }
  return index;
};

type Step04DatumWithState = DoubleSpendStep04Datum & {
  readonly data: NonNullable<DoubleSpendStep04Datum["data"]>;
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
  const datum = Data.from(threadUtxo.datum, DoubleSpendStep04Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error("Step 04 input datum must carry verified tx2 state data.");
  }
  return datum as Step04DatumWithState;
};

const sameMidgardTxInput = (
  left: MidgardTxInput,
  right: MidgardTxInput,
): boolean =>
  left.tx_id === right.tx_id && left.output_index === right.output_index;

const compareCmlOutRefs = (
  left: { readonly txHash: string; readonly outputIndex: number },
  right: { readonly txHash: string; readonly outputIndex: number },
): number => {
  const txHashOrder = Buffer.from(left.txHash, "hex").compare(
    Buffer.from(right.txHash, "hex"),
  );
  if (txHashOrder !== 0) {
    return txHashOrder;
  }
  return left.outputIndex - right.outputIndex;
};

const findInputIndex = (tx: CML.Transaction, target: UTxO): bigint => {
  const inputs = tx.body().inputs();
  const orderedInputs = Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  }).sort(compareCmlOutRefs);
  const inputIndex = orderedInputs.findIndex(
    (input) =>
      input.txHash === target.txHash &&
      input.outputIndex === target.outputIndex,
  );
  if (inputIndex < 0) {
    throw new Error(`Draft transaction does not spend ${outRefLabel(target)}.`);
  }
  return BigInt(inputIndex);
};

const findFraudProofOutputIndex = ({
  tx,
  fraudProofAddress,
  fraudProofUnit,
  fraudProofDatum,
}: {
  readonly tx: CML.Transaction;
  readonly fraudProofAddress: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
}): bigint => {
  const outputs = tx.body().outputs();
  const matches: number[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address === fraudProofAddress &&
      output.datum === fraudProofDatum &&
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
  tx2SpendInputsRefInputIndex,
  doubleSpentInputIndex,
}: {
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
  readonly tx2SpendInputsRefInputIndex: bigint;
  readonly doubleSpentInputIndex: bigint;
}): RedeemerBuilder => ({
  kind: "self",
  makeRedeemer: (inputIndex) =>
    Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
            tx2_spend_inputs_ref_input_index: tx2SpendInputsRefInputIndex,
            double_spent_input_index: doubleSpentInputIndex,
          },
        ],
      },
      DoubleSpendStep04SpendRedeemer,
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
    {
      Success: { burning_token_asset_name: computationThreadAssetName },
    },
    FraudProofComputationThreadRedeemer,
  );

export const submitStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  tx2SpendInputCbors,
  doubleSpentInputIndex,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly tx2SpendInputCbors: readonly string[];
  readonly doubleSpentInputIndex: bigint;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitStep04Result> => {
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const catalogue = parsedDeploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;
  const doubleSpendCategory = catalogue?.categories.doubleSpend;
  if (doubleSpendCategory === undefined) {
    throw new Error(
      "Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.categories.doubleSpend.",
    );
  }

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
  const deployedFraudProofSpendHash = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofSpend",
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
    label: "fraudProofSpend script",
    deployed: deployedFraudProofSpendHash,
    derived: contracts.fraudProof.spendingScriptHash,
  });
  requireMatchingScriptHash({
    label: "fraudProofDoubleSpend step-01 script",
    deployed: deployedDoubleSpendHash,
    derived: contracts.doubleSpend.firstStep.spendingScriptHash,
  });

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "step-04 computation-thread UTxO",
  });
  if (threadUtxo.address !== contracts.doubleSpend.steps[3].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at double-spend step 04.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    doubleSpendCategoryId: doubleSpendCategory.categoryId,
  });
  const inputDatum = requireStep04Datum({ threadUtxo, signer });
  const tx2SpendInputsHash = hashSpendInputCbors(tx2SpendInputCbors);
  if (tx2SpendInputsHash !== inputDatum.data.verified_tx2_spend_inputs_hash) {
    throw new Error(
      `--tx2-inputs hash mismatch: provided preimage hashes to ${tx2SpendInputsHash}, expected ${inputDatum.data.verified_tx2_spend_inputs_hash}.`,
    );
  }
  if (doubleSpentInputIndex >= BigInt(tx2SpendInputCbors.length)) {
    throw new Error(
      `doubleSpentInputIndex ${doubleSpentInputIndex.toString()} is out of bounds for ${tx2SpendInputCbors.length.toString()} tx2 inputs.`,
    );
  }
  if (doubleSpentInputIndex > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error("doubleSpentInputIndex exceeds the safe integer range.");
  }
  const tx2SpendInputsWitness = spendInputsWitnessFromCbors(
    tx2SpendInputCbors,
    "--tx2-inputs",
  );
  const doubleSpentInputCbor = tx2SpendInputCbors[Number(doubleSpentInputIndex)]!;
  const doubleSpentInput =
    tx2SpendInputsWitness.inputs[Number(doubleSpentInputIndex)]!;
  if (!sameMidgardTxInput(doubleSpentInput, inputDatum.data.double_spent_input)) {
    throw new Error(
      `--tx2-inputs[${doubleSpentInputIndex.toString()}] does not match the double-spent input carried by step 04 datum.`,
    );
  }

  signer.selectWallet(lucid);
  const tx2SpendInputsReferenceWitness =
    await ensureSpendInputsReferenceWitness({
      lucid,
      address: signer.address,
      paymentKeyHash: signer.paymentKeyHash,
      witness: tx2SpendInputsWitness,
      awaitConfirmation,
    });
  const sortedReferenceInputs = [
    tx2SpendInputsReferenceWitness.utxo,
  ].sort((left, right) =>
    compareParsedOutRefs(
      parsedOutRefFromUtxo(left),
      parsedOutRefFromUtxo(right),
    ),
  );
  const tx2SpendInputsRefInputIndex = referenceInputIndex(
    sortedReferenceInputs,
    tx2SpendInputsReferenceWitness.utxo,
  );
  const walletUtxosWithoutWitness = excludeUtxo(
    await lucid.wallet().getUtxos(),
    tx2SpendInputsReferenceWitness.utxo,
  );
  const feeCandidates =
    tx2SpendInputsReferenceWitness.spentFeeInput === undefined
      ? walletUtxosWithoutWitness
      : excludeUtxo(
          walletUtxosWithoutWitness,
          tx2SpendInputsReferenceWitness.spentFeeInput,
        );
  const feeInput = selectFeeInput(feeCandidates);
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
  const computationThreadSuccessRedeemer =
    makeComputationThreadSuccessRedeemer(threadToken.assetName);

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
          tx2SpendInputsRefInputIndex,
          doubleSpentInputIndex,
        }),
      )
      .readFrom(sortedReferenceInputs)
      .mintAssets(
        { [threadToken.unit]: -1n },
        computationThreadSuccessRedeemer,
      )
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
      .attach.SpendingValidator(contracts.doubleSpend.steps[3].spendingScript)
      .attach.MintingPolicy(contracts.computationThread.mintingScript)
      .attach.MintingPolicy(contracts.fraudProof.mintingScript);

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
      fraudProofDatum,
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
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    fourthStepAddress: contracts.doubleSpend.steps[3].spendingScriptAddress,
    verifiedTx2SpendInputsHash:
      inputDatum.data.verified_tx2_spend_inputs_hash,
    doubleSpentInputIndex: Number(doubleSpentInputIndex),
    doubleSpentInput,
    doubleSpentInputCbor,
    tx2SpendInputsWitnessOutRef: tx2SpendInputsReferenceWitness.outRef,
    tx2SpendInputsWitnessCreated: tx2SpendInputsReferenceWitness.created,
    tx2SpendInputsRefInputIndex: Number(tx2SpendInputsRefInputIndex),
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

export const submitStep04FromFiles = async (
  config: SubmitStep04CliConfig,
): Promise<SubmitStep04Result> => {
  const [blueprint, deploymentInfo, tx2InputsJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.tx2InputsPath),
    makeLucidForSubmitInit(config),
  ]);
  const tx2SpendInputCbors = parseSubmitStep04Tx2Inputs(tx2InputsJson);
  const signer = resolveProverSigner(config);
  return await submitStep04({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    tx2SpendInputCbors,
    doubleSpentInputIndex: parseDoubleSpentInputIndex(
      config.doubleSpentInputIndex,
      tx2SpendInputCbors.length,
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};
