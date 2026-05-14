import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";
import {
  DoubleSpendStep03Datum,
  DoubleSpendStep03SpendRedeemer,
  DoubleSpendStep04Datum,
  buildDoubleSpendFaultProofContracts,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import { computeHash32, encodeCbor } from "@al-ft/midgard-core";
import { Effect } from "effect";
import { parseContractDeploymentInfo } from "./inspect-contracts.js";
import {
  fetchUtxoByOutRef,
  makeLucidForSubmitInit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  requireDeploymentScriptHash,
  resolveProverSigner,
  type ResolvedProverSigner,
  type SubmitProviderConfig,
} from "./submit-init.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  inputIndex,
  parseHex,
  parseInteger,
  requireComputationThreadToken,
  requireMatchingScriptHash,
  selectFeeInput,
} from "./submit-step-01.js";

const STEP_03_OUTPUT_INDEX = 0n;

export type SubmitStep03CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly tx1InputsPath: string;
  readonly doubleSpentInputIndex: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  readonly fourthStepAddress: string;
  readonly verifiedTx1SpendInputsHash: string;
  readonly verifiedTx2SpendInputsHash: string;
  readonly doubleSpentInputIndex: number;
  readonly doubleSpentInputCbor: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const parseSubmitStep03Tx1Inputs = (
  value: unknown,
): readonly string[] => parseSpendInputCbors(value, "--tx1-inputs");

export const parseSpendInputCbors = (
  value: unknown,
  label: string,
): readonly string[] => {
  if (!Array.isArray(value)) {
    throw new Error(`${label} must be a JSON array of raw input CBOR hex strings.`);
  }
  return value.map((entry, index) =>
    parseHex(entry, `${label}[${index.toString()}]`),
  );
};

export const hashSpendInputCbors = (inputCbors: readonly string[]): string =>
  computeHash32(
    encodeCbor(inputCbors.map((inputCbor) => Buffer.from(inputCbor, "hex"))),
  ).toString("hex");

const parseDoubleSpentInputIndex = (
  value: string,
  inputCount: number,
): bigint => {
  const index = parseInteger(value, "--double-spent-input-index");
  if (index >= BigInt(inputCount)) {
    throw new Error(
      `--double-spent-input-index ${index.toString()} is out of bounds for ${inputCount.toString()} tx1 inputs.`,
    );
  }
  if (index > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error("--double-spent-input-index exceeds the safe integer range.");
  }
  return index;
};

type Step03DatumWithState = DoubleSpendStep03Datum & {
  readonly data: NonNullable<DoubleSpendStep03Datum["data"]>;
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
  const datum = Data.from(threadUtxo.datum, DoubleSpendStep03Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error("Step 03 input datum must carry verified tx input hashes.");
  }
  return datum as Step03DatumWithState;
};

export const submitStep03 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  tx1SpendInputCbors,
  doubleSpentInputIndex,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly tx1SpendInputCbors: readonly string[];
  readonly doubleSpentInputIndex: bigint;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitStep03Result> => {
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

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "step-03 computation-thread UTxO",
  });
  if (
    threadUtxo.address !== contracts.doubleSpend.steps[2].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at double-spend step 03.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    doubleSpendCategoryId: doubleSpendCategory.categoryId,
  });
  const inputDatum = requireStep03Datum({ threadUtxo, signer });
  const tx1SpendInputsHash = hashSpendInputCbors(tx1SpendInputCbors);
  if (tx1SpendInputsHash !== inputDatum.data.verified_tx1_spend_inputs_hash) {
    throw new Error(
      `--tx1-inputs hash mismatch: provided preimage hashes to ${tx1SpendInputsHash}, expected ${inputDatum.data.verified_tx1_spend_inputs_hash}.`,
    );
  }
  if (doubleSpentInputIndex >= BigInt(tx1SpendInputCbors.length)) {
    throw new Error(
      `doubleSpentInputIndex ${doubleSpentInputIndex.toString()} is out of bounds for ${tx1SpendInputCbors.length.toString()} tx1 inputs.`,
    );
  }
  if (doubleSpentInputIndex > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error("doubleSpentInputIndex exceeds the safe integer range.");
  }
  const doubleSpentInputCbor = tx1SpendInputCbors[Number(doubleSpentInputIndex)]!;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const spendInputIndex = inputIndex(threadUtxo, feeInput);
  const redeemer = Data.to(
    {
      Continue: [
          {
            input_index: spendInputIndex,
            output_index: STEP_03_OUTPUT_INDEX,
            tx1_spend_input_cbors: [...tx1SpendInputCbors],
            double_spent_input_index: doubleSpentInputIndex,
          },
      ],
    },
    DoubleSpendStep03SpendRedeemer,
  );
  const step04Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        verified_tx2_spend_inputs_hash:
          inputDatum.data.verified_tx2_spend_inputs_hash,
        double_spent_input_cbor: doubleSpentInputCbor,
      },
    },
    DoubleSpendStep04Datum,
  );
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      contracts.doubleSpend.steps[3].spendingScriptAddress,
      {
        kind: "inline",
        value: step04Datum,
      },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(contracts.doubleSpend.steps[2].spendingScript);

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
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    thirdStepAddress: contracts.doubleSpend.steps[2].spendingScriptAddress,
    fourthStepAddress: contracts.doubleSpend.steps[3].spendingScriptAddress,
    verifiedTx1SpendInputsHash:
      inputDatum.data.verified_tx1_spend_inputs_hash,
    verifiedTx2SpendInputsHash:
      inputDatum.data.verified_tx2_spend_inputs_hash,
    doubleSpentInputIndex: Number(doubleSpentInputIndex),
    doubleSpentInputCbor,
    inputIndex: Number(spendInputIndex),
    outputIndex: Number(STEP_03_OUTPUT_INDEX),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitStep03FromFiles = async (
  config: SubmitStep03CliConfig,
): Promise<SubmitStep03Result> => {
  const [blueprint, deploymentInfo, tx1InputsJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.tx1InputsPath),
    makeLucidForSubmitInit(config),
  ]);
  const tx1SpendInputCbors = parseSubmitStep03Tx1Inputs(tx1InputsJson);
  const signer = resolveProverSigner(config);
  return await submitStep03({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    tx1SpendInputCbors,
    doubleSpentInputIndex: parseDoubleSpentInputIndex(
      config.doubleSpentInputIndex,
      tx1SpendInputCbors.length,
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};
