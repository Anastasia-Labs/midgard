/**
 * ⚠️ **STALE AS OF #575 — do not build a datum or redeemer from this module
 * and expect chain to accept it. Owner: #579.** The rebind, its three concrete
 * divergences, and why they are not re-derived in this lane are explained once
 * in `docs/fault-proofs/offchain-builder-staleness-575.md`.
 *
 * `input-no-idx` step-02 submitter (Goal task `Q13`, §9.1 output 8).
 *
 * Opens the spend-inputs commitment carried by step 01. Nothing in the prepared
 * file is trusted: the complete preimage is re-encoded with the canonical
 * `encode_midgard_tx_input` twin and re-committed with
 * `bounded_collection_v1.from_items(0, ...)`, and the result must equal the
 * `verified_tx_inputs_hash` read back from the **on-chain** step-01 datum. Only
 * then is the challenged input forwarded to step 03.
 *
 * §3.2: the complete list may travel directly, through a typed Ada-only
 * publication reference, or one canonical authenticated item at a time through
 * the same computation thread. No partial list or caller-selected fold cursor
 * is accepted.
 */
import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  buildInputNoIdxSpendInputFoldOpeningsV1,
  INPUT_NO_IDX_STEP02_DIRECT_INPUT_LIMIT_V1,
  inputNoIdxSpendInputsCommitmentV1,
  InputNoIdxStep02Datum,
  InputNoIdxStep02SpendRedeemer,
  InputNoIdxStep03Datum,
  type MidgardTxInput,
  PublishedSpendInputsV1,
  type PublishedSpendInputsV1 as PublishedSpendInputsV1Data,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  verifyInputNoIdxSpendInputFoldOpeningV1,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  CML,
  coreToTxOutput,
  credentialToAddress,
  Data,
  getAddressDetails,
  type LucidEvolution,
  type Network,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

import { parseHex, requireRecord } from "./json-file.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  type ResolvedProverSigner,
  resolveInputNoIdxDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  excludeUtxo,
  minimumLovelaceForInlineDatumOutput,
  resolveProtocolParameters,
} from "./spend-input-witness.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";

/** Prepared spend-inputs preimage produced by `prepare-input-no-idx`. */
export type SubmitInputNoIdxInputsPreimage = {
  readonly inputsPreimage: readonly MidgardTxInput[];
  readonly badInputsIndex: number;
};

export type InputNoIdxPublishedSpendInputsReferenceV1 = {
  readonly utxo: UTxO;
  readonly outRef: string;
  readonly datum: PublishedSpendInputsV1Data;
  readonly datumCbor: string;
  readonly lovelace: bigint;
  readonly txHash: string;
  /** Fee input consumed by the publication transaction. */
  readonly spentFeeInput: UTxO;
  readonly measurement: InputNoIdxSpendInputsPublicationMeasurementV1;
};

export type InputNoIdxSpendInputsPublicationMeasurementV1 = {
  /** Exact bytes of the fully signed Cardano transaction. */
  readonly signedTxBytes: number;
  readonly signedTxCbor: string;
  readonly fee: bigint;
  readonly outputMinAda: bigint;
  readonly inputCount: number;
  readonly referenceInputCount: number;
  readonly outputCount: number;
  readonly collateralInputCount: number;
  readonly vkeyWitnessCount: number;
  readonly redeemerCount: number;
  readonly publicationOutputIndex: number;
  readonly maxOutputValueBytes: number;
  readonly txByteMargin: number;
  readonly valueByteMargin: number;
};

export type SignedInputNoIdxSpendInputsPublicationV1 = {
  readonly signed: TxSigned;
  readonly address: string;
  readonly datum: PublishedSpendInputsV1Data;
  readonly datumCbor: string;
  readonly lovelace: bigint;
  readonly spentFeeInput: UTxO;
  readonly measurement: InputNoIdxSpendInputsPublicationMeasurementV1;
};

const onlyLovelace = (utxo: UTxO): boolean =>
  Object.keys(utxo.assets).every((unit) => unit === "lovelace");

/**
 * Wallet inputs eligible for step-02 fee and collateral selection.
 *
 * A publication is matched by out-ref rather than object identity because the
 * provider may return a fresh copy of the same UTxO. The returned array is
 * transaction-local; this helper never mutates the Lucid wallet override.
 */
export const inputNoIdxStep02WalletInputs = ({
  walletUtxos,
  publicationUtxo,
}: {
  readonly walletUtxos: readonly UTxO[];
  readonly publicationUtxo?: UTxO;
}): UTxO[] =>
  publicationUtxo === undefined
    ? [...walletUtxos]
    : [...excludeUtxo(walletUtxos, publicationUtxo)];

export const selectInputNoIdxStep02FeeInput = ({
  walletUtxos,
  publicationUtxo,
}: {
  readonly walletUtxos: readonly UTxO[];
  readonly publicationUtxo?: UTxO;
}): UTxO =>
  selectFeeInput(
    inputNoIdxStep02WalletInputs({ walletUtxos, publicationUtxo }),
  );

const findTypedPublicationOutputIndex = ({
  transaction: tx,
  address,
  datum,
}: {
  readonly transaction: CML.Transaction;
  readonly address: string;
  readonly datum: string;
}): number => {
  const expectedDatum = canonicalPlutusDataCbor(datum);
  const outputs = tx.body().outputs();
  const matches: number[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address === address &&
      output.datum != null &&
      canonicalPlutusDataCbor(output.datum) === expectedDatum &&
      Object.keys(output.assets).every((unit) => unit === "lovelace") &&
      (output.assets.lovelace ?? 0n) > 0n
    ) {
      matches.push(index);
    }
  }
  if (matches.length !== 1) {
    throw new Error(
      `Input-no-idx publication transaction must contain exactly one matching Ada-only typed output; found ${matches.length.toString()}.`,
    );
  }
  return matches[0]!;
};

/** Builds and signs a whole-list publication without submitting it. */
export const buildSignedInputNoIdxSpendInputsPublicationV1 = async ({
  lucid,
  network,
  signer,
  computationThreadPolicyId,
  computationThreadAssetName,
  verifiedTxInputsHash,
  inputsPreimage,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly verifiedTxInputsHash: string;
  readonly inputsPreimage: SubmitInputNoIdxInputsPreimage;
}): Promise<SignedInputNoIdxSpendInputsPublicationV1> => {
  const inputs = inputsPreimage.inputsPreimage;
  if (inputs.length === 0) {
    throw new Error(
      "Input-no-idx publication requires the complete non-empty inputs list.",
    );
  }
  if (inputs[inputsPreimage.badInputsIndex] === undefined) {
    throw new Error(
      `Input-no-idx publication badInputsIndex ${inputsPreimage.badInputsIndex.toString()} is out of range for the complete ${inputs.length.toString()}-item list.`,
    );
  }
  const derivedCommitment = inputNoIdxSpendInputsCommitmentV1(inputs);
  if (derivedCommitment !== verifiedTxInputsHash) {
    throw new Error(
      `Published inputs do not open the committed spend-inputs hash: derived=${derivedCommitment}, expected=${verifiedTxInputsHash}.`,
    );
  }
  const address = credentialToAddress(network, {
    type: "Key",
    hash: signer.paymentKeyHash,
  });
  const datum: PublishedSpendInputsV1Data = {
    version: 1n,
    computation_thread_policy_id: computationThreadPolicyId,
    computation_thread_asset_name: computationThreadAssetName,
    fraud_prover: signer.paymentKeyHash,
    verified_tx_inputs_hash: verifiedTxInputsHash,
    item_count: BigInt(inputs.length),
    inputs: [...inputs],
  };
  const datumCbor = Data.to(datum, PublishedSpendInputsV1);
  const protocolParameters = await resolveProtocolParameters(lucid);
  const lovelace = minimumLovelaceForInlineDatumOutput({
    address,
    datum: datumCbor,
    coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
  });

  signer.selectWallet(lucid);
  const spentFeeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const unsigned = await lucid
    .newTx()
    .collectFrom([spentFeeInput])
    .pay.ToAddressWithData(
      address,
      { kind: "inline", value: datumCbor },
      { lovelace },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const outputIndex = findTypedPublicationOutputIndex({
    transaction: unsigned.toTransaction(),
    address,
    datum: datumCbor,
  });
  const signed = await unsigned.sign.withWallet().complete();
  const transaction = signed.toTransaction();
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const signedTxCbor = signed.toCBOR();
  const outputs = body.outputs();
  let maxOutputValueBytes = 0;
  for (let index = 0; index < outputs.len(); index += 1) {
    maxOutputValueBytes = Math.max(
      maxOutputValueBytes,
      outputs.get(index).amount().to_cbor_bytes().length,
    );
  }
  const signedTxBytes = Buffer.from(signedTxCbor, "hex").length;
  return {
    signed,
    address,
    datum,
    datumCbor,
    lovelace,
    spentFeeInput,
    measurement: {
      signedTxBytes,
      signedTxCbor,
      fee: body.fee(),
      outputMinAda: lovelace,
      inputCount: body.inputs().len(),
      referenceInputCount: body.reference_inputs()?.len() ?? 0,
      outputCount: outputs.len(),
      collateralInputCount: body.collateral_inputs()?.len() ?? 0,
      vkeyWitnessCount: witnessSet.vkeywitnesses()?.len() ?? 0,
      redeemerCount: witnessSet.redeemers()?.to_flat_format().len() ?? 0,
      publicationOutputIndex: outputIndex,
      maxOutputValueBytes,
      txByteMargin: protocolParameters.maxTxSize - signedTxBytes,
      valueByteMargin: protocolParameters.maxValSize - maxOutputValueBytes,
    },
  };
};

/**
 * Publishes a whole typed spend-input list at the prover's enterprise key
 * address and returns only the exact confirmed output reference.
 */
export const publishInputNoIdxSpendInputsV1 = async (
  args: Parameters<typeof buildSignedInputNoIdxSpendInputsPublicationV1>[0],
): Promise<InputNoIdxPublishedSpendInputsReferenceV1> => {
  const built = await buildSignedInputNoIdxSpendInputsPublicationV1(args);
  const txHash = await built.signed.submit();
  const { lucid } = args;
  await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  const outRef = `${txHash}#${built.measurement.publicationOutputIndex.toString()}`;
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef: {
      txHash,
      outputIndex: built.measurement.publicationOutputIndex,
    },
    label: "confirmed input-no-idx published spend-inputs UTxO",
  });
  if (
    utxo.address !== built.address ||
    utxo.datum == null ||
    utxo.datumHash != null ||
    canonicalPlutusDataCbor(utxo.datum) !==
      canonicalPlutusDataCbor(built.datumCbor) ||
    !onlyLovelace(utxo) ||
    (utxo.assets.lovelace ?? 0n) <= 0n ||
    utxo.scriptRef !== undefined
  ) {
    throw new Error(
      `Confirmed input-no-idx publication UTxO ${outRef} does not reproduce the exact typed Ada-only output.`,
    );
  }
  return {
    utxo,
    outRef,
    datum: built.datum,
    datumCbor: built.datumCbor,
    lovelace: utxo.assets.lovelace ?? 0n,
    txHash,
    spentFeeInput: built.spentFeeInput,
    measurement: built.measurement,
  };
};

const parseNonNegativeInteger = (value: unknown, label: string): number => {
  const parsed = typeof value === "number" ? value : Number(value);
  if (!Number.isInteger(parsed) || parsed < 0) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  return parsed;
};

export const parseSubmitInputNoIdxInputsPreimage = (
  value: unknown,
): SubmitInputNoIdxInputsPreimage => {
  const record = requireRecord(value, "--inputs-preimage");
  const rawInputs = record.inputsPreimage;
  if (!Array.isArray(rawInputs)) {
    throw new Error("--inputs-preimage.inputsPreimage must be a JSON array.");
  }
  const inputsPreimage = rawInputs.map((item, index) => {
    const entry = requireRecord(
      item,
      `--inputs-preimage.inputsPreimage[${index.toString()}]`,
    );
    return {
      tx_id: parseHex(
        entry.tx_id,
        `--inputs-preimage.inputsPreimage[${index.toString()}].tx_id`,
        32,
      ),
      output_index: BigInt(
        parseNonNegativeInteger(
          entry.output_index,
          `--inputs-preimage.inputsPreimage[${index.toString()}].output_index`,
        ),
      ),
    };
  });
  return {
    inputsPreimage,
    badInputsIndex: parseNonNegativeInteger(
      record.badInputsIndex,
      "--inputs-preimage.badInputsIndex",
    ),
  };
};

export type SubmitInputNoIdxStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly inputsPreimagePath: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitInputNoIdxStep02Result = {
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
  readonly secondStepAddress: string;
  readonly thirdStepAddress: string;
  readonly verifiedTxInputsHash: string;
  readonly inputsPreimageItemCount: number;
  readonly badInputsIndex: number;
  readonly badInputTxId: string;
  readonly badInputOutputIndex: number;
  /** The on-chain execution branch used by this submission. */
  readonly step02Execution: "direct" | "published" | "fold-start" | "fold-next";
  /** Whether this submission advanced the thread to step 03. */
  readonly terminal: boolean;
  /** Present only while the thread remains at step 02. */
  readonly nextFoldItemIndex?: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly publicationOutRef?: string;
  readonly publicationReferenceInputIndex?: number;
  readonly awaitedConfirmation: boolean;
};

/**
 * Resumes the ordered fold only after every prior continuation is observable.
 * The caller supplies only the complete preimage; each next cursor is read
 * from the output of the preceding confirmed transaction.
 */
export type SubmitInputNoIdxStep02UntilTerminalResult = {
  readonly submissions: readonly SubmitInputNoIdxStep02Result[];
  readonly terminal: SubmitInputNoIdxStep02Result;
};

type InputNoIdxStep02Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly publicationReferenceInputIndex?: bigint;
};

type InputNoIdxStep02DatumWithState = InputNoIdxStep02Datum & {
  readonly data: NonNullable<InputNoIdxStep02Datum["data"]>;
};

type Step02Submission = {
  readonly execution: "direct" | "published" | "fold-start" | "fold-next";
  readonly terminal: boolean;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly buildArgs: (layout: InputNoIdxStep02Layout) => unknown;
  readonly nextFoldItemIndex?: number;
};

const requirePublishedInputsMatch = ({
  utxo,
  signer,
  computationThreadPolicyId,
  computationThreadAssetName,
  verifiedTxInputsHash,
  inputs,
}: {
  readonly utxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly verifiedTxInputsHash: string;
  readonly inputs: readonly MidgardTxInput[];
}): void => {
  const details = getAddressDetails(utxo.address);
  if (
    details.paymentCredential?.type !== "Key" ||
    details.paymentCredential.hash !== signer.paymentKeyHash ||
    details.stakeCredential !== undefined
  ) {
    throw new Error(
      "Input-no-idx publication must be locked at the fraud prover's enterprise payment-key address.",
    );
  }
  if (
    utxo.datum == null ||
    utxo.datumHash != null ||
    !onlyLovelace(utxo) ||
    (utxo.assets.lovelace ?? 0n) <= 0n ||
    utxo.scriptRef !== undefined
  ) {
    throw new Error(
      "Input-no-idx publication must carry an inline typed datum in an Ada-only output without a reference script.",
    );
  }
  const expected: PublishedSpendInputsV1Data = {
    version: 1n,
    computation_thread_policy_id: computationThreadPolicyId,
    computation_thread_asset_name: computationThreadAssetName,
    fraud_prover: signer.paymentKeyHash,
    verified_tx_inputs_hash: verifiedTxInputsHash,
    item_count: BigInt(inputs.length),
    inputs: [...inputs],
  };
  const expectedCbor = Data.to(expected, PublishedSpendInputsV1);
  if (
    canonicalPlutusDataCbor(utxo.datum) !==
    canonicalPlutusDataCbor(expectedCbor)
  ) {
    throw new Error(
      "Input-no-idx publication datum does not match the computation thread and complete inputs preimage.",
    );
  }
};

const transactionInputsContain = (
  inputs: CML.TransactionInputList | undefined,
  utxo: UTxO,
): boolean => {
  if (inputs === undefined) {
    return false;
  }
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    if (
      input.transaction_id().to_hex() === utxo.txHash &&
      input.index() === BigInt(utxo.outputIndex)
    ) {
      return true;
    }
  }
  return false;
};

const requirePublicationKeptUnspent = (
  tx: CML.Transaction,
  publicationUtxo: UTxO,
): void => {
  const body = tx.body();
  if (
    transactionInputsContain(body.inputs(), publicationUtxo) ||
    transactionInputsContain(body.collateral_inputs(), publicationUtxo)
  ) {
    throw new Error(
      `Input-no-idx publication ${outRefLabel(publicationUtxo)} was selected as a spending or collateral input.`,
    );
  }
};

const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): InputNoIdxStep02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, InputNoIdxStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Input-no-idx step 02 input datum must carry the verified spend-inputs hash.",
    );
  }
  return datum as InputNoIdxStep02DatumWithState;
};

const sameInput = (
  left: MidgardTxInput | null,
  right: MidgardTxInput | null,
): boolean =>
  left === null || right === null
    ? left === right
    : left.tx_id === right.tx_id && left.output_index === right.output_index;

const requireFoldingStateMatchesPreimage = ({
  state,
  inputsPreimage,
}: {
  readonly state: Extract<
    NonNullable<InputNoIdxStep02Datum["data"]>,
    { readonly Folding: unknown }
  >["Folding"];
  readonly inputsPreimage: SubmitInputNoIdxInputsPreimage;
}): void => {
  const itemCount = Number(state.item_count);
  const nextItemIndex = Number(state.next_item_index);
  const badInputsIndex = Number(state.bad_inputs_index);
  if (
    !Number.isSafeInteger(itemCount) ||
    !Number.isSafeInteger(nextItemIndex) ||
    !Number.isSafeInteger(badInputsIndex) ||
    itemCount !== inputsPreimage.inputsPreimage.length ||
    nextItemIndex <= 0 ||
    nextItemIndex >= itemCount ||
    badInputsIndex !== inputsPreimage.badInputsIndex
  ) {
    throw new Error(
      "Input-no-idx step 02 folding datum does not match the complete inputs preimage.",
    );
  }
  const expectedSelected =
    badInputsIndex < nextItemIndex
      ? inputsPreimage.inputsPreimage[badInputsIndex]!
      : null;
  if (!sameInput(state.selected_input, expectedSelected)) {
    throw new Error(
      "Input-no-idx step 02 folding datum has an unexpected selected input.",
    );
  }
};

export const submitInputNoIdxStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  inputsPreimage,
  publicationReference,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly inputsPreimage: SubmitInputNoIdxInputsPreimage;
  /** Exact confirmed tier-2 output or its exact out-ref. Programmatic only. */
  readonly publicationReference?:
    | string
    | InputNoIdxPublishedSpendInputsReferenceV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitInputNoIdxStep02Result> => {
  const { nonExistentInputNoIndexCategory, contracts } =
    await resolveInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const chain = contracts.nonExistentInputNoIndex;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "input-no-idx step-02 computation-thread UTxO",
  });
  if (threadUtxo.address !== chain.steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at input-no-idx step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: nonExistentInputNoIndexCategory.categoryId,
    categoryLabel: "input-no-idx",
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });
  const inputState = inputDatum.data;
  const verifiedTxInputsHash =
    "Direct" in inputState
      ? inputState.Direct.verified_tx_inputs_hash
      : inputState.Folding.verified_tx_inputs_hash;

  // Re-derive the commitment the validator will recompute, from the preimage
  // itself, and require it to open the on-chain state.
  const derivedCommitment = inputNoIdxSpendInputsCommitmentV1(
    inputsPreimage.inputsPreimage,
  );
  if (derivedCommitment !== verifiedTxInputsHash) {
    throw new Error(
      `--inputs-preimage does not open the committed spend-inputs hash: derived=${derivedCommitment}, thread=${verifiedTxInputsHash}.`,
    );
  }
  const badInput = inputsPreimage.inputsPreimage[inputsPreimage.badInputsIndex];
  if (badInput === undefined) {
    throw new Error(
      `--inputs-preimage.badInputsIndex ${inputsPreimage.badInputsIndex.toString()} is out of range for a ${inputsPreimage.inputsPreimage.length.toString()}-item preimage.`,
    );
  }

  const isDirect = "Direct" in inputState;
  if (publicationReference !== undefined && !isDirect) {
    throw new Error(
      "Input-no-idx CompletePublished can be used only from the Direct step-02 state.",
    );
  }
  const publicationUtxo =
    publicationReference === undefined
      ? undefined
      : typeof publicationReference === "string"
        ? await fetchUtxoByOutRef({
            lucid,
            outRef: parseOutRef(
              publicationReference,
              "input-no-idx publication out-ref",
            ),
            label: "confirmed input-no-idx published spend-inputs UTxO",
          })
        : publicationReference.utxo;
  if (publicationUtxo !== undefined) {
    if (
      publicationReference !== undefined &&
      typeof publicationReference !== "string" &&
      publicationReference.outRef !== outRefLabel(publicationUtxo)
    ) {
      throw new Error(
        "Input-no-idx publication reference out-ref does not match its exact UTxO.",
      );
    }
    requirePublishedInputsMatch({
      utxo: publicationUtxo,
      signer,
      computationThreadPolicyId: contracts.computationThread.policyId,
      computationThreadAssetName: threadToken.assetName,
      verifiedTxInputsHash,
      inputs: inputsPreimage.inputsPreimage,
    });
  }
  const mustFold =
    publicationUtxo === undefined &&
    (!isDirect ||
      inputsPreimage.inputsPreimage.length >
        INPUT_NO_IDX_STEP02_DIRECT_INPUT_LIMIT_V1);
  if (!mustFold && !isDirect) {
    throw new Error("Unreachable input-no-idx step 02 execution state.");
  }

  if (!isDirect) {
    requireFoldingStateMatchesPreimage({
      state: inputState.Folding,
      inputsPreimage,
    });
  }

  signer.selectWallet(lucid);
  const walletUtxos = await lucid.wallet().getUtxos();
  const feeInput = selectInputNoIdxStep02FeeInput({
    walletUtxos,
    publicationUtxo,
  });
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bad_input_tx_id: badInput.tx_id,
        bad_input_output_index: badInput.output_index,
      },
    },
    InputNoIdxStep03Datum,
  );
  let submission: Step02Submission;
  if (publicationUtxo !== undefined) {
    submission = {
      execution: "published",
      terminal: true,
      outputAddress: chain.steps[2].spendingScriptAddress,
      outputDatum: step03Datum,
      buildArgs: (layout) => {
        if (layout.publicationReferenceInputIndex === undefined) {
          throw new Error(
            "Input-no-idx CompletePublished layout is missing its publication reference input.",
          );
        }
        return {
          CompletePublished: {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            publication_reference_input_index:
              layout.publicationReferenceInputIndex,
            bad_inputs_index: BigInt(inputsPreimage.badInputsIndex),
          },
        };
      },
    };
  } else if (!mustFold) {
    submission = {
      execution: "direct",
      terminal: true,
      outputAddress: chain.steps[2].spendingScriptAddress,
      outputDatum: step03Datum,
      buildArgs: (layout) => ({
        Complete: {
          input_index: layout.inputIndex,
          output_index: layout.outputIndex,
          inputs_preimage: [...inputsPreimage.inputsPreimage],
          bad_inputs_index: BigInt(inputsPreimage.badInputsIndex),
        },
      }),
    };
  } else {
    const itemIndex = isDirect ? 0 : Number(inputState.Folding.next_item_index);
    const opening = buildInputNoIdxSpendInputFoldOpeningsV1(
      inputsPreimage.inputsPreimage,
    )[itemIndex];
    if (
      opening === undefined ||
      !verifyInputNoIdxSpendInputFoldOpeningV1({
        inputs: inputsPreimage.inputsPreimage,
        opening,
      })
    ) {
      throw new Error(
        "Failed to derive an authenticated input-no-idx step 02 fold opening from the complete inputs preimage.",
      );
    }
    const nextItemIndex = itemIndex + 1;
    const selectedInput =
      inputsPreimage.badInputsIndex === itemIndex
        ? inputsPreimage.inputsPreimage[itemIndex]!
        : isDirect
          ? null
          : inputState.Folding.selected_input;
    const terminal = nextItemIndex === inputsPreimage.inputsPreimage.length;
    const outputDatum = terminal
      ? step03Datum
      : Data.to(
          {
            fraud_prover: signer.paymentKeyHash,
            data: {
              Folding: {
                verified_tx_inputs_hash: verifiedTxInputsHash,
                item_count: BigInt(inputsPreimage.inputsPreimage.length),
                next_item_index: BigInt(nextItemIndex),
                bad_inputs_index: BigInt(inputsPreimage.badInputsIndex),
                selected_input: selectedInput,
              },
            },
          },
          InputNoIdxStep02Datum,
        );
    submission = {
      execution: isDirect ? "fold-start" : "fold-next",
      terminal,
      outputAddress: terminal
        ? chain.steps[2].spendingScriptAddress
        : chain.steps[1].spendingScriptAddress,
      outputDatum,
      ...(terminal ? {} : { nextFoldItemIndex: nextItemIndex }),
      buildArgs: (layout) =>
        isDirect
          ? {
              FoldStart: {
                input_index: layout.inputIndex,
                output_index: layout.outputIndex,
                bad_inputs_index: BigInt(inputsPreimage.badInputsIndex),
                input_cbor: opening.inputCbor,
                collection_proof: opening.collectionProof,
              },
            }
          : {
              FoldNext: {
                input_index: layout.inputIndex,
                output_index: layout.outputIndex,
                input_cbor: opening.inputCbor,
                collection_proof: opening.collectionProof,
              },
            },
    };
  }
  const outputMatches = computationThreadOutputPredicate({
    address: submission.outputAddress,
    datum: submission.outputDatum,
    unit: threadToken.unit,
  });
  let resolvedLayout: InputNoIdxStep02Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "input-no-idx step 02");
    const layout: InputNoIdxStep02Layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "input-no-idx step 02"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "input-no-idx step 02 output",
      ),
      ...(publicationUtxo === undefined
        ? {}
        : {
            publicationReferenceInputIndex: requireReferenceInputIndex(
              ctx,
              publicationUtxo,
              "input-no-idx step 02 published spend inputs",
            ),
          }),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [submission.buildArgs(layout)],
      } as never,
      InputNoIdxStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const txWithInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  const txWithReferences =
    publicationUtxo === undefined
      ? txWithInputs
      : txWithInputs.readFrom([publicationUtxo]);
  const tx = txWithReferences.pay
    .ToContract(
      submission.outputAddress,
      { kind: "inline", value: submission.outputDatum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(chain.steps[1].spendingScript);

  const unsigned = await tx.complete({
    localUPLCEval: true,
    ...(publicationUtxo === undefined
      ? {}
      : {
          presetWalletInputs: inputNoIdxStep02WalletInputs({
            walletUtxos,
            publicationUtxo,
          }),
        }),
  });
  if (publicationUtxo !== undefined) {
    requirePublicationKeptUnspent(unsigned.toTransaction(), publicationUtxo);
  }
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve input-no-idx step 02 layout.",
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
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: chain.steps[1].spendingScriptAddress,
    thirdStepAddress: chain.steps[2].spendingScriptAddress,
    verifiedTxInputsHash,
    inputsPreimageItemCount: inputsPreimage.inputsPreimage.length,
    badInputsIndex: inputsPreimage.badInputsIndex,
    badInputTxId: badInput.tx_id,
    badInputOutputIndex: Number(badInput.output_index),
    step02Execution: submission.execution,
    terminal: submission.terminal,
    ...(submission.nextFoldItemIndex === undefined
      ? {}
      : { nextFoldItemIndex: submission.nextFoldItemIndex }),
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    ...(publicationUtxo === undefined
      ? {}
      : { publicationOutRef: outRefLabel(publicationUtxo) }),
    ...(resolvedLayout.publicationReferenceInputIndex === undefined
      ? {}
      : {
          publicationReferenceInputIndex: Number(
            resolvedLayout.publicationReferenceInputIndex,
          ),
        }),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitInputNoIdxStep02FromFiles = async (
  config: SubmitInputNoIdxStep02CliConfig,
): Promise<SubmitInputNoIdxStep02Result> => {
  const [blueprint, deploymentInfo, inputsPreimageJson, lucid] =
    await Promise.all([
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.inputsPreimagePath),
      makeLucidForSubmit(config),
    ]);
  const signer = resolveProverSigner(config);
  return await submitInputNoIdxStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    inputsPreimage: parseSubmitInputNoIdxInputsPreimage(inputsPreimageJson),
    awaitConfirmation: config.awaitConfirmation,
  });
};

export const submitInputNoIdxStep02UntilTerminal = async ({
  awaitConfirmation = true,
  ...args
}: Omit<Parameters<typeof submitInputNoIdxStep02>[0], "awaitConfirmation"> & {
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitInputNoIdxStep02UntilTerminalResult> => {
  if (!awaitConfirmation) {
    throw new Error(
      "input-no-idx fold resume requires confirmation before it can locate the next computation-thread UTxO.",
    );
  }
  const submissions: SubmitInputNoIdxStep02Result[] = [];
  let currentThreadOutRef = args.threadOutRef;
  for (
    let attempt = 0;
    attempt < args.inputsPreimage.inputsPreimage.length;
    attempt += 1
  ) {
    const submission = await submitInputNoIdxStep02({
      ...args,
      threadOutRef: currentThreadOutRef,
      awaitConfirmation: true,
    });
    submissions.push(submission);
    if (submission.terminal) {
      return { submissions, terminal: submission };
    }
    currentThreadOutRef = submission.nextThreadOutRef;
  }
  throw new Error(
    "input-no-idx fold did not reach its terminal step-03 transition within the authenticated preimage length.",
  );
};

export const submitInputNoIdxStep02UntilTerminalFromFiles = async (
  config: SubmitInputNoIdxStep02CliConfig,
): Promise<SubmitInputNoIdxStep02UntilTerminalResult> => {
  const [blueprint, deploymentInfo, inputsPreimageJson, lucid] =
    await Promise.all([
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.inputsPreimagePath),
      makeLucidForSubmit(config),
    ]);
  const signer = resolveProverSigner(config);
  return await submitInputNoIdxStep02UntilTerminal({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    inputsPreimage: parseSubmitInputNoIdxInputsPreimage(inputsPreimageJson),
    awaitConfirmation: config.awaitConfirmation,
  });
};
