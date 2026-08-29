/**
 * `reference-input-no-idx` step-04 submitter (Goal task `Q31`).
 *
 * Finalizes the proof: burns the computation thread, mints the permanent
 * fraud-proof token and locks it at the always-fails fraud-proof address.
 *
 * **Re-derived onto the §8.8 door by #604.** The redeemer used to reproduce the
 * producing transaction's whole `outputs_preimage: List<MidgardTxOutput>`; it
 * now carries a `FieldOpeningV1` over §2.5 field **2**, and the door's
 * authenticated item count is the output count the out-of-range verdict rests
 * on (§5.2). Thread state carries that transaction's `producing_tx_id` rather
 * than its `outputs_hash`, which is why this builder takes the producing
 * transaction's compact CBOR.
 *
 * Nothing in the prepared file is trusted. The complete outputs preimage is
 * re-encoded with the canonical `encode_midgard_tx_output` twin, checked against
 * the commitment the producing transaction carries *at field 2*, and the rule
 * itself is then re-run locally (`bad_reference_input_output_index >=
 * |outputs|`), so a thread whose challenged index exists in its producing
 * transaction cannot be finalized off-chain either.
 */
import {
  encodeMidgardTxOutputCanonicalV1,
  type FieldOpeningV1,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  isReferenceInputNoIdxViolationV1,
  MIDGARD_FIELD_INDEX_V1,
  type MidgardTxOutput,
  ReferenceInputNoIdxStep04Datum,
  ReferenceInputNoIdxStep04SpendRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  toUnit,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpeningV1,
  parseNativeTxCompactCborV1,
  planFaultProofFieldOpeningV1,
} from "./field-opening-v1.js";
import { parseHex, requireRecord } from "./json-file.js";
import { midgardTxOutputFromCanonicalCborV1 } from "./prepare-input-no-idx.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  type ResolvedProverSigner,
  resolveProverSigner,
  resolveReferenceInputNoIdxDeploymentContracts,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
  witnessSpendingValidatorCarriageV1,
} from "./witness-reference-scripts-v1.js";

/** Prepared outputs preimage produced by `prepare-reference-input-no-idx`. */
export type SubmitReferenceInputNoIdxOutputsPreimage = {
  readonly outputsPreimage: readonly MidgardTxOutput[];
};

/**
 * The prepared file carries the canonical `encode_midgard_tx_output` bytes, so
 * the structured redeemer value is re-projected here rather than trusted: any
 * item the canonical decoder cannot invert, or whose re-encoding is not
 * byte-identical, is rejected before a transaction is built.
 *
 * `prepare-reference-input-no-idx` writes its `outputsPreimageCbor` artifact as
 * a bare JSON array of canonical items, so both that shape and the enclosing
 * `{ outputsPreimageCbor }` record are accepted.
 */
export const parseSubmitReferenceInputNoIdxOutputsPreimage = (
  value: unknown,
): SubmitReferenceInputNoIdxOutputsPreimage => {
  const rawOutputs = Array.isArray(value)
    ? value
    : requireRecord(value, "--outputs-preimage").outputsPreimageCbor;
  if (!Array.isArray(rawOutputs)) {
    throw new Error(
      "--outputs-preimage must be a JSON array, or a JSON object with an outputsPreimageCbor array.",
    );
  }
  const outputsPreimage = rawOutputs.map((item, index) => {
    const label = `--outputs-preimage.outputsPreimageCbor[${index.toString()}]`;
    const canonicalCbor = Buffer.from(parseHex(item, label), "hex");
    const projected = midgardTxOutputFromCanonicalCborV1(canonicalCbor);
    if (!encodeMidgardTxOutputCanonicalV1(projected).equals(canonicalCbor)) {
      throw new Error(`${label} is not a canonical Midgard output encoding.`);
    }
    return projected;
  });
  return { outputsPreimage };
};

export type SubmitReferenceInputNoIdxStep04CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly outputsPreimagePath: string;
  /**
   * JSON `{ "nativeTxCompactCbor": "<hex>" }` — the **producing** transaction's
   * compact structure. New in #604: the door authenticates its field 2 against
   * the `producing_tx_id` the thread anchored.
   */
  readonly nativeTxCompactPath: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitReferenceInputNoIdxStep04Result = {
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
  /** §4's flat commitment for the producing transaction's field 2. */
  readonly producingTxOutputsHash: string;
  /** The §2.5 anchor the thread carried for the producing transaction. */
  readonly producingTxId: string;
  readonly producingTxOutputCount: number;
  readonly badReferenceInputOutputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type ReferenceInputNoIdxStep04DatumWithState =
  ReferenceInputNoIdxStep04Datum & {
    readonly data: NonNullable<ReferenceInputNoIdxStep04Datum["data"]>;
  };

type ReferenceInputNoIdxStep04ResolvedLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

type ReferenceInputNoIdxStep04SpendLayout = Omit<
  ReferenceInputNoIdxStep04ResolvedLayout,
  "computationThreadMintRedeemerIndex"
>;

const requireStep04Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): ReferenceInputNoIdxStep04DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, ReferenceInputNoIdxStep04Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Reference-input-no-idx step 04 input datum must carry the producing outputs commitment.",
    );
  }
  return datum as ReferenceInputNoIdxStep04DatumWithState;
};

const fraudProofOutputPredicate = ({
  fraudProofAddress,
  fraudProofUnit,
  fraudProofDatum,
}: {
  readonly fraudProofAddress: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
}): ((output: TxOutput) => boolean) =>
  outputWithDatumAndUnitPredicate({
    address: fraudProofAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });

const makeReferenceInputNoIdxStep04SpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  outputsOpening,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly outputsOpening: FieldOpeningV1;
  readonly onLayout: (layout: ReferenceInputNoIdxStep04SpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "reference-input-no-idx step 04");
    const layout: ReferenceInputNoIdxStep04SpendLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "reference-input-no-idx step 04",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputPredicate({
          fraudProofAddress,
          fraudProofUnit,
          fraudProofDatum,
        }),
        "reference-input-no-idx step 04 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "reference-input-no-idx step 04 fraud-proof",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            outputs_opening: outputsOpening,
          },
        ],
      },
      ReferenceInputNoIdxStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const makeFraudProofMintRedeemer = ({
  fraudProofPolicyId,
  computationThreadPolicyId,
  computationThreadAssetName,
  onComputationThreadMintRedeemerIndex,
}: {
  readonly fraudProofPolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly onComputationThreadMintRedeemerIndex: (index: bigint) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      fraudProofPolicyId,
      "reference-input-no-idx step 04 fraud-proof mint",
    );
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "reference-input-no-idx step 04 computation-thread burn",
    );
    onComputationThreadMintRedeemerIndex(computationThreadMintRedeemerIndex);
    return Data.to(
      {
        computation_thread_token_asset_name: computationThreadAssetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const makeComputationThreadSuccessRedeemer = ({
  computationThreadPolicyId,
  computationThreadAssetName,
}: {
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      computationThreadPolicyId,
      "reference-input-no-idx step 04 computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const submitReferenceInputNoIdxStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  outputsPreimage,
  nativeTxCompactCbor,
  referenceScriptUtxo,
  witnessReferenceScripts,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly outputsPreimage: SubmitReferenceInputNoIdxOutputsPreimage;
  /** The **producing** transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  /** The mandatory published step-04 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitReferenceInputNoIdxStep04Result> => {
  const { referenceInputNoIdxCategory, contracts } =
    await resolveReferenceInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const chain = contracts.referenceInputNoIdx;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "reference-input-no-idx step-04 computation-thread UTxO",
  });
  if (threadUtxo.address !== chain.steps[3].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at reference-input-no-idx step 04.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: referenceInputNoIdxCategory.categoryId,
    categoryLabel: "reference-input-no-idx",
  });
  const inputDatum = requireStep04Datum({ threadUtxo, signer });
  const producingTxId = inputDatum.data.producing_tx_id;
  const badReferenceInputOutputIndex =
    inputDatum.data.bad_reference_input_output_index;

  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.outputs,
    anchorTxId: producingTxId,
    nativeTxCompactCbor,
    itemCbors: outputsPreimage.outputsPreimage.map(
      encodeMidgardTxOutputCanonicalV1,
    ),
    owner: signer.paymentKeyHash,
    label: "Reference-input-no-idx step 04 outputs",
  });
  const producingTxOutputsHash = planned.commitment;
  const stepScriptCarriage = witnessSpendingValidatorCarriageV1({
    script: chain.steps[3].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "reference-input-no-idx step 04 validator",
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "reference-input-no-idx step 04 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "reference-input-no-idx step 04 fraud-proof mint",
  });
  // The complete reference-input set the built transaction will declare, in
  // build order — the opening derivation must see all of it (bug fc635c8f).
  const referenceInputs = [
    ...stepScriptCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const outputsOpening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    label: "Reference-input-no-idx step 04 outputs",
  });
  // §5.2: the count the verdict rests on is the door's authenticated one, which
  // for tiers 1–2 is derived by walking the same preimage the door hashes.
  const producingTxOutputCount = planned.itemCount;
  if (
    !isReferenceInputNoIdxViolationV1({
      badReferenceInputOutputIndex,
      producingTxOutputCount,
    })
  ) {
    throw new Error(
      `Reference-input-no-idx step 04 cannot finalize: output index ${badReferenceInputOutputIndex.toString()} exists in a producing transaction with ${producingTxOutputCount.toString()} outputs; an existing transaction reference input cannot be proven non-existent.`,
    );
  }

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
  let spendLayout: ReferenceInputNoIdxStep04SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const collected = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeReferenceInputNoIdxStep04SpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        outputsOpening,
        onLayout: (layout) => {
          spendLayout = layout;
        },
      }),
    );
  // Without published witnesses this step reads nothing, and `readFrom([])`
  // is an error rather than a no-op, so the branch is on whether the
  // carriages produced reference inputs at all.
  const tx = (
    referenceInputs.length === 0
      ? collected
      : collected.readFrom([...referenceInputs])
  )
    .mintAssets(
      { [threadToken.unit]: -1n },
      makeComputationThreadSuccessRedeemer({
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: threadToken.assetName,
      }),
    )
    .mintAssets(
      { [fraudProofUnit]: 1n },
      makeFraudProofMintRedeemer({
        fraudProofPolicyId: contracts.fraudProof.policyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: threadToken.assetName,
        onComputationThreadMintRedeemerIndex: (index) => {
          computationThreadMintRedeemerIndex = index;
        },
      }),
    )
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      fraudProofAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(stepScriptCarriage.attach(tx)),
  );

  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve reference-input-no-idx step 04 layout.",
    );
  }
  const resolvedLayout: ReferenceInputNoIdxStep04ResolvedLayout = {
    ...spendLayout,
    computationThreadMintRedeemerIndex,
  };
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
    fourthStepAddress: chain.steps[3].spendingScriptAddress,
    producingTxOutputsHash,
    producingTxId,
    producingTxOutputCount,
    badReferenceInputOutputIndex: Number(badReferenceInputOutputIndex),
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

export const submitReferenceInputNoIdxStep04FromFiles = async (
  config: SubmitReferenceInputNoIdxStep04CliConfig,
): Promise<SubmitReferenceInputNoIdxStep04Result> => {
  const [
    blueprint,
    deploymentInfo,
    outputsPreimageJson,
    nativeTxCompactJson,
    lucid,
  ] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.outputsPreimagePath),
    readJsonFile(config.nativeTxCompactPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitReferenceInputNoIdxStep04({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    outputsPreimage:
      parseSubmitReferenceInputNoIdxOutputsPreimage(outputsPreimageJson),
    nativeTxCompactCbor: parseNativeTxCompactCborV1(
      nativeTxCompactJson,
      "--native-tx-compact",
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};
