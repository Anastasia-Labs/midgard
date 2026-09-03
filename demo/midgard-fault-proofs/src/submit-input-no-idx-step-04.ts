/**
 * `input-no-idx` step-04 submitter (Goal task `Q13`, §9.1 output 8).
 *
 * Finalizes the proof: burns the computation thread, mints the permanent
 * fraud-proof token and locks it at the always-fails fraud-proof address.
 *
 * **Re-derived onto the §8.8 door by #604.** The redeemer used to reproduce the
 * producing transaction's whole `outputs_preimage: List<MidgardTxOutput>`; it now
 * carries a `FieldOpening` over §2.5 field **2**, and the door's authenticated
 * item count is the output count the out-of-range verdict rests on (§5.2). Thread
 * state carries that transaction's `producing_tx_id` rather than its
 * `outputs_hash`, which is why this builder takes the producing transaction's
 * compact CBOR.
 *
 * Nothing in the prepared file is trusted. The complete outputs preimage is
 * re-encoded with the canonical `encode_midgard_tx_output` twin and checked
 * against the commitment the producing transaction carries *at field 2*; the rule
 * itself is then re-run locally (`bad_input_output_index >= |outputs|`), so a
 * thread whose challenged index exists in its producing transaction cannot be
 * finalized off-chain either.
 */
import {
  encodeMidgardTxOutputCanonical,
  type FieldOpening,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  InputNoIdxStep04Datum,
  InputNoIdxStep04SpendRedeemer,
  isInputNoIdxViolation,
  MIDGARD_FIELD_INDEX,
  type MidgardTxOutput,
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
  faultProofFieldOpening,
  parseNativeTxCompactCbor,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "./field-opening-v1.js";
import { parseHex, requireRecord } from "./json-file.js";
import { midgardTxOutputFromCanonicalCbor } from "./prepare-input-no-idx.js";
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
import { excludeUtxo } from "./spend-input-witness.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
} from "./witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "./workflow/transaction-boundary-v1.js";

/** Prepared outputs preimage produced by `prepare-input-no-idx`. */
export type SubmitInputNoIdxOutputsPreimage = {
  readonly outputsPreimage: readonly MidgardTxOutput[];
};

/**
 * The prepared file carries the canonical `encode_midgard_tx_output` bytes, so
 * the structured redeemer value is re-projected here rather than trusted: any
 * item the canonical decoder cannot invert, or whose re-encoding is not
 * byte-identical, is rejected before a transaction is built.
 */
export const parseSubmitInputNoIdxOutputsPreimage = (
  value: unknown,
): SubmitInputNoIdxOutputsPreimage => {
  const record = requireRecord(value, "--outputs-preimage");
  const rawOutputs = record.outputsPreimageCbor;
  if (!Array.isArray(rawOutputs)) {
    throw new Error(
      "--outputs-preimage.outputsPreimageCbor must be a JSON array.",
    );
  }
  const outputsPreimage = rawOutputs.map((item, index) => {
    const label = `--outputs-preimage.outputsPreimageCbor[${index.toString()}]`;
    const canonicalCbor = Buffer.from(parseHex(item, label), "hex");
    const projected = midgardTxOutputFromCanonicalCbor(canonicalCbor);
    if (!encodeMidgardTxOutputCanonical(projected).equals(canonicalCbor)) {
      throw new Error(`${label} is not a canonical Midgard output encoding.`);
    }
    return projected;
  });
  return { outputsPreimage };
};

export type SubmitInputNoIdxStep04CliConfig = SubmitProviderConfig & {
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

export type SubmitInputNoIdxStep04Result = {
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
  /** The door's authenticated item count for field 2 (§5.2). */
  readonly producingTxOutputCount: number;
  /** The §8.4 tier the ladder picked for field 2 — decided by size alone. */
  readonly carriageTier: string;
  readonly badInputOutputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type InputNoIdxStep04DatumWithState = InputNoIdxStep04Datum & {
  readonly data: NonNullable<InputNoIdxStep04Datum["data"]>;
};

type InputNoIdxStep04ResolvedLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

type InputNoIdxStep04SpendLayout = Omit<
  InputNoIdxStep04ResolvedLayout,
  "computationThreadMintRedeemerIndex"
>;

const requireStep04Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): InputNoIdxStep04DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, InputNoIdxStep04Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Input-no-idx step 04 input datum must carry the producing outputs commitment.",
    );
  }
  return datum as InputNoIdxStep04DatumWithState;
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

const makeInputNoIdxStep04SpendRedeemer = ({
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
  readonly outputsOpening: FieldOpening;
  readonly onLayout: (layout: InputNoIdxStep04SpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "input-no-idx step 04");
    const layout: InputNoIdxStep04SpendLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "input-no-idx step 04"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputPredicate({
          fraudProofAddress,
          fraudProofUnit,
          fraudProofDatum,
        }),
        "input-no-idx step 04 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "input-no-idx step 04 fraud-proof",
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
      InputNoIdxStep04SpendRedeemer,
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
      "input-no-idx step 04 fraud-proof mint",
    );
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "input-no-idx step 04 computation-thread burn",
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
      "input-no-idx step 04 computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const submitInputNoIdxStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  outputsPreimage,
  nativeTxCompactCbor,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  witnessReferenceScripts,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly outputsPreimage: SubmitInputNoIdxOutputsPreimage;
  /** The **producing** transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  /** The mandatory published step-04 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitInputNoIdxStep04Result> => {
  const { nonExistentInputNoIndexCategory, contracts } =
    await resolveInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const chain = contracts.nonExistentInputNoIndex;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "input-no-idx step-04 computation-thread UTxO",
  });
  if (threadUtxo.address !== chain.steps[3].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at input-no-idx step 04.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: nonExistentInputNoIndexCategory.categoryId,
    categoryLabel: "input-no-idx",
  });
  const inputDatum = requireStep04Datum({ threadUtxo, signer });
  const producingTxId = inputDatum.data.producing_tx_id;
  const badInputOutputIndex = inputDatum.data.bad_input_output_index;

  const planned = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: producingTxId,
    nativeTxCompactCbor,
    itemCbors: outputsPreimage.outputsPreimage.map(
      encodeMidgardTxOutputCanonical,
    ),
    owner: signer.paymentKeyHash,
    label: "Input-no-idx step 04 outputs",
  });
  const producingTxOutputsHash = planned.commitment;
  signer.selectWallet(lucid);
  // §8's ladder decides whether anything has to exist on-chain first. Tier 1
  // publishes nothing; tiers 2–3 publish raw carriage located by content
  // (§8.7), and a publication that already exists at this address is reused
  // rather than republished.
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "Input-no-idx step 04 outputs",
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: chain.steps[3].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "input-no-idx step 04 validator",
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "input-no-idx step 04 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "input-no-idx step 04 fraud-proof mint",
  });
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...stepScriptCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const outputsOpening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificate.policyId,
    label: "Input-no-idx step 04 outputs",
  });
  // §5.2: the count the verdict rests on is the door's authenticated one.
  const producingTxOutputCount = planned.itemCount;
  if (
    !isInputNoIdxViolation({
      badInputOutputIndex,
      producingTxOutputCount,
    })
  ) {
    throw new Error(
      `Input-no-idx step 04 cannot finalize: output index ${badInputOutputIndex.toString()} exists in a producing transaction with ${producingTxOutputCount.toString()} outputs; an existing transaction input cannot be proven non-existent.`,
    );
  }

  // A fresh tier-2 publication carries enough min-ADA to top the fee sort;
  // never spend anything this transaction must read.
  const feeInput = selectFeeInput(
    carriageUtxos.reduce<readonly UTxO[]>(
      (candidates, utxo) => excludeUtxo(candidates, utxo),
      await lucid.wallet().getUtxos(),
    ),
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
  let spendLayout: InputNoIdxStep04SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const txWithInputs = lucid.newTx().collectFrom([feeInput]);
  const txWithReferences =
    referenceInputs.length === 0
      ? txWithInputs
      : txWithInputs.readFrom([...referenceInputs]);
  const tx = txWithReferences
    .collectFrom(
      [threadUtxo],
      makeInputNoIdxStep04SpendRedeemer({
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
      "BuildTxWithRedeemer did not resolve input-no-idx step 04 layout.",
    );
  }
  const resolvedLayout: InputNoIdxStep04ResolvedLayout = {
    ...spendLayout,
    computationThreadMintRedeemerIndex,
  };
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof non-existent-input-no-index step-04",
          utxo: referenceScriptUtxo,
          expectedScript:
            contracts.nonExistentInputNoIndex.steps[3].spendingScript,
        },
        {
          role: "V1 fraud-proof computation-thread minting",
          utxo: witnessReferenceScripts?.computationThreadMint,
          expectedScript: contracts.computationThread.mintingScript,
        },
        {
          role: "V1 fraud-proof token minting",
          utxo: witnessReferenceScripts?.fraudProofMint,
          expectedScript: contracts.fraudProof.mintingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `input-no-idx step-04 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
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
    carriageTier: planned.plan.tier,
    badInputOutputIndex: Number(badInputOutputIndex),
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

export const submitInputNoIdxStep04FromFiles = async (
  config: SubmitInputNoIdxStep04CliConfig,
): Promise<SubmitInputNoIdxStep04Result> => {
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
  return await submitInputNoIdxStep04({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    outputsPreimage: parseSubmitInputNoIdxOutputsPreimage(outputsPreimageJson),
    nativeTxCompactCbor: parseNativeTxCompactCbor(
      nativeTxCompactJson,
      "--native-tx-compact",
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};
