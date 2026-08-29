/**
 * `invalid-signature` step-02 submitter (Goal task `Q15`, §9.1 output 8).
 *
 * Finalizes the proof: burns the computation thread, mints the permanent
 * fraud-proof token and locks it at the always-fails fraud-proof address.
 *
 * **Re-derived onto the §8.8 door by #604, and this is the family that shows why
 * `NativeTxAnchorV1` has two arms.** Field 7 lives in the witness set, and §3's
 * transaction-id preimage is the body alone, so the id does not commit it. The
 * thread therefore carries `bad_tx_witness_set_hash` — read by step-01 off the
 * compact structure the block committed — and this step's opening must be the
 * `WitnessFieldOpening` arm, carrying the transaction's
 * `NativeTxWitnessSetCompact` for the door to check against it. Both the arm and
 * the §8.3 erratum E2 tier-3 refusal are derived from the field index by
 * `fieldOpeningV1ForField`, never chosen here.
 *
 * Nothing in the prepared JSON is trusted. The anchor and the committed
 * `witness_set_hash` are read back from the **on-chain** step-01 datum; the
 * supplied witness set must hash to that value and the supplied witness list
 * must be the §5.1 preimage the transaction committed *at field 7*; and the
 * accused witness is re-tested with the same Ed25519 verification the validator
 * performs. A thread that cannot conclude therefore fails here instead of
 * burning a submission on-chain.
 */
import {
  encodeMidgardAddressWitnessCanonicalV1,
  type FieldOpeningV1,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  InvalidSignatureStep02Datum,
  InvalidSignatureStep02SpendRedeemer,
  MIDGARD_FIELD_INDEX_V1,
  type MidgardAddressWitness,
  type NativeTxWitnessSetCompact,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  verifyAddressWitness,
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
  publishFaultProofFieldCarriageV1,
} from "./field-opening-v1.js";
import {
  parseHex,
  parseSafeNonNegativeInteger,
  requireRecord,
} from "./json-file.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  type ResolvedProverSigner,
  resolveInvalidSignatureDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import { parseSubmitInvalidSignatureWitnessSetCompact } from "./submit-invalid-signature-step-01.js";
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

/**
 * Complete positional address-witness list, as `prepare-invalid-signature`
 * writes it to `invalid-signature-addr-tx-wits-preimage.json`. The commitment
 * fixes the item count as well as each item's content, so the list is only ever
 * accepted whole — a partial list can never open it.
 */
export const parseSubmitInvalidSignatureAddrTxWitsPreimage = (
  value: unknown,
): readonly MidgardAddressWitness[] => {
  if (!Array.isArray(value)) {
    throw new Error("--addr-tx-wits-preimage must be a JSON array.");
  }
  return value.map((item, index) => {
    const label = `--addr-tx-wits-preimage[${index.toString()}]`;
    const entry = requireRecord(item, label);
    return {
      verification_key: parseHex(
        entry.verificationKey,
        `${label}.verificationKey`,
        32,
      ),
      signature: parseHex(entry.signature, `${label}.signature`, 64),
    };
  });
};

export type SubmitInvalidSignatureStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly addrTxWitsPreimagePath: string;
  /**
   * JSON `{ "nativeTxCompactCbor": "<hex>" }` — the disputed transaction's
   * compact structure. New in #604.
   */
  readonly nativeTxCompactPath: string;
  /**
   * The bad transaction's compact witness set, the same file step-01 takes. The
   * door authenticates it against the `witness_set_hash` the thread anchored
   * before reading field 7 out of it.
   */
  readonly witnessSetCompactPath: string;
  readonly badAddrTxWitIndex: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitInvalidSignatureStep02Result = {
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
  readonly secondStepAddress: string;
  readonly badTxId: string;
  /** §4's flat commitment for field 7 — re-derived here and by the door. */
  readonly badAddrTxWitsHash: string;
  /** The witness-set half of `WitnessAnchor`, as the thread carried it. */
  readonly badTxWitnessSetHash: string;
  readonly addrTxWitsPreimageItemCount: number;
  readonly badAddrTxWitIndex: number;
  readonly badAddrTxWitVerificationKey: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type InvalidSignatureStep02DatumWithState = InvalidSignatureStep02Datum & {
  readonly data: NonNullable<InvalidSignatureStep02Datum["data"]>;
};

type InvalidSignatureStep02ResolvedLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

type InvalidSignatureStep02SpendLayout = Omit<
  InvalidSignatureStep02ResolvedLayout,
  "computationThreadMintRedeemerIndex"
>;

const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): InvalidSignatureStep02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, InvalidSignatureStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Invalid-signature step 02 input datum must carry the bad transaction id and its address-witness collection hash.",
    );
  }
  return datum as InvalidSignatureStep02DatumWithState;
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

const makeInvalidSignatureStep02SpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  addrTxWitsOpening,
  badAddrTxWitIndex,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly addrTxWitsOpening: FieldOpeningV1;
  readonly badAddrTxWitIndex: bigint;
  readonly onLayout: (layout: InvalidSignatureStep02SpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "invalid-signature step 02");
    const layout: InvalidSignatureStep02SpendLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "invalid-signature step 02",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputPredicate({
          fraudProofAddress,
          fraudProofUnit,
          fraudProofDatum,
        }),
        "invalid-signature step 02 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "invalid-signature step 02 fraud-proof",
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
            addr_tx_wits_opening: addrTxWitsOpening,
            bad_addr_tx_wit_index: badAddrTxWitIndex,
          },
        ],
      },
      InvalidSignatureStep02SpendRedeemer,
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
      "invalid-signature step 02 fraud-proof mint",
    );
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "invalid-signature step 02 computation-thread burn",
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
      "invalid-signature step 02 computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const submitInvalidSignatureStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  addrTxWitsPreimage,
  nativeTxCompactCbor,
  witnessSetCompact,
  badAddrTxWitIndex,
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
  /** Complete positional address-witness list opened by this step. */
  readonly addrTxWitsPreimage: readonly MidgardAddressWitness[];
  /** The disputed transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  /** That transaction's compact witness set — §2.5's other half. */
  readonly witnessSetCompact: NativeTxWitnessSetCompact;
  readonly badAddrTxWitIndex: bigint;
  /** The published step-02 reference script; inline-attached when absent. */
  readonly referenceScriptUtxo?: UTxO;
  /** Published witness reference scripts; each absent entry inline-attaches. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitInvalidSignatureStep02Result> => {
  const { invalidSignatureCategory, contracts } =
    await resolveInvalidSignatureDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "invalid-signature step-02 computation-thread UTxO",
  });
  if (
    threadUtxo.address !==
    contracts.invalidSignature.steps[1].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at invalid-signature step 02.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: invalidSignatureCategory.categoryId,
    categoryLabel: "invalid-signature",
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });
  const badTxId = inputDatum.data.bad_tx_id;
  const badTxWitnessSetHash = inputDatum.data.bad_tx_witness_set_hash;

  // Mirror every check the door makes, in its order: the compact bytes
  // re-derive to the anchored id, the supplied witness set hashes to the
  // anchored `witness_set_hash`, and the supplied witness list is the §5.1
  // preimage that witness set commits at field 7.
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.addressWitnesses,
    anchorTxId: badTxId,
    nativeTxCompactCbor,
    itemCbors: addrTxWitsPreimage.map(encodeMidgardAddressWitnessCanonicalV1),
    owner: signer.paymentKeyHash,
    witnessSet: witnessSetCompact,
    anchorWitnessSetHash: badTxWitnessSetHash,
    label: "Invalid-signature step 02 address-witnesses",
  });
  const badAddrTxWitsHash = planned.commitment;

  signer.selectWallet(lucid);
  // §8.4: publish tier-2 field carriage before the final transaction selects
  // fee inputs or resolves indices into the complete reference-input set.
  const published = await publishFaultProofFieldCarriageV1({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "Invalid-signature step 02 address-witnesses field",
  });
  const stepScriptCarriage = witnessSpendingValidatorCarriageV1({
    script: contracts.invalidSignature.steps[1].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "invalid-signature step 02 validator",
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "invalid-signature step 02 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "invalid-signature step 02 fraud-proof mint",
  });
  // The complete reference-input set, built before the field opening derives
  // any carriage indices from it.
  const referenceInputs = [
    ...published,
    ...stepScriptCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const addrTxWitsOpening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    label: "Invalid-signature step 02 address-witnesses",
  });
  const badAddrTxWit = addrTxWitsPreimage[Number(badAddrTxWitIndex)];
  if (badAddrTxWit === undefined) {
    throw new Error(
      `--bad-addr-tx-wit-index ${badAddrTxWitIndex.toString()} is out of range for a ${addrTxWitsPreimage.length.toString()}-witness preimage.`,
    );
  }
  if (verifyAddressWitness({ txId: badTxId, witness: badAddrTxWit })) {
    throw new Error(
      `Address witness ${badAddrTxWitIndex.toString()} signs transaction ${badTxId} validly, so it does not violate the signature ledger rule.`,
    );
  }

  // A tier-2 publication sits at the prover address under a large inline datum
  // (and its min-ADA), so it tops the fee selector's descending-lovelace sort;
  // exclude datum-carrying UTxOs so the referenced publication is never spent.
  const feeInput = selectFeeInput(
    (await lucid.wallet().getUtxos()).filter(
      (utxo) => utxo.datum == null && utxo.datumHash == null,
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
  let spendLayout: InvalidSignatureStep02SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const withInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeInvalidSignatureStep02SpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        addrTxWitsOpening,
        badAddrTxWitIndex,
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
  // `readFrom([])` is an error rather than a no-op, so the branch is on
  // whether any witness published a reference script at all.
  const chained =
    referenceInputs.length === 0
      ? withInputs
      : withInputs.readFrom(referenceInputs);
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(stepScriptCarriage.attach(chained)),
  );

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve invalid-signature step 02 layout.",
    );
  }
  const resolvedLayout: InvalidSignatureStep02ResolvedLayout = {
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
    secondStepAddress:
      contracts.invalidSignature.steps[1].spendingScriptAddress,
    badTxId,
    badAddrTxWitsHash,
    badTxWitnessSetHash,
    addrTxWitsPreimageItemCount: addrTxWitsPreimage.length,
    badAddrTxWitIndex: Number(badAddrTxWitIndex),
    badAddrTxWitVerificationKey: badAddrTxWit.verification_key,
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

export const submitInvalidSignatureStep02FromFiles = async (
  config: SubmitInvalidSignatureStep02CliConfig,
): Promise<SubmitInvalidSignatureStep02Result> => {
  const [
    blueprint,
    deploymentInfo,
    addrTxWitsPreimageJson,
    nativeTxCompactJson,
    witnessSetCompactJson,
    lucid,
  ] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.addrTxWitsPreimagePath),
    readJsonFile(config.nativeTxCompactPath),
    readJsonFile(config.witnessSetCompactPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitInvalidSignatureStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    addrTxWitsPreimage: parseSubmitInvalidSignatureAddrTxWitsPreimage(
      addrTxWitsPreimageJson,
    ),
    nativeTxCompactCbor: parseNativeTxCompactCborV1(
      nativeTxCompactJson,
      "--native-tx-compact",
    ),
    witnessSetCompact: parseSubmitInvalidSignatureWitnessSetCompact(
      witnessSetCompactJson,
    ),
    badAddrTxWitIndex: parseSafeNonNegativeInteger(
      config.badAddrTxWitIndex,
      "--bad-addr-tx-wit-index",
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};
