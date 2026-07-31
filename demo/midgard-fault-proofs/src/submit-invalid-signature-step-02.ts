import {
  computeAddressWitnessesHash,
  computeWitnessSetHash,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  InvalidSignatureStep02Datum,
  InvalidSignatureStep02SpendRedeemer,
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
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";

export type SubmitInvalidSignatureStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly witnessSetPreimagePath: string;
  readonly addrTxWitsPreimagePath: string;
  readonly badAddressWitnessIndex: string;
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
  readonly badTxWitsHash: string;
  readonly addrTxWitsHash: string;
  readonly badAddressWitnessIndex: number;
  readonly badAddressWitnessVerificationKey: string;
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
      "Invalid-signature step 02 input datum must carry the bad transaction's id and witness set hash.",
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
  witnessSetPreimage,
  addrTxWitsPreimage,
  badAddressWitnessIndex,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly witnessSetPreimage: NativeTxWitnessSetCompact;
  readonly addrTxWitsPreimage: readonly MidgardAddressWitness[];
  readonly badAddressWitnessIndex: bigint;
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
            witness_set_preimage: witnessSetPreimage,
            addr_tx_wits_preimage: [...addrTxWitsPreimage],
            bad_address_witness_index: badAddressWitnessIndex,
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
  witnessSetPreimage,
  addrTxWitsPreimage,
  badAddressWitnessIndex,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly witnessSetPreimage: NativeTxWitnessSetCompact;
  readonly addrTxWitsPreimage: readonly MidgardAddressWitness[];
  readonly badAddressWitnessIndex: bigint;
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
  const { bad_tx_id: badTxId, bad_tx_wits_hash: badTxWitsHash } =
    inputDatum.data;

  // Mirror every category-specific check the validator makes, so a proof that
  // cannot conclude fails here instead of burning a submission on-chain.
  const recomputedWitnessSetHash = computeWitnessSetHash(witnessSetPreimage);
  if (recomputedWitnessSetHash !== badTxWitsHash) {
    throw new Error(
      `Witness set preimage hashes to ${recomputedWitnessSetHash}, which does not match the witness set hash ${badTxWitsHash} carried by the step-02 datum.`,
    );
  }
  const recomputedAddrTxWitsHash =
    computeAddressWitnessesHash(addrTxWitsPreimage);
  if (recomputedAddrTxWitsHash !== witnessSetPreimage.addr_tx_wits_hash) {
    throw new Error(
      `Address witnesses preimage hashes to ${recomputedAddrTxWitsHash}, which does not match the addr_tx_wits_hash ${witnessSetPreimage.addr_tx_wits_hash} in the witness set preimage.`,
    );
  }
  const badWitness = addrTxWitsPreimage[Number(badAddressWitnessIndex)];
  if (badWitness === undefined) {
    throw new Error(
      `Bad address witness index ${badAddressWitnessIndex.toString()} is out of range of the ${addrTxWitsPreimage.length}-witness preimage.`,
    );
  }
  if (verifyAddressWitness({ txId: badTxId, witness: badWitness })) {
    throw new Error(
      `Address witness at index ${badAddressWitnessIndex.toString()} carries a valid signature for transaction ${badTxId}, so it does not violate the signature ledger rule.`,
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
  let spendLayout: InvalidSignatureStep02SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const tx = lucid
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
        witnessSetPreimage,
        addrTxWitsPreimage,
        badAddressWitnessIndex,
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
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(
      contracts.invalidSignature.steps[1].spendingScript,
    )
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript);

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
    badTxWitsHash,
    addrTxWitsHash: witnessSetPreimage.addr_tx_wits_hash,
    badAddressWitnessIndex: Number(badAddressWitnessIndex),
    badAddressWitnessVerificationKey: badWitness.verification_key,
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
    witnessSetPreimageJson,
    addrTxWitsPreimageJson,
    lucid,
  ] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.witnessSetPreimagePath),
    readJsonFile(config.addrTxWitsPreimagePath),
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
    witnessSetPreimage: parseWitnessSetPreimage(witnessSetPreimageJson),
    addrTxWitsPreimage: parseAddrTxWitsPreimage(addrTxWitsPreimageJson),
    badAddressWitnessIndex: BigInt(config.badAddressWitnessIndex),
    awaitConfirmation: config.awaitConfirmation,
  });
};

const requireHex32 = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/.test(value)) {
    throw new Error(`${label} must be a 32-byte lowercase hex string.`);
  }
  return value;
};

export const parseWitnessSetPreimage = (
  value: unknown,
): NativeTxWitnessSetCompact => {
  if (typeof value !== "object" || value === null) {
    throw new Error("Witness set preimage must be a JSON object.");
  }
  const record = value as Record<string, unknown>;
  return {
    addr_tx_wits_hash: requireHex32(
      record.addrTxWitsHash ?? record.addr_tx_wits_hash,
      "witnessSetPreimage.addrTxWitsHash",
    ),
    script_tx_wits_hash: requireHex32(
      record.scriptTxWitsHash ?? record.script_tx_wits_hash,
      "witnessSetPreimage.scriptTxWitsHash",
    ),
    redeemer_tx_wits_hash: requireHex32(
      record.redeemerTxWitsHash ?? record.redeemer_tx_wits_hash,
      "witnessSetPreimage.redeemerTxWitsHash",
    ),
  };
};

export const parseAddrTxWitsPreimage = (
  value: unknown,
): readonly MidgardAddressWitness[] => {
  const entries = Array.isArray(value)
    ? value
    : typeof value === "object" &&
        value !== null &&
        Array.isArray((value as { addrTxWits?: unknown }).addrTxWits)
      ? (value as { addrTxWits: unknown[] }).addrTxWits
      : undefined;
  if (entries === undefined) {
    throw new Error(
      "Address witnesses preimage must be a JSON array, or an object with an addrTxWits array.",
    );
  }
  return entries.map((entry, index) => {
    if (typeof entry !== "object" || entry === null) {
      throw new Error(`addrTxWits[${index}] must be a JSON object.`);
    }
    const record = entry as Record<string, unknown>;
    const verificationKey =
      record.verificationKey ?? record.verification_key ?? record.vkey;
    const signature = record.signature;
    if (
      typeof verificationKey !== "string" ||
      !/^[0-9a-f]{64}$/.test(verificationKey)
    ) {
      throw new Error(
        `addrTxWits[${index}].verificationKey must be a 32-byte lowercase hex string.`,
      );
    }
    if (typeof signature !== "string" || !/^[0-9a-f]{128}$/.test(signature)) {
      throw new Error(
        `addrTxWits[${index}].signature must be a 64-byte lowercase hex string.`,
      );
    }
    return { verification_key: verificationKey, signature };
  });
};
