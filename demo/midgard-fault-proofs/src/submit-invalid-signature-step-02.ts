/**
 * ⚠️ **STALE AS OF #575 — do not build a datum or redeemer from this module
 * and expect chain to accept it. Owner: #579.** The rebind, its three concrete
 * divergences, and why they are not re-derived in this lane are explained once
 * in `docs/fault-proofs/offchain-builder-staleness-575.md`.
 *
 * `invalid-signature` step-02 submitter (Goal task `Q15`, §9.1 output 8).
 *
 * Finalizes the proof: burns the computation thread, mints the permanent
 * fraud-proof token and locks it at the always-fails fraud-proof address.
 *
 * Nothing in the prepared JSON is trusted. The accused transaction id and the
 * canonical `addr_tx_wits_hash` are read back from the **on-chain** step-01
 * datum, the supplied witness list is re-committed with the
 * `bounded_collection_v1.from_items(7, ...)` twin and must equal that hash, and
 * the accused witness is re-tested with the same Ed25519 verification the
 * validator performs. A thread that cannot conclude therefore fails here
 * instead of burning a submission on-chain.
 */
import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  invalidSignatureAddressWitnessesCommitmentV1,
  InvalidSignatureStep02Datum,
  InvalidSignatureStep02SpendRedeemer,
  type MidgardAddressWitness,
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
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";

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
  readonly badAddrTxWitsHash: string;
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
  addrTxWitsPreimage,
  badAddrTxWitIndex,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly addrTxWitsPreimage: readonly MidgardAddressWitness[];
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
            addr_tx_wits_preimage: [...addrTxWitsPreimage],
            bad_addr_tx_wit_index: badAddrTxWitIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
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
  badAddrTxWitIndex,
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
  readonly badAddrTxWitIndex: bigint;
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
  const badAddrTxWitsHash = inputDatum.data.bad_addr_tx_wits_hash;

  // Mirror every category-specific check the validator makes, in its order.
  // Re-derive the canonical collection commitment from the preimage itself and
  // require it to open the state step 01 forwarded.
  const derivedAddrTxWitsHash =
    invalidSignatureAddressWitnessesCommitmentV1(addrTxWitsPreimage);
  if (derivedAddrTxWitsHash !== badAddrTxWitsHash) {
    throw new Error(
      `--addr-tx-wits-preimage does not open the committed address-witness collection: derived=${derivedAddrTxWitsHash}, thread=${badAddrTxWitsHash}.`,
    );
  }
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
        addrTxWitsPreimage,
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
    badAddrTxWitsHash,
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
  const [blueprint, deploymentInfo, addrTxWitsPreimageJson, lucid] =
    await Promise.all([
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
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
    addrTxWitsPreimage: parseSubmitInvalidSignatureAddrTxWitsPreimage(
      addrTxWitsPreimageJson,
    ),
    badAddrTxWitIndex: parseSafeNonNegativeInteger(
      config.badAddrTxWitIndex,
      "--bad-addr-tx-wit-index",
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};
