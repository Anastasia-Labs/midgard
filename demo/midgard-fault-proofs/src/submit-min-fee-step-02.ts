/** Authenticate all nine field lengths and finalize a strict min-fee fault. */
import {
  encodeMidgardNativeTxProofFieldLengthsV1,
  encodeMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";
import {
  type FieldCarriageV1,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  hasMinFeeViolationV1,
  type MinFeeStep02Args,
  MinFeeStep02Datum,
  MinFeeStep02SpendRedeemer,
  type MinFeeStep02State,
  minimumFeeFromProofSourceV1,
  type NativeTxWitnessSetCompact,
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
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldCarriageV1,
  type FaultProofFieldOpeningPlanV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "./field-opening-v1.js";
import type { MinFeeContractsV1 } from "./min-fee-contracts-v1.js";
import {
  minFeeStepLabelV1,
  minFeeSubmitError,
  requireMinFeeReferenceScriptV1,
  requireMinFeeStepStateV1,
  requireMinFeeThreadUtxoV1,
} from "./min-fee-submit-common-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "./runtime.js";
import { excludeUtxo } from "./spend-input-witness.js";
import { selectFeeInput } from "./submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";

const STEP_LABEL = minFeeStepLabelV1(1);
const FIELD_COUNT = 9;

export type MinFeeFieldItemCborsV1 = readonly [
  readonly Uint8Array[],
  readonly Uint8Array[],
  readonly Uint8Array[],
  readonly Uint8Array[],
  readonly Uint8Array[],
  readonly Uint8Array[],
  readonly Uint8Array[],
  readonly Uint8Array[],
  readonly Uint8Array[],
];

const uniqueUtxos = (utxos: readonly UTxO[]): readonly UTxO[] => {
  const seen = new Set<string>();
  return utxos.filter((utxo) => {
    const key = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
};

const witnessSetCore = (witnessSet: NativeTxWitnessSetCompact) => ({
  addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
  scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
  redeemerTxWitsHash: Buffer.from(witnessSet.redeemer_tx_wits_hash, "hex"),
});

const planFields = ({
  state,
  nativeTxCompactCbor,
  witnessSet,
  fieldItemCbors,
  signer,
  publishCarriages,
}: {
  readonly state: MinFeeStep02State;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly fieldItemCbors: MinFeeFieldItemCborsV1;
  readonly signer: ResolvedProverSigner;
  readonly publishCarriages: boolean;
}): readonly FaultProofFieldOpeningPlanV1[] =>
  fieldItemCbors.map((itemCbors, fieldIndex) =>
    planFaultProofFieldOpeningV1({
      fieldIndex,
      anchorTxId: state.bad_tx_id,
      nativeTxCompactCbor,
      itemCbors,
      owner: signer.paymentKeyHash,
      publish: publishCarriages,
      ...(fieldIndex < 6
        ? {}
        : {
            witnessSet,
            anchorWitnessSetHash: state.bad_tx.witness_set_hash,
          }),
      label: `${STEP_LABEL} field ${fieldIndex.toString()}`,
    }),
  );

export type SubmitMinFeeStep02Result = {
  readonly txHash: string;
  readonly fraudProofOutRef: string;
  readonly fraudProofUnit: string;
  readonly computationThreadUnit: string;
  readonly fraudulentHeaderHash: string;
  readonly canonicalTxSize: bigint;
  readonly minimumFee: bigint;
  readonly fee: bigint;
  readonly fieldPreimageLengths: readonly number[];
  readonly fieldCarriageTiers: readonly string[];
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMinFeeStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  fieldItemCbors,
  referenceScriptUtxo,
  certificateUtxos = [],
  publishCarriages = false,
  unsafeSkipLocalViolationCheckForTest = false,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MinFeeContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly fieldItemCbors: MinFeeFieldItemCborsV1;
  /** Mandatory: min-fee validators are reference-script-only. */
  readonly referenceScriptUtxo: UTxO;
  /** Existing §8.6 certificates, needed only when a field selects tier 3. */
  readonly certificateUtxos?: readonly UTxO[];
  /** Force publication of otherwise-inline fields to reduce redeemer size. */
  readonly publishCarriages?: boolean;
  /** Emulator negative only: submit an honest boundary to the real validator. */
  readonly unsafeSkipLocalViolationCheckForTest?: boolean;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMinFeeStep02Result> => {
  if (fieldItemCbors.length !== FIELD_COUNT) {
    throw minFeeSubmitError("step-02 requires exactly nine field preimages.");
  }
  const { threadUtxo, threadToken } = await requireMinFeeThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    stepIndex: 1,
    threadOutRef,
  });
  const state = requireMinFeeStepStateV1({
    threadUtxo,
    signer,
    schema: MinFeeStep02Datum,
  });
  if (state.bad_tx_body_fee !== state.bad_tx.body.fee) {
    throw minFeeSubmitError(
      "step-02 state fee disagrees with its compact body.",
    );
  }
  const plans = planFields({
    state,
    nativeTxCompactCbor,
    witnessSet,
    fieldItemCbors,
    signer,
    publishCarriages,
  });
  const source = {
    compactCbor: Buffer.from(nativeTxCompactCbor, "hex"),
    witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompactV1(
      witnessSetCore(witnessSet),
    ),
    fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengthsV1(
      plans.map((plan) => plan.preimage.length),
    ),
  };
  const boundary = minimumFeeFromProofSourceV1({
    source,
    minFeeA: state.min_fee_a,
    minFeeB: state.min_fee_b,
  });
  const violation = hasMinFeeViolationV1({
    fee: state.bad_tx_body_fee,
    minFeeA: state.min_fee_a,
    minFeeB: state.min_fee_b,
    canonicalTxSize: boundary.canonicalTxSize,
  });
  if (!violation && !unsafeSkipLocalViolationCheckForTest) {
    throw minFeeSubmitError(
      `honest fee ${state.bad_tx_body_fee.toString()} satisfies exact minimum ${boundary.minimumFee.toString()}.`,
    );
  }

  signer.selectWallet(lucid);
  const published: UTxO[] = [];
  // Publication transactions share the prover wallet, so serialize them to
  // avoid selecting the same fee input before the preceding spend confirms.
  for (const [fieldIndex, planned] of plans.entries()) {
    published.push(
      ...(await publishFaultProofFieldCarriageV1({
        lucid,
        signer,
        planned,
        publisherAddress: signer.address,
        label: `${STEP_LABEL} field ${fieldIndex.toString()}`,
      })),
    );
  }
  const stepReference = requireMinFeeReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    stepIndex: 1,
  });
  const referenceInputs = uniqueUtxos([
    ...published,
    ...certificateUtxos,
    stepReference,
  ]);
  const fieldCarriages = plans.map(
    (planned, fieldIndex): FieldCarriageV1 =>
      faultProofFieldCarriageV1({
        planned,
        referenceInputs,
        certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        label: `${STEP_LABEL} field ${fieldIndex.toString()}`,
      }),
  ) as unknown as MinFeeStep02Args["field_carriages"];

  // A fresh tier-2 publication carries enough min-ADA to top the fee sort;
  // never spend anything this transaction must read.
  const feeInput = selectFeeInput(
    referenceInputs.reduce<readonly UTxO[]>(
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
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let spendLayout:
    | {
        readonly inputIndex: bigint;
        readonly outputIndex: bigint;
        readonly fraudProofMintRedeemerIndex: bigint;
      }
    | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${STEP_LABEL} proof output`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        `${STEP_LABEL} proof mint`,
      ),
    };
    spendLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            native_tx_compact_cbor: nativeTxCompactCbor,
            witness_set: witnessSet,
            field_carriages: fieldCarriages,
          },
        ],
      },
      MinFeeStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} thread burn`,
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const proofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      `${STEP_LABEL} proof mint`,
    );
    const threadRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} thread burn`,
    );
    computationThreadMintRedeemerIndex = threadRedeemerIndex;
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: threadRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([...referenceInputs])
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, proofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw minFeeSubmitError("step-02 layout was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    fraudProofOutRef: `${txHash}#${spendLayout.outputIndex.toString()}`,
    fraudProofUnit,
    computationThreadUnit: threadToken.unit,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    canonicalTxSize: boundary.canonicalTxSize,
    minimumFee: boundary.minimumFee,
    fee: state.bad_tx_body_fee,
    fieldPreimageLengths: plans.map((plan) => plan.preimage.length),
    fieldCarriageTiers: plans.map((plan) => plan.plan.tier),
    inputIndex: Number(spendLayout.inputIndex),
    outputIndex: Number(spendLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
