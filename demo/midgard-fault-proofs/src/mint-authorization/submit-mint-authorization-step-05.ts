/**
 * `mint-authorization` step-05 submitter: the proven verdict finalizes —
 * the computation-thread NFT burns and the permanent fraud-proof token
 * mints to the fraud-proof address under the generic finalization
 * validations.
 *
 * The validator's closed-verdict shape gate is re-checked locally first:
 * the thread state must carry a direction in the family's {0, 1} domain.
 */
import type { MintAuthorizationStep05StateV1 } from "@al-ft/midgard-sdk";
import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED_V1,
  MintAuthorizationStep05Datum,
  MintAuthorizationStep05SpendRedeemer,
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
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
  witnessSpendingValidatorCarriageV1,
} from "../witness-reference-scripts-v1.js";
import type { MintAuthorizationContractsV1 } from "./contracts-v1.js";
import {
  mintAuthorizationStepLabelV1,
  mintAuthorizationSubmitError,
  requireMintAuthorizationReferenceScriptV1,
  requireMintAuthorizationStepStateV1,
  requireMintAuthorizationThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = mintAuthorizationStepLabelV1(4);

/** Twin of the validator's closed-verdict shape gate. */
export const assertMintAuthorizationStep05FinalizableV1 = (
  state: MintAuthorizationStep05StateV1,
): void => {
  if (
    state.direction !== MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1 &&
    state.direction !== MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED_V1
  ) {
    throw mintAuthorizationSubmitError(
      `thread state carries direction ${state.direction.toString()}, outside {0, 1}.`,
    );
  }
};

export type SubmitMintAuthorizationStep05Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAssetName: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  /** The closed verdict the token finalized. */
  readonly verdictState: MintAuthorizationStep05StateV1;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step05SpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

export const submitMintAuthorizationStep05 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintAuthorizationContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The mandatory published step-05 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Published witness reference scripts required by this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMintAuthorizationStep05Result> => {
  const { threadUtxo, threadToken } =
    await requireMintAuthorizationThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 4,
      threadOutRef,
    });
  const state: MintAuthorizationStep05StateV1 =
    requireMintAuthorizationStepStateV1({
      threadUtxo,
      signer,
      schema: MintAuthorizationStep05Datum,
      stepIndex: 4,
    });
  assertMintAuthorizationStep05FinalizableV1(state);

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
  const fraudProofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let spendLayout: Step05SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout: Step05SpendLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputMatches,
        `${STEP_LABEL} fraud-proof`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        `${STEP_LABEL} fraud-proof`,
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
          },
        ],
      },
      MintAuthorizationStep05SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} computation-thread burn`,
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const fraudProofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      `${STEP_LABEL} fraud-proof mint`,
    );
    computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} computation-thread burn`,
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${STEP_LABEL} computation-thread mint`,
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: `${STEP_LABEL} fraud-proof mint`,
  });
  const stepReference =
    referenceScriptUtxo === undefined
      ? undefined
      : requireMintAuthorizationReferenceScriptV1({
          utxo: referenceScriptUtxo,
          expectedScriptHash: contracts.steps[4].spendingScriptHash,
          stepIndex: 4,
        });
  const stepCarriage = witnessSpendingValidatorCarriageV1({
    script: contracts.steps[4].spendingScript,
    referenceUtxo: stepReference,
    label: `${STEP_LABEL} spending validator`,
  });
  const referenceInputs = [
    ...stepCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const withReferences =
    referenceInputs.length === 0 ? base : base.readFrom(referenceInputs);
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(stepCarriage.attach(withReferences)),
  );

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw mintAuthorizationSubmitError(
      "BuildTxWithRedeemer did not resolve the step-05 layout.",
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
    fraudProofOutRef: `${txHash}#${spendLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    verdictState: state,
    inputIndex: Number(spendLayout.inputIndex),
    outputIndex: Number(spendLayout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      spendLayout.fraudProofMintRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};
