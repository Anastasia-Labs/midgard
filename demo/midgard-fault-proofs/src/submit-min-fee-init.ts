/** Pre-registration init for the standalone min-fee computation thread. */
import {
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  HUB_ORACLE_ASSET_NAME,
  Proof,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";

import {
  MIN_FEE_CATEGORY_LABEL,
  type MinFeeContractsV1,
} from "./min-fee-contracts-v1.js";
import {
  type MinFeeCatalogueCategoryV1,
  minFeeSubmitError,
} from "./min-fee-submit-common-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodePhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "./runtime.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
  witnessWithdrawalValidatorCarriageV1,
} from "./witness-reference-scripts-v1.js";

type LucidDataSchema = Parameters<typeof Data.to>[1];

export type SubmitMinFeeInitResult = {
  readonly txHash: string;
  readonly fraudProver: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly nextThreadOutRef: string;
  readonly firstStepOutputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMinFeeInit = async ({
  lucid,
  blueprint,
  network,
  contracts,
  category,
  catalogue,
  signer,
  fraudulentBlockOutRef,
  fraudulentHeaderHash,
  witnessReferenceScripts,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: MinFeeContractsV1;
  readonly category: MinFeeCatalogueCategoryV1;
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly signer: ResolvedProverSigner;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash?: string;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMinFeeInitResult> => {
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw minFeeSubmitError(
      `catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  const [catalogueUtxo, hubOracleUtxo, fraudulentBlockUtxo] = await Promise.all(
    [
      requireSingletonUtxo({
        lucid,
        address: catalogue.spendingScriptAddress,
        unit: toUnit(catalogue.policyId, FRAUD_PROOF_CATALOGUE_ASSET_NAME),
        label: `${MIN_FEE_CATEGORY_LABEL} init catalogue`,
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(contracts.hubOraclePolicyId),
        ),
        unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: `${MIN_FEE_CATEGORY_LABEL} init hub oracle`,
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          fraudulentBlockOutRef,
          "--fraudulent-block-out-ref",
        ),
        label: `${MIN_FEE_CATEGORY_LABEL} fraudulent block`,
      }),
    ],
  );
  const resolvedHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo,
    configuredHeaderHash: fraudulentHeaderHash,
  });
  const assetName = `${category.categoryId}${resolvedHeaderHash}`;
  const threadUnit = toUnit(contracts.computationThread.policyId, assetName);
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(network, phasScript);
  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${MIN_FEE_CATEGORY_LABEL} init computation-thread mint`,
  });
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriageV1({
    script: phasScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${MIN_FEE_CATEGORY_LABEL} init PHAS membership`,
  });
  const referenceInputs = [
    catalogueUtxo,
    hubOracleUtxo,
    fraudulentBlockUtxo,
    ...computationThreadMintCarriage.referenceInputs,
    ...phasMembershipCarriage.referenceInputs,
  ];
  const firstDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: null },
    FraudProofComputationThreadStepDatum,
  );
  const firstOutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[0].spendingScriptAddress,
    datum: firstDatum,
    unit: threadUnit,
  });
  let firstOutputIndex: bigint | undefined;
  const mintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${MIN_FEE_CATEGORY_LABEL} init thread mint`,
    );
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      firstOutputMatches,
      `${MIN_FEE_CATEGORY_LABEL} init first step`,
    );
    firstOutputIndex = outputIndex;
    return Data.to(
      {
        Init: {
          first_step_output_index: outputIndex,
          fraud_category_id: category.categoryId,
          fraud_category: category.scriptHash,
          fraud_category_membership_proof: Data.from(
            category.membershipProofCbor,
            Proof,
          ),
          fraud_proof_catalogue_ref_input_index: requireReferenceInputIndex(
            ctx,
            catalogueUtxo,
            `${MIN_FEE_CATEGORY_LABEL} init catalogue`,
          ),
          inclusion_proof_script_redeemer_index: requireWithdrawalRedeemerIndex(
            ctx,
            phasRewardAddress,
            `${MIN_FEE_CATEGORY_LABEL} init catalogue membership`,
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            `${MIN_FEE_CATEGORY_LABEL} init hub oracle`,
          ),
          fraudulent_block_ref_input_index: requireReferenceInputIndex(
            ctx,
            fraudulentBlockUtxo,
            `${MIN_FEE_CATEGORY_LABEL} init block`,
          ),
        },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  const base = lucid
    .newTx()
    .readFrom(referenceInputs)
    .withdraw(
      phasRewardAddress,
      0n,
      encodePhasMembershipProofRedeemer({
        root: catalogue.root,
        keyCbor: Data.to(
          category.categoryId,
          Data.Bytes({
            minLength: 4,
            maxLength: 4,
          }) as unknown as LucidDataSchema,
        ),
        valueCbor: Data.to(
          category.scriptHash,
          Data.Bytes({
            minLength: 28,
            maxLength: 28,
          }) as unknown as LucidDataSchema,
        ),
        membershipProofCbor: category.membershipProofCbor,
      }),
    )
    .mintAssets({ [threadUnit]: 1n }, mintRedeemer)
    .pay.ToContract(
      contracts.steps[0].spendingScriptAddress,
      { kind: "inline", value: firstDatum },
      { [threadUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = phasMembershipCarriage.attach(
    computationThreadMintCarriage.attach(base),
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (firstOutputIndex === undefined) {
    throw minFeeSubmitError("init output index was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    fraudProver: signer.paymentKeyHash,
    fraudulentHeaderHash: resolvedHeaderHash,
    computationThreadUnit: threadUnit,
    nextThreadOutRef: `${txHash}#${firstOutputIndex.toString()}`,
    firstStepOutputIndex: Number(firstOutputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
