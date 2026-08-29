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
  DEFAULT_CONFIRMATION_POLL_MS,
  encodePhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
  witnessWithdrawalValidatorCarriageV1,
} from "../witness-reference-scripts-v1.js";
import {
  WITHDRAWN_INPUT_CATEGORY_LABEL,
  type WithdrawnInputContractsV1,
} from "./contracts-v1.js";
import {
  type WithdrawnInputCatalogueCategoryV1,
  withdrawnInputSubmitError,
} from "./submit-common-v1.js";

type LucidDataSchema = Parameters<typeof Data.to>[1];

export type SubmitWithdrawnInputInitResult = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly firstStepAddress: string;
  readonly fraudCategoryId: string;
};

export const submitWithdrawnInputInit = async ({
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
  readonly contracts: WithdrawnInputContractsV1;
  readonly category: WithdrawnInputCatalogueCategoryV1;
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly signer: ResolvedProverSigner;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash?: string;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitWithdrawnInputInitResult> => {
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw withdrawnInputSubmitError(
      `catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  const [catalogueUtxo, hubOracleUtxo, fraudulentBlockUtxo] = await Promise.all(
    [
      requireSingletonUtxo({
        lucid,
        address: catalogue.spendingScriptAddress,
        unit: toUnit(catalogue.policyId, FRAUD_PROOF_CATALOGUE_ASSET_NAME),
        label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} catalogue`,
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(contracts.hubOraclePolicyId),
        ),
        unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} hub oracle`,
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          fraudulentBlockOutRef,
          "--fraudulent-block-out-ref",
        ),
        label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} fraudulent block`,
      }),
    ],
  );
  const headerHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo,
    configuredHeaderHash: fraudulentHeaderHash,
  });
  const computationThreadAssetName = `${category.categoryId}${headerHash}`;
  const computationThreadUnit = toUnit(
    contracts.computationThread.policyId,
    computationThreadAssetName,
  );
  const membershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const membershipAddress = phasMembershipRewardAddress(
    network,
    membershipScript,
  );
  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} init computation-thread mint`,
  });
  const membershipCarriage = witnessWithdrawalValidatorCarriageV1({
    script: membershipScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} init PHAS membership`,
  });
  const referenceInputs = [
    catalogueUtxo,
    hubOracleUtxo,
    fraudulentBlockUtxo,
    ...computationThreadMintCarriage.referenceInputs,
    ...membershipCarriage.referenceInputs,
  ];
  const firstStepDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: null },
    FraudProofComputationThreadStepDatum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[0].spendingScriptAddress,
    datum: firstStepDatum,
    unit: computationThreadUnit,
  });
  let outputIndex: bigint | undefined;
  const mintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${WITHDRAWN_INPUT_CATEGORY_LABEL} init`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${WITHDRAWN_INPUT_CATEGORY_LABEL} first step`,
    );
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
            `${WITHDRAWN_INPUT_CATEGORY_LABEL} catalogue`,
          ),
          inclusion_proof_script_redeemer_index: requireWithdrawalRedeemerIndex(
            ctx,
            membershipAddress,
            `${WITHDRAWN_INPUT_CATEGORY_LABEL} catalogue membership`,
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            `${WITHDRAWN_INPUT_CATEGORY_LABEL} hub oracle`,
          ),
          fraudulent_block_ref_input_index: requireReferenceInputIndex(
            ctx,
            fraudulentBlockUtxo,
            `${WITHDRAWN_INPUT_CATEGORY_LABEL} fraudulent block`,
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
      membershipAddress,
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
    .mintAssets({ [computationThreadUnit]: 1n }, mintRedeemer)
    .pay.ToContract(
      contracts.steps[0].spendingScriptAddress,
      { kind: "inline", value: firstStepDatum },
      { [computationThreadUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = membershipCarriage.attach(
    computationThreadMintCarriage.attach(base),
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (outputIndex === undefined) {
    throw withdrawnInputSubmitError("init output index was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    fraudulentHeaderHash: headerHash,
    computationThreadAssetName,
    computationThreadUnit,
    firstStepAddress: contracts.steps[0].spendingScriptAddress,
    fraudCategoryId: category.categoryId,
  };
};
