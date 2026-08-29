/**
 * `native-script-decoding` thread init (offchain plan §4.2).
 *
 * Mirrors the generic tail of `src/submit-init.ts` — catalogue, hub-oracle
 * and fraudulent-block reference inputs, the PHAS membership withdrawal
 * carrying the category proof, and the computation-thread `Init` mint — but
 * takes the family's explicit contracts record and catalogue category: the
 * `nativeScriptDecoding` entry in the production `submitInit` category union
 * is parent-owned and lands at registration (design §10 Q2).
 */
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
  NATIVE_SCRIPT_DECODING_CATEGORY_LABEL,
  type NativeScriptDecodingContractsV1,
  type NativeScriptDecodingStepContractV1,
} from "./contracts-v1.js";
import {
  type NativeScriptDecodingCatalogueCategoryV1,
  nativeScriptDecodingSubmitError,
} from "./submit-common-v1.js";

type LucidDataSchema = Parameters<typeof Data.to>[1];

export type NativeScriptDecodingInitContractsV1 = Omit<
  NativeScriptDecodingContractsV1,
  "steps"
> & {
  readonly steps: readonly [
    NativeScriptDecodingStepContractV1,
    ...NativeScriptDecodingStepContractV1[],
  ];
};

export type SubmitNativeScriptDecodingInitResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly firstStepAddress: string;
  readonly firstStepOutputIndex: number;
  /** `txHash#index` of the freshly minted thread, ready for step-01. */
  readonly nextThreadOutRef: string;
  readonly fraudCategoryId: string;
  readonly awaitedConfirmation: boolean;
};

export const submitNativeScriptDecodingInit = async ({
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
  readonly contracts: NativeScriptDecodingInitContractsV1;
  readonly category: NativeScriptDecodingCatalogueCategoryV1;
  /** The deployed fraud-proof catalogue: its NFT policy, spend address, and MPF root. */
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
}): Promise<SubmitNativeScriptDecodingInitResult> => {
  // The registered category must be the very step-01 this chain deploys —
  // a divergence would mint a thread the family cannot spend.
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw nativeScriptDecodingSubmitError(
      `catalogue category registers ${category.scriptHash}, but the deployed step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }

  const [catalogueUtxo, hubOracleUtxo, fraudulentBlockUtxo] = await Promise.all(
    [
      requireSingletonUtxo({
        lucid,
        address: catalogue.spendingScriptAddress,
        unit: toUnit(catalogue.policyId, FRAUD_PROOF_CATALOGUE_ASSET_NAME),
        label: `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} init fraud-proof catalogue`,
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(contracts.hubOraclePolicyId),
        ),
        unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} init hub oracle`,
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          fraudulentBlockOutRef,
          "--fraudulent-block-out-ref",
        ),
        label: `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} fraudulent block UTxO`,
      }),
    ],
  );
  const resolvedHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo,
    configuredHeaderHash: fraudulentHeaderHash,
  });
  const computationThreadAssetName = `${category.categoryId}${resolvedHeaderHash}`;
  const computationThreadUnit = toUnit(
    contracts.computationThread.policyId,
    computationThreadAssetName,
  );
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} init computation-thread mint`,
  });
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriageV1({
    script: phasMembershipScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} init PHAS membership`,
  });
  const referenceInputs = [
    catalogueUtxo,
    hubOracleUtxo,
    fraudulentBlockUtxo,
    ...computationThreadMintCarriage.referenceInputs,
    ...phasMembershipCarriage.referenceInputs,
  ];
  const firstStepDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: null },
    FraudProofComputationThreadStepDatum,
  );
  const firstStepOutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[0].spendingScriptAddress,
    datum: firstStepDatum,
    unit: computationThreadUnit,
  });
  let firstStepOutputIndex: bigint | undefined;
  const computationThreadMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} init computation-thread mint`,
    );
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      firstStepOutputMatches,
      `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} init first step`,
    );
    firstStepOutputIndex = outputIndex;
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
            `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} init fraud-proof catalogue`,
          ),
          inclusion_proof_script_redeemer_index: requireWithdrawalRedeemerIndex(
            ctx,
            phasRewardAddress,
            `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} init PHAS membership`,
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} init hub oracle`,
          ),
          fraudulent_block_ref_input_index: requireReferenceInputIndex(
            ctx,
            fraudulentBlockUtxo,
            `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} init fraudulent block`,
          ),
        },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  const chainedTx = lucid
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
    .mintAssets({ [computationThreadUnit]: 1n }, computationThreadMintRedeemer)
    .pay.ToContract(
      contracts.steps[0].spendingScriptAddress,
      { kind: "inline", value: firstStepDatum },
      { [computationThreadUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = phasMembershipCarriage.attach(
    computationThreadMintCarriage.attach(chainedTx),
  );

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (firstStepOutputIndex === undefined) {
    throw nativeScriptDecodingSubmitError(
      "BuildTxWithRedeemer did not resolve the init output index.",
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
    fraudulentBlockOutRef,
    fraudulentHeaderHash: resolvedHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName,
    computationThreadUnit,
    firstStepAddress: contracts.steps[0].spendingScriptAddress,
    firstStepOutputIndex: Number(firstStepOutputIndex),
    nextThreadOutRef: `${txHash}#${firstStepOutputIndex.toString()}`,
    fraudCategoryId: category.categoryId,
    awaitedConfirmation: awaitConfirmation,
  };
};
