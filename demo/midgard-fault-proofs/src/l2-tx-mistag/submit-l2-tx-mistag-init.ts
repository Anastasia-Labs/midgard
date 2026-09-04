import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
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
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import {
  L2_TX_MISTAG_CATEGORY_LABEL,
  type L2TxMistagContracts,
} from "./contracts.js";
import {
  type L2TxMistagCatalogueCategory,
  l2TxMistagSubmitError,
} from "./submit-common.js";

export type SubmitL2TxMistagInitResult = {
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
  readonly nextThreadOutRef: string;
  readonly fraudCategoryId: string;
  readonly awaitedConfirmation: boolean;
};

export const submitL2TxMistagInit = async ({
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
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: L2TxMistagContracts;
  readonly category: L2TxMistagCatalogueCategory;
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly signer: ResolvedProverSigner;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash?: string;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitL2TxMistagInitResult> => {
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw l2TxMistagSubmitError(
      `catalogue category registers ${category.scriptHash}, but step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  if (!/^[0-9a-f]{8}$/u.test(category.categoryId)) {
    throw l2TxMistagSubmitError(
      "category id must be four bytes of lowercase hex.",
    );
  }

  const [catalogueUtxo, hubOracleUtxo, fraudulentBlockUtxo] = await Promise.all(
    [
      requireSingletonUtxo({
        lucid,
        address: catalogue.spendingScriptAddress,
        unit: toUnit(catalogue.policyId, FRAUD_PROOF_CATALOGUE_ASSET_NAME),
        label: `${L2_TX_MISTAG_CATEGORY_LABEL} init fraud-proof catalogue`,
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(contracts.hubOraclePolicyId),
        ),
        unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: `${L2_TX_MISTAG_CATEGORY_LABEL} init hub oracle`,
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          fraudulentBlockOutRef,
          "--fraudulent-block-out-ref",
        ),
        label: `${L2_TX_MISTAG_CATEGORY_LABEL} fraudulent block UTxO`,
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
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${L2_TX_MISTAG_CATEGORY_LABEL} init computation-thread mint`,
  });
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriage({
    script: phasMembershipScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${L2_TX_MISTAG_CATEGORY_LABEL} init PHAS membership`,
  });
  const referenceInputs = [
    catalogueUtxo,
    hubOracleUtxo,
    fraudulentBlockUtxo,
    ...computationThreadMintCarriage.referenceInputs,
    ...phasMembershipCarriage.referenceInputs,
  ];
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
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
  const mintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${L2_TX_MISTAG_CATEGORY_LABEL} init computation-thread mint`,
    );
    firstStepOutputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      firstStepOutputMatches,
      `${L2_TX_MISTAG_CATEGORY_LABEL} init first step`,
    );
    return Data.to(
      {
        Init: {
          first_step_output_index: firstStepOutputIndex,
          fraud_category_id: category.categoryId,
          fraud_category: category.scriptHash,
          fraud_category_membership_proof: Data.from(
            category.membershipProofCbor,
            Proof,
          ),
          fraud_proof_catalogue_ref_input_index: requireReferenceInputIndex(
            ctx,
            catalogueUtxo,
            `${L2_TX_MISTAG_CATEGORY_LABEL} init catalogue`,
          ),
          inclusion_proof_script_redeemer_index: requireWithdrawalRedeemerIndex(
            ctx,
            phasRewardAddress,
            `${L2_TX_MISTAG_CATEGORY_LABEL} init membership`,
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            `${L2_TX_MISTAG_CATEGORY_LABEL} init hub oracle`,
          ),
          fraudulent_block_ref_input_index: requireReferenceInputIndex(
            ctx,
            fraudulentBlockUtxo,
            `${L2_TX_MISTAG_CATEGORY_LABEL} init fraudulent block`,
          ),
        },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  const tx = lucid
    .newTx()
    .readFrom(referenceInputs)
    .withdraw(
      phasRewardAddress,
      0n,
      encodePhasMembershipProofRedeemer({
        root: catalogue.root,
        keyCbor: Data.to(
          category.categoryId,
          asLucidSchema(
            Data.Bytes({
              minLength: 4,
              maxLength: 4,
            }),
          ),
        ),
        valueCbor: Data.to(
          category.scriptHash,
          asLucidSchema(
            Data.Bytes({
              minLength: 28,
              maxLength: 28,
            }),
          ),
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

  const unsigned = await phasMembershipCarriage
    .attach(computationThreadMintCarriage.attach(tx))
    .complete({ localUPLCEval: true });
  if (firstStepOutputIndex === undefined) {
    throw l2TxMistagSubmitError("init output index was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof computation-thread minting",
          utxo: witnessReferenceScripts?.computationThreadMint,
          expectedScript: contracts.computationThread.mintingScript,
        },
        {
          role: "membership proof withdrawal",
          utxo: witnessReferenceScripts?.phasMembershipWithdraw,
          expectedScript: phasMembershipScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw l2TxMistagSubmitError(
      `init provider returned ${txHash}, expected ${expectedTxHash}.`,
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
