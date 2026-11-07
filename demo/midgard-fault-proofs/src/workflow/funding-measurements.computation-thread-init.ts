import { DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type FraudProofCatalogueCategoryName,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import { CML, Data, type LucidEvolution } from "@lucid-evolution/lucid";

import {
  evaluateWorkflowFundingTransaction,
  measurementOutputCbor,
  measurementOutRef,
  measurementReferenceScript,
} from "./funding-measurements.evaluate.js";
import {
  readWorkflowFundingMeasurementContext,
  WORKFLOW_FUNDING_MEASUREMENT_TOOL,
  type WorkflowFundingMeasurementContext,
} from "./funding-measurements.identity.js";
import { recordWorkflowFundingActionMeasurement } from "./funding-measurements.recording.js";
import { createWorkflowFundingRequirements } from "./funding-requirements.admit-workflow-funding-requirements.js";
import {
  hasFundingPaymentCredential,
  outputValue,
} from "./funding-requirements.canonical-transaction.js";
import { fundingAction } from "./funding-requirements.funding-action.js";
import { type WorkflowFundingAsset } from "./funding-requirements.workflow-funding-controlled-output.js";
import { type LocallyEvaluatedTransaction } from "./transaction-boundary.js";

const assets = (
  value: Readonly<Record<string, bigint>>,
): readonly WorkflowFundingAsset[] =>
  Object.entries(value)
    .filter(([unit]) => unit !== "lovelace")
    .sort(([a], [b]) => a.localeCompare(b, "en"))
    .map(([unit, quantity]) => ({ unit, quantity: quantity.toString() }));

/**
 * Measures the real shared Init builder. Its category, output identity and
 * custody are decoded from the signed transaction; caller labels cannot turn
 * a wallet transfer into a proof. Init alone never seals a lifecycle profile.
 */
export const measureComputationThreadInitFunding = async (input: {
  readonly context: WorkflowFundingMeasurementContext;
  readonly lucid: LucidEvolution;
  readonly transaction: LocallyEvaluatedTransaction;
  readonly category: FraudProofCatalogueCategoryName;
}) => {
  const { identity, manifest } = readWorkflowFundingMeasurementContext(
    input.context,
  );
  const category =
    manifest.contracts.fraudProofCatalogueMint?.fraudProofCatalogue?.categories[
      input.category
    ];
  const firstStep =
    manifest.contracts[
      DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY[input.category]
    ];
  if (
    category === undefined ||
    firstStep === undefined ||
    category.scriptHash !== firstStep.scriptHash
  )
    throw new Error(
      "funding measurement category is absent from the deployed catalogue",
    );
  const measured = await evaluateWorkflowFundingTransaction(input);
  const body = measured.transaction.body();
  const mint = body.mint();
  const policy = manifest.contracts.computationThreadMint!.scriptHash;
  const policies = mint?.keys();
  if (policies?.len() !== 1 || policies.get(0).to_hex() !== policy)
    throw new Error(
      "funding measurement is not a computation-thread Init mint",
    );
  const redeemers = measured.transaction
    .witness_set()
    .redeemers()!
    .to_flat_format();
  if (
    redeemers.len() !== 2 ||
    !Array.from({ length: redeemers.len() }, (_, index) =>
      redeemers.get(index),
    ).some(
      (redeemer) =>
        redeemer.tag() === CML.RedeemerTag.Reward && redeemer.index() === 0n,
    )
  )
    throw new Error(
      "funding measurement Init must invoke exactly the thread mint and membership withdrawal",
    );
  const initRedeemers = [];
  for (let index = 0; index < redeemers.len(); index += 1) {
    const redeemer = redeemers.get(index);
    if (redeemer.tag() === CML.RedeemerTag.Mint && redeemer.index() === 0n)
      initRedeemers.push(redeemer);
  }
  if (initRedeemers.length !== 1)
    throw new Error(
      "funding measurement lacks the unique computation-thread Init redeemer",
    );
  const decoded = Data.from(
    initRedeemers[0]!.data().to_cbor_hex(),
    FraudProofComputationThreadRedeemer,
  );
  if (
    typeof decoded !== "object" ||
    !("Init" in decoded) ||
    decoded.Init.fraud_category_id !== category.categoryId ||
    decoded.Init.fraud_category !== category.scriptHash
  )
    throw new Error(
      "funding measurement Init category/redeemer binding differs",
    );
  const outputIndex = Number(decoded.Init.first_step_output_index);
  const outputHex = measured.canonical.outputCborHex[outputIndex];
  if (!Number.isSafeInteger(outputIndex) || outputHex === undefined)
    throw new Error("funding measurement Init output index is invalid");
  const thread = outputValue(outputHex);
  const threadAssets = assets(thread.assets);
  if (
    threadAssets.length !== 1 ||
    threadAssets[0]!.quantity !== "1" ||
    !threadAssets[0]!.unit.startsWith(policy + category.categoryId) ||
    threadAssets[0]!.unit.length !== 120 ||
    mint!.get(
      policies.get(0),
      CML.AssetName.from_hex(threadAssets[0]!.unit.slice(56)),
    ) !== 1n
  )
    throw new Error("funding measurement Init thread token is not exact");
  const outputCredential = CML.TransactionOutput.from_cbor_hex(outputHex)
    .address()
    .payment_cred()
    ?.as_script();
  if (
    outputCredential?.to_hex() !== firstStep.scriptHash ||
    thread.datum == null
  )
    throw new Error(
      "funding measurement Init output is not at the deployed first step",
    );
  const datum = Data.from(thread.datum, FraudProofComputationThreadStepDatum);
  if (
    datum.fraud_prover !== identity.fundingPaymentKeyHash ||
    datum.data !== null
  )
    throw new Error(
      "funding measurement Init custody belongs to another prover",
    );
  const fundingControlledInputs = measured.inputs.map((utxo) => {
    const resolvedOutputCborHex = measurementOutputCbor(utxo);
    if (
      !hasFundingPaymentCredential(
        resolvedOutputCborHex,
        identity.fundingPaymentKeyHash,
      )
    )
      throw new Error(
        "funding measurement Init has an unmodeled non-wallet input",
      );
    return {
      outRef: measurementOutRef(utxo),
      resolvedOutputCborHex,
      role: "wallet_funding" as const,
      semanticRole: "wallet_funding" as const,
      contractAddress: utxo.address,
      identityAssets: assets(utxo.assets),
      fundingLovelace: utxo.assets.lovelace!.toString(),
      fundingAssets: assets(utxo.assets),
      sourceActionKind: null,
      sourceOutputIndex: null,
    };
  });
  const fundingControlledOutputs = measured.canonical.outputCborHex.map(
    (hex, index) => {
      const output = outputValue(hex);
      if (index === outputIndex)
        return {
          outputIndex: index,
          role: "locked_reusable" as const,
          custodyRole: "carrier" as const,
          semanticRole: "proof_thread" as const,
          contractAddress: output.address,
          fundingLovelace: output.assets.lovelace!.toString(),
          // The freshly minted identity NFT is not purchased prover capital.
          fundingAssets: [],
        };
      if (!hasFundingPaymentCredential(hex, identity.fundingPaymentKeyHash))
        throw new Error(
          "funding measurement Init has an unmodeled output owner",
        );
      return {
        outputIndex: index,
        role: "wallet_change" as const,
        custodyRole: "none" as const,
        semanticRole: "wallet_change" as const,
        contractAddress: output.address,
        fundingLovelace: output.assets.lovelace!.toString(),
        fundingAssets: assets(output.assets),
      };
    },
  );
  const referenceInputs = measured.references
    .map((utxo) => {
      const name = Object.entries(manifest.contracts).find(
        ([, entry]) =>
          entry.refScriptUTxO?.txHash === utxo.txHash &&
          entry.refScriptUTxO.outputIndex === utxo.outputIndex,
      )?.[0];
      return {
        role: name ?? "readOnly",
        outRef: measurementOutRef(utxo),
        ...measurementReferenceScript(utxo),
      };
    })
    .sort(
      (left, right) =>
        left.role.localeCompare(right.role, "en") ||
        left.outRef.localeCompare(right.outRef, "en"),
    );
  const action = {
    actionKind: "init",
    signedTransactionCborHex: measured.canonical.signedTransactionCborHex,
    fundingControlledInputs,
    fundingControlledOutputs,
    referenceInputs,
    referenceScriptBytes: measured.evidence.referenceScriptBytes,
    requiredBondLovelace: "0",
    requiredRewardCustodyLovelace: "0",
    requiredNativeAssets: [],
    collateralRequired: true,
    conflictRetryCount: 0,
  };
  const bodyArtifact = {
    schemaVersion: WORKFLOW_FUNDING_MEASUREMENT_TOOL,
    status: "partial" as const,
    transactionStatus: "locally_evaluated_prepared" as const,
    identity,
    category: input.category,
    shape: "shared-computation-thread-init" as const,
    action: fundingAction(action, 0, false, identity.fundingPaymentKeyHash),
    evidence: measured.evidence,
    missingCoverage: [
      "family-steps",
      "field-carriers",
      "proof-completion",
      "removal-and-correction",
      "maximum-reachable-shapes",
      "conflict-retries",
    ],
  };
  const recorded = recordWorkflowFundingActionMeasurement(bodyArtifact);
  const measurementArtifactSha256 = recorded.measurementArtifactSha256;
  // Reuse the release consumer's exact signature, capital and custody checks.
  createWorkflowFundingRequirements({
    scope: { kind: "fraud_proof_category", category: input.category },
    deploymentFingerprint: identity.manifestId,
    blueprintSha256: identity.blueprintSha256,
    protocolParametersDigest: identity.protocolParametersDigest,
    economicsPolicyDigest: identity.economicsPolicyDigest,
    fundingPaymentKeyHash: identity.fundingPaymentKeyHash,
    measurementToolVersion: WORKFLOW_FUNDING_MEASUREMENT_TOOL,
    measurementArtifactSha256,
    actions: [action],
  });
  return recorded;
};
