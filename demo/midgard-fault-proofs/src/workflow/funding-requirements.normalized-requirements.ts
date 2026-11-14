import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import { outputValue } from "./funding-requirements.canonical-transaction.js";
import {
  fundingAction,
  type NormalizedRequirements,
} from "./funding-requirements.funding-action.js";
import {
  digest,
  digestField,
  exact,
  isPlainObject,
  isProtocolFundedWorkflowAction,
  MEASUREMENT_VERSION,
  PAYMENT_KEY_HASH,
  WORKFLOW_FUNDING_REQUIREMENTS,
  type WorkflowFundingRequirementsInput,
  type WorkflowFundingScope,
} from "./funding-requirements.workflow-funding-controlled-output.js";

export const normalizedRequirements = (
  value: unknown,
  requireDerivedFields: boolean,
): NormalizedRequirements => {
  const record = exact(
    value,
    [
      "scope",
      "deploymentFingerprint",
      "blueprintSha256",
      "protocolParametersDigest",
      "economicsPolicyDigest",
      "fundingPaymentKeyHash",
      "measurementToolVersion",
      "measurementArtifactSha256",
      "actions",
      ...(requireDerivedFields ? ["schemaVersion", "profileDigest"] : []),
    ],
    "funding requirements",
  );
  const scopeRecord = exact(
    record.scope,
    isPlainObject(record.scope) && record.scope.kind === "fraud_proof_category"
      ? ["kind", "category"]
      : ["kind", "lifecycle"],
    "funding requirements scope",
  );
  const scope: WorkflowFundingScope = (() => {
    if (scopeRecord.kind === "fraud_proof_category") {
      if (
        typeof scopeRecord.category !== "string" ||
        !FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.includes(
          scopeRecord.category as FraudProofCatalogueCategoryName,
        )
      ) {
        throw new Error(
          "funding requirements category is not in the canonical catalogue",
        );
      }
      return Object.freeze({
        kind: "fraud_proof_category" as const,
        category: scopeRecord.category as FraudProofCatalogueCategoryName,
      });
    }
    if (
      scopeRecord.kind !== "da_availability_lifecycle" ||
      scopeRecord.lifecycle !== "challenge_response_timeout_correction"
    ) {
      throw new Error("funding requirements scope is unsupported");
    }
    return Object.freeze({
      kind: "da_availability_lifecycle" as const,
      lifecycle: "challenge_response_timeout_correction" as const,
    });
  })();
  if (
    typeof record.fundingPaymentKeyHash !== "string" ||
    !PAYMENT_KEY_HASH.test(record.fundingPaymentKeyHash)
  ) {
    throw new Error(
      "funding requirements payment credential must be a 28-byte key hash",
    );
  }
  if (
    typeof record.measurementToolVersion !== "string" ||
    !MEASUREMENT_VERSION.test(record.measurementToolVersion)
  ) {
    throw new Error(
      "funding requirements measurement tool version is not canonical",
    );
  }
  if (!Array.isArray(record.actions) || record.actions.length === 0) {
    throw new Error("funding requirements actions cannot be empty");
  }
  const actions = record.actions.map((action, index) =>
    fundingAction(
      action,
      index,
      requireDerivedFields,
      record.fundingPaymentKeyHash as string,
    ),
  );
  if (
    new Set(actions.map((action) => action.actionKind)).size !== actions.length
  ) {
    throw new Error("funding requirements action kinds must be unique");
  }
  const actionIndexByKind = new Map(
    actions.map((action, actionIndex) => [action.actionKind, actionIndex]),
  );
  const releasedSources = new Set<string>();
  for (let actionIndex = 0; actionIndex < actions.length; actionIndex += 1) {
    const action = actions[actionIndex]!;
    const inputFundingAssets = new Map<string, bigint>();
    const outputFundingAssets = new Map<string, bigint>();
    const inputFundingLovelace = action.fundingControlledInputs.reduce(
      (total, controlled) => {
        for (const { unit, quantity } of controlled.fundingAssets) {
          inputFundingAssets.set(
            unit,
            (inputFundingAssets.get(unit) ?? 0n) + BigInt(quantity),
          );
        }
        return total + BigInt(controlled.fundingLovelace);
      },
      0n,
    );
    const outputFundingLovelace = action.fundingControlledOutputs.reduce(
      (total, controlled) => {
        for (const { unit, quantity } of controlled.fundingAssets) {
          outputFundingAssets.set(
            unit,
            (outputFundingAssets.get(unit) ?? 0n) + BigInt(quantity),
          );
        }
        return total + BigInt(controlled.fundingLovelace);
      },
      0n,
    );
    const exactFee = CML.Transaction.from_cbor_hex(
      action.signedTransactionCborHex,
    )
      .body()
      .fee();
    const protocolFunded = isProtocolFundedWorkflowAction(action);
    if (protocolFunded) {
      const protocolInputs = action.fundingControlledInputs.reduce(
        (total, input) =>
          total +
          (outputValue(input.resolvedOutputCborHex).assets.lovelace ?? 0n),
        0n,
      );
      const protocolOutputs = action.outputCborHex.reduce(
        (total, output) => total + (outputValue(output).assets.lovelace ?? 0n),
        0n,
      );
      if (protocolInputs !== protocolOutputs + exactFee) {
        throw new Error(
          `${action.actionKind} protocol-funded Ada is not conserved`,
        );
      }
    }
    if (
      inputFundingLovelace !==
        outputFundingLovelace + (protocolFunded ? 0n : exactFee) ||
      inputFundingAssets.size !== outputFundingAssets.size ||
      [...inputFundingAssets].some(
        ([unit, quantity]) => outputFundingAssets.get(unit) !== quantity,
      )
    ) {
      throw new Error(
        `${action.actionKind} funding-controlled value is not conserved`,
      );
    }
    let exactBond = 0n;
    let exactReward = 0n;
    const exactNativeAssets = new Map<string, bigint>();
    for (const controlled of action.fundingControlledOutputs) {
      if (controlled.role === "wallet_change") continue;
      const lovelace = BigInt(controlled.fundingLovelace);
      if (controlled.custodyRole === "bond") exactBond += lovelace;
      if (controlled.custodyRole === "reward") exactReward += lovelace;
      for (const { unit, quantity: rawQuantity } of controlled.fundingAssets) {
        const quantity = BigInt(rawQuantity);
        exactNativeAssets.set(
          unit,
          (exactNativeAssets.get(unit) ?? 0n) + quantity,
        );
      }
    }
    if (
      BigInt(action.requiredBondLovelace) !== exactBond ||
      BigInt(action.requiredRewardCustodyLovelace) !== exactReward
    ) {
      throw new Error(
        `${action.actionKind} declared custody differs from locked outputs`,
      );
    }
    const declaredNativeAssets = new Map(
      action.requiredNativeAssets.map(({ unit, quantity }) => [
        unit,
        BigInt(quantity),
      ]),
    );
    if (
      declaredNativeAssets.size !== exactNativeAssets.size ||
      [...exactNativeAssets].some(
        ([unit, quantity]) => declaredNativeAssets.get(unit) !== quantity,
      )
    ) {
      throw new Error(
        `${action.actionKind} declared native custody differs from locked outputs`,
      );
    }
    for (const controlled of action.fundingControlledInputs) {
      if (controlled.role !== "released_locked") continue;
      const sourceActionIndex = actionIndexByKind.get(
        controlled.sourceActionKind!,
      );
      if (sourceActionIndex === undefined || sourceActionIndex >= actionIndex) {
        throw new Error(
          `${action.actionKind} released-lock source is not an earlier action`,
        );
      }
      const source = actions[sourceActionIndex]!;
      const sourceOutput = source.fundingControlledOutputs.find(
        ({ outputIndex }) => outputIndex === controlled.sourceOutputIndex,
      );
      const sourceIdentity = `${source.actionKind}#${controlled.sourceOutputIndex!.toString()}`;
      if (
        sourceOutput?.role !== "locked_reusable" ||
        controlled.outRef !==
          `${source.transactionHash}#${controlled.sourceOutputIndex!.toString()}` ||
        controlled.resolvedOutputCborHex !==
          source.outputCborHex[controlled.sourceOutputIndex!] ||
        controlled.semanticRole !== sourceOutput.semanticRole ||
        controlled.contractAddress !== sourceOutput.contractAddress ||
        JSON.stringify(controlled.identityAssets) !==
          JSON.stringify(
            Object.entries(
              outputValue(source.outputCborHex[controlled.sourceOutputIndex!]!)
                .assets,
            )
              .filter(([unit]) => unit !== "lovelace")
              .sort(([left], [right]) => left.localeCompare(right))
              .map(([unit, quantity]) => ({
                unit,
                quantity: quantity.toString(),
              })),
          ) ||
        controlled.fundingLovelace !== sourceOutput.fundingLovelace ||
        JSON.stringify(controlled.fundingAssets) !==
          JSON.stringify(sourceOutput.fundingAssets) ||
        releasedSources.has(sourceIdentity)
      ) {
        throw new Error(
          `${action.actionKind} released-lock source is not exact and reusable`,
        );
      }
      releasedSources.add(sourceIdentity);
    }
  }
  if (
    requireDerivedFields &&
    record.schemaVersion !== WORKFLOW_FUNDING_REQUIREMENTS
  ) {
    throw new Error("funding requirements schema version is unsupported");
  }
  return Object.freeze({
    scope,
    deploymentFingerprint: digestField(
      record.deploymentFingerprint,
      "funding requirements deployment fingerprint",
    ),
    blueprintSha256: digestField(
      record.blueprintSha256,
      "funding requirements blueprint digest",
    ),
    protocolParametersDigest: digestField(
      record.protocolParametersDigest,
      "funding requirements protocol-parameters digest",
    ),
    economicsPolicyDigest: digestField(
      record.economicsPolicyDigest,
      "funding requirements economics-policy digest",
    ),
    fundingPaymentKeyHash: record.fundingPaymentKeyHash,
    measurementToolVersion: record.measurementToolVersion,
    measurementArtifactSha256: digestField(
      record.measurementArtifactSha256,
      "funding requirements measurement artifact digest",
    ),
    actions: Object.freeze(actions),
    ...(requireDerivedFields
      ? {
          profileDigest: digestField(
            record.profileDigest,
            "funding requirements profile digest",
          ),
        }
      : {}),
  });
};

export const digestInput = (value: NormalizedRequirements): unknown => ({
  scope: value.scope,
  deploymentFingerprint: value.deploymentFingerprint,
  blueprintSha256: value.blueprintSha256,
  protocolParametersDigest: value.protocolParametersDigest,
  economicsPolicyDigest: value.economicsPolicyDigest,
  fundingPaymentKeyHash: value.fundingPaymentKeyHash,
  measurementToolVersion: value.measurementToolVersion,
  measurementArtifactSha256: value.measurementArtifactSha256,
  actions: value.actions,
});

export const computeWorkflowFundingRequirementsDigest = (
  value: WorkflowFundingRequirementsInput,
): string => digest(digestInput(normalizedRequirements(value, false)));
