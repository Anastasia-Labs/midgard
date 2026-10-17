import {
  ACTION_DERIVED_FIELDS,
  ACTION_MEASUREMENT_FIELDS,
  canonicalTransaction,
  hasFundingPaymentCredential,
} from "./funding-requirements.canonical-transaction.js";
import {
  fundingControlledInput,
  fundingControlledOutput,
  fundingReferenceInput,
} from "./funding-requirements.funding-controlled-input.js";
import {
  ACTION_KIND,
  exact,
  fundingAsset,
  isProtocolFundedWorkflowAction,
  natural,
  safeNaturalNumber,
  type WorkflowFundingAction,
  type WorkflowFundingRequirements,
} from "./funding-requirements.workflow-funding-controlled-output.js";

export const fundingAction = (
  value: unknown,
  index: number,
  requireDerivedFields: boolean,
  fundingPaymentKeyHash: string,
): WorkflowFundingAction => {
  const field = `funding requirements actions[${index.toString()}]`;
  const record = exact(
    value,
    [
      ...ACTION_MEASUREMENT_FIELDS,
      ...(requireDerivedFields ? ACTION_DERIVED_FIELDS : []),
    ],
    field,
  );
  if (
    typeof record.actionKind !== "string" ||
    !ACTION_KIND.test(record.actionKind)
  ) {
    throw new Error(`${field}.actionKind is not a stable action identifier`);
  }
  if (!Array.isArray(record.requiredNativeAssets)) {
    throw new Error(`${field}.requiredNativeAssets must be an array`);
  }
  if (
    !Array.isArray(record.fundingControlledInputs) ||
    !Array.isArray(record.fundingControlledOutputs) ||
    !Array.isArray(record.referenceInputs)
  ) {
    throw new Error(`${field} funding-controlled flow must be arrays`);
  }
  if (typeof record.collateralRequired !== "boolean") {
    throw new Error(`${field}.collateralRequired must be boolean`);
  }
  const requiredNativeAssets = record.requiredNativeAssets.map(
    (asset, assetIndex) =>
      fundingAsset(
        asset,
        `${field}.requiredNativeAssets[${assetIndex.toString()}]`,
      ),
  );
  for (
    let assetIndex = 1;
    assetIndex < requiredNativeAssets.length;
    assetIndex += 1
  ) {
    if (
      requiredNativeAssets[assetIndex - 1]!.unit >=
      requiredNativeAssets[assetIndex]!.unit
    ) {
      throw new Error(
        `${field}.requiredNativeAssets must be strictly unit-sorted`,
      );
    }
  }
  const transaction = canonicalTransaction(
    record.signedTransactionCborHex,
    `${field}.signedTransactionCborHex`,
    fundingPaymentKeyHash,
  );
  const fundingControlledInputs = record.fundingControlledInputs.map(
    (entry, controlledIndex) =>
      fundingControlledInput(
        entry,
        `${field}.fundingControlledInputs[${controlledIndex.toString()}]`,
        transaction.inputOutRefs,
        fundingPaymentKeyHash,
      ),
  );
  if (
    fundingControlledInputs.length !== transaction.inputOutRefs.length ||
    new Set(fundingControlledInputs.map(({ outRef }) => outRef)).size !==
      fundingControlledInputs.length ||
    fundingControlledInputs.some(
      ({ outRef }, inputIndex) =>
        outRef !== transaction.inputOutRefs[inputIndex],
    ) ||
    fundingControlledInputs.length === 0
  ) {
    throw new Error(
      `${field} must classify every exact transaction input once`,
    );
  }
  const fundingControlledOutputs = record.fundingControlledOutputs.map(
    (entry, controlledIndex) =>
      fundingControlledOutput(
        entry,
        `${field}.fundingControlledOutputs[${controlledIndex.toString()}]`,
        transaction.outputCborHex,
        fundingPaymentKeyHash,
      ),
  );
  const protocolFunded = isProtocolFundedWorkflowAction({
    fundingControlledInputs,
    fundingControlledOutputs,
  });
  if (
    protocolFunded
      ? fundingControlledOutputs.some(
          ({ role }) => role !== "protocol" && role !== "protocol_reward",
        )
      : fundingControlledOutputs.some(
          ({ role }) => role === "protocol_reward",
        ) || fundingControlledInputs.every(({ role }) => role === "protocol")
  ) {
    throw new Error(
      `${field} protocol rewards require exclusively protocol-funded inputs and outputs`,
    );
  }
  const referenceInputs = record.referenceInputs.map((entry, referenceIndex) =>
    fundingReferenceInput(
      entry,
      `${field}.referenceInputs[${referenceIndex.toString()}]`,
    ),
  );
  const referenceOutRefs = referenceInputs.map(({ outRef }) => outRef).sort();
  if (
    referenceOutRefs.length !== transaction.referenceInputOutRefs.length ||
    referenceOutRefs.some(
      (outRef, referenceIndex) =>
        outRef !== transaction.referenceInputOutRefs[referenceIndex],
    ) ||
    referenceInputs.some((reference, referenceIndex) => {
      if (referenceIndex === 0) return false;
      const previous = referenceInputs[referenceIndex - 1]!;
      return (
        previous.role.localeCompare(reference.role) > 0 ||
        (previous.role === reference.role &&
          previous.outRef.localeCompare(reference.outRef) >= 0)
      );
    })
  ) {
    throw new Error(`${field} reference-script identity set is not exact`);
  }
  if (
    fundingControlledOutputs.length !== transaction.outputCborHex.length ||
    (!protocolFunded &&
      !fundingControlledOutputs.some(({ role }) => role === "wallet_change")) ||
    new Set(fundingControlledOutputs.map(({ outputIndex }) => outputIndex))
      .size !== fundingControlledOutputs.length ||
    fundingControlledOutputs.some(
      ({ outputIndex }, controlledIndex) => outputIndex !== controlledIndex,
    )
  ) {
    throw new Error(
      `${field} must classify every exact output once with wallet change or protocol reward`,
    );
  }
  for (
    let outputIndex = 0;
    outputIndex < transaction.outputCborHex.length;
    outputIndex += 1
  ) {
    if (
      hasFundingPaymentCredential(
        transaction.outputCborHex[outputIndex]!,
        fundingPaymentKeyHash,
      ) !==
      fundingControlledOutputs.some(
        (output) =>
          output.outputIndex === outputIndex &&
          (output.role === "wallet_change" ||
            output.role === "protocol_reward"),
      )
    ) {
      throw new Error(
        `${field} wallet change output classification is incomplete`,
      );
    }
  }
  if (requireDerivedFields) {
    const executionUnits = exact(
      record.executionUnits,
      ["memory", "steps"],
      `${field}.executionUnits`,
    );
    const suppliedOutputs = record.outputCborHex;
    if (
      record.txBodyCborHex !== transaction.txBodyCborHex ||
      record.transactionHash !== transaction.transactionHash ||
      !Array.isArray(record.inputOutRefs) ||
      record.inputOutRefs.length !== transaction.inputOutRefs.length ||
      record.inputOutRefs.some(
        (outRef, inputIndex) => outRef !== transaction.inputOutRefs[inputIndex],
      ) ||
      !Array.isArray(record.referenceInputOutRefs) ||
      record.referenceInputOutRefs.length !==
        transaction.referenceInputOutRefs.length ||
      record.referenceInputOutRefs.some(
        (outRef, inputIndex) =>
          outRef !== transaction.referenceInputOutRefs[inputIndex],
      ) ||
      record.txBodyBytes !== transaction.txBodyBytes ||
      record.signedTransactionBytes !== transaction.signedTransactionBytes ||
      record.signedTransactionSha256 !== transaction.signedTransactionSha256 ||
      executionUnits.memory !== transaction.executionUnits.memory ||
      executionUnits.steps !== transaction.executionUnits.steps ||
      !Array.isArray(suppliedOutputs) ||
      suppliedOutputs.length !== transaction.outputCborHex.length ||
      suppliedOutputs.some(
        (output, outputIndex) =>
          output !== transaction.outputCborHex[outputIndex],
      )
    ) {
      throw new Error(
        `${field} derived Cardano transaction measurements differ`,
      );
    }
  }
  const referenceScriptBytes = safeNaturalNumber(
    record.referenceScriptBytes,
    `${field}.referenceScriptBytes`,
  );
  if (
    referenceInputs.reduce(
      (total, reference) => total + (reference.scriptBytes ?? 0),
      0,
    ) !== referenceScriptBytes
  ) {
    throw new Error(`${field} reference-script byte total is inconsistent`);
  }
  return Object.freeze({
    actionKind: record.actionKind,
    ...transaction,
    fundingControlledInputs: Object.freeze(fundingControlledInputs),
    fundingControlledOutputs: Object.freeze(fundingControlledOutputs),
    referenceInputs: Object.freeze(referenceInputs),
    referenceScriptBytes,
    requiredBondLovelace: natural(
      record.requiredBondLovelace,
      `${field}.requiredBondLovelace`,
    ),
    requiredRewardCustodyLovelace: natural(
      record.requiredRewardCustodyLovelace,
      `${field}.requiredRewardCustodyLovelace`,
    ),
    requiredNativeAssets: Object.freeze(requiredNativeAssets),
    collateralRequired: record.collateralRequired,
    conflictRetryCount: safeNaturalNumber(
      record.conflictRetryCount,
      `${field}.conflictRetryCount`,
    ),
  });
};

export type NormalizedRequirements = Omit<
  WorkflowFundingRequirements,
  "schemaVersion" | "profileDigest"
> &
  Partial<Pick<WorkflowFundingRequirements, "profileDigest">>;
