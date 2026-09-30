import {
  canonicalOutputCbor,
  fundingContribution,
  hasFundingPaymentCredential,
  outputValue,
} from "./funding-requirements.canonical-transaction.js";
import {
  ACTION_KIND,
  exact,
  fundingAsset,
  OUT_REF,
  REFERENCE_ROLE,
  type WorkflowFundingControlledInput,
  type WorkflowFundingControlledOutput,
  type WorkflowFundingReferenceInput,
} from "./funding-requirements.workflow-funding-controlled-output.js";

export const fundingControlledInput = (
  value: unknown,
  field: string,
  transactionInputOutRefs: readonly string[],
  fundingPaymentKeyHash: string,
): WorkflowFundingControlledInput => {
  const record = exact(
    value,
    [
      "outRef",
      "resolvedOutputCborHex",
      "role",
      "semanticRole",
      "contractAddress",
      "identityAssets",
      "fundingLovelace",
      "fundingAssets",
      "sourceActionKind",
      "sourceOutputIndex",
    ],
    field,
  );
  if (
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    !transactionInputOutRefs.includes(record.outRef) ||
    (record.role !== "wallet_funding" &&
      record.role !== "released_locked" &&
      record.role !== "protocol")
  ) {
    throw new Error(`${field} is not an exact transaction input`);
  }
  const resolvedOutputCborHex = canonicalOutputCbor(
    record.resolvedOutputCborHex,
    `${field}.resolvedOutputCborHex`,
  );
  const contribution = fundingContribution({
    value: record,
    field,
    outputCborHex: resolvedOutputCborHex,
  });
  const exactValue = outputValue(resolvedOutputCborHex).assets;
  const exactOutput = outputValue(resolvedOutputCborHex);
  if (
    typeof record.contractAddress !== "string" ||
    record.contractAddress !== exactOutput.address ||
    !Array.isArray(record.identityAssets)
  ) {
    throw new Error(`${field} semantic input authority is invalid`);
  }
  const identityAssets = record.identityAssets.map((asset, assetIndex) =>
    fundingAsset(asset, `${field}.identityAssets[${assetIndex.toString()}]`),
  );
  if (
    identityAssets.some(
      (asset, assetIndex) =>
        assetIndex > 0 &&
        identityAssets[assetIndex - 1]!.unit.localeCompare(asset.unit) >= 0,
    ) ||
    identityAssets.length !==
      Object.keys(exactValue).filter((unit) => unit !== "lovelace").length ||
    identityAssets.some(
      ({ unit, quantity }) => exactValue[unit] !== BigInt(quantity),
    )
  ) {
    throw new Error(`${field} semantic input assets are not exact`);
  }
  if (record.role === "wallet_funding") {
    if (
      record.semanticRole !== "wallet_funding" ||
      record.sourceActionKind !== null ||
      record.sourceOutputIndex !== null ||
      !hasFundingPaymentCredential(
        resolvedOutputCborHex,
        fundingPaymentKeyHash,
      ) ||
      BigInt(contribution.fundingLovelace) !== (exactValue.lovelace ?? 0n) ||
      contribution.fundingAssets.length !==
        Object.keys(exactValue).filter((unit) => unit !== "lovelace").length ||
      contribution.fundingAssets.some(
        ({ unit, quantity }) => exactValue[unit] !== BigInt(quantity),
      )
    ) {
      throw new Error(`${field} wallet-funding authority is invalid`);
    }
  } else if (
    record.role === "released_locked" &&
    ((record.semanticRole !== "proof_thread" &&
      record.semanticRole !== "field_carrier" &&
      record.semanticRole !== "prover_bond" &&
      record.semanticRole !== "prover_reward" &&
      record.semanticRole !== "challenger_bond" &&
      record.semanticRole !== "availability_carrier" &&
      record.semanticRole !== "correction_lock") ||
      typeof record.sourceActionKind !== "string" ||
      !ACTION_KIND.test(record.sourceActionKind) ||
      !Number.isSafeInteger(record.sourceOutputIndex) ||
      (record.sourceOutputIndex as number) < 0)
  ) {
    throw new Error(`${field} released-lock source is invalid`);
  } else if (
    record.role === "protocol" &&
    (record.semanticRole !== "protocol_state" ||
      record.sourceActionKind !== null ||
      record.sourceOutputIndex !== null ||
      hasFundingPaymentCredential(
        resolvedOutputCborHex,
        fundingPaymentKeyHash,
      ) ||
      contribution.fundingLovelace !== "0" ||
      contribution.fundingAssets.length !== 0)
  ) {
    throw new Error(`${field} protocol input claims funding authority`);
  }
  return Object.freeze({
    outRef: record.outRef,
    resolvedOutputCborHex,
    role: record.role,
    semanticRole:
      record.semanticRole as WorkflowFundingControlledInput["semanticRole"],
    contractAddress: record.contractAddress,
    identityAssets: Object.freeze(identityAssets),
    ...contribution,
    sourceActionKind:
      record.role !== "released_locked"
        ? null
        : (record.sourceActionKind as string),
    sourceOutputIndex:
      record.role !== "released_locked"
        ? null
        : (record.sourceOutputIndex as number),
  });
};

export const fundingControlledOutput = (
  value: unknown,
  field: string,
  outputCborHex: readonly string[],
  fundingPaymentKeyHash: string,
): WorkflowFundingControlledOutput => {
  const record = exact(
    value,
    [
      "outputIndex",
      "role",
      "custodyRole",
      "semanticRole",
      "contractAddress",
      "fundingLovelace",
      "fundingAssets",
    ],
    field,
  );
  if (
    !Number.isSafeInteger(record.outputIndex) ||
    (record.outputIndex as number) < 0 ||
    (record.outputIndex as number) >= outputCborHex.length ||
    (record.role !== "wallet_change" &&
      record.role !== "locked_reusable" &&
      record.role !== "locked_permanent" &&
      record.role !== "protocol" &&
      record.role !== "protocol_reward") ||
    (record.custodyRole !== "none" &&
      record.custodyRole !== "bond" &&
      record.custodyRole !== "reward" &&
      record.custodyRole !== "native_asset" &&
      record.custodyRole !== "carrier")
  ) {
    throw new Error(`${field} is invalid`);
  }
  const index = record.outputIndex as number;
  const exactOutput = outputValue(outputCborHex[index]!);
  if (
    typeof record.contractAddress !== "string" ||
    record.contractAddress !== exactOutput.address ||
    (record.semanticRole !== "wallet_change" &&
      record.semanticRole !== "protocol_state" &&
      record.semanticRole !== "proof_thread" &&
      record.semanticRole !== "field_carrier" &&
      record.semanticRole !== "prover_bond" &&
      record.semanticRole !== "prover_reward" &&
      record.semanticRole !== "challenger_bond" &&
      record.semanticRole !== "availability_carrier" &&
      record.semanticRole !== "correction_lock")
  ) {
    throw new Error(`${field} semantic authority is invalid`);
  }
  const contribution = fundingContribution({
    value: record,
    field,
    outputCborHex: outputCborHex[index]!,
  });
  const isFundingChange = hasFundingPaymentCredential(
    outputCborHex[index]!,
    fundingPaymentKeyHash,
  );
  if (
    (record.role === "wallet_change" || record.role === "protocol_reward") !==
      isFundingChange ||
    (record.role === "wallet_change" &&
      (record.custodyRole !== "none" ||
        record.semanticRole !== "wallet_change" ||
        BigInt(contribution.fundingLovelace) !==
          (exactOutput.assets.lovelace ?? 0n) ||
        contribution.fundingAssets.length !==
          Object.keys(exactOutput.assets).filter((unit) => unit !== "lovelace")
            .length ||
        contribution.fundingAssets.some(
          ({ unit, quantity }) => exactOutput.assets[unit] !== BigInt(quantity),
        ))) ||
    (record.role === "protocol" &&
      (record.custodyRole !== "none" ||
        record.semanticRole !== "protocol_state" ||
        contribution.fundingLovelace !== "0" ||
        contribution.fundingAssets.length !== 0)) ||
    (record.role === "protocol_reward" &&
      (record.custodyRole !== "none" ||
        record.semanticRole !== "prover_reward" ||
        contribution.fundingLovelace !== "0" ||
        contribution.fundingAssets.length !== 0 ||
        Object.keys(exactOutput.assets).some((unit) => unit !== "lovelace"))) ||
    ((record.role === "locked_reusable" ||
      record.role === "locked_permanent") &&
      (record.custodyRole === "none" ||
        record.semanticRole === "wallet_change" ||
        record.semanticRole === "protocol_state" ||
        (contribution.fundingLovelace === "0" &&
          contribution.fundingAssets.length === 0)))
  ) {
    throw new Error(`${field} role differs from its exact output authority`);
  }
  return Object.freeze({
    outputIndex: index,
    role: record.role,
    custodyRole: record.custodyRole,
    semanticRole: record.semanticRole,
    contractAddress: record.contractAddress,
    ...contribution,
  });
};

export const fundingReferenceInput = (
  value: unknown,
  field: string,
): WorkflowFundingReferenceInput => {
  const record = exact(
    value,
    ["role", "outRef", "scriptHash", "scriptBytes"],
    field,
  );
  if (
    typeof record.role !== "string" ||
    !REFERENCE_ROLE.test(record.role) ||
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    !(
      (record.scriptHash === null && record.scriptBytes === null) ||
      (typeof record.scriptHash === "string" &&
        /^[0-9a-f]{56}$/u.test(record.scriptHash) &&
        Number.isSafeInteger(record.scriptBytes) &&
        (record.scriptBytes as number) >= 1)
    )
  ) {
    throw new Error(`${field} is invalid`);
  }
  return Object.freeze({
    role: record.role,
    outRef: record.outRef,
    scriptHash: record.scriptHash as string | null,
    scriptBytes: record.scriptBytes as number | null,
  });
};
