import {
  computeDeploymentManifestJsonDigest,
  type DeploymentManifestCardanoProtocolParameters,
  normalizeDeploymentManifestJsonValue,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { l1SlotConfigsEqual } from "@al-ft/midgard-core/l1-slot-config";
import {
  CML,
  createCostModels,
  Emulator,
  fromCMLRedeemerTag,
  type LucidEvolution,
  makeAikenEvaluator,
  type ProtocolParameters,
  toScriptRef,
  type UTxO,
  utxoToCore,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  readWorkflowFundingMeasurementContext,
  type WorkflowFundingMeasurementContext,
} from "./funding-measurements.identity.js";
import {
  canonicalTransaction,
  hasFundingPaymentCredential,
} from "./funding-requirements.canonical-transaction.js";
import { workflowRuntimeFundingMinimumFee } from "./runtime-funding-policy.js";
import {
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
} from "./transaction-boundary.js";

export const measurementOutRef = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;
export const measurementOutputCbor = (utxo: UTxO): string =>
  utxoToCore(utxo).output().to_cbor_hex();

/** Ledger fee size: raw Plutus bytes, serialized native script bytes. */
export const measurementReferenceScript = (utxo: UTxO) => {
  if (utxo.scriptRef == null) return { scriptHash: null, scriptBytes: null };
  const script = toScriptRef(utxo.scriptRef);
  const bytes =
    script.as_native()?.to_cbor_bytes() ??
    script.as_plutus_v1()?.to_raw_bytes() ??
    script.as_plutus_v2()?.to_raw_bytes() ??
    script.as_plutus_v3()?.to_raw_bytes();
  if (bytes === undefined)
    throw new Error("funding measurement unsupported reference script");
  return {
    scriptHash: validatorToScriptHash(utxo.scriptRef),
    scriptBytes: bytes.length,
  };
};

const protocolSnapshot = (parameters: ProtocolParameters) =>
  normalizeDeploymentManifestJsonValue(
    {
      ...parameters,
      priceMem: parameters.priceMem.toString(),
      priceStep: parameters.priceStep.toString(),
    },
    "funding measurement local ledger parameters",
  );

const assertProtocolParameters = (
  actual: ProtocolParameters,
  expected: DeploymentManifestCardanoProtocolParameters,
): void => {
  const pairs: readonly (readonly [bigint | number, string])[] = [
    [actual.minFeeA, expected.minFeeA],
    [actual.minFeeB, expected.minFeeB],
    [actual.maxTxSize, expected.maxTxSize],
    [actual.maxValSize, expected.maxValueSize],
    [actual.maxTxExMem, expected.maxTxExUnits.memory],
    [actual.maxTxExSteps, expected.maxTxExUnits.steps],
    [actual.coinsPerUtxoByte, expected.coinsPerUtxoByte],
    [actual.collateralPercentage, expected.collateralPercentage],
    [actual.maxCollateralInputs, expected.maxCollateralInputs],
  ];
  const price = (value: { numerator: string; denominator: string }) =>
    Number(value.numerator) / Number(value.denominator);
  if (
    pairs.some(([value, decimal]) => value.toString() !== decimal) ||
    actual.priceMem !== price(expected.priceMemory) ||
    actual.priceStep !== price(expected.priceSteps) ||
    actual.minFeeRefScriptCostPerByte !==
      price(expected.referenceScriptFee.base)
  )
    throw new Error(
      "funding measurement local protocol parameters differ from deployment",
    );
};

const inputRefs = (inputs: CML.TransactionInputList | undefined): string[] => {
  const values: string[] = [];
  for (let index = 0; index < (inputs?.len() ?? 0); index += 1) {
    const input = inputs!.get(index);
    values.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return values.sort();
};

/** Fresh local phase-two replay of the exact signed bytes and resolved UTxOs. */
export const evaluateWorkflowFundingTransaction = async (input: {
  readonly context: WorkflowFundingMeasurementContext;
  readonly lucid: LucidEvolution;
  readonly transaction: LocallyEvaluatedTransaction;
}) => {
  const { manifest, identity } = readWorkflowFundingMeasurementContext(
    input.context,
  );
  const signedCbor = input.transaction.signed.toCBOR();
  const canonical = canonicalTransaction(
    signedCbor,
    "funding measurement signed transaction",
    identity.fundingPaymentKeyHash,
  );
  if (
    canonical.transactionHash !== input.transaction.txHash ||
    canonical.transactionHash !== input.transaction.signed.toHash()
  )
    throw new Error(
      "funding measurement changed the signed transaction identity",
    );
  requireReferenceOnlyScriptWitnesses({
    transaction: input.transaction,
    label: "funding measurement",
  });
  const transaction = CML.Transaction.from_cbor_hex(signedCbor);
  if (!transaction.is_valid())
    throw new Error(
      "funding measurement requires a successful script transaction",
    );
  const config = input.lucid.config();
  if (
    config.provider === undefined ||
    config.network === undefined ||
    config.slotConfig === undefined ||
    config.protocolParameters === undefined
  )
    throw new Error(
      "funding measurement needs a configured local provider and slot mapping",
    );
  const emulatorEvidence =
    identity.environment === "emulator" && config.provider instanceof Emulator;
  if (
    !l1SlotConfigsEqual(config.slotConfig, manifest.l1SlotConfig) &&
    !emulatorEvidence
  )
    throw new Error(
      "funding measurement local slot mapping differs from deployment",
    );
  // Emulator clients use Custom with the explicitly configured deployment mapping.
  if (
    config.network !== manifest.network &&
    !(
      identity.environment === "emulator" &&
      config.provider instanceof Emulator &&
      config.network === "Custom" &&
      manifest.network === "Preprod"
    )
  )
    throw new Error(
      "funding measurement local network differs from deployment",
    );
  const parameters = await config.provider.getProtocolParameters();
  assertProtocolParameters(
    parameters,
    manifest.cardanoProtocolParameters.snapshot,
  );
  assertProtocolParameters(
    config.protocolParameters,
    manifest.cardanoProtocolParameters.snapshot,
  );
  if (
    computeDeploymentManifestJsonDigest(protocolSnapshot(parameters)) !==
    computeDeploymentManifestJsonDigest(
      protocolSnapshot(config.protocolParameters),
    )
  )
    throw new Error("funding measurement evaluator parameters are stale");
  const body = transaction.body();
  const collateralOutRefs = inputRefs(body.collateral_inputs());
  const allOutRefs = [
    ...new Set([
      ...canonical.inputOutRefs,
      ...canonical.referenceInputOutRefs,
      ...collateralOutRefs,
    ]),
  ].sort();
  const resolved = await config.provider.getUtxosByOutRef(
    allOutRefs.map((value) => {
      const [txHash, index] = value.split("#");
      return { txHash: txHash!, outputIndex: Number(index) };
    }),
  );
  const byRef = new Map(
    resolved.map((utxo) => [measurementOutRef(utxo), utxo]),
  );
  if (
    byRef.size !== resolved.length ||
    byRef.size !== allOutRefs.length ||
    allOutRefs.some((ref) => !byRef.has(ref))
  )
    throw new Error(
      "funding measurement could not resolve every exact input/reference/collateral",
    );
  const references = canonical.referenceInputOutRefs.map(
    (outRef) => byRef.get(outRef)!,
  );
  for (const reference of references) {
    if (reference.scriptRef == null) continue;
    const entry = Object.values(manifest.contracts).find(
      (contract) =>
        contract.refScriptUTxO?.txHash === reference.txHash &&
        contract.refScriptUTxO.outputIndex === reference.outputIndex,
    );
    const binding = Object.values(manifest.referenceScripts).find(
      (record) => record.outRef === measurementOutRef(reference),
    );
    if (
      binding === undefined ||
      reference.assets[binding.roleUnit] !== 1n ||
      entry === undefined ||
      entry.scriptHash !== validatorToScriptHash(reference.scriptRef) ||
      entry.contract.type !== reference.scriptRef.type ||
      toScriptRef({
        type: entry.contract.type,
        script: entry.contract.cborHex,
      }).to_canonical_cbor_hex() !==
        toScriptRef(reference.scriptRef).to_canonical_cbor_hex()
    )
      throw new Error(
        "funding measurement reference script is not the deployed program/outref",
      );
  }
  const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
  if (redeemers === undefined || redeemers.len() === 0)
    throw new Error(
      "funding measurement refuses a wallet transfer without a proof invocation",
    );
  const evaluation = await makeAikenEvaluator().evaluate({
    tx: signedCbor,
    additionalUTxOs: allOutRefs.map((ref) => byRef.get(ref)!),
    context: {
      network: config.network,
      slotConfig: config.slotConfig,
      protocolParameters: parameters,
      costModels: createCostModels(parameters.costModels),
    },
  });
  const evaluated = new Map(
    evaluation.map((entry) => [
      `${entry.redeemer_tag}:${entry.redeemer_index.toString()}`,
      entry,
    ]),
  );
  if (
    evaluated.size !== evaluation.length ||
    evaluated.size !== redeemers.len()
  )
    throw new Error(
      "funding measurement evaluated redeemer set differs from signed budgets",
    );
  for (let index = 0; index < redeemers.len(); index += 1) {
    const redeemer = redeemers.get(index);
    const result = evaluated.get(
      `${fromCMLRedeemerTag(redeemer.tag())}:${redeemer.index().toString()}`,
    );
    if (
      result === undefined ||
      !Number.isSafeInteger(result.ex_units.mem) ||
      !Number.isSafeInteger(result.ex_units.steps) ||
      result.ex_units.mem < 0 ||
      result.ex_units.steps < 0 ||
      BigInt(result.ex_units.mem) > redeemer.ex_units().mem() ||
      BigInt(result.ex_units.steps) > redeemer.ex_units().steps()
    )
      throw new Error(
        "funding measurement local evaluation exceeds or differs from signed redeemers",
      );
  }
  const referenceScriptBytes = references.reduce(
    (sum, ref) => sum + (measurementReferenceScript(ref).scriptBytes ?? 0),
    0,
  );
  if (
    canonical.signedTransactionBytes > parameters.maxTxSize ||
    BigInt(canonical.executionUnits.memory) > parameters.maxTxExMem ||
    BigInt(canonical.executionUnits.steps) > parameters.maxTxExSteps ||
    BigInt(referenceScriptBytes) >
      BigInt(
        manifest.cardanoProtocolParameters.snapshot.referenceScriptFee
          .maximumSizeBytes,
      )
  )
    throw new Error(
      "funding measurement signed transaction exceeds protocol limits",
    );
  for (const hex of canonical.outputCborHex) {
    const output = CML.TransactionOutput.from_cbor_hex(hex);
    if (
      output.amount().coin() <
      CML.min_ada_required(output, parameters.coinsPerUtxoByte)
    )
      throw new Error("funding measurement output is below ledger minimum Ada");
  }
  const minimumFee = workflowRuntimeFundingMinimumFee({
    parameters: manifest.cardanoProtocolParameters.snapshot,
    transactionBytes: BigInt(canonical.signedTransactionBytes),
    memory: BigInt(canonical.executionUnits.memory),
    steps: BigInt(canonical.executionUnits.steps),
    referenceScriptBytes: BigInt(referenceScriptBytes),
  });
  if (body.fee() < minimumFee)
    throw new Error(
      "funding measurement signed fee is below the ledger minimum",
    );
  const collateral = collateralOutRefs.map((ref) => byRef.get(ref)!);
  if (
    collateral.length === 0 ||
    collateral.length > parameters.maxCollateralInputs ||
    collateral.some(
      (utxo) =>
        !hasFundingPaymentCredential(
          measurementOutputCbor(utxo),
          identity.fundingPaymentKeyHash,
        ) || Object.keys(utxo.assets).some((unit) => unit !== "lovelace"),
    )
  )
    throw new Error("funding measurement collateral is missing or unsupported");
  const collateralLovelace = collateral.reduce(
    (total, utxo) => total + utxo.assets.lovelace!,
    0n,
  );
  const collateralReturn = body.collateral_return();
  if (
    collateralReturn !== undefined &&
    (!hasFundingPaymentCredential(
      collateralReturn.to_cbor_hex(),
      identity.fundingPaymentKeyHash,
    ) ||
      collateralReturn.amount().has_multiassets())
  )
    throw new Error(
      "funding measurement collateral return belongs to another owner",
    );
  const returned = collateralReturn?.amount().coin() ?? 0n;
  const totalCollateral = body.total_collateral();
  if (
    returned > collateralLovelace ||
    (totalCollateral !== undefined &&
      totalCollateral !== collateralLovelace - returned) ||
    collateralLovelace - returned <
      (body.fee() * BigInt(parameters.collateralPercentage) + 99n) / 100n
  )
    throw new Error(
      "funding measurement collateral return/total is not exact or sufficient",
    );
  return {
    canonical,
    transaction,
    inputs: canonical.inputOutRefs.map((ref) => byRef.get(ref)!),
    references,
    collateral,
    evidence: {
      evaluator: "lucid-evolution-0.8.0:aiken-local-phase-two" as const,
      evaluatorNetwork: config.network,
      evaluatorSlotConfig: { ...config.slotConfig },
      protocolParameters: protocolSnapshot(parameters),
      protocolParametersSha256: computeDeploymentManifestJsonDigest(
        protocolSnapshot(parameters),
      ),
      costModelsSha256: computeDeploymentManifestJsonDigest(
        parameters.costModels,
      ),
      protocolMajorVersion: parameters.protocolMajorVersion ?? null,
      evaluatedRedeemers: evaluation,
      feeLovelace: body.fee().toString(),
      minimumFeeLovelace: minimumFee.toString(),
      referenceScriptBytes,
      collateralLovelace: collateralLovelace.toString(),
      totalCollateralLovelace: totalCollateral?.toString() ?? null,
      collateralReturnCborHex: collateralReturn?.to_cbor_hex() ?? null,
      resolvedInputs: allOutRefs.map((ref) => ({
        outRef: ref,
        outputCborHex: measurementOutputCbor(byRef.get(ref)!),
      })),
      outputMinimumLovelace: canonical.outputCborHex.map((hex) =>
        CML.min_ada_required(
          CML.TransactionOutput.from_cbor_hex(hex),
          BigInt(manifest.cardanoProtocolParameters.snapshot.coinsPerUtxoByte),
        ).toString(),
      ),
    },
  };
};
