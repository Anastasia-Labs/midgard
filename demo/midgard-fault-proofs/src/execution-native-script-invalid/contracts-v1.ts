import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

export const EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES_V1 =
  Object.freeze(
    Array.from(
      { length: 6 },
      (_, index) =>
        `fraud_proofs/execution_native_script_invalid/step_${String(index + 1).padStart(2, "0")}.main.spend`,
    ),
  );
export const EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES_V1 =
  Object.freeze([
    "fraud_proofs/execution_native_script_invalid/accepted_reconstruction_init.main.spend",
    "fraud_proofs/execution_native_script_invalid/accepted_spend_prefix.main.spend",
    "fraud_proofs/execution_native_script_invalid/accepted_mint_prefix.main.spend",
    "fraud_proofs/execution_native_script_invalid/accepted_observer_prefix.main.spend",
    "fraud_proofs/execution_native_script_invalid/accepted_receive_prefix.main.spend",
    "fraud_proofs/execution_native_script_invalid/accepted_inline_source.main.spend",
    "fraud_proofs/execution_native_script_invalid/accepted_reference_source.main.spend",
  ] as const);
export const EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL =
  "execution-native-script-invalid" as const;
export type ExecutionNativeScriptInvalidStepContractV1 = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;
export type ExecutionNativeScriptInvalidContractsV1 = Readonly<{
  steps: readonly ExecutionNativeScriptInvalidStepContractV1[];
  acceptedPrelude?: readonly ExecutionNativeScriptInvalidStepContractV1[];
  computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptAddress: string;
  };
  hubOraclePolicyId: string;
  stateQueuePolicyId: string;
  fieldPreimageCertificatePolicyId: string;
}>;
type Blueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

export type ExecutionNativeScriptInvalidAppliedScriptsV1 =
  readonly ExecutionNativeScriptInvalidStepContractV1[] &
    Readonly<{
      acceptedPrelude: readonly ExecutionNativeScriptInvalidStepContractV1[];
    }>;

export const applyExecutionNativeScriptInvalidScriptsV1 = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  hubOracleScriptHash,
  fieldPreimageCertificatePolicyId,
}: {
  blueprint: Blueprint;
  network: Network;
  computationThreadPolicyId: string;
  fraudProofPolicyId: string;
  fraudProofTokenAddressData: Data;
  hubOracleScriptHash: string;
  fieldPreimageCertificatePolicyId: string;
}): ExecutionNativeScriptInvalidAppliedScriptsV1 => {
  const apply = (index: number, parameters: readonly Data[]) => {
    const blueprintTitle =
      EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES_V1[index]!;
    const validator = blueprint.validators.find(
      ({ title }) => title === blueprintTitle,
    );
    if (validator === undefined)
      throw new Error(
        `executionNativeScriptInvalid: blueprint omitted ${blueprintTitle}`,
      );
    if ((validator.parameters?.length ?? 0) !== parameters.length)
      throw new Error(
        `executionNativeScriptInvalid: ${blueprintTitle} parameter arity changed`,
      );
    const spendingScript: Script = {
      type: "PlutusV3",
      script: applyParamsToScript(validator.compiledCode, [...parameters]),
    };
    return Object.freeze({
      blueprintTitle,
      spendingScript,
      spendingScriptHash: validatorToScriptHash(spendingScript),
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      referenceOutRef: `${"0".repeat(64)}#0`,
    });
  };
  const applyTitle = (blueprintTitle: string, parameters: readonly Data[]) => {
    const validator = blueprint.validators.find(
      ({ title }) => title === blueprintTitle,
    );
    if (validator === undefined)
      throw new Error(
        `executionNativeScriptInvalid: blueprint omitted ${blueprintTitle}`,
      );
    if ((validator.parameters?.length ?? 0) !== parameters.length)
      throw new Error(
        `executionNativeScriptInvalid: ${blueprintTitle} parameter arity changed`,
      );
    const spendingScript: Script = {
      type: "PlutusV3",
      script: applyParamsToScript(validator.compiledCode, [...parameters]),
    };
    return Object.freeze({
      blueprintTitle,
      spendingScript,
      spendingScriptHash: validatorToScriptHash(spendingScript),
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      referenceOutRef: `${"0".repeat(64)}#0`,
    });
  };
  const step06 = apply(5, [
    computationThreadPolicyId,
    fraudProofPolicyId,
    fraudProofTokenAddressData,
  ]);
  const step05 = apply(4, [
    step06.spendingScriptHash,
    computationThreadPolicyId,
    fieldPreimageCertificatePolicyId,
  ]);
  const step04 = apply(3, [
    step05.spendingScriptHash,
    computationThreadPolicyId,
    fraudProofPolicyId,
    fraudProofTokenAddressData,
    fieldPreimageCertificatePolicyId,
  ]);
  const step03 = apply(2, [
    step04.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step02 = apply(1, [
    step03.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const acceptedReferenceSource = applyTitle(
    EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES_V1[6],
    [
      step03.spendingScriptHash,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ],
  );
  const acceptedInlineSource = applyTitle(
    EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES_V1[5],
    [
      step03.spendingScriptHash,
      acceptedReferenceSource.spendingScriptHash,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ],
  );
  const acceptedReceive = applyTitle(
    EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES_V1[4],
    [
      acceptedInlineSource.spendingScriptHash,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ],
  );
  const acceptedObserver = applyTitle(
    EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES_V1[3],
    [
      acceptedReceive.spendingScriptHash,
      acceptedInlineSource.spendingScriptHash,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ],
  );
  const acceptedMint = applyTitle(
    EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES_V1[2],
    [
      acceptedObserver.spendingScriptHash,
      acceptedInlineSource.spendingScriptHash,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ],
  );
  const acceptedSpend = applyTitle(
    EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES_V1[1],
    [
      acceptedMint.spendingScriptHash,
      acceptedInlineSource.spendingScriptHash,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ],
  );
  const acceptedInit = applyTitle(
    EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES_V1[0],
    [acceptedSpend.spendingScriptHash, computationThreadPolicyId],
  );
  const step01 = apply(0, [
    acceptedInit.spendingScriptHash,
    step02.spendingScriptHash,
    computationThreadPolicyId,
    hubOracleScriptHash,
  ]);
  const logicalSteps = [step01, step02, step03, step04, step05, step06];
  Object.defineProperty(logicalSteps, "acceptedPrelude", {
    value: Object.freeze([
      acceptedInit,
      acceptedSpend,
      acceptedMint,
      acceptedObserver,
      acceptedReceive,
      acceptedInlineSource,
      acceptedReferenceSource,
    ]),
    enumerable: false,
  });
  return logicalSteps as unknown as ExecutionNativeScriptInvalidAppliedScriptsV1;
};
