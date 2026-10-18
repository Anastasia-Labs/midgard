import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import type { ScriptDecodeFaultProofChainInputs } from "../shared.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const WITNESS_SCRIPT_DECODING_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofWitnessScriptDecoding"),
  step02: deployedTitle("fraudProofWitnessScriptDecodingStep02"),
  step03: deployedTitle("fraudProofWitnessScriptDecodingStep03"),
  step04: deployedTitle("fraudProofWitnessScriptDecodingStep04"),
} as const;

export type WitnessScriptDecodingFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly scriptDecode: AuthenticatedValidator;
  readonly witnessScriptDecoding: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildWitnessScriptDecodingFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildWitnessScriptDecodingChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  scriptDecodePolicyId,
}: ScriptDecodeFaultProofChainInputs): Effect.Effect<
  WitnessScriptDecodingFaultProofContracts["witnessScriptDecoding"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      WITNESS_SCRIPT_DECODING_FAULT_PROOF_TITLES.step04,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build witness-script-decoding step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      WITNESS_SCRIPT_DECODING_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build witness-script-decoding step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      WITNESS_SCRIPT_DECODING_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
        scriptDecodePolicyId,
      ],
      "Failed to build witness-script-decoding step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      WITNESS_SCRIPT_DECODING_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build witness-script-decoding step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03, step04] };
  });

export const buildWitnessScriptDecodingFaultProofContracts = (
  params: BuildWitnessScriptDecodingFaultProofContractsParams,
): Effect.Effect<WitnessScriptDecodingFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const witnessScriptDecoding = yield* buildWitnessScriptDecodingChain({
      ...params,
      ...shared,
    });
    return { ...shared, witnessScriptDecoding };
  });
