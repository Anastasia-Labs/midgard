import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
  type SharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const REDEEMER_CANONICITY_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/redeemer_canonicity/step_01.main.spend",
  step02: "fraud_proofs/redeemer_canonicity/step_02.main.spend",
  step03: "fraud_proofs/redeemer_canonicity/step_03.main.spend",
} as const;

export type RedeemerCanonicityFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly redeemerCanonicity: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildRedeemerCanonicityFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildRedeemerCanonicityChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  RedeemerCanonicityFaultProofContracts["redeemerCanonicity"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      REDEEMER_CANONICITY_FAULT_PROOF_TITLES.step03,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build redeemer-canonicity step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      REDEEMER_CANONICITY_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build redeemer-canonicity step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      REDEEMER_CANONICITY_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build redeemer-canonicity step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03] };
  });

export const buildRedeemerCanonicityFaultProofContracts = (
  params: BuildRedeemerCanonicityFaultProofContractsParams,
): Effect.Effect<RedeemerCanonicityFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const redeemerCanonicity = yield* buildRedeemerCanonicityChain({
      ...params,
      ...shared,
    });
    return { ...shared, redeemerCanonicity };
  });
