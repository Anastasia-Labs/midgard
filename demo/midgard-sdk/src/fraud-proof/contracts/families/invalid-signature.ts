import { Data, Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  makeSpendingValidator,
  tryBuild,
} from "../blueprint.js";
import { buildSharedFaultProofContracts } from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const INVALID_SIGNATURE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/invalid_signature/step_01.main.spend",
  step02: "fraud_proofs/invalid_signature/step_02.main.spend",
} as const;

export type InvalidSignatureFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly invalidSignature: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type BuildInvalidSignatureFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildInvalidSignatureChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
}): Effect.Effect<
  InvalidSignatureFaultProofContracts["invalidSignature"],
  Error
> =>
  Effect.gen(function* () {
    const invalidSignatureStep02 = yield* tryBuild(
      "Failed to build invalid-signature step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            INVALID_SIGNATURE_FAULT_PROOF_TITLES.step02,
            [
              computationThread.policyId,
              fraudProof.policyId,
              fraudProofTokenAddressData,
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );

    const invalidSignatureStep01 = yield* tryBuild(
      "Failed to build invalid-signature step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            INVALID_SIGNATURE_FAULT_PROOF_TITLES.step01,
            [
              invalidSignatureStep02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: invalidSignatureStep01,
      steps: [invalidSignatureStep01, invalidSignatureStep02],
    };
  });

export const buildInvalidSignatureFaultProofContracts = (
  params: BuildInvalidSignatureFaultProofContractsParams,
): Effect.Effect<InvalidSignatureFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const invalidSignature = yield* buildInvalidSignatureChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      invalidSignature,
    };
  });
