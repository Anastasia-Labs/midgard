import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import type { CertifiedFaultProofChainInputs } from "../shared.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const MINT_ITEM_NON_CANONICAL_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/mint_item_non_canonical/step_01.main.spend",
  step02: "fraud_proofs/mint_item_non_canonical/step_02.main.spend",
  step03: "fraud_proofs/mint_item_non_canonical/step_03.main.spend",
  step04: "fraud_proofs/mint_item_non_canonical/step_04.main.spend",
} as const;

export type MintItemNonCanonicalFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly mintItemNonCanonical: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildMintItemNonCanonicalFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildMintItemNonCanonicalChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: CertifiedFaultProofChainInputs): Effect.Effect<
  MintItemNonCanonicalFaultProofContracts["mintItemNonCanonical"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      MINT_ITEM_NON_CANONICAL_FAULT_PROOF_TITLES.step04,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build mint-item-non-canonical step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MINT_ITEM_NON_CANONICAL_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build mint-item-non-canonical step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MINT_ITEM_NON_CANONICAL_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build mint-item-non-canonical step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MINT_ITEM_NON_CANONICAL_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build mint-item-non-canonical step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03, step04] };
  });

export const buildMintItemNonCanonicalFaultProofContracts = (
  params: BuildMintItemNonCanonicalFaultProofContractsParams,
): Effect.Effect<MintItemNonCanonicalFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const mintItemNonCanonical = yield* buildMintItemNonCanonicalChain({
      ...params,
      ...shared,
    });
    return { ...shared, mintItemNonCanonical };
  });
