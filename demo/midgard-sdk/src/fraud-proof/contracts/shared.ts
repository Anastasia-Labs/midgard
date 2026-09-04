/**
 * Contracts shared by every family and the spending-step builder.
 */

import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../common.js";
import {
  applyBlueprintParams,
  asAddressDataParam,
  getUnappliedScript,
  makeAuthenticatedValidator,
  makeMintingPolicy,
  makeSpendingValidator,
  tryBuild,
} from "./blueprint.js";
import { FAULT_PROOF_SHARED_TITLES } from "./titles.js";
import { type BuildFaultProofContractsParams } from "./types.js";

export type SharedFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificate: MintingValidator;
  /**
   * The §8.6 field-preimage certificate minting policy id. #592 gave the
   * step validators that consult a carried field preimage a trailing
   * `field_preimage_certificate_policy_id` parameter; the certificate
   * validator itself takes no parameters, so this id is a pure function of
   * the blueprint. It is derived here rather than accepted from callers
   * precisely because it cannot vary independently of the blueprint the
   * other contracts are built from — a caller-supplied value could only ever
   * agree or be wrong.
   */
  readonly fieldPreimageCertificatePolicyId: string;
};

export const buildSharedFaultProofContracts = ({
  blueprint,
  network,
  hubOraclePolicyId,
  fraudProofCataloguePolicyId,
}: BuildFaultProofContractsParams): Effect.Effect<
  SharedFaultProofContracts,
  Error
> =>
  Effect.gen(function* () {
    const computationThread = yield* tryBuild(
      "Failed to build computation-thread minting policy",
      () =>
        makeMintingPolicy(
          applyBlueprintParams(
            blueprint,
            FAULT_PROOF_SHARED_TITLES.computationThreadMint,
            [fraudProofCataloguePolicyId, hubOraclePolicyId],
          ),
        ),
    );

    const fraudProof = yield* tryBuild(
      "Failed to build fraud-proof token validator",
      () =>
        makeAuthenticatedValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FAULT_PROOF_SHARED_TITLES.fraudProofMint,
            [computationThread.policyId],
          ),
          getUnappliedScript(
            blueprint,
            FAULT_PROOF_SHARED_TITLES.fraudProofSpend,
          ),
        ),
    );

    const fraudProofTokenAddressData = yield* asAddressDataParam(
      fraudProof.spendingScriptAddress,
    );

    const fieldPreimageCertificate = yield* tryBuild(
      "Failed to build field-preimage certificate minting policy",
      () =>
        makeMintingPolicy(
          getUnappliedScript(
            blueprint,
            FAULT_PROOF_SHARED_TITLES.fieldPreimageCertificateMint,
          ),
        ),
    );

    return {
      computationThread,
      fraudProof,
      fraudProofTokenAddressData,
      fieldPreimageCertificate,
      fieldPreimageCertificatePolicyId: fieldPreimageCertificate.policyId,
    };
  });

export const buildFaultProofSpendingStep = (
  context: Pick<BuildFaultProofContractsParams, "blueprint" | "network">,
  title: string,
  params: readonly Data[],
  description: string,
): Effect.Effect<SpendingValidator, Error> =>
  tryBuild(description, () =>
    makeSpendingValidator(
      context.network,
      applyBlueprintParams(context.blueprint, title, params),
    ),
  );
