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
  buildComputationThreadValidator,
  buildFraudProofValidator,
} from "../../protocol-contracts.js";
import { buildFieldGrammarCertificateValidator } from "../field-grammar-certificate.build-field-grammar-certificate-validator.js";
import {
  applyBlueprintParams,
  asAddressDataParam,
  getUnappliedScript,
  makeMintingPolicy,
  makeSpendingValidator,
  tryBuild,
} from "./blueprint.js";
import {
  buildScriptDecodeValidator,
  type ScriptDecodeValidator,
} from "./script-decode.js";
import { FAULT_PROOF_SHARED_TITLES } from "./titles.js";
import { type BuildFaultProofContractsParams } from "./types.js";

/** Inputs needed to link a family to already-derived deployment identities. */
export type FaultProofChainInputs = Pick<
  BuildFaultProofContractsParams,
  "blueprint" | "network" | "hubOraclePolicyId"
> & {
  readonly computationThread: Pick<MintingValidator, "policyId">;
  readonly fraudProof: Pick<AuthenticatedValidator, "policyId">;
  readonly fraudProofTokenAddressData: Data;
};

export type CertifiedFaultProofChainInputs = FaultProofChainInputs & {
  readonly fieldPreimageCertificatePolicyId: string;
};

/**
 * A certified chain with a step that can also read a variable-width field's
 * item or count through the field grammar certificate.
 */
export type FieldGrammarCertifiedChainInput = {
  readonly fieldGrammarCertificatePolicyId: string;
};

/** A certified chain whose steps also read script-decode records. */
export type ScriptDecodeFaultProofChainInputs =
  CertifiedFaultProofChainInputs & {
    readonly scriptDecodePolicyId: string;
  };

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
  /**
   * The field grammar certificate: one script for its mint and spend
   * handlers, applied to the field-preimage certificate policy above, so its
   * policy id is the spend credential and a pure function of the blueprint.
   * Steps that read a variable-width field's item or count through a
   * certificate take this policy id as a parameter.
   */
  readonly fieldGrammarCertificate: SpendingValidator & MintingValidator;
  readonly fieldGrammarCertificatePolicyId: string;
  /**
   * The script-decode family: runs at its spending address, records minted
   * under its policy. The validator takes its class stakers' hashes and its
   * handlers share one script, so the policy id is the script hash and a pure
   * function of the blueprint, derived here for the same reason as the
   * certificate policy above.
   */
  readonly scriptDecode: ScriptDecodeValidator;
  readonly scriptDecodePolicyId: string;
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
        buildComputationThreadValidator(blueprint, {
          fraudProofCatalogue: { policyId: fraudProofCataloguePolicyId },
          hubOracle: { policyId: hubOraclePolicyId },
        }),
    );

    const fraudProof = yield* tryBuild(
      "Failed to build fraud-proof token validator",
      () => buildFraudProofValidator(blueprint, network, computationThread),
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

    const fieldGrammarCertificate = yield* tryBuild(
      "Failed to build field grammar certificate validator",
      () =>
        buildFieldGrammarCertificateValidator({
          blueprint,
          network,
          fieldPreimageCertificatePolicyId: fieldPreimageCertificate.policyId,
        }),
    );

    const scriptDecode = yield* tryBuild(
      "Failed to build script-decode validator",
      () => buildScriptDecodeValidator(blueprint, network),
    );

    return {
      computationThread,
      fraudProof,
      fraudProofTokenAddressData,
      fieldPreimageCertificate,
      fieldPreimageCertificatePolicyId: fieldPreimageCertificate.policyId,
      fieldGrammarCertificate,
      fieldGrammarCertificatePolicyId: fieldGrammarCertificate.policyId,
      scriptDecode,
      scriptDecodePolicyId: scriptDecode.policyId,
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
