import {
  FieldPreimageLengthStep01DatumSchema,
  FieldPreimageLengthStep02DatumSchema,
  FieldPreimageLengthStep03DatumSchema,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  FIELD_PREIMAGE_LENGTH_CONFIG,
  FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS,
  type FieldPreimageLengthLucidBuilders,
  type FieldPreimageLengthReferenceScripts,
  type LoadManifestBoundFieldPreimageLengthConfig,
  type ManifestBoundFieldPreimageLengthConfig,
  TX_ID,
} from "./config.create-concrete-field-preimage-length-lucid-builders.js";
import type {
  FieldPreimageLengthSubmissionKind,
  PreparedFieldPreimageLengthWorkflow,
} from "./workflow.js";

/**
 * Manifest-bound production routing. Direction selects distinct physical
 * scripts at both dispatch and authentication; it can never be supplied by a
 * caller independently of the admitted evidence.
 */
export const createFieldPreimageLengthLucidSubmission = ({
  config,
  builders,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly builders: FieldPreimageLengthLucidBuilders;
}): Readonly<{
  submit: (
    action: FieldPreimageLengthSubmissionKind,
    prepared: PreparedFieldPreimageLengthWorkflow,
  ) => Promise<string>;
}> =>
  Object.freeze({
    submit: async (action, prepared) => {
      if (prepared.headerHash !== config.binding.definition.headerHash) {
        throw new Error(
          "field-preimage-length evidence targets a different manifest-bound header",
        );
      }
      const context = Object.freeze({ config, prepared });
      const accepted = prepared.direction === "wrongfulAcceptance";
      const builder =
        action === "init"
          ? builders.init
          : action === "dispatch"
            ? accepted
              ? builders.dispatchAccepted
              : builders.dispatchForced
            : action === "authenticate"
              ? accepted
                ? builders.authenticateAccepted
                : builders.authenticateForced
              : action === "finalize"
                ? builders.finalize
                : action === "remove"
                  ? builders.remove
                  : action === "cancelDispatch"
                    ? builders.cancelDispatch
                    : action === "cancelAuthentication"
                      ? builders.cancelAuthentication
                      : builders.cancelTerminal;
      const transactionId = await builder(context);
      if (!TX_ID.test(transactionId)) {
        throw new Error(
          `field-preimage-length ${action} submitter returned a non-canonical transaction id`,
        );
      }
      return transactionId;
    },
  });

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"fieldPreimageLengthMismatch">;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxo({
    binding,
    contractName,
    utxo,
  });

/**
 * Loads the category exclusively from a finalized manifest and rejects any
 * caller-selected script bytes, hashes, network, catalogue proof, or reference
 * out-ref. The accepted/forced split is intentionally represented by two
 * different reference inputs.
 */
export const loadManifestBoundFieldPreimageLengthConfig = async (
  input: LoadManifestBoundFieldPreimageLengthConfig,
): Promise<ManifestBoundFieldPreimageLengthConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "fieldPreimageLengthMismatch",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas: [
      FieldPreimageLengthStep01DatumSchema,
      FieldPreimageLengthStep02DatumSchema,
      FieldPreimageLengthStep02DatumSchema,
      FieldPreimageLengthStep03DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });
  const names = FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS;
  const references = input.referenceScripts;
  const referenceScripts = Object.freeze({
    step01: bindReference({
      binding,
      contractName: names.step01,
      utxo: references.step01,
    }),
    step02Accepted: bindReference({
      binding,
      contractName: names.step02Accepted,
      utxo: references.step02Accepted,
    }),
    step02Forced: bindReference({
      binding,
      contractName: names.step02Forced,
      utxo: references.step02Forced,
    }),
    step03: bindReference({
      binding,
      contractName: names.step03,
      utxo: references.step03,
    }),
    fieldPreimageCertificateMint: bindReference({
      binding,
      contractName: names.fieldPreimageCertificateMint,
      utxo: references.fieldPreimageCertificateMint,
    }),
    witnesses: Object.freeze({
      ...references.witnesses,
      computationThreadMint: bindReference({
        binding,
        contractName: names.computationThreadMint,
        utxo: references.witnesses.computationThreadMint,
      }),
      fraudProofMint: bindReference({
        binding,
        contractName: names.fraudProofMint,
        utxo: references.witnesses.fraudProofMint,
      }),
      phasMembershipWithdraw: bindReference({
        binding,
        contractName: names.phasMembershipWithdraw,
        utxo: references.witnesses.phasMembershipWithdraw,
      }),
    }),
  });
  return fieldPreimageLengthConfigFromBinding({
    ...input,
    binding,
    referenceScripts,
  });
};

/** Builds category contracts from an authenticated binding and checked references. */
export const fieldPreimageLengthConfigFromBinding = (input: {
  readonly binding: ManifestBoundFieldPreimageLengthConfig["binding"];
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly referenceScripts: FieldPreimageLengthReferenceScripts;
}): ManifestBoundFieldPreimageLengthConfig => {
  const { binding, referenceScripts } = input;
  const contracts = binding.resolvedContracts.contracts;
  const chain = contracts.fieldPreimageLengthMismatch;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined) {
    throw new Error(
      "field-preimage-length deployment omitted the resolved category chain",
    );
  }
  if (certificate === null) {
    throw new Error(
      "field-preimage-length deployment omitted the field-preimage certificate policy",
    );
  }
  if (
    chain.steps.length !== 4 ||
    chain.steps[0].spendingScriptHash !== chain.firstStep.spendingScriptHash ||
    chain.steps[1].spendingScriptHash !==
      chain.acceptedStep02.spendingScriptHash ||
    chain.steps[2].spendingScriptHash !== chain.forcedStep02.spendingScriptHash
  ) {
    throw new Error(
      "field-preimage-length resolved chain changed its four-script physical topology",
    );
  }
  return Object.freeze({
    schemaVersion: FIELD_PREIMAGE_LENGTH_CONFIG,
    lucid: input.lucid,
    signer: input.signer,
    binding,
    contracts: {
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      fieldPreimageCertificate: {
        policyId: certificate.policyId,
        mintingScript: certificate.mintingScript,
        mintingScriptCBOR: certificate.mintingScript.script,
      },
      fieldPreimageLengthMismatch: chain,
    },
    referenceScripts,
  });
};
