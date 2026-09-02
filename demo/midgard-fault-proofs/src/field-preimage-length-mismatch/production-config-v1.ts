import type {
  CommittedFieldClaimV1,
  ForcedInclusionTxV1,
  HeaderV1,
  OutputReference,
  RootMembershipProof,
} from "@al-ft/midgard-sdk";
import {
  type FieldPreimageLengthMismatchFaultProofContracts,
  FieldPreimageLengthStep01DatumV1Schema,
  FieldPreimageLengthStep02DatumV1Schema,
  FieldPreimageLengthStep03DatumV1Schema,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import {
  type FieldPreimageLengthClaimResolverV1,
  submitFieldPreimageLengthAcceptedAuthenticationV1,
  submitFieldPreimageLengthAcceptedDispatchV1,
  submitFieldPreimageLengthCancelV1,
  submitFieldPreimageLengthForcedAuthenticationV1,
  submitFieldPreimageLengthForcedDispatchV1,
  submitFieldPreimageLengthInitV1,
  submitFieldPreimageLengthTerminalV1,
} from "./submit-lucid-v1.js";
import type {
  FieldPreimageLengthActionV1,
  FieldPreimageLengthJournalV1,
  FieldPreimageLengthSubmissionKindV1,
  PreparedFieldPreimageLengthWorkflowV1,
} from "./workflow-v1.js";
import { runFieldPreimageLengthWorkflowV1 } from "./workflow-v1.js";

export const FIELD_PREIMAGE_LENGTH_PRODUCTION_CONFIG_V1 =
  "midgard-field-preimage-length-mismatch-production-config-v1" as const;

export const FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS_V1 = Object.freeze({
  step01: "fraudProofFieldPreimageLengthMismatch",
  step02Accepted: "fraudProofFieldPreimageLengthMismatchStep02Accepted",
  step02Forced: "fraudProofFieldPreimageLengthMismatchStep02Forced",
  step03: "fraudProofFieldPreimageLengthMismatchStep03",
  computationThreadMint: "fraudProofComputationThreadMint",
  fraudProofMint: "fraudProofMint",
  phasMembershipWithdraw: "phasMembershipWithdraw",
  fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
} as const);

export type FieldPreimageLengthProductionReferenceScriptsV1 = Readonly<{
  step01: UTxO;
  step02Accepted: UTxO;
  step02Forced: UTxO;
  step03: UTxO;
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

export type ManifestBoundFieldPreimageLengthConfigV1 = Readonly<{
  schemaVersion: typeof FIELD_PREIMAGE_LENGTH_PRODUCTION_CONFIG_V1;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBindingV1<"fieldPreimageLengthMismatch">;
  contracts: FieldPreimageLengthMismatchFaultProofContracts;
  referenceScripts: FieldPreimageLengthProductionReferenceScriptsV1;
}>;

export type LoadManifestBoundFieldPreimageLengthConfigV1 = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: FieldPreimageLengthProductionReferenceScriptsV1;
}>;

export type FieldPreimageLengthLucidSubmissionContextV1 = Readonly<{
  config: ManifestBoundFieldPreimageLengthConfigV1;
  prepared: PreparedFieldPreimageLengthWorkflowV1;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
}>;

export type FieldPreimageLengthLucidSubmitterV1 = (
  context: FieldPreimageLengthLucidSubmissionContextV1,
) => Promise<string>;

/** Concrete builder slots required by production wiring; no stage may no-op. */
export type FieldPreimageLengthLucidBuildersV1 = Readonly<{
  init: FieldPreimageLengthLucidSubmitterV1;
  dispatchAccepted: FieldPreimageLengthLucidSubmitterV1;
  dispatchForced: FieldPreimageLengthLucidSubmitterV1;
  authenticateAccepted: FieldPreimageLengthLucidSubmitterV1;
  authenticateForced: FieldPreimageLengthLucidSubmitterV1;
  finalize: FieldPreimageLengthLucidSubmitterV1;
  remove: FieldPreimageLengthLucidSubmitterV1;
  cancelDispatch: FieldPreimageLengthLucidSubmitterV1;
  cancelAuthentication: FieldPreimageLengthLucidSubmitterV1;
  cancelTerminal: FieldPreimageLengthLucidSubmitterV1;
}>;

/** Authenticated chain/evidence material resolved afresh before each action. */
export type FieldPreimageLengthProductionStageV1 = Readonly<{
  fraudulentBlockOutRef: string;
  threadOutRef?: string;
  stateQueueBlockOutRef?: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  acceptedClaim?: CommittedFieldClaimV1;
  acceptedClaimResolver?: FieldPreimageLengthClaimResolverV1;
  acceptedCarriageReferenceInputs?: readonly UTxO[];
  forcedDirection?: 0n | 1n;
  forcedHeader?: HeaderV1;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedClaim?: CommittedFieldClaimV1;
  forcedClaimResolver?: FieldPreimageLengthClaimResolverV1;
  forcedCarriageReferenceInputs?: readonly UTxO[];
  cancelStepIndex?: 0 | 1 | 2 | 3;
}>;

const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`field-preimage-length missing ${label}`);
  return value;
};

/**
 * Binds all ten production slots to the real Lucid builders. The resolver is
 * deliberately called per action so restart replay uses authenticated current
 * out-refs rather than journal-cached transaction layout.
 */
export const createConcreteFieldPreimageLengthLucidBuildersV1 = ({
  resolveStage,
  remove,
  boundary,
}: {
  readonly resolveStage: (
    context: FieldPreimageLengthLucidSubmissionContextV1 & {
      readonly action: Exclude<FieldPreimageLengthActionV1, "complete">;
    },
  ) => Promise<FieldPreimageLengthProductionStageV1>;
  readonly remove: FieldPreimageLengthLucidSubmitterV1;
  readonly boundary?: (
    action: Exclude<FieldPreimageLengthActionV1, "complete">,
    prepared: PreparedFieldPreimageLengthWorkflowV1,
  ) => FraudProofPreSubmitBoundaryV1;
}): FieldPreimageLengthLucidBuildersV1 => {
  const stage = async (
    context: FieldPreimageLengthLucidSubmissionContextV1,
    action: Exclude<FieldPreimageLengthActionV1, "complete">,
  ) => await resolveStage({ ...context, action });
  const cancel =
    (fallback: 0 | 1 | 2 | 3): FieldPreimageLengthLucidSubmitterV1 =>
    async (context) => {
      const resolved = await stage(context, "dispatch");
      const result = await submitFieldPreimageLengthCancelV1({
        config: context.config,
        threadOutRef: required(resolved.threadOutRef, "cancel thread out-ref"),
        stepIndex: resolved.cancelStepIndex ?? fallback,
        preSubmitBoundary: boundary?.("dispatch", context.prepared),
      });
      return result.txHash;
    };
  return Object.freeze({
    init: async (context) => {
      const resolved = await stage(context, "init");
      return (
        await submitFieldPreimageLengthInitV1({
          config: context.config,
          fraudulentBlockOutRef: resolved.fraudulentBlockOutRef,
          preSubmitBoundary: boundary?.("init", context.prepared),
        })
      ).txHash;
    },
    dispatchAccepted: async (context) => {
      const resolved = await stage(context, "dispatch");
      return (
        await submitFieldPreimageLengthAcceptedDispatchV1({
          config: context.config,
          threadOutRef: required(
            resolved.threadOutRef,
            "dispatch thread out-ref",
          ),
          stateQueueBlockOutRef: required(
            resolved.stateQueueBlockOutRef,
            "state-queue block out-ref",
          ),
          inclusion: required(resolved.acceptedInclusion, "accepted inclusion"),
          ...(resolved.acceptedClaim === undefined
            ? {}
            : { claim: resolved.acceptedClaim }),
          ...(resolved.acceptedClaimResolver === undefined
            ? {}
            : { claimResolver: resolved.acceptedClaimResolver }),
          carriageReferenceInputs:
            resolved.acceptedCarriageReferenceInputs ?? [],
          preSubmitBoundary: boundary?.("dispatch", context.prepared),
        })
      ).txHash;
    },
    dispatchForced: async (context) => {
      const resolved = await stage(context, "dispatch");
      return (
        await submitFieldPreimageLengthForcedDispatchV1({
          config: context.config,
          threadOutRef: required(
            resolved.threadOutRef,
            "dispatch thread out-ref",
          ),
          direction: required(resolved.forcedDirection, "forced direction"),
          preSubmitBoundary: boundary?.("dispatch", context.prepared),
        })
      ).txHash;
    },
    authenticateAccepted: async (context) => {
      const resolved = await stage(context, "authenticate");
      return (
        await submitFieldPreimageLengthAcceptedAuthenticationV1({
          config: context.config,
          threadOutRef: required(
            resolved.threadOutRef,
            "authentication thread out-ref",
          ),
          ...(resolved.acceptedClaim === undefined
            ? {}
            : { claim: resolved.acceptedClaim }),
          ...(resolved.acceptedClaimResolver === undefined
            ? {}
            : { claimResolver: resolved.acceptedClaimResolver }),
          prepared: context.prepared,
          carriageReferenceInputs:
            resolved.acceptedCarriageReferenceInputs ?? [],
          preSubmitBoundary: boundary?.("authenticate", context.prepared),
        })
      ).txHash;
    },
    authenticateForced: async (context) => {
      const resolved = await stage(context, "authenticate");
      return (
        await submitFieldPreimageLengthForcedAuthenticationV1({
          config: context.config,
          threadOutRef: required(
            resolved.threadOutRef,
            "authentication thread out-ref",
          ),
          header: required(resolved.forcedHeader, "forced header"),
          membership: required(resolved.forcedMembership, "forced membership"),
          ...(resolved.forcedClaim === undefined
            ? {}
            : { claim: resolved.forcedClaim }),
          ...(resolved.forcedClaimResolver === undefined
            ? {}
            : { claimResolver: resolved.forcedClaimResolver }),
          prepared: context.prepared,
          carriageReferenceInputs: resolved.forcedCarriageReferenceInputs ?? [],
          preSubmitBoundary: boundary?.("authenticate", context.prepared),
        })
      ).txHash;
    },
    finalize: async (context) => {
      const resolved = await stage(context, "finalize");
      return (
        await submitFieldPreimageLengthTerminalV1({
          config: context.config,
          threadOutRef: required(
            resolved.threadOutRef,
            "terminal thread out-ref",
          ),
          preSubmitBoundary: boundary?.("finalize", context.prepared),
        })
      ).txHash;
    },
    remove: async (context) =>
      await remove({
        ...context,
        preSubmitBoundary: boundary?.("remove", context.prepared),
      }),
    cancelDispatch: cancel(0),
    cancelAuthentication: cancel(1),
    cancelTerminal: cancel(3),
  });
};

const TX_ID = /^[0-9a-f]{64}$/u;

/**
 * Manifest-bound production routing. Direction selects distinct physical
 * scripts at both dispatch and authentication; it can never be supplied by a
 * caller independently of the admitted evidence.
 */
export const createFieldPreimageLengthLucidSubmissionV1 = ({
  config,
  builders,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfigV1;
  readonly builders: FieldPreimageLengthLucidBuildersV1;
}): Readonly<{
  submit: (
    action: FieldPreimageLengthSubmissionKindV1,
    prepared: PreparedFieldPreimageLengthWorkflowV1,
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

/** Durable manifest-bound runner: captured tx ids are reconciled before retry. */
export const runManifestBoundFieldPreimageLengthWorkflowV1 = async ({
  config,
  builders,
  load,
  save,
  observeConfirmed,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfigV1;
  readonly builders: FieldPreimageLengthLucidBuildersV1;
  readonly load: () => Promise<FieldPreimageLengthJournalV1>;
  readonly save: (journal: FieldPreimageLengthJournalV1) => Promise<void>;
  readonly observeConfirmed: (
    action: "init" | "dispatch" | "authenticate" | "finalize" | "remove",
    transactionId: string,
  ) => Promise<boolean>;
}): Promise<FieldPreimageLengthJournalV1> => {
  const routed = createFieldPreimageLengthLucidSubmissionV1({
    config,
    builders,
  });
  return await runFieldPreimageLengthWorkflowV1({
    load,
    save,
    submit: async (action, prepared) => await routed.submit(action, prepared),
    observeConfirmed,
  });
};

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: FraudProofWorkflowDeploymentBindingV1<"fieldPreimageLengthMismatch">;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxoV1({
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
export const loadManifestBoundFieldPreimageLengthConfigV1 = async (
  input: LoadManifestBoundFieldPreimageLengthConfigV1,
): Promise<ManifestBoundFieldPreimageLengthConfigV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "fieldPreimageLengthMismatch",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas: [
      FieldPreimageLengthStep01DatumV1Schema,
      FieldPreimageLengthStep02DatumV1Schema,
      FieldPreimageLengthStep02DatumV1Schema,
      FieldPreimageLengthStep03DatumV1Schema,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });
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
  const names = FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS_V1;
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
  return Object.freeze({
    schemaVersion: FIELD_PREIMAGE_LENGTH_PRODUCTION_CONFIG_V1,
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
