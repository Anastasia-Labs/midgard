import type {
  CommittedFieldClaim,
  ForcedInclusionTx,
  Header,
  OutputReference,
  RootMembershipProof,
} from "@al-ft/midgard-sdk";
import {
  type FieldPreimageLengthMismatchFaultProofContracts,
  FieldPreimageLengthStep01DatumSchema,
  FieldPreimageLengthStep02DatumSchema,
  FieldPreimageLengthStep03DatumSchema,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import {
  type FieldPreimageLengthClaimResolver,
  submitFieldPreimageLengthAcceptedAuthentication,
  submitFieldPreimageLengthAcceptedDispatch,
  submitFieldPreimageLengthCancel,
  submitFieldPreimageLengthForcedAuthentication,
  submitFieldPreimageLengthForcedDispatch,
  submitFieldPreimageLengthInit,
  submitFieldPreimageLengthTerminal,
} from "./submit-lucid-v1.js";
import type {
  FieldPreimageLengthAction,
  FieldPreimageLengthJournal,
  FieldPreimageLengthSubmissionKind,
  PreparedFieldPreimageLengthWorkflow,
} from "./workflow-v1.js";
import { runFieldPreimageLengthWorkflow } from "./workflow-v1.js";

export const FIELD_PREIMAGE_LENGTH_CONFIG =
  "midgard-field-preimage-length-mismatch-production-config-v1" as const;

export const FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS = Object.freeze({
  step01: "fraudProofFieldPreimageLengthMismatch",
  step02Accepted: "fraudProofFieldPreimageLengthMismatchStep02Accepted",
  step02Forced: "fraudProofFieldPreimageLengthMismatchStep02Forced",
  step03: "fraudProofFieldPreimageLengthMismatchStep03",
  computationThreadMint: "fraudProofComputationThreadMint",
  fraudProofMint: "fraudProofMint",
  phasMembershipWithdraw: "phasMembershipWithdraw",
  fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
} as const);

export type FieldPreimageLengthReferenceScripts = Readonly<{
  step01: UTxO;
  step02Accepted: UTxO;
  step02Forced: UTxO;
  step03: UTxO;
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

export type ManifestBoundFieldPreimageLengthConfig = Readonly<{
  schemaVersion: typeof FIELD_PREIMAGE_LENGTH_CONFIG;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBinding<"fieldPreimageLengthMismatch">;
  contracts: FieldPreimageLengthMismatchFaultProofContracts;
  referenceScripts: FieldPreimageLengthReferenceScripts;
}>;

export type LoadManifestBoundFieldPreimageLengthConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: FieldPreimageLengthReferenceScripts;
}>;

export type FieldPreimageLengthLucidSubmissionContext = Readonly<{
  config: ManifestBoundFieldPreimageLengthConfig;
  prepared: PreparedFieldPreimageLengthWorkflow;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
}>;

export type FieldPreimageLengthLucidSubmitter = (
  context: FieldPreimageLengthLucidSubmissionContext,
) => Promise<string>;

/** Concrete builder slots required by production wiring; no stage may no-op. */
export type FieldPreimageLengthLucidBuilders = Readonly<{
  init: FieldPreimageLengthLucidSubmitter;
  dispatchAccepted: FieldPreimageLengthLucidSubmitter;
  dispatchForced: FieldPreimageLengthLucidSubmitter;
  authenticateAccepted: FieldPreimageLengthLucidSubmitter;
  authenticateForced: FieldPreimageLengthLucidSubmitter;
  finalize: FieldPreimageLengthLucidSubmitter;
  remove: FieldPreimageLengthLucidSubmitter;
  cancelDispatch: FieldPreimageLengthLucidSubmitter;
  cancelAuthentication: FieldPreimageLengthLucidSubmitter;
  cancelTerminal: FieldPreimageLengthLucidSubmitter;
}>;

/** Authenticated chain/evidence material resolved afresh before each action. */
export type FieldPreimageLengthStage = Readonly<{
  fraudulentBlockOutRef: string;
  threadOutRef?: string;
  stateQueueBlockOutRef?: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  acceptedClaim?: CommittedFieldClaim;
  acceptedClaimResolver?: FieldPreimageLengthClaimResolver;
  acceptedCarriageReferenceInputs?: readonly UTxO[];
  forcedDirection?: 0n | 1n;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTx>;
  forcedClaim?: CommittedFieldClaim;
  forcedClaimResolver?: FieldPreimageLengthClaimResolver;
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
export const createConcreteFieldPreimageLengthLucidBuilders = ({
  resolveStage,
  remove,
  boundary,
}: {
  readonly resolveStage: (
    context: FieldPreimageLengthLucidSubmissionContext & {
      readonly action: Exclude<FieldPreimageLengthAction, "complete">;
    },
  ) => Promise<FieldPreimageLengthStage>;
  readonly remove: FieldPreimageLengthLucidSubmitter;
  readonly boundary?: (
    action: Exclude<FieldPreimageLengthAction, "complete">,
    prepared: PreparedFieldPreimageLengthWorkflow,
  ) => FraudProofPreSubmitBoundary;
}): FieldPreimageLengthLucidBuilders => {
  const stage = async (
    context: FieldPreimageLengthLucidSubmissionContext,
    action: Exclude<FieldPreimageLengthAction, "complete">,
  ) => await resolveStage({ ...context, action });
  const cancel =
    (fallback: 0 | 1 | 2 | 3): FieldPreimageLengthLucidSubmitter =>
    async (context) => {
      const resolved = await stage(context, "dispatch");
      const result = await submitFieldPreimageLengthCancel({
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
        await submitFieldPreimageLengthInit({
          config: context.config,
          fraudulentBlockOutRef: resolved.fraudulentBlockOutRef,
          preSubmitBoundary: boundary?.("init", context.prepared),
        })
      ).txHash;
    },
    dispatchAccepted: async (context) => {
      const resolved = await stage(context, "dispatch");
      return (
        await submitFieldPreimageLengthAcceptedDispatch({
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
        await submitFieldPreimageLengthForcedDispatch({
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
        await submitFieldPreimageLengthAcceptedAuthentication({
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
        await submitFieldPreimageLengthForcedAuthentication({
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
        await submitFieldPreimageLengthTerminal({
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

/** Durable manifest-bound runner: captured tx ids are reconciled before retry. */
export const runManifestBoundFieldPreimageLengthWorkflow = async ({
  config,
  builders,
  load,
  save,
  observeConfirmed,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly builders: FieldPreimageLengthLucidBuilders;
  readonly load: () => Promise<FieldPreimageLengthJournal>;
  readonly save: (journal: FieldPreimageLengthJournal) => Promise<void>;
  readonly observeConfirmed: (
    action: "init" | "dispatch" | "authenticate" | "finalize" | "remove",
    transactionId: string,
  ) => Promise<boolean>;
}): Promise<FieldPreimageLengthJournal> => {
  const routed = createFieldPreimageLengthLucidSubmission({
    config,
    builders,
  });
  return await runFieldPreimageLengthWorkflow({
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
