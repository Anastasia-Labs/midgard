/**
 * Manifest-bound family assembly: one module that turns a family definition
 * plus a runtime config into a manifest-bound workflow, and one generic
 * run-or-resume that launches it. Every family used to restate this
 * lifecycle by hand; a fix to the signer assertion, the certificate
 * requirement or the prerequisite order now lands here once.
 *
 * Assembly order, which the table-driven test locks at the interface:
 *   1. bind the deployment with the definition's step datum schemas;
 *   2. assert the signer is the manifest-network enterprise address;
 *   3. require the field-preimage certificate when the definition declares it;
 *   4. bind every reference script (step contract names, then declared
 *      witness roles, then the certificate mint) against the finalized
 *      manifest;
 *   5. open the lazy L1 observation port and require its raw-L1 and
 *      publication authorities;
 *   6. ask the family for its transaction port and replayer, and build the
 *      adapter the definition's arm names: linear, or cursor from the arm's
 *      spec and action refiner;
 *   7. decorate the adapter: field carriage first (in declared order), then
 *      proof chunks, each only when declared;
 *   8. attach the terminal verifier, release-finality authority and any
 *      `extend` members, and freeze.
 *
 * Reference binding precedes provider startup so an offline unit test can
 * assemble every definition, and so a wrong reference fails before a
 * transport is opened.
 */
import type { UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { createCursorFamilyWorkflowAdapter } from "./cursor-family-adapter.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding.js";
import type {
  FamilyAssemblyContext,
  FamilyCategory,
  FamilyDefinition,
  FamilyReferenceScripts,
  FamilyTransactionPort,
  FaultProofWitnessRole,
  FieldPreimageCertificateBinding,
  LinearFamilyStepDatumSchema,
  ManifestBoundFamilyWorkflow,
  ManifestBoundFamilyWorkflowConfig,
} from "./family-definition.js";
import {
  familyStepContractNames,
  LINEAR_FAMILY_DEFINITION_VERSION,
} from "./family-definition.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
  observeFraudProofWorkflowHeader,
} from "./family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "./field-carriage-prerequisite.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";
import { createLinearFamilyWorkflowAdapter } from "./linear-family-adapter.js";
import type { LinearFamilyCategory } from "./linear-family-spec.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  withProofChunkPrerequisite,
} from "./proof-chunk-prerequisite.js";

const requireDefinition = <
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number,
>(
  definition: FamilyDefinition<Category, Witness, Certificate, StepCount>,
): readonly string[] => {
  if (
    definition.definitionVersion !== LINEAR_FAMILY_DEFINITION_VERSION ||
    !Object.isFrozen(definition)
  ) {
    throw new Error(
      `${definition.category} family definition changed identity`,
    );
  }
  const { adapter } = definition;
  if (adapter.kind === "cursor") {
    const names = adapter.stepContractNames as readonly string[];
    if (
      adapter.spec.category !== definition.category ||
      names.length !== adapter.spec.stepCount
    ) {
      throw new Error(
        `${definition.category} definition's cursor spec disagrees with its step contract names`,
      );
    }
  }
  const stepContractNames = familyStepContractNames(definition);
  const schemas = definition.stepDatumSchemas as readonly unknown[];
  if (schemas.length !== stepContractNames.length) {
    throw new Error(
      `${definition.category} definition declares ${schemas.length.toString()} step datum schemas for a ${stepContractNames.length.toString()}-step spec`,
    );
  }
  if (
    new Set(definition.witnessRoles).size !== definition.witnessRoles.length
  ) {
    throw new Error(
      `${definition.category} definition repeats a witness reference role`,
    );
  }
  return stepContractNames;
};

const bindReferenceScripts = <
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number,
>({
  definition,
  stepContractNames,
  binding,
  supplied,
}: {
  readonly definition: FamilyDefinition<
    Category,
    Witness,
    Certificate,
    StepCount
  >;
  readonly stepContractNames: readonly string[];
  readonly binding: Parameters<
    typeof requireManifestBoundReferenceScriptUtxo
  >[0]["binding"];
  readonly supplied: FamilyReferenceScripts<
    Category,
    Witness,
    Certificate,
    StepCount
  >;
}): FamilyReferenceScripts<Category, Witness, Certificate, StepCount> => {
  const { category } = definition;
  const suppliedSteps = supplied.steps as readonly UTxO[];
  if (suppliedSteps.length !== stepContractNames.length) {
    throw new Error(
      `${category} workflow config supplied ${suppliedSteps.length.toString()} step reference scripts for a ${stepContractNames.length.toString()}-step spec`,
    );
  }
  const steps = stepContractNames.map((contractName, index) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: suppliedSteps[index]!,
    }),
  );
  const suppliedWitnesses = supplied.witnesses as Readonly<
    Partial<Record<FaultProofWitnessRole, UTxO>>
  >;
  const witnesses = Object.fromEntries(
    definition.witnessRoles.map((role) => {
      const utxo = suppliedWitnesses[role];
      if (utxo === undefined) {
        throw new Error(
          `${category} workflow config omitted witness reference script ${role}`,
        );
      }
      return [
        role,
        requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: role,
          utxo,
        }),
      ];
    }),
  );
  const certificateMint = definition.fieldPreimageCertificate
    ? {
        fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "fieldPreimageCertificateMint",
          utxo: (
            supplied as unknown as {
              readonly fieldPreimageCertificateMint: UTxO;
            }
          ).fieldPreimageCertificateMint,
        }),
      }
    : {};
  return Object.freeze({
    steps: Object.freeze(steps),
    witnesses: Object.freeze(witnesses),
    ...certificateMint,
  }) as unknown as FamilyReferenceScripts<
    Category,
    Witness,
    Certificate,
    StepCount
  >;
};

/**
 * The family's transaction port and the undecorated adapter its arm names.
 * The linear arm's port is typed for `Category & LinearFamilyCategory`;
 * `familyStepContractNames` has already resolved the category's linear spec,
 * so the category is linear here even though the generic cannot show it.
 */
const familyAdapter = <
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number,
>({
  definition,
  context,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly definition: FamilyDefinition<
    Category,
    Witness,
    Certificate,
    StepCount
  >;
  readonly context: FamilyAssemblyContext<
    Category,
    Witness,
    Certificate,
    StepCount
  >;
  readonly stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}): {
  readonly transactions: FamilyTransactionPort<Category>;
  readonly adapter: FraudProofFamilyWorkflowAdapter;
} => {
  const arm = definition.adapter;
  if (arm.kind === "linear") {
    const transactions = arm.transactionPort(context);
    return {
      transactions,
      adapter: createLinearFamilyWorkflowAdapter({
        category: definition.category as Category & LinearFamilyCategory,
        l1: context.l1 as FraudProofFamilyL1ObservationPort<
          Category & LinearFamilyCategory
        >,
        transactions,
        stateQueueMutationLeaseCoordinator,
      }),
    };
  }
  const transactions = arm.transactionPort(context);
  return {
    transactions,
    adapter: createCursorFamilyWorkflowAdapter({
      spec: arm.spec,
      l1: context.l1,
      transactions,
      stateQueueMutationLeaseCoordinator,
      ...(arm.refineAction === undefined
        ? {}
        : { refineAction: arm.refineAction }),
    }),
  };
};

/**
 * Assembles the manifest-bound workflow for one family definition. The
 * result is frozen; its `adapter` is the family adapter with the declared
 * prerequisites applied, and its `replayer` is the definition's replayer
 * bound to this assembly's context.
 */
export const assembleManifestBoundFamilyWorkflow = async <
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number = number,
>(
  definition: FamilyDefinition<Category, Witness, Certificate, StepCount>,
  config: ManifestBoundFamilyWorkflowConfig<
    Category,
    Witness,
    Certificate,
    StepCount
  >,
): Promise<ManifestBoundFamilyWorkflow<Category, Certificate, StepCount>> => {
  const stepContractNames = requireDefinition(definition);
  const { category } = definition;
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas:
      definition.stepDatumSchemas as readonly LinearFamilyStepDatumSchema[],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  let certificate: FieldPreimageCertificateBinding | null = null;
  if (definition.fieldPreimageCertificate) {
    if (binding.fieldPreimageCertificate === null) {
      throw new Error(
        `${category} manifest omitted the field-preimage certificate policy`,
      );
    }
    certificate = binding.fieldPreimageCertificate;
  }
  const references = bindReferenceScripts({
    definition,
    stepContractNames,
    binding,
    supplied: config.referenceScripts,
  });
  const observed = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (observed.rawL1 === undefined || observed.publications === undefined) {
    throw new Error(
      `${category} requires authenticated raw L1 and publication authorities`,
    );
  }
  // One port object serves the context, the adapter, the prerequisites and
  // the workflow, so identity checks against `workflow.l1` hold everywhere.
  const l1 = observed as typeof observed & {
    readonly rawL1: NonNullable<typeof observed.rawL1>;
    readonly publications: NonNullable<typeof observed.publications>;
  };
  const context: FamilyAssemblyContext<
    Category,
    Witness,
    Certificate,
    StepCount
  > = Object.freeze({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    references,
    l1,
    certificate: certificate as FamilyAssemblyContext<
      Category,
      Witness,
      Certificate,
      StepCount
    >["certificate"],
    ...(config.replayContext === undefined
      ? {}
      : { replayContext: config.replayContext }),
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const { transactions, adapter: familyAdapterBase } = familyAdapter({
    definition,
    context,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const replayer = definition.replayer(context);
  let adapter = familyAdapterBase;
  const transactionConfirmed = async (input: {
    readonly headerHash: string;
    readonly txHash: string;
  }) => await l1.transactionConfirmed(input);
  for (const requirement of definition.fieldCarriage ?? []) {
    adapter = withFieldCarriagePrerequisite({
      category,
      base: adapter,
      prerequisite: createAuthenticatedFieldCarriagePrerequisitePort({
        category,
        lucid: config.lucid,
        network: binding.network,
        signer: config.signer,
        publications: l1.publications,
        requirementForAction: (input) =>
          requirement.requirementForAction(context, input),
        transactionConfirmed,
      }),
      ...(requirement.rawDatum === undefined
        ? {}
        : { rawDatum: requirement.rawDatum }),
    });
  }
  const { proofChunk } = definition;
  if (proofChunk !== undefined) {
    adapter = withProofChunkPrerequisite({
      category,
      base: adapter,
      prerequisite: createAuthenticatedProofChunkPrerequisitePort({
        category,
        lucid: config.lucid,
        network: binding.network,
        signer: config.signer,
        publications: l1.publications,
        maximumTransactionBytes: binding.cardanoProtocolParameters.maxTxSize,
        proofCborForAction: (input) => proofChunk(context, input),
        transactionConfirmed,
      }),
    });
  }
  const workflow: ManifestBoundFamilyWorkflow<
    Category,
    Certificate,
    StepCount
  > = {
    definition,
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
    replayer,
    ...(config.replayContext === undefined
      ? {}
      : { replayContext: config.replayContext }),
  };
  return Object.freeze({
    ...workflow,
    ...(definition.extend === undefined
      ? {}
      : definition.extend(context, workflow)),
  });
};

/**
 * Runs or resumes an assembled workflow: observes its header, then hands the
 * single-adapter registry, the definition's replayer and the workflow's
 * terminal verifier and release-finality authority to the retained-DA runner.
 */
export const runOrResumeManifestBoundFamilyWorkflow = async <
  Category extends FamilyCategory,
  Certificate extends boolean,
  StepCount extends number = number,
>({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundFamilyWorkflow<
    Category,
    Certificate,
    StepCount
  >;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> =>
  await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: workflow.replayer,
    ...(workflow.replayContext === undefined
      ? {}
      : { replayContext: workflow.replayContext }),
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: [workflow.definition.category],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
