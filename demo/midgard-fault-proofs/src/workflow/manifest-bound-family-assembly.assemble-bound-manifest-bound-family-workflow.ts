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
  FamilyDeploymentContext,
  FamilyRuntimeArguments,
  FaultProofWitnessRole,
  FieldPreimageCertificateBinding,
  LinearFamilyStepDatumSchema,
  ManifestBoundFamilyWorkflow,
  ManifestBoundFamilyWorkflowConfig,
} from "./family-definition.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyL1ObservationPort,
} from "./family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "./field-carriage-prerequisite.js";
import {
  bindReferenceScripts,
  boundDefinitions,
  familyAdapter,
  requireDefinition,
} from "./manifest-bound-family-assembly.bind-reference-scripts.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  withProofChunkPrerequisite,
} from "./proof-chunk-prerequisite.js";

/**
 * Assembles the manifest-bound workflow for one family definition. The
 * result is frozen; its `adapter` is the family adapter with the declared
 * prerequisites applied, and its `replayer` is the definition's replayer
 * bound to this assembly's context.
 */
export const bindManifestBoundFamilyWorkflow = async <
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number = number,
  Runtime extends object = Readonly<Record<never, never>>,
>(
  definition: FamilyDefinition<
    Category,
    Witness,
    Certificate,
    StepCount,
    Runtime
  >,
  config: ManifestBoundFamilyWorkflowConfig<
    Category,
    Witness,
    Certificate,
    StepCount
  >,
): Promise<
  FamilyDeploymentContext<Category, Witness, Certificate, StepCount>
> => {
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
  const auxiliaryReferences = Object.freeze(
    Object.fromEntries(
      Object.entries(definition.auxiliaryReferenceScripts ?? {}).map(
        ([role, contractName]) => {
          const utxo = config.auxiliaryReferenceScripts?.[role];
          if (utxo === undefined) {
            throw new Error(
              `${category} workflow config omitted auxiliary reference script ${role}`,
            );
          }
          return [
            role,
            requireManifestBoundReferenceScriptUtxo({
              binding,
              contractName,
              utxo,
            }),
          ];
        },
      ),
    ),
  );
  const observed = createFraudProofFamilyL1ObservationPort({
    l1: config.l1Source,
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
  const deployment = Object.freeze({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    references,
    auxiliaryReferences,
    l1,
    l1Source: config.l1Source,
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
  boundDefinitions.set(deployment, definition);
  return deployment;
};

/** Assemble one run over an already authenticated deployment, without rebinding L1. */
export const assembleBoundManifestBoundFamilyWorkflow = <
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number = number,
  Runtime extends object = Readonly<Record<never, never>>,
>(
  definition: FamilyDefinition<
    Category,
    Witness,
    Certificate,
    StepCount,
    Runtime
  >,
  deployment: FamilyDeploymentContext<
    Category,
    Witness,
    Certificate,
    StepCount
  >,
  ...runtime: FamilyRuntimeArguments<Runtime>
): ManifestBoundFamilyWorkflow<Category, Certificate, StepCount, Runtime> => {
  requireDefinition(definition);
  const { category } = definition;
  if (boundDefinitions.get(deployment) !== definition) {
    throw new Error(`${category} deployment was not bound for this definition`);
  }
  const { binding, l1 } = deployment;
  if (binding.definition.category !== category || l1.category !== category) {
    throw new Error(`${category} bound deployment changed category`);
  }
  const transactionConfirmed = async (input: {
    readonly headerHash: string;
    readonly txHash: string;
  }) => await l1.transactionConfirmed(input);
  // These ports capture lazy requirement callbacks. Sharing their handles with
  // transaction ports ensures capture resolves the same authenticated evidence
  // that the adapter's prerequisites published.
  const fieldCarriagePrerequisites = Object.freeze(
    (definition.fieldCarriage ?? []).map((requirement) =>
      createAuthenticatedFieldCarriagePrerequisitePort({
        category,
        lucid: deployment.lucid,
        network: binding.network,
        signer: deployment.signer,
        publications: l1.publications,
        requirementForAction: (input) =>
          requirement.requirementForAction(context, input),
        transactionConfirmed,
      }),
    ),
  );
  const context: FamilyAssemblyContext<
    Category,
    Witness,
    Certificate,
    StepCount,
    Runtime
  > = Object.freeze({
    ...deployment,
    runtime: (runtime[0] ?? {}) as Runtime,
    fieldCarriagePrerequisites,
  });
  const { transactions, adapter: familyAdapterBase } = familyAdapter({
    definition,
    context,
    stateQueueMutationLeaseCoordinator:
      deployment.stateQueueMutationLeaseCoordinator,
  });
  const replayer = definition.replayer(context);
  let adapter = familyAdapterBase;
  for (const [index, requirement] of (
    definition.fieldCarriage ?? []
  ).entries()) {
    adapter = withFieldCarriagePrerequisite({
      category,
      base: adapter,
      prerequisite: fieldCarriagePrerequisites[index]!,
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
        lucid: deployment.lucid,
        network: binding.network,
        signer: deployment.signer,
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
    StepCount,
    Runtime
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
    ...(deployment.replayContext === undefined
      ? {}
      : { replayContext: deployment.replayContext }),
  };
  return Object.freeze({
    ...workflow,
    ...(definition.extend === undefined
      ? {}
      : definition.extend(context, workflow)),
  });
};

/** Bind a deployment and assemble its family workflow in one operation. */
export const assembleManifestBoundFamilyWorkflow = async <
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number = number,
  Runtime extends object = Readonly<Record<never, never>>,
>(
  definition: FamilyDefinition<
    Category,
    Witness,
    Certificate,
    StepCount,
    Runtime
  >,
  config: ManifestBoundFamilyWorkflowConfig<
    Category,
    Witness,
    Certificate,
    StepCount
  >,
  ...runtime: FamilyRuntimeArguments<Runtime>
): Promise<
  ManifestBoundFamilyWorkflow<Category, Certificate, StepCount, Runtime>
> =>
  assembleBoundManifestBoundFamilyWorkflow(
    definition,
    await bindManifestBoundFamilyWorkflow(definition, config),
    ...runtime,
  );
