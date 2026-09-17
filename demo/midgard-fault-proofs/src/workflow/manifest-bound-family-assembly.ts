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
 *   4. bind every reference script (spec step names, then declared witness
 *      roles, then the certificate mint) against the finalized manifest;
 *   5. open the lazy L1 observation port and require its raw-L1 and
 *      publication authorities;
 *   6. ask the family for its transaction port and replayer;
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

import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding.js";
import type {
  FaultProofWitnessRole,
  FieldPreimageCertificateBinding,
  LinearFamilyAssemblyContext,
  LinearFamilyDefinition,
  LinearFamilyReferenceScripts,
  LinearFamilyStepDatumSchema,
  ManifestBoundLinearFamilyWorkflow,
  ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import { LINEAR_FAMILY_DEFINITION_VERSION } from "./family-definition.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  observeFraudProofWorkflowHeader,
} from "./family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "./field-carriage-prerequisite.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";
import { createLinearFamilyWorkflowAdapter } from "./linear-family-adapter.js";
import {
  type LinearFamilyCategory,
  linearFamilySpec,
} from "./linear-family-spec.js";
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
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
>(
  definition: LinearFamilyDefinition<Category, Witness, Certificate>,
): void => {
  if (
    definition.definitionVersion !== LINEAR_FAMILY_DEFINITION_VERSION ||
    !Object.isFrozen(definition)
  ) {
    throw new Error(
      `${definition.category} family definition changed identity`,
    );
  }
  const spec = linearFamilySpec(definition.category);
  const schemas = definition.stepDatumSchemas as readonly unknown[];
  if (schemas.length !== spec.steps.length) {
    throw new Error(
      `${definition.category} definition declares ${schemas.length.toString()} step datum schemas for a ${spec.steps.length.toString()}-step spec`,
    );
  }
  if (
    new Set(definition.witnessRoles).size !== definition.witnessRoles.length
  ) {
    throw new Error(
      `${definition.category} definition repeats a witness reference role`,
    );
  }
};

const bindReferenceScripts = <
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
>({
  definition,
  binding,
  supplied,
}: {
  readonly definition: LinearFamilyDefinition<Category, Witness, Certificate>;
  readonly binding: Parameters<
    typeof requireManifestBoundReferenceScriptUtxo
  >[0]["binding"];
  readonly supplied: LinearFamilyReferenceScripts<
    Category,
    Witness,
    Certificate
  >;
}): LinearFamilyReferenceScripts<Category, Witness, Certificate> => {
  const { category } = definition;
  const spec = linearFamilySpec(category);
  const suppliedSteps = supplied.steps as readonly UTxO[];
  if (suppliedSteps.length !== spec.steps.length) {
    throw new Error(
      `${category} workflow config supplied ${suppliedSteps.length.toString()} step reference scripts for a ${spec.steps.length.toString()}-step spec`,
    );
  }
  const steps = spec.steps.map((step, index) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: step.manifestContractName,
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
  }) as unknown as LinearFamilyReferenceScripts<Category, Witness, Certificate>;
};

/**
 * Assembles the manifest-bound workflow for one family definition. The
 * result is frozen; its `adapter` is the family adapter with the declared
 * prerequisites applied, and its `replayer` is the definition's replayer
 * bound to this assembly's context.
 */
export const assembleManifestBoundFamilyWorkflow = async <
  Category extends LinearFamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
>(
  definition: LinearFamilyDefinition<Category, Witness, Certificate>,
  config: ManifestBoundLinearFamilyWorkflowConfig<
    Category,
    Witness,
    Certificate
  >,
): Promise<ManifestBoundLinearFamilyWorkflow<Category, Certificate>> => {
  requireDefinition(definition);
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
  const context: LinearFamilyAssemblyContext<Category, Witness, Certificate> =
    Object.freeze({
      binding,
      lucid: config.lucid,
      signer: config.signer,
      references,
      l1,
      certificate: certificate as LinearFamilyAssemblyContext<
        Category,
        Witness,
        Certificate
      >["certificate"],
      ...(config.replayContext === undefined
        ? {}
        : { replayContext: config.replayContext }),
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
    });
  if (definition.adapter.kind !== "linear") {
    throw new Error(
      `${category} cursor adapter arm is not assembled by this module yet`,
    );
  }
  const transactions = definition.adapter.transactionPort(context);
  const replayer = definition.replayer(context);
  let adapter: FraudProofFamilyWorkflowAdapter =
    createLinearFamilyWorkflowAdapter({
      category,
      l1,
      transactions,
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
    });
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
  const workflow: ManifestBoundLinearFamilyWorkflow<Category, Certificate> = {
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
  Category extends LinearFamilyCategory,
  Certificate extends boolean,
>({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundLinearFamilyWorkflow<Category, Certificate>;
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
