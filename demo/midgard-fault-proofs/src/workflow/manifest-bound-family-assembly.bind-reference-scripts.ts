import type { UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { createCursorFamilyWorkflowAdapter } from "./cursor-family-adapter.js";
import { requireManifestBoundReferenceScriptUtxo } from "./deployment-manifest-binding.js";
import type {
  FamilyAssemblyContext,
  FamilyCategory,
  FamilyDefinition,
  FamilyReferenceScripts,
  FamilyTransactionPort,
  FaultProofWitnessRole,
} from "./family-definition.js";
import {
  familyStepContractNames,
  LINEAR_FAMILY_DEFINITION_VERSION,
} from "./family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "./family-l1-observation.js";
import { createLinearFamilyWorkflowAdapter } from "./linear-family-adapter.js";
import type { LinearFamilyCategory } from "./linear-family-spec.js";
import { type FraudProofFamilyWorkflowAdapter } from "./orchestrator.js";

// A bound context is valid only for the definition whose references it checked.
// Structural copies and same-category definitions cannot reuse that authority.
export const boundDefinitions = new WeakMap<object, object>();

export const requireDefinition = <
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number,
  Runtime extends object,
>(
  definition: FamilyDefinition<
    Category,
    Witness,
    Certificate,
    StepCount,
    Runtime
  >,
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
    if (
      adapter.refineAction !== undefined &&
      adapter.createRefineAction !== undefined
    ) {
      throw new Error(
        `${definition.category} definition declares two action refiners`,
      );
    }
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

export const bindReferenceScripts = <
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number,
  Runtime extends object,
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
    StepCount,
    Runtime
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
export const familyAdapter = <
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number,
  Runtime extends object,
>({
  definition,
  context,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly definition: FamilyDefinition<
    Category,
    Witness,
    Certificate,
    StepCount,
    Runtime
  >;
  readonly context: FamilyAssemblyContext<
    Category,
    Witness,
    Certificate,
    StepCount,
    Runtime
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
      transactions: transactions as FamilyTransactionPort<Category>,
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
  const refineAction = arm.createRefineAction?.(context) ?? arm.refineAction;
  return {
    transactions: transactions as FamilyTransactionPort<Category>,
    adapter: createCursorFamilyWorkflowAdapter({
      spec: arm.spec,
      l1: context.l1,
      transactions,
      stateQueueMutationLeaseCoordinator,
      ...(refineAction === undefined ? {} : { refineAction }),
    }),
  };
};
