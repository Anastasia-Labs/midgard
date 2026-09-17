import type { UTxO } from "@lucid-evolution/lucid";

/**
 * Runtime inputs for cursor definitions whose transaction ports are built for
 * each invocation. The assembly table exercises binding and adapter wiring;
 * its mocked retained-DA runner never asks these fixtures for proof material.
 */
export const cursorAssemblyRuntimeFixture = ({
  category,
  binding,
  l1,
  config,
}: {
  readonly category: string;
  readonly binding: unknown;
  readonly l1: unknown;
  readonly config: {
    readonly lucid: unknown;
    readonly signer: unknown;
    readonly source: unknown;
    readonly stateQueueMutationLeaseCoordinator: unknown;
    readonly referenceScripts: {
      readonly steps: readonly UTxO[];
      readonly witnesses: unknown;
      readonly fieldPreimageCertificateMint?: UTxO;
    };
    readonly auxiliaryReferenceScripts?: unknown;
  };
}): Readonly<Record<string, unknown>> | undefined => {
  if (
    category !== "executionNativeScriptInvalid" &&
    category !== "resolvedOutputNonCanonical" &&
    category !== "transactionOutputNonCanonical" &&
    category !== "witnessScriptDecoding"
  )
    return undefined;

  const referenceScripts = {
    ...Object.fromEntries(
      config.referenceScripts.steps.map((step, index) => [
        `step${(index + 1).toString().padStart(2, "0")}`,
        step,
      ]),
    ),
    witnesses: config.referenceScripts.witnesses,
    fieldPreimageCertificateMint:
      config.referenceScripts.fieldPreimageCertificateMint,
  };
  const deployment = {
    binding,
    l1,
    ...config,
    references: config.referenceScripts,
    auxiliaryReferences: config.auxiliaryReferenceScripts ?? {},
  };
  const workflow = {
    binding,
    l1,
    deployment,
    lucid: config.lucid,
    signer: config.signer,
    source: config.source,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    config: {
      binding,
      lucid: config.lucid,
      signer: config.signer,
      contracts: {},
      referenceScripts,
    },
    references: {
      ...config.referenceScripts,
      removal: config.auxiliaryReferenceScripts ?? {},
    },
    historicalCheckpointStore: {},
    historicalSource: {},
  };
  return category === "executionNativeScriptInvalid" ||
    category === "resolvedOutputNonCanonical"
    ? { workflow, sources: [] }
    : { workflow };
};
