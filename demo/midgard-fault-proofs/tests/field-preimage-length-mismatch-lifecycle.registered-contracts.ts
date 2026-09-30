import {
  buildFaultProofContracts,
  buildFieldPreimageLengthMismatchFaultProofContracts,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { expect } from "vitest";

import type { ManifestBoundFieldPreimageLengthConfig } from "../src/field-preimage-length-mismatch/config.js";
import { network } from "./support/emulator/blueprints.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import type { CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import { expectRegisteredChainParity } from "./support/emulator/registered-chain.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";

const measuredFit = createMeasuredFitRecorder(
  "field-preimage-length-mismatch",
  "lifecycle",
  "32,768-byte certified field preimage, inline carriage, both proof directions and cancellation",
);

export const REASON = "FieldPreimageLengthMismatch";

export const WORKFLOW =
  "midgard-field-preimage-length-mismatch-workflow-v1" as const;

export const coverage = createLifecycleCoverageRecorder();

export const fitRows: {
  readonly stage: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
}[] = [];

export const emitFit = (
  stage: string,
  measurement: CompleteSignedTransactionMeasurement,
): void => {
  fitRows.push({ stage, measurement });
  measuredFit.record(
    stage,
    measurement,
    measurement.executionMemory === 0n ? "publication" : "lifecycle",
  );
  expect(measurement.l1ByteMargin).toBeGreaterThan(0);
  expect(measurement.executionMemory).toBeLessThanOrEqual(16_500_000n);
  expect(measurement.executionSteps).toBeLessThanOrEqual(10_000_000_000n);
  console.info(
    `[field-preimage-length-fit] ${JSON.stringify({
      stage,
      signedBytes: measurement.completeSignedBytes,
      byteMargin: measurement.l1ByteMargin,
      memory: measurement.executionMemory.toString(),
      memoryMargin: (16_500_000n - measurement.executionMemory).toString(),
      cpu: measurement.executionSteps.toString(),
      cpuMargin: (10_000_000_000n - measurement.executionSteps).toString(),
    })}`,
  );
};

/**
 * A refusal raised by local UPLC evaluation of the real applied script, as
 * opposed to one of the family's own pre-construction guards. The submitters
 * prefix every off-chain refusal with the family label; a script failure
 * surfaces from Lucid's evaluator without it.
 */
export const expectOnChainRefusal = async (
  attempt: () => Promise<unknown>,
  label: string,
): Promise<void> => {
  let message: string | undefined;
  try {
    await attempt();
  } catch (error) {
    message = error instanceof Error ? error.message : String(error);
  }
  if (message === undefined) {
    throw new Error(
      `${label}: the applied script accepted a mutated transaction`,
    );
  }
  expect(message).not.toMatch(/field-preimage-length-mismatch:/u);
  expect(message).not.toMatch(/forced leaf differs/u);
  expect(message).toMatch(/eval|script|uplc|redeemer|budget|fail/iu);
  console.info(
    `[field-preimage-length-refusal] ${label}: ${message.slice(0, 200)}`,
  );
};

export const successorSwappedConfig = (
  config: ManifestBoundFieldPreimageLengthConfig,
): ManifestBoundFieldPreimageLengthConfig => {
  const chain = config.contracts.fieldPreimageLengthMismatch;
  return {
    ...config,
    contracts: {
      ...config.contracts,
      fieldPreimageLengthMismatch: {
        ...chain,
        acceptedStep02: chain.forcedStep02,
      },
    },
  };
};

export const flipFirstByte = (bytes: Uint8Array): Buffer => {
  const flipped = Buffer.from(bytes);
  flipped[0] = (flipped[0]! ^ 0x01) & 0xff;
  return flipped;
};

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family SDK builder and the central SDK
 * chain builder are the two application paths that must reproduce it step
 * for step before the suite drives it.
 */
export const registeredContracts = async (harness: Harness) => {
  const registered =
    harness.contracts.fraudProofContracts.fieldPreimageLengthMismatch;
  const category = harness.catalogue.categories.fieldPreimageLengthMismatch;
  if (registered === undefined || category === undefined) {
    throw new Error("field-preimage-length deployment is absent");
  }
  const params = () => ({
    eventHistoryBounds: {
      inlineLimitBytes: 512n,
      maxPayloadBytes: 5000n,
      maxPayloadNodes: 512n,
    },
    blueprint: parseFaultProofBlueprint(structuredClone(harness.realBlueprint)),
    network,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    fraudProofCataloguePolicyId: harness.contracts.fraudProofCatalogue.policyId,
    referenceScriptAuthPolicyId: harness.contracts.referenceScriptAuth.policyId,
  });
  const family = await Effect.runPromise(
    buildFieldPreimageLengthMismatchFaultProofContracts(params()),
  );
  expectRegisteredChainParity({
    registered,
    applied: family.fieldPreimageLengthMismatch.steps,
    category,
  });
  expect(
    family.fieldPreimageLengthMismatch.acceptedStep02.spendingScriptHash,
  ).toBe(registered.steps[1].spendingScriptHash);
  expect(
    family.fieldPreimageLengthMismatch.forcedStep02.spendingScriptHash,
  ).toBe(registered.steps[2].spendingScriptHash);
  expect(family.fieldPreimageCertificate.policyId).toBe(
    harness.contracts.fieldPreimageCertificate.policyId,
  );
  const central = await Effect.runPromise(buildFaultProofContracts(params()));
  expectRegisteredChainParity({
    registered,
    applied: central.fieldPreimageLengthMismatch.steps,
    category,
  });
  expect(category.categoryId).toBe("00000020");
  return { chain: registered, category };
};

export type SetupOptions = Readonly<{
  forced?: boolean;
  acceptedPreimageBytes?: number;
  /** Commit the honest length vector: the accepted block is not at fault. */
  honestAccepted?: boolean;
  /**
   * A forced leaf the family opens straight from the retained root entries:
   * `verdict` is the operator's, and `mismatch` overstates field 0 by one
   * byte in the committed length vector. Reconstruction refuses such a
   * payload, so these leaves never come out of `reconstructDaPayload`.
   */
  forcedLeaf?: {
    readonly verdict: "rejected" | "valid";
    readonly mismatch: boolean;
  };
}>;
