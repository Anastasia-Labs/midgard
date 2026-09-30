import {
  type FraudProofCatalogueDeploymentInfo,
  Header,
  type MidgardValidators,
  SCHEDULER_ASSET_NAME,
} from "@al-ft/midgard-sdk";
import { Emulator, Lucid, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  resolveProverSigner,
  type StateQueueMutationLeaseCoordinator,
} from "../../src/index.js";
import { submitInit, submitStep04 } from "./legacy-submit-emulator.js";
import { buildTransactionInclusionFixture } from "./submit-init-emulator-fixtures.build-transaction-inclusion-fixture.js";
import { type SuccessorBlockFixture } from "./submit-init-emulator-fixtures.submit-successor-block-tx.js";
import {
  type Blueprint,
  buildRemovalDeploymentInfo,
  type CompleteSignedTransactionMeasurement,
  publishFaultProofWitnessReferenceScripts,
  publishFraudProofChainReferenceScripts,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export type ProvedDoubleSpendFixture = {
  readonly emulator: Emulator;
  readonly realBlueprint: Blueprint;
  readonly funderLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverSigner: ReturnType<typeof resolveProverSigner>;
  readonly contracts: MidgardValidators;
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly transactionInclusion: Awaited<
    ReturnType<typeof buildTransactionInclusionFixture>
  >;
  readonly fraudulentHeader: Header;
  readonly headerHash: string;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly successors: readonly SuccessorBlockFixture[];
  readonly deploymentInfo: ReturnType<typeof buildRemovalDeploymentInfo>;
  readonly removalReferenceScriptPublications: Awaited<
    ReturnType<typeof publishRemovalReferenceScripts>
  >;
  readonly fraudulentBlockOutRef: string;
  readonly submitInitResult: Awaited<ReturnType<typeof submitInit>>;
  readonly submitInitMeasurement: CompleteSignedTransactionMeasurement;
  readonly step04Result: Awaited<ReturnType<typeof submitStep04>>;
  readonly step04Measurement: CompleteSignedTransactionMeasurement;
  readonly doubleSpendStepReferenceScripts: Awaited<
    ReturnType<typeof publishFraudProofChainReferenceScripts>
  >;
  readonly witnessReferenceScripts: Awaited<
    ReturnType<typeof publishFaultProofWitnessReferenceScripts>
  >;
  readonly fraudProofUtxo: UTxO;
  readonly proverPaymentKeyHash: string;
};

export type RemovalEvent =
  | { readonly kind: "stateQueue.utxosAt"; readonly call: number }
  | { readonly kind: "scheduler.utxosAtWithUnit"; readonly call: number }
  | { readonly kind: "awaitTx"; readonly txHash: string }
  | { readonly kind: "lease.acquire" }
  | { readonly kind: "lease.renew"; readonly call: number }
  | { readonly kind: "lease.release" }
  | { readonly kind: "lease.fail"; readonly error: string };

export const eventIndexes = (
  events: readonly RemovalEvent[],
  kind: RemovalEvent["kind"],
): number[] =>
  events.flatMap((event, index) => (event.kind === kind ? [index] : []));

export const createRecordingLeaseCoordinator = (
  events: RemovalEvent[],
): StateQueueMutationLeaseCoordinator => {
  let renewCalls = 0;
  return {
    acquire: async () => {
      events.push({ kind: "lease.acquire" });
      return {
        token: "emulator-fault-proof-removal",
        source: "emulator",
        renew: async () => {
          renewCalls += 1;
          events.push({ kind: "lease.renew", call: renewCalls });
        },
        release: async () => {
          events.push({ kind: "lease.release" });
        },
        fail: async (error: string) => {
          events.push({ kind: "lease.fail", error });
        },
      };
    },
  };
};

export const instrumentLucidForRemoval = ({
  lucid,
  contracts,
  events,
  failStateQueueUtxosAtCall,
  failSchedulerUtxosAtWithUnitCall,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly events: RemovalEvent[];
  readonly failStateQueueUtxosAtCall?: number;
  readonly failSchedulerUtxosAtWithUnitCall?: number;
}): Awaited<ReturnType<typeof Lucid>> => {
  let stateQueueUtxosAtCalls = 0;
  let schedulerUtxosAtWithUnitCalls = 0;
  const schedulerUnit = toUnit(
    contracts.scheduler.policyId,
    SCHEDULER_ASSET_NAME,
  );
  return new Proxy(lucid, {
    get(target, property, receiver) {
      if (property === "utxosAt") {
        return async (address: string, ...rest: unknown[]) => {
          if (address === contracts.stateQueue.spendingScriptAddress) {
            stateQueueUtxosAtCalls += 1;
            events.push({
              kind: "stateQueue.utxosAt",
              call: stateQueueUtxosAtCalls,
            });
            if (stateQueueUtxosAtCalls === failStateQueueUtxosAtCall) {
              throw new Error("instrumented state-queue topology load failure");
            }
          }
          return await target.utxosAt(address, ...(rest as []));
        };
      }
      if (property === "utxosAtWithUnit") {
        return async (address: string, unit: string, ...rest: unknown[]) => {
          if (
            address === contracts.scheduler.spendingScriptAddress &&
            unit === schedulerUnit
          ) {
            schedulerUtxosAtWithUnitCalls += 1;
            events.push({
              kind: "scheduler.utxosAtWithUnit",
              call: schedulerUtxosAtWithUnitCalls,
            });
            if (
              schedulerUtxosAtWithUnitCalls === failSchedulerUtxosAtWithUnitCall
            ) {
              throw new Error("instrumented scheduler lookup failure");
            }
          }
          return await target.utxosAtWithUnit(address, unit, ...(rest as []));
        };
      }
      if (property === "awaitTx") {
        return async (txHash: string, ...rest: unknown[]) => {
          events.push({ kind: "awaitTx", txHash });
          return await target.awaitTx(txHash, ...(rest as []));
        };
      }
      const value = Reflect.get(target, property, receiver);
      return typeof value === "function" ? value.bind(target) : value;
    },
  });
};

/**
 * Successor fixtures are often assembled after publishing reference scripts,
 * each of which advances the emulator. Keep the successor monotonic with its
 * predecessor without allowing those preliminary transactions to leave the
 * commit validity interval behind the live emulator clock.
 */
export const emulatorSuccessorHeaderStart = ({
  predecessorEndTime,
  emulator,
}: {
  readonly predecessorEndTime: bigint;
  readonly emulator: Emulator;
}): number => {
  const predecessorEnd = Number(predecessorEndTime);
  const targetStart = Math.max(predecessorEnd, emulator.now());
  expect(
    targetStart,
    "successor fixture predecessor window must remain live to preserve exact header contiguity",
  ).toBe(predecessorEnd);
  return targetStart;
};

/** Setup and successor submissions advance the emulator by roughly 20s. */
export const EMULATOR_HEADER_CLOCK_HEADROOM_MS = 60_000;
