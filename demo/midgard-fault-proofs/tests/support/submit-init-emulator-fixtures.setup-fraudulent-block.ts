import { outRefLabel } from "@al-ft/midgard-core";
import {
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import { Data, Emulator, Lucid, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  type FraudProofPreSubmitBoundary,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../../src/index.js";
import { countedTransactionsRoot } from "./submit-init-emulator-fixtures.build-non-existent-input-fixture.js";
import { expectStateQueueHeaderOrder } from "./submit-init-emulator-fixtures.expect-state-queue-header-order.js";
import { type ProvedDoubleSpendFixture } from "./submit-init-emulator-fixtures.instrument-lucid-for-removal.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeHeader,
  network,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export const submitRemovalForFixture = async (
  fixture: ProvedDoubleSpendFixture,
  options: {
    readonly lucid?: Awaited<ReturnType<typeof Lucid>>;
    readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
    readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  } = {},
) => {
  const removeNow = BigInt(fixture.emulator.now());
  return await submitRemoveFraudulentBlock({
    lucid: options.lucid ?? fixture.proverLucid,
    blueprint: fixture.realBlueprint,
    deploymentInfo: fixture.deploymentInfo,
    network,
    signer: fixture.proverSigner,
    fraudulentHeaderHash: fixture.headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
    validTo: removeNow + 300_000n,
    ...(options.stateQueueMutationLeaseCoordinator === undefined
      ? {}
      : {
          stateQueueMutationLeaseCoordinator:
            options.stateQueueMutationLeaseCoordinator,
        }),
    ...(options.preSubmitBoundary === undefined
      ? {}
      : { preSubmitBoundary: options.preSubmitBoundary }),
  });
};

export const expectRemovedFraudProofState = async (
  fixture: ProvedDoubleSpendFixture,
) => {
  await expectStateQueueHeaderOrder({
    lucid: fixture.funderLucid,
    contracts: fixture.contracts,
    expectedHeaderHashes: [],
  });
  await expect(
    fixture.funderLucid.utxosAtWithUnit(
      fixture.contracts.stateQueue.spendingScriptAddress,
      fixture.setup.stateQueueBlockUnit,
    ),
  ).resolves.toHaveLength(0);
  for (const successor of fixture.successors) {
    await expect(
      fixture.funderLucid.utxosAtWithUnit(
        fixture.contracts.stateQueue.spendingScriptAddress,
        successor.successorBlockUnit,
      ),
    ).resolves.toHaveLength(0);
  }
  await expect(
    fixture.funderLucid.utxosAtWithUnit(
      fixture.contracts.activeOperators.spendingScriptAddress,
      fixture.setup.activeOperatorNodeUnit,
    ),
  ).resolves.toHaveLength(0);
  const [finalSchedulerUtxo] = await fixture.funderLucid.utxosAtWithUnit(
    fixture.contracts.scheduler.spendingScriptAddress,
    toUnit(fixture.contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
  );
  if (finalSchedulerUtxo === undefined) {
    throw new Error("Remove transaction did not preserve the scheduler");
  }
  expect(Data.from(finalSchedulerUtxo.datum!, SchedulerDatum)).toBe(
    "NoActiveOperators",
  );
  const [finalRootUtxo] = await fixture.funderLucid.utxosAtWithUnit(
    fixture.contracts.stateQueue.spendingScriptAddress,
    fixture.setup.stateQueueRootUnit,
  );
  if (finalRootUtxo === undefined) {
    throw new Error("Remove transaction did not preserve the state-queue root");
  }
  const finalRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(finalRootUtxo, fixture.contracts.stateQueue.policyId),
  );
  expect(finalRoot.datum.next).toBe("Empty");
  const retainedFraudProof = await expectSingleUtxoWithUnit(
    fixture.proverLucid,
    fixture.step04Result.fraudProofAddress,
    fixture.step04Result.fraudProofUnit,
  );
  expect(outRefLabel(retainedFraudProof)).toBe(
    outRefLabel(fixture.fraudProofUtxo),
  );
  expect(retainedFraudProof.assets[fixture.step04Result.fraudProofUnit]).toBe(
    1n,
  );
};

/**
 * Publish the fraudulent block the family suites then prove against: sample
 * the emulator clock one slot before the aligned boundary, commit the
 * fixture's raw transactions root under the counted-root domain, and submit
 * the setup transaction. Every caller passed the same code; only the fixture
 * type differed, so the parameter is structural.
 */
export const setupFraudulentBlock = async ({
  funderLucid,
  emulator,
  contracts,
  catalogue,
  fixture,
}: {
  readonly funderLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly emulator: Emulator;
  readonly contracts: Awaited<
    ReturnType<typeof buildMinimalFaultProofContracts>
  >;
  readonly catalogue: Awaited<ReturnType<typeof buildCatalogueDeploymentInfo>>;
  readonly fixture: {
    readonly transactionsRoot: string;
    readonly l2TransactionCount: bigint;
    readonly prevUtxosRoot?: string;
    readonly utxosRoot?: string;
    /**
     * Optional predecessor duration for journeys whose setup transactions
     * advance the emulator beyond the one-second default header window.
     */
    readonly headerDurationMs?: number;
  };
}) => {
  const funderKeyHash = await funderPaymentKeyHash(funderLucid);
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const baseHeader = makeHeader(
    funderKeyHash,
    headerStartTime,
    await countedTransactionsRoot(
      fixture.transactionsRoot,
      fixture.l2TransactionCount,
    ),
    fixture.l2TransactionCount,
  );
  const fraudulentHeader = {
    ...baseHeader,
    ...(fixture.headerDurationMs === undefined
      ? {}
      : {
          endTime: baseHeader.startTime + BigInt(fixture.headerDurationMs),
        }),
    ...(fixture.prevUtxosRoot === undefined
      ? {}
      : { prevUtxosRoot: fixture.prevUtxosRoot }),
    ...(fixture.utxosRoot === undefined
      ? {}
      : { utxosRoot: fixture.utxosRoot }),
  };
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo: (await funderLucid.wallet().getUtxos())[0]!,
    catalogue,
    header: fraudulentHeader,
  });
  return { ...setup, header: fraudulentHeader };
};
