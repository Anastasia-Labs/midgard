/** A second operator for removal tests. `enrollSuccessor` registers and
 * activates it, `retireOperator` retires the fixture's operator so the
 * scheduler rotates to it, and `commitSuccessorsBy` commits successors of the
 * proved block under its key. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { OperatorLifecycleReferenceScripts } from "./emulator/reference-scripts.js";
import {
  type ProvedDoubleSpendFixture,
  submitSuccessorBlockTx,
  type SuccessorBlockFixture,
} from "./submit-init-emulator-fixtures.js";
import { onboardEmulatorOperator } from "./submit-init-emulator-shared.js";

/** Register and activate an operator whose key sorts after `lowerKey`. */
export const enrollSuccessor = async (
  fixture: ProvedDoubleSpendFixture,
  lowerKey: string,
) => {
  let account = generateEmulatorAccount({ lovelace: 0n });
  while (paymentCredentialOf(account.address).hash <= lowerKey)
    account = generateEmulatorAccount({ lovelace: 0n });
  const keyHash = paymentCredentialOf(account.address).hash;
  const funding = await fixture.funderLucid
    .newTx()
    .pay.ToAddress(account.address, { lovelace: 3_000_000_000n })
    .complete();
  await fixture.funderLucid.awaitTx(
    await (await funding.sign.withWallet().complete()).submit(),
  );
  const lucid = await Lucid(fixture.emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const nodes = await onboardEmulatorOperator({
    lucid,
    contracts: fixture.contracts,
    operatorKeyHash: keyHash,
    registrationSlots: 2,
    awaitActivation: (activationTime) => {
      while (BigInt(fixture.emulator.now()) <= activationTime)
        fixture.emulator.awaitSlot(1);
    },
  });
  return { lucid, keyHash, ...nodes };
};

export type EnrolledOperator = Awaited<ReturnType<typeof enrollSuccessor>>;

/** Voluntarily retire `operatorKeyHash`, submitted by `lucid`. */
export const retireOperator = async (
  fixture: ProvedDoubleSpendFixture,
  operatorKeyHash: string,
  lucid: Awaited<ReturnType<typeof Lucid>>,
) => {
  const { contracts, emulator } = fixture;
  const lifecycle = (
    contracts as typeof contracts & {
      readonly operatorLifecycleReferenceScripts: OperatorLifecycleReferenceScripts;
    }
  ).operatorLifecycleReferenceScripts;
  const refs = fixture.removalReferenceScriptPublications.published;
  const snapshot = await Effect.runPromise(
    SDK.fetchOperatorDirectorySnapshotProgram(lucid, contracts),
  );
  const validFrom = BigInt(emulator.now()) - 1_000n;
  const validTo = BigInt(emulator.now()) + 60_000n;
  const retirement = SDK.deriveRetireOperatorWitnesses({
    snapshot,
    contracts,
    operatorKeyHash,
    validTo,
    schedulerSpendingScriptRef: refs.schedulerSpend,
  });
  const { tx } = await Effect.runPromise(
    SDK.buildUnsignedRetireOperatorTxProgram({
      lucid,
      contracts,
      operatorKeyHash,
      activeOperatorScriptRefs: lifecycle.active,
      retiredOperatorScriptRefs: [
        {
          name: "retired-operators spending",
          utxo: refs.retiredOperatorsSpend,
        },
        { name: "retired-operators minting", utxo: refs.retiredOperatorsMint },
      ],
      hubOracleRefInput: snapshot.hubOracle.utxo,
      ...retirement,
      retiredNodeLovelace: retirement.activeNode.utxo.assets.lovelace!,
      mode: "voluntary",
      validFrom,
      validTo,
    }),
  );
  await lucid.awaitTx(await (await tx.sign.withWallet().complete()).submit());
  return retirement;
};

/** Reads the live scheduler UTxO of the fixture's deployment. */
export const liveSchedulerOf = (fixture: ProvedDoubleSpendFixture) => {
  const { contracts, funderLucid } = fixture;
  const schedulerUnit = toUnit(
    contracts.scheduler.policyId,
    SDK.SCHEDULER_ASSET_NAME,
  );
  return async (): Promise<UTxO> =>
    (
      await funderLucid.utxosAtWithUnit(
        contracts.scheduler.spendingScriptAddress,
        schedulerUnit,
      )
    )[0]!;
};

/** Commit `count` successors of the proved block, each signed and bonded by
 * `operator`, which the scheduler must already name. */
export const commitSuccessorsBy = async (
  fixture: ProvedDoubleSpendFixture,
  operator: EnrolledOperator,
  count: number,
): Promise<SuccessorBlockFixture[]> => {
  const { contracts, emulator, funderLucid } = fixture;
  const snapshot = await Effect.runPromise(
    SDK.fetchOperatorDirectorySnapshotProgram(funderLucid, contracts),
  );
  const liveScheduler = liveSchedulerOf(fixture);
  let predecessor = fixture.fraudulentHeader;
  let anchorBlockUnit = fixture.setup.stateQueueBlockUnit;
  const successors: SuccessorBlockFixture[] = [];
  for (let index = 0; index < count; index += 1) {
    const endTime = BigInt(emulator.now() + 60_000) - 1n;
    const header: SDK.Header = {
      ...fixture.fraudulentHeader,
      operatorVkey: operator.keyHash,
      prevHeaderHash:
        index === 0
          ? fixture.headerHash
          : successors[index - 1]!.successorHeaderHash,
      prevUtxosRoot: predecessor.utxosRoot,
      startTime: predecessor.endTime,
      endTime:
        endTime > predecessor.endTime ? endTime : predecessor.endTime + 60_000n,
    };
    const [activeOperatorNode] = await funderLucid.utxosAtWithUnit(
      contracts.activeOperators.spendingScriptAddress,
      operator.activeNodeUnit,
    );
    if (activeOperatorNode === undefined)
      throw new Error("rotated active node missing");
    const successor = await submitSuccessorBlockTx({
      lucid: operator.lucid,
      emulator,
      contracts,
      anchorBlockUnit,
      header,
      hubOracle: snapshot.hubOracle.utxo,
      scheduler: await liveScheduler(),
      activeOperatorNode,
      activeOperatorNodeUnit: operator.activeNodeUnit,
      validFrom: BigInt(emulator.now()) - 1_000n,
    });
    successors.push(successor);
    predecessor = successor.header;
    anchorBlockUnit = successor.successorBlockUnit;
  }
  return successors;
};
