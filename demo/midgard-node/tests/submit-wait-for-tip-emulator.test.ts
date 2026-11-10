/**
 * Time-gated node transactions against an L1 whose observed ledger tip
 * moves in block-sized jumps while the wall clock runs on. The submit check
 * judges a lower bound against that tip, so a transaction whose lower bound
 * is set from the wall clock, or from a protocol time the tip has not
 * reached, is not admissible there.
 */
import { SUBMIT_SLOT_VALIDITY_BUFFER } from "@al-ft/midgard-core/ogmios-slot";
import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, Emulator, Lucid } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  submitAbsorbAfterProtectionProgram,
  submitInitializePayoutAfterProtectionProgram,
} from "../src/commands/reserve-payout.js";
import { openL1Access } from "../src/l1-access.js";
import { openPlan } from "../src/services/intent-journal.js";
import {
  L1LedgerBehindError,
  ledgerSubmitSlotSnapshot,
} from "../src/services/l1-ledger-tip.js";
import { awaitLedgerTipAtLowerBound } from "../src/transactions/ledger-tip-lower-bound.js";
import {
  recoverOperatorBondProgram,
  retireOperatorProgram,
} from "../src/transactions/operators/exit.js";
import { registerAndActivateOperatorProgram } from "../src/transactions/register-active-operator.js";
import { observeEmulatorTip } from "./helpers/emulator-observed-tip.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import {
  appointFirstSchedulerOperator,
  fetchInactivityDirectorySnapshot,
  initOperatorInactivityFixture,
} from "./helpers/operator-inactivity.js";
import {
  EMULATOR_REQUIRED_BOND_LOVELACE,
  initOperatorLifecycleFixture,
} from "./operator-lifecycle-emulator.build-operator-lifecycle-snapshot.js";
import { registerSecondOperatorBehindActiveFirst } from "./operator-lifecycle-emulator.register-second-operator-behind-active-first.js";
import { makeReserveLifecycleBuilderFixture } from "./reserve-payout-builders.make-reserve-lifecycle-builder-fixture.js";

const economics = {
  requiredBondLovelace: 900_000_000n,
  slashingPenaltyLovelace: 500_000_000n,
  inactivitySlashingPenaltyLovelace: 100_000_000n,
  fraudProverRewardLovelace: 400_000_000n,
};

describe("time-gated operator transactions against a block-sized ledger tip", () => {
  it("builds and submits an activation only once the observed tip reaches its activation time", async () => {
    const fixture = await initOperatorLifecycleFixture();
    const { emulator, referenceScriptsLucid, contracts } = fixture;
    const { secondLucid, activationTime } =
      await registerSecondOperatorBehindActiveFirst(fixture);
    const activationSlot = secondLucid.unixTimeToSlot(Number(activationTime));
    // A block every 40 slots, the last one 5 slots ago.
    const origin = secondLucid.unixTimeToSlot(emulator.now()) - 5;
    const blockSlots = 40;
    const tipAt = (wallSlot: number) =>
      wallSlot - ((wallSlot - origin) % blockSlots);
    const submissions = observeEmulatorTip(secondLucid, emulator, tipAt);

    const activated = await runWithoutFollower(
      registerAndActivateOperatorProgram(
        secondLucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );

    expect(activated.registerTxHash).toBeNull();
    expect(activated.activateTxHash).toHaveLength(64);
    const bounded = submissions.filter(
      (submission) => submission.invalidBeforeSlot !== undefined,
    );
    expect(bounded).toHaveLength(1);
    const [activation] = bounded;
    expect(activation!.invalidBeforeSlot).toBeGreaterThanOrEqual(
      activationSlot,
    );
    expect(activation!.observedTipSlot).toBeGreaterThanOrEqual(
      activation!.invalidBeforeSlot! + SUBMIT_SLOT_VALIDITY_BUFFER,
    );
  }, 420_000);

  it("retires with a lower bound backdated from the observed tip while the wall clock is more than a minute ahead of it", async () => {
    const fixture = await initOperatorInactivityFixture(2);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const idle = fixture.operators.find(
      ({ keyHash }) => keyHash !== appointed.operatorKeyHash,
    )!;
    const idleLucid = await fixture.lucidFor(idle.keyHash);
    // No block for 70 slots: the tip stays where it was while the wall
    // clock runs on.
    const stalledTip = idleLucid.unixTimeToSlot(fixture.emulator.now()) - 70;
    const submissions = observeEmulatorTip(
      idleLucid,
      fixture.emulator,
      () => stalledTip,
    );

    const retirement = await runWithoutFollower(
      retireOperatorProgram(
        idleLucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        { operatorKeyHash: idle.keyHash, mode: "voluntary", economics },
      ),
    );

    expect(retirement.txHash).toHaveLength(64);
    const snapshot = await fetchInactivityDirectorySnapshot(fixture);
    expect(SDK.findNodeByKey(snapshot.retired, idle.keyHash)).toBeDefined();
    expect(submissions).toHaveLength(1);
    expect(submissions[0]!.invalidBeforeSlot).toBeLessThanOrEqual(
      stalledTip - SUBMIT_SLOT_VALIDITY_BUFFER,
    );
  }, 420_000);

  it("recovers a bond whose unlock the tip reaches only after a whole validity window, with the upper bound taken after the wait", async () => {
    const fixture = await initOperatorInactivityFixture(2);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const idle = fixture.operators.find(
      ({ keyHash }) => keyHash !== appointed.operatorKeyHash,
    )!;
    const idleLucid = await fixture.lucidFor(idle.keyHash);
    await runWithoutFollower(
      retireOperatorProgram(
        idleLucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        { operatorKeyHash: idle.keyHash, mode: "voluntary", economics },
      ),
    );
    // A bond hold that ended 10 slots ago. The exit endpoints cannot write
    // one (only a block commitment or a settlement claim does), so the
    // retired node's datum on the ledger is given one.
    const wallSlot = idleLucid.unixTimeToSlot(fixture.emulator.now());
    const unlockSlot = wallSlot - 10;
    const retiredNode = SDK.findNodeByKey(
      (await fetchInactivityDirectorySnapshot(fixture)).retired,
      idle.keyHash,
    )!;
    const entry = Object.values(fixture.emulator.ledger).find(
      ({ utxo }) =>
        utxo.txHash === retiredNode.utxo.txHash &&
        utxo.outputIndex === retiredNode.utxo.outputIndex,
    )!;
    const datum = Data.from(entry.utxo.datum!, SDK.LinkedListDatum);
    if (!("Node" in datum.data)) throw new Error("not an operator node");
    entry.utxo = {
      ...entry.utxo,
      datum: Data.to(
        {
          ...datum,
          data: {
            Node: {
              data: SDK.castRetiredOperatorDatumToData({
                bond_unlock_time: BigInt(idleLucid.slotToUnixTime(unlockSlot)),
              }) as Data,
            },
          },
        },
        SDK.LinkedListDatum,
      ),
    };
    // The last block landed just before the unlock; the next lands 130
    // slots from now, after a whole operator validity window has passed.
    const nextBlockSlot = wallSlot + 130;
    const submissions = observeEmulatorTip(
      idleLucid,
      fixture.emulator,
      (slot) => (slot < nextBlockSlot ? unlockSlot - 1 : slot),
    );

    const recovery = await runWithoutFollower(
      recoverOperatorBondProgram(
        idleLucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        { operatorKeyHash: idle.keyHash },
      ),
    );

    expect(recovery.txHash).toHaveLength(64);
    const snapshot = await fetchInactivityDirectorySnapshot(fixture);
    expect(SDK.findNodeByKey(snapshot.retired, idle.keyHash)).toBeUndefined();
    expect(submissions).toHaveLength(1);
    expect(submissions[0]!.invalidBeforeSlot).toBeGreaterThan(unlockSlot);
    expect(submissions[0]!.observedTipSlot).toBeGreaterThanOrEqual(
      submissions[0]!.invalidBeforeSlot! + SUBMIT_SLOT_VALIDITY_BUFFER,
    );
  }, 420_000);
});

describe("history retirements against a block-sized ledger tip", () => {
  it.each(["absorb", "initialize"] as const)(
    "%s waits for the observed tip to pass a protected Order's bound, then rebuilds and submits",
    async (retirement) => {
      // Protected at the first build by the wall clock as well as by the tip.
      const protectedUntil = BigInt(Date.now() + 12_000);
      const f = await makeReserveLifecycleBuilderFixture({ protectedUntil });
      const common = {
        hubOracleRefInput: f.hubOracleRefInput,
        referenceScripts: f.referenceScripts,
        referenceScriptsAddress: f.referenceScriptsAddress,
        settlementRefInput: f.settlementRefInput,
      };
      // The last block landed one slot before the bound and the next lands
      // 40 slots on, while the wall clock (the emulator's) is already past
      // the bound: a retirement built from the wall clock is ahead of the
      // tip.
      const protectedSlot = f.lucid.unixTimeToSlot(Number(protectedUntil));
      const nextBlockSlot = f.lucid.unixTimeToSlot(f.emulator.now()) + 40;
      expect(nextBlockSlot - 40).toBeGreaterThan(protectedSlot);
      const submissions = observeEmulatorTip(f.lucid, f.emulator, (wallSlot) =>
        wallSlot < nextBlockSlot ? protectedSlot - 1 : wallSlot,
      );

      const txHash = await runWithoutFollower(
        Effect.flatMap(openPlan, (plan) =>
          retirement === "absorb"
            ? submitAbsorbAfterProtectionProgram(
                f.lucid,
                f.contracts,
                {
                  ...common,
                  deposit: f.deposit,
                  membershipProof: f.depositMembershipProof,
                },
                plan,
              )
            : submitInitializePayoutAfterProtectionProgram(
                f.lucid,
                f.contracts,
                {
                  ...common,
                  withdrawal: f.withdrawal,
                  membershipProof: f.withdrawalMembershipProof,
                },
                plan,
              ),
        ),
      );

      expect(txHash).toMatch(/^[0-9a-f]{64}$/);
      const [retired] = await f.lucid.utxosByOutRef([
        retirement === "absorb" ? f.deposit.utxo : f.withdrawal.utxo,
      ]);
      expect(retired).toBeUndefined();
      const bounded = submissions.filter(
        (submission) => submission.invalidBeforeSlot !== undefined,
      );
      expect(bounded).toHaveLength(1);
      expect(bounded[0]!.invalidBeforeSlot).toBeGreaterThanOrEqual(
        protectedSlot,
      );
      expect(bounded[0]!.observedTipSlot).toBeGreaterThanOrEqual(
        bounded[0]!.invalidBeforeSlot! + SUBMIT_SLOT_VALIDITY_BUFFER,
      );
    },
    420_000,
  );
});

/**
 * A node access over an empty emulator chain whose tip is the wall slot, or
 * `tipAt` of it; each tip read first answers with `failures`' next error, if
 * any. A read past the 20th is refused as not transient, so a wait that
 * never ends fails instead. Returns the client and the number of reads.
 */
const flakyTipChain = async (
  failures: Error[],
  tipAt: (wallSlot: number) => number = (wallSlot) => wallSlot,
) => {
  const emulator = new Emulator([]);
  const lucid = await Lucid(emulator, "Custom");
  emulator.awaitSlot(200);
  const slotConfig = lucid.config().slotConfig!;
  const wallSlot = () => lucid.unixTimeToSlot(emulator.now());
  const viewPoint = () =>
    Promise.resolve({ slot: wallSlot(), id: `emulator:${wallSlot()}` });
  let reads = 0;
  openL1Access(
    {
      kind: "node",
      provider: emulator,
      endpoint: "test",
      slotConfig: () => Promise.resolve(slotConfig),
      tipSlot: () => Promise.resolve(wallSlot()),
      viewPoint,
      synchronizedViewPoint: viewPoint,
      submitSlotSnapshot: () => {
        reads += 1;
        if (reads > 20)
          return Promise.reject(new Error("the wait read the tip 20 times"));
        const failure = failures.shift();
        if (failure !== undefined) return Promise.reject(failure);
        return Promise.resolve(
          ledgerSubmitSlotSnapshot({
            slotConfig,
            ledgerTipSlot: tipAt(wallSlot()),
            nowMs: emulator.now(),
            boundMs: 60_000,
          }),
        );
      },
      close: () => Promise.resolve(),
    },
    {
      slotLengthMs: slotConfig.slotLength,
      monotonicNowMs: () => emulator.now(),
    },
  );
  return { emulator, lucid, wallSlot, reads: () => reads };
};

describe("waiting for the observed ledger tip to reach a lower bound", () => {
  it("reads the tip again after a transient read failure and returns once it reaches the bound", async () => {
    const chain = await flakyTipChain([
      new L1ProviderTransientError("transport", "unavailable"),
      new L1ProviderTransientError("transport", "unavailable"),
    ]);
    const boundSlot = chain.wallSlot() + 5;

    const reached = await Effect.runPromise(
      awaitLedgerTipAtLowerBound(
        chain.lucid,
        chain.lucid.slotToUnixTime(boundSlot),
        "test",
      ),
    );

    expect(reached.ledgerTipSlot).toBe(boundSlot + SUBMIT_SLOT_VALIDITY_BUFFER);
    expect(chain.reads()).toBeGreaterThan(2);
  });

  it("ends the wait at once on the tip source's staleness refusal", async () => {
    // The tip 100 slots behind the wall, past the source's 60 s bound.
    const chain = await flakyTipChain([], (wallSlot) => wallSlot - 100);
    const startSlot = chain.wallSlot();

    const failure = await Effect.runPromise(
      Effect.flip(
        awaitLedgerTipAtLowerBound(
          chain.lucid,
          chain.lucid.slotToUnixTime(startSlot + 5),
          "test",
        ),
      ),
    );

    expect(failure).toBeInstanceOf(L1LedgerBehindError);
    expect(chain.reads()).toBe(1);
    expect(chain.wallSlot()).toBe(startSlot);
  });
});
