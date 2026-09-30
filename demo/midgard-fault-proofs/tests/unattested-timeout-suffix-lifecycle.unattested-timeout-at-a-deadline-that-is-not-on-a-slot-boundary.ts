import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  resolveTimeoutCorrectionValidityRange,
  selectTimeoutCorrectionTarget,
} from "../src/remove-unattested-block.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nodeUnit } from "./unattested-timeout-suffix-lifecycle.build-contracts.js";
import { setup } from "./unattested-timeout-suffix-lifecycle.setup.js";

describe("real unattested timeout suffix lifecycle", () => {
  it("removes an expired tail while retaining its immature attested predecessor and root", async () => {
    const f = await setup(0);
    const predecessor = await f.node(f.hashes[0]!);
    const root = await f.one(f.rootUnit);
    expect(BigInt(f.emulator.now())).toBeLessThan(
      f.headers[0]!.endTime + 604_800_000n,
    );
    await f.submit(await f.terminal());
    const retained = await f.node(f.hashes[0]!);
    expect(retained.datum.next).toBe("Empty");
    expect(retained.datum.data).toEqual(predecessor.datum.data);
    expect(retained.utxo.assets).toEqual(predecessor.utxo.assets);
    expect(await f.one(f.rootUnit)).toEqual(root);
    expect(
      await f.lucid.utxosAtWithUnit(
        f.config.stateQueueAddress,
        nodeUnit(f.contracts.stateQueuePolicyId, f.hashes[1]!),
      ),
    ).toEqual([]);
    expect(
      Data.from((await f.one(f.lockUnit)).datum!, SDK.CorrectionLockDatum),
    ).toBe("Idle");
  }, 120_000);

  it("prunes multiple descendants before removing the interior target and releases only the terminal lock", async () => {
    const f = await setup(2);
    const predecessor = await f.node(f.hashes[0]!);
    const root = await f.one(f.rootUnit);
    for (const descendant of f.hashes.slice(2)) {
      await f.submit(
        SDK.incompletePruneUnattestedBlockDescendantTxProgram(
          f.lucid,
          f.config,
          {
            ...(await f.common()),
            predecessorRefInput: await f.node(f.hashes[0]!),
            removedDescendantUTxO: await f.node(descendant),
          },
        ),
      );
      expect(await f.node(f.hashes[0]!)).toEqual(predecessor);
      expect(
        Data.from((await f.one(f.lockUnit)).datum!, SDK.CorrectionLockDatum),
      ).toEqual({
        Locked: {
          target_header_hash: f.hashes[1],
          correction_identity: "AttestationTimeout",
        },
      });
    }
    await f.submit(await f.terminal());
    expect((await f.node(f.hashes[0]!)).datum.next).toBe("Empty");
    expect((await f.node(f.hashes[0]!)).datum.data).toEqual(
      predecessor.datum.data,
    );
    expect(await f.one(f.rootUnit)).toEqual(root);
    expect(
      Data.from((await f.one(f.lockUnit)).datum!, SDK.CorrectionLockDatum),
    ).toBe("Idle");
    expect((await f.lucid.utxosAt(f.config.stateQueueAddress)).length).toBe(2);
  }, 120_000);

  it("refuses premature and already-attested targets in the applied validators", async () => {
    const premature = await setup(0);
    const deadline =
      premature.headers[1]!.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS;
    await expectOnchainRefusal(async () =>
      (await premature.terminal(deadline - 1000n)).complete({
        localUPLCEval: true,
      }),
    );
    const attested = await setup(0, true);
    await expectOnchainRefusal(async () =>
      (await attested.terminal()).complete({ localUPLCEval: true }),
    );
  }, 120_000);

  it("refuses a malicious builder view that attempts to splice out a nonterminal target", async () => {
    const f = await setup(1);
    const common = await f.common();
    // Alter only the builder's parsed view. The consumed datum remains the
    // original on-chain datum linking to a descendant, so UPLC must refuse it.
    const tx = SDK.incompleteRemoveLastUnattestedBlockTxProgram(
      f.lucid,
      f.config,
      {
        ...common,
        timedOutBlockUTxO: {
          ...common.timedOutBlockUTxO,
          datum: { ...common.timedOutBlockUTxO.datum, next: "Empty" },
        },
        predecessorUTxO: await f.node(f.hashes[0]!),
      },
    );
    await expectOnchainRefusal(() => tx.complete({ localUPLCEval: true }));
  }, 120_000);
});

describe("unattested timeout at a deadline that is not on a slot boundary", () => {
  // Live block windows end at ...999 ms, so the timeout deadline sits 1 ms
  // before a slot boundary. The ledger presents a validity lower bound as the
  // start of its slot; a lower bound of the raw deadline is presented 999 ms
  // early and the validator's `lower >= end_time + timeout` refuses it.
  const liveFixture = async (descendantCount: number) => {
    const f = await setup(descendantCount, false, { liveBlockEndTimes: true });
    const deadline = f.headers[1]!.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS;
    const deadlineSlot = f.lucid.unixTimeToSlot(Number(deadline));
    const deadlineSlotStart = BigInt(f.lucid.slotToUnixTime(deadlineSlot));
    expect(deadline - deadlineSlotStart).toBe(999n);
    const advanceToSlot = (slot: number) => {
      expect(slot).toBeGreaterThanOrEqual(f.emulator.slot);
      f.emulator.awaitSlot(slot - f.emulator.slot);
    };
    const expectTargetRemoved = async () =>
      expect(
        await f.lucid.utxosAtWithUnit(
          f.config.stateQueueAddress,
          nodeUnit(f.contracts.stateQueuePolicyId, f.hashes[1]!),
        ),
      ).toEqual([]);
    return {
      ...f,
      deadline,
      deadlineSlot,
      advanceToSlot,
      expectTargetRemoved,
    };
  };

  it("removes the target when built at the deadline instant, in the first slot at or after it", async () => {
    const f = await liveFixture(0);
    f.advanceToSlot(f.deadlineSlot + 1);
    expect(BigInt(f.emulator.now())).toBe(f.deadline + 1n);
    const range = resolveTimeoutCorrectionValidityRange(
      f.lucid,
      f.deadline,
      f.deadline,
    );
    expect(range.validFrom).toBe(f.deadline + 1n);
    await f.submit(await f.terminal(range.validFrom, range.validTo));
    await f.expectTargetRemoved();
    expect(
      Data.from((await f.one(f.lockUnit)).datum!, SDK.CorrectionLockDatum),
    ).toBe("Idle");
  }, 120_000);

  it("prunes and removes a few seconds past the deadline, well inside the backdate window", async () => {
    const f = await liveFixture(1);
    f.advanceToSlot(f.deadlineSlot + 6);
    const nowMs = BigInt(f.emulator.now());
    expect(nowMs - f.deadline).toBe(5_001n);
    // The backdated lower bound is still before the deadline here, so the
    // range starts at the deadline's slot boundary.
    const range = resolveTimeoutCorrectionValidityRange(
      f.lucid,
      f.deadline,
      nowMs,
    );
    expect(range.validFrom).toBe(f.deadline + 1n);
    await f.submit(
      SDK.incompletePruneUnattestedBlockDescendantTxProgram(f.lucid, f.config, {
        ...(await f.common(range.validFrom, range.validTo)),
        predecessorRefInput: await f.node(f.hashes[0]!),
        removedDescendantUTxO: await f.node(f.hashes[2]!),
      }),
    );
    await f.submit(await f.terminal(range.validFrom, range.validTo));
    await f.expectTargetRemoved();
  }, 120_000);

  it("refuses premature attempts off-chain and on-chain, and refuses the raw deadline as a lower bound", async () => {
    const f = await liveFixture(0);
    // The slot containing the deadline starts before it: the correction is
    // not ready, and forcing its start as the lower bound is refused.
    f.advanceToSlot(f.deadlineSlot);
    const prematureNow = BigInt(f.emulator.now());
    expect(prematureNow).toBeLessThan(f.deadline);
    const queue = await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(f.lucid, f.config),
    );
    const selected = await selectTimeoutCorrectionTarget(
      queue,
      prematureNow,
      "Idle",
    );
    expect(selected?.deadline).toBe(f.deadline);
    // submitUnattestedTimeoutCorrection answers "not-ready" on exactly this.
    expect(prematureNow < selected!.deadline).toBe(true);
    await expectOnchainRefusal(async () =>
      (await f.terminal(prematureNow)).complete({ localUPLCEval: true }),
    );

    // Once the ledger reaches the deadline's slot boundary, the raw deadline
    // (floored to the slot that contains it) is still refused, while the
    // slot-aligned lower bound is accepted.
    f.advanceToSlot(f.deadlineSlot + 1);
    await expectOnchainRefusal(async () =>
      (await f.terminal(f.deadline)).complete({ localUPLCEval: true }),
    );
    const range = resolveTimeoutCorrectionValidityRange(
      f.lucid,
      f.deadline,
      BigInt(f.emulator.now()),
    );
    await f.submit(await f.terminal(range.validFrom, range.validTo));
    await f.expectTargetRemoved();
  }, 120_000);
});
