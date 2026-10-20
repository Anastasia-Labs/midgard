import { CML } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  WATCHER_ORPHAN_CANCEL_MAX_FEE_LOVELACE,
  WATCHER_ORPHAN_CANCEL_VALIDITY_MS,
} from "../../src/funding/prover-funding-orphan-cancel.transaction.js";
import {
  ORPHAN_CANCEL_EXPIRED,
  ORPHAN_CANCEL_INVALIDATED,
} from "../../src/funding/prover-funding-recovery.orphan-cancel.js";
import {
  WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION,
  WatcherProverFundingOrphanedReservationError,
} from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import {
  cleanupFundingRecoveryFixtures,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import {
  hashOf,
  orphanToCancel,
  recordedCancel,
  sweepToCancel,
} from "./orphan-cancel-fixture.js";
import { admitFresh, doublyLeased } from "./orphaned-reservation-fixture.js";
import { completionHandoff } from "./sqlite-prover-funding-reservation-store.signed-transition.js";
import { fundingOf } from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

const inFlight = expect.stringContaining(
  "a cancel of its funding is in flight",
);

describe("an orphan whose attempt without expiry left the mempool, of a target that is gone", () => {
  it("is cancelled after three agreeing reads; the cancel lands, the coins return to the wallet, and the hold and alert clear", async () => {
    const setup = await orphanToCancel();
    const { fixture, chain, wallet, alerts } = setup;
    const funding = fundingOf(fixture);

    // Two agreeing reads sign nothing.
    await setup.sweep();
    await setup.sweep();
    expect(wallet.signed).toEqual([]);
    expect(setup.targets).toContainEqual({
      category: "doubleSpend",
      headerHash: fixture.old.headerHash,
    });

    const signed = await setup.sweep();
    expect(signed).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          reservationId: fixture.plan.reservationId,
          readiness: WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION,
          detail: inFlight,
        }),
      ]),
    );
    expect(wallet.signed).toHaveLength(1);
    expect(wallet.submitted).toHaveLength(1);
    const cancel = wallet.submitted[0]!;
    const hash = hashOf(cancel);
    expect(wallet.signed).toEqual([hash]);
    expect(await recordedCancel(fixture)).toEqual({
      transactionHash: hash,
      signedTransactionCborHex: cancel,
      inputs: [funding.outRef],
    });

    // It returns the attempt's funding, less its fee, to the wallet, and
    // expires within the hour.
    const body = CML.Transaction.from_cbor_hex(cancel).body();
    expect(body.outputs().len()).toBe(1);
    const output = body.outputs().get(0);
    expect(output.address().to_bech32()).toBe(walletAddress);
    expect(output.amount().coin() + body.fee()).toBe(BigInt(funding.lovelace));
    expect(body.fee() <= WATCHER_ORPHAN_CANCEL_MAX_FEE_LOVELACE).toBe(true);
    expect(Number(body.ttl()) * 1000).toBeLessThanOrEqual(
      setup.now() + WATCHER_ORPHAN_CANCEL_VALIDITY_MS,
    );

    // It lands; the original attempt can no longer land.
    chain.set(hash, "landed");
    chain.set(fixture.transactionHash, "spent");
    expect(await setup.sweep()).toEqual([
      expect.objectContaining({ detail: inFlight }),
    ]);
    expect(wallet.submitted).toHaveLength(1);

    // The cancel is final first: it is kept until the attempt is too.
    chain.set(hash, "landed final");
    expect(await setup.sweep()).toEqual([
      expect.objectContaining({
        detail: expect.stringContaining("its cancel landed"),
      }),
    ]);
    expect(await recordedCancel(fixture)).not.toBeNull();

    // Both are final: the orphan settles, nothing of it leased.
    chain.set(fixture.transactionHash, "invalidated final");
    expect(await setup.sweep()).toEqual([]);
    expect(await setup.orphan()).toMatchObject({
      state: "released",
      activeInputs: [],
      pendingTransition: null,
    });
    expect(await recordedCancel(fixture)).toBeNull();
    expect(await fixture.store.readReservedOutRefs({})).not.toContain(
      funding.outRef,
    );
    expect(await doublyLeased(fixture)).toEqual([]);
    expect(alerts).toEqual([
      { subjectDigest: fixture.plan.reservationId, active: true },
      { subjectDigest: fixture.plan.reservationId, active: false },
    ]);
    expect(wallet.signed).toHaveLength(1);
  });

  it("is not cancelled once the old attempt lands first: the cancel cannot land, and the orphan settles as included", async () => {
    const setup = await orphanToCancel();
    const { fixture, chain, wallet } = setup;
    const cancel = (await sweepToCancel(setup))!;
    const hash = hashOf(cancel);

    // The old attempt lands; the cancel's input is spent, so it is not sent
    // again.
    chain.set(fixture.transactionHash, "landed");
    chain.set(hash, "spent");
    await setup.sweep();
    await setup.sweep();
    expect(await setup.orphan()).toMatchObject({ state: "active" });
    expect(wallet.submitted).toEqual([cancel]);

    // The cancel is final first: it is dropped, and none follows it.
    chain.set(hash, "invalidated final");
    expect(await setup.sweep()).toEqual([
      expect.objectContaining({
        detail: expect.stringContaining(ORPHAN_CANCEL_INVALIDATED),
      }),
    ]);
    expect(await recordedCancel(fixture)).toBeNull();

    chain.set(fixture.transactionHash, "landed final");
    expect(await setup.sweep()).toEqual([]);
    expect(await setup.orphan()).toMatchObject({
      state: "released",
      activeInputs: [],
    });
    expect(wallet.signed).toEqual([hash]);
    expect(await doublyLeased(fixture)).toEqual([]);
  });

  it("is cancelled once its workflow completed, while its target can still run", async () => {
    const setup = await orphanToCancel();
    const { fixture, gone, wallet } = setup;
    gone.value = false;
    const completed = completionHandoff(fixture.plan);
    const read = fixture.store.readCompletionHandoff.bind(fixture.store);
    setup.overrides.current = {
      readCompletionHandoff: async (reserved) =>
        reserved.reservationId === fixture.plan.reservationId
          ? completed
          : await read(reserved),
    };
    await setup.restart();
    expect(await sweepToCancel(setup)).toBeDefined();
    expect(wallet.signed).toHaveLength(1);
    expect(setup.targets).toEqual([]);
  });

  it("sends one cancel when the process stops after signing one but before recording it", async () => {
    const setup = await orphanToCancel();
    const { fixture, chain, wallet, overrides } = setup;
    overrides.current = {
      recordOrphanCancel: async () => {
        throw new Error("the process stopped");
      },
    };
    await setup.restart();
    for (let read = 0; read < 3; read += 1) await setup.sweep();
    // The signed bytes were never recorded, so they were never sent.
    expect(wallet.signed).toHaveLength(1);
    expect(wallet.submitted).toEqual([]);
    expect(await recordedCancel(fixture)).toBeNull();

    overrides.current = {};
    await setup.restart();
    const cancel = (await sweepToCancel(setup))!;
    expect(wallet.submitted).toEqual([cancel]);
    expect((await recordedCancel(fixture))?.signedTransactionCborHex).toBe(
      cancel,
    );
    chain.set(hashOf(cancel), "landed final");
    chain.set(fixture.transactionHash, "invalidated final");
    await setup.sweep();
    expect(await setup.orphan()).toMatchObject({ state: "released" });
  });

  it("submits the recorded cancel, never a new one, when the process stops before its submission reaches the node", async () => {
    const setup = await orphanToCancel();
    const { fixture, chain, wallet } = setup;
    wallet.failures.submit = true;
    const cancel = (await sweepToCancel(setup))!;
    expect(await recordedCancel(fixture)).toMatchObject({
      signedTransactionCborHex: cancel,
    });

    wallet.failures.submit = false;
    await setup.restart();
    await setup.sweep();
    expect(wallet.signed).toEqual([hashOf(cancel)]);
    expect(wallet.submitted).toEqual([cancel, cancel]);

    chain.set(hashOf(cancel), "landed final");
    chain.set(fixture.transactionHash, "invalidated final");
    expect(await setup.sweep()).toEqual([]);
    expect(wallet.signed).toHaveLength(1);
    expect(await setup.orphan()).toMatchObject({ state: "released" });
  });

  it("submits the same bytes again, and signs nothing, when the process stops after submitting the cancel", async () => {
    const setup = await orphanToCancel();
    const { fixture, chain, wallet } = setup;
    const cancel = (await sweepToCancel(setup))!;
    const hash = hashOf(cancel);
    chain.set(hash, "mempool");

    // The same process does not send a cancel the mempool holds again.
    await setup.sweep();
    expect(wallet.submitted).toEqual([cancel]);

    await setup.restart();
    await setup.sweep();
    await setup.sweep();
    expect(wallet.signed).toEqual([hash]);
    expect(wallet.submitted).toEqual([cancel, cancel]);

    // Once it landed, a restart sends nothing.
    chain.set(hash, "landed");
    await setup.restart();
    await setup.sweep();
    expect(wallet.submitted).toEqual([cancel, cancel]);

    chain.set(hash, "landed final");
    chain.set(fixture.transactionHash, "invalidated final");
    expect(await setup.sweep()).toEqual([]);
    expect(wallet.signed).toHaveLength(1);
  });

  it("signs a second cancel only after the first expired unlanded past release finality, and that one lands", async () => {
    const setup = await orphanToCancel();
    const { fixture, chain, wallet, alerts } = setup;
    const first = (await sweepToCancel(setup))!;

    // Within release finality nothing new is signed, however often it reads;
    // dropped from the mempool, the recorded cancel is sent again.
    chain.set(hashOf(first), "dropped");
    for (let read = 0; read < 4; read += 1) await setup.sweep();
    expect(wallet.signed).toHaveLength(1);
    expect(wallet.submitted).toEqual(Array.from({ length: 5 }, () => first));

    chain.set(hashOf(first), "expired final");
    expect(await setup.sweep()).toEqual([
      expect.objectContaining({
        detail: expect.stringContaining(ORPHAN_CANCEL_EXPIRED),
      }),
    ]);
    expect(await recordedCancel(fixture)).toBeNull();
    await setup.sweep();
    await setup.sweep();
    expect(wallet.signed).toHaveLength(1);

    await setup.sweep();
    const second = wallet.submitted.at(-1)!;
    expect(wallet.signed).toHaveLength(2);
    expect(hashOf(second)).not.toBe(hashOf(first));
    expect((await recordedCancel(fixture))?.transactionHash).toBe(
      hashOf(second),
    );

    chain.set(hashOf(second), "landed final");
    chain.set(fixture.transactionHash, "invalidated final");
    expect(await setup.sweep()).toEqual([]);
    expect(await setup.orphan()).toMatchObject({
      state: "released",
      activeInputs: [],
    });
    expect(await doublyLeased(fixture)).toEqual([]);
    expect(alerts.at(-1)).toEqual({
      subjectDigest: fixture.plan.reservationId,
      active: false,
    });
  });
});

describe("an orphan that is not cancelled", () => {
  it("is never cancelled while its target can still run, and a fresh execution takes it over", async () => {
    const setup = await orphanToCancel();
    const { fixture, gone, wallet } = setup;
    gone.value = false;
    for (let read = 0; read < 6; read += 1) await setup.sweep();
    expect(wallet.signed).toEqual([]);
    expect(setup.targets.length).toBeGreaterThanOrEqual(6);

    await admitFresh(fixture, setup.process.current.factory);
    expect(await setup.orphan()).toMatchObject({
      state: "released",
      activeInputs: [],
    });
    gone.value = true;
    for (let read = 0; read < 4; read += 1) await setup.sweep();
    expect(wallet.signed).toEqual([]);
    expect(await doublyLeased(fixture)).toEqual([]);
  });

  it("is never cancelled while its attempt has an expiry", async () => {
    const setup = await orphanToCancel({ ttl: 100n });
    for (let read = 0; read < 6; read += 1)
      expect(await setup.sweep()).not.toContainEqual(
        expect.objectContaining({ detail: expect.stringContaining("cancel") }),
      );
    expect(setup.wallet.signed).toEqual([]);
    expect(await recordedCancel(setup.fixture)).toBeNull();
  });

  it("is never cancelled while its attempt is in the mempool or landed", async () => {
    const setup = await orphanToCancel();
    const { fixture, chain, wallet } = setup;
    for (const state of ["mempool", "landed", "spent"] as const) {
      chain.set(fixture.transactionHash, state);
      for (let read = 0; read < 4; read += 1) await setup.sweep();
    }
    expect(wallet.signed).toEqual([]);
  });

  it("is never cancelled while another attempt of it is in the mempool", async () => {
    const setup = await orphanToCancel({ replaced: true });
    const { chain, wallet, replacement } = setup;
    chain.set(replacement!.transactionHash, "mempool");
    for (let read = 0; read < 4; read += 1) await setup.sweep();
    expect(wallet.signed).toEqual([]);
  });

  it("is never cancelled when its records name no target", async () => {
    const setup = await orphanToCancel();
    setup.overrides.current = { readRecordedSubmissions: async () => [] };
    await setup.restart();
    for (let read = 0; read < 4; read += 1) await setup.sweep();
    expect(setup.wallet.signed).toEqual([]);
    expect(setup.targets).toEqual([]);
  });

  it("needs three agreeing reads in a row: a read that disagrees starts the count again", async () => {
    const setup = await orphanToCancel();
    const { gone, wallet } = setup;
    await setup.sweep();
    await setup.sweep();
    gone.value = false;
    await setup.sweep();
    gone.value = true;
    await setup.sweep();
    await setup.sweep();
    expect(wallet.signed).toEqual([]);
    await setup.sweep();
    expect(wallet.signed).toHaveLength(1);
  });
});

describe("an orphan with a cancel in flight", () => {
  it("is never handed over to a fresh execution", async () => {
    const setup = await orphanToCancel();
    const { fixture } = setup;
    await sweepToCancel(setup);
    await expect(
      admitFresh(fixture, setup.process.current.factory),
    ).rejects.toBeInstanceOf(WatcherProverFundingOrphanedReservationError);
  });

  it("is refused by the store's hand-over even when the cancel is not seen", async () => {
    const setup = await orphanToCancel();
    const { fixture, overrides } = setup;
    await sweepToCancel(setup);
    overrides.current = { readOrphanCancel: async () => null };
    await setup.restart();
    await expect(
      admitFresh(fixture, setup.process.current.factory),
    ).rejects.toThrow("prover reservation hand-over mismatch");
    expect(await recordedCancel(fixture)).not.toBeNull();
  });
});
