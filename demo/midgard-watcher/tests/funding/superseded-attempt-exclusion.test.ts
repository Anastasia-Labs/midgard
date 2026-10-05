import { rm } from "node:fs/promises";
import { DatabaseSync } from "node:sqlite";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { afterEach, expect, it } from "vitest";

import type { WatcherProverFundingReservationPlan } from "../../src/funding/prover-funding-reservation.js";
import { unsafeOpenWatcherSqliteProverFundingReservationStoreForTest } from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import {
  excludesEveryUncoveredAttempt,
  supersededAttemptFundingOutRefs,
  uncoveredSupersededAttempts,
} from "../../src/funding/sqlite-prover-funding-reservation-store.superseded-exclusion.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import {
  abandonmentHandoff,
  openStore,
  plan,
  prepareTransition,
  signedTransition,
  temporaryDirectories,
} from "./sqlite-prover-funding-reservation-store.signed-transition.js";
import {
  abandoned,
  attempt,
  hash,
  ref,
} from "./superseded-attempt-transitions.js";

afterEach(async () => {
  await Promise.all(
    temporaryDirectories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});

const first = `${"11".repeat(32)}#0`;
const second = `${"13".repeat(32)}#0`;

/** One reservation with two funding inputs, so a replacement could avoid the
 * superseded attempt's input if the store let it. */
const twoFundingPlan = (): WatcherProverFundingReservationPlan => {
  const base = plan("aa", "66");
  return Object.freeze({
    ...base,
    inputs: Object.freeze(
      [
        ...base.inputs,
        Object.freeze({
          outRef: second,
          role: "funding" as const,
          lovelace: "100000000",
          assets: Object.freeze([]),
        }),
      ].sort((left, right) => (left.outRef < right.outRef ? -1 : 1)),
    ),
    fundingLovelace: "200000000",
  });
};

/** Writes an attempt that expired at the tip: abandoned without retirement. */
const supersede = async (
  store: Awaited<ReturnType<typeof openStore>>["runtime"]["store"],
  owner: WatcherProverFundingReservationPlan,
  revision: string,
  input: { readonly inputHash: string; readonly outputLovelace: bigint },
) => {
  const signed = signedTransition({ ...input, validityUpperBound: 100n });
  const pending = await prepareTransition(store, {
    plan: owner,
    expectedRevision: revision,
    actionKind: "proof.init",
    ...signed,
    consumedOutRefs: [`${input.inputHash}#0`],
  });
  const {
    reconciliation: { retirement: _retirement, ...reconciliation },
    ...rest
  } = abandonmentHandoff(owner, signed.transactionHash, "proof.init");
  const handoff = { ...rest, reconciliation };
  const abandoned = await store.abandonPendingTransition({
    plan: owner,
    expectedRevision: pending.revision,
    transitionDigest: pending.pendingTransition!.transitionDigest,
    handoff,
  });
  const acknowledged = await store.acknowledgeAbandonment({
    plan: owner,
    expectedRevision: abandoned.revision,
    handoff,
  });
  return { signed, record: acknowledged };
};

it("refuses a replacement that avoids every input of an attempt superseded at the tip, and admits one that spends it at once", async () => {
  const opened = await openStore();
  const { store } = opened.runtime;
  try {
    const owner = twoFundingPlan();
    await store.reserve(owner);
    const { record } = await supersede(store, owner, "0", {
      inputHash: "11".repeat(32),
      outputLovelace: 99_000_000n,
    });
    // No hold: the superseded attempt leaves no pending transition and no
    // unacknowledged abandonment, and it needs no retirement first.
    expect(record.pendingTransition).toBeNull();
    expect(
      await store.readSupersededAttemptFundingOutRefs!({
        reservationId: owner.reservationId,
      }),
    ).toEqual([[first]]);
    // Its consumed input stays leased against every other reservation.
    await expect(store.reserve(plan("bb", "77"))).rejects.toThrow("reserved");

    const avoiding = signedTransition({
      inputHash: "13".repeat(32),
      validityUpperBound: 200n,
    });
    await expect(
      prepareTransition(store, {
        plan: owner,
        expectedRevision: record.revision,
        actionKind: "proof.init",
        ...avoiding,
        consumedOutRefs: [second],
      }),
    ).rejects.toThrow("must share an input with each superseded attempt");

    const replacement = signedTransition({
      inputHash: "11".repeat(32),
      outputLovelace: 98_000_000n,
      validityUpperBound: 200n,
    });
    const pending = await prepareTransition(store, {
      plan: owner,
      expectedRevision: record.revision,
      actionKind: "proof.init",
      ...replacement,
      consumedOutRefs: [first],
    });
    expect(pending.pendingTransition?.transactionHash).toBe(
      replacement.transactionHash,
    );
    // The recorded replacement covers the superseded attempt.
    expect(
      await store.readSupersededAttemptFundingOutRefs!({
        reservationId: owner.reservationId,
      }),
    ).toEqual([]);
  } finally {
    opened.runtime.close();
  }
});

it("applies the same exclusion to a legacy abandonment written without retirement", async () => {
  const opened = await openStore();
  const owner = twoFundingPlan();
  await opened.runtime.store.reserve(owner);
  const signed = signedTransition({ validityUpperBound: 100n });
  const pending = await prepareTransition(opened.runtime.store, {
    plan: owner,
    expectedRevision: "0",
    actionKind: "proof.init",
    ...signed,
    consumedOutRefs: [first],
  });
  // The pre-fix flow: abandoned with retirement evidence, acknowledged...
  const handoff = abandonmentHandoff(
    owner,
    signed.transactionHash,
    "proof.init",
  );
  const abandoned = await opened.runtime.store.abandonPendingTransition({
    plan: owner,
    expectedRevision: pending.revision,
    transitionDigest: pending.pendingTransition!.transitionDigest,
    handoff,
  });
  await opened.runtime.store.acknowledgeAbandonment({
    plan: owner,
    expectedRevision: abandoned.revision,
    handoff,
  });
  opened.runtime.close();
  // ...then persisted in the exact legacy format, which carries none.
  const database = new DatabaseSync(opened.path);
  const row = database
    .prepare("SELECT canonical_json FROM watcher_prover_funding_abandonment_v1")
    .get() as { canonical_json: string };
  const legacy = JSON.parse(row.canonical_json);
  delete legacy.handoff.reconciliation.retirement;
  database
    .prepare(
      "UPDATE watcher_prover_funding_abandonment_v1 SET canonical_json=?, record_digest=?",
    )
    .run(
      watcherCanonicalJson(legacy),
      computeDeploymentManifestJsonDigest(legacy),
    );
  database.close();
  const reopened =
    await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
      { path: opened.path },
      () => undefined,
    );
  try {
    const [record] = (await reopened.store.readAll()) as {
      revision: string;
      pendingTransition: unknown;
    }[];
    expect(record!.pendingTransition).toBeNull();
    expect(
      await reopened.store.readSupersededAttemptFundingOutRefs!({
        reservationId: owner.reservationId,
      }),
    ).toEqual([[first]]);
    await expect(
      prepareTransition(reopened.store, {
        plan: owner,
        expectedRevision: record!.revision,
        actionKind: "proof.init",
        ...signedTransition({
          inputHash: "13".repeat(32),
          validityUpperBound: 200n,
        }),
        consumedOutRefs: [second],
      }),
    ).rejects.toThrow("must share an input with each superseded attempt");
    const replacement = signedTransition({
      outputLovelace: 98_000_000n,
      validityUpperBound: 200n,
    });
    await expect(
      prepareTransition(reopened.store, {
        plan: owner,
        expectedRevision: record!.revision,
        actionKind: "proof.init",
        ...replacement,
        consumedOutRefs: [first],
      }),
    ).resolves.toMatchObject({
      pendingTransition: { transactionHash: replacement.transactionHash },
    });
  } finally {
    reopened.close();
  }
});

it("asks a replacement to exclude every uncovered superseded attempt at once", async () => {
  const opened = await openStore();
  const { store } = opened.runtime;
  try {
    const owner = twoFundingPlan();
    await store.reserve(owner);
    const a = await supersede(store, owner, "0", {
      inputHash: "11".repeat(32),
      outputLovelace: 99_000_000n,
    });
    // B spends A's input, so B covers A while B is recorded...
    const b = await supersede(store, owner, a.record.revision, {
      inputHash: "11".repeat(32),
      outputLovelace: 97_000_000n,
    });
    // ...until B is itself superseded: both are uncovered, sharing one input.
    expect(
      await store.readSupersededAttemptFundingOutRefs!({
        reservationId: owner.reservationId,
      }),
    ).toEqual([[first], [first]]);
    await expect(
      prepareTransition(store, {
        plan: owner,
        expectedRevision: b.record.revision,
        actionKind: "proof.init",
        ...signedTransition({
          inputHash: "13".repeat(32),
          validityUpperBound: 300n,
        }),
        consumedOutRefs: [second],
      }),
    ).rejects.toThrow("must share an input with each superseded attempt");
  } finally {
    opened.runtime.close();
  }
});

it("accepts a shared protocol input as the exclusion, and refuses a replacement that shares nothing while a funding input is left", async () => {
  const opened = await openStore();
  const { store } = opened.runtime;
  const node = "a1".repeat(32);
  try {
    const owner = twoFundingPlan();
    await store.reserve(owner);
    const signed = signedTransition({
      protocolInputHashes: [node],
      validityUpperBound: 100n,
    });
    const pending = await prepareTransition(store, {
      plan: owner,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...signed,
      consumedOutRefs: [first],
    });
    const {
      reconciliation: { retirement: _retirement, ...reconciliation },
      ...rest
    } = abandonmentHandoff(owner, signed.transactionHash, "proof.init");
    const handoff = { ...rest, reconciliation };
    const abandoned = await store.abandonPendingTransition({
      plan: owner,
      expectedRevision: pending.revision,
      transitionDigest: pending.pendingTransition!.transitionDigest,
      handoff,
    });
    const record = await store.acknowledgeAbandonment({
      plan: owner,
      expectedRevision: abandoned.revision,
      handoff,
    });
    await expect(
      prepareTransition(store, {
        plan: owner,
        expectedRevision: record.revision,
        actionKind: "proof.init",
        ...signedTransition({
          inputHash: "13".repeat(32),
          protocolInputHashes: ["a2".repeat(32)],
          validityUpperBound: 200n,
        }),
        consumedOutRefs: [second],
      }),
    ).rejects.toThrow("must share an input with each superseded attempt");
    // The same challenged node, other funding: mutually exclusive.
    const replacement = signedTransition({
      inputHash: "13".repeat(32),
      protocolInputHashes: [node],
      validityUpperBound: 200n,
    });
    await expect(
      prepareTransition(store, {
        plan: owner,
        expectedRevision: record.revision,
        actionKind: "proof.init",
        ...replacement,
        consumedOutRefs: [second],
      }),
    ).resolves.toMatchObject({
      pendingTransition: { transactionHash: replacement.transactionHash },
    });
    expect(
      await store.readSupersededAttemptFundingOutRefs!({
        reservationId: owner.reservationId,
      }),
    ).toEqual([]);
  } finally {
    opened.runtime.close();
  }
});

it("computes the uncovered attempts and admits replacements in both polarities", () => {
  // a spends funding x and node n; b spends funding y and z.
  const a = attempt([hash("a1"), hash("e1")], [ref("a1")]);
  const b = attempt([hash("b1"), hash("b2")], [ref("b1"), ref("b2")]);
  const live = attempt([hash("c1"), hash("e1")], [ref("c1")]);

  // A retired attempt is bookkeeping; one covered by a shared protocol input
  // (the node e1) needs nothing more.
  expect(
    uncoveredSupersededAttempts({
      abandoned: [abandoned(a, true)],
      submissions: [a],
    }),
  ).toEqual([]);
  expect(
    uncoveredSupersededAttempts({
      abandoned: [abandoned(a)],
      submissions: [a, live],
    }),
  ).toEqual([]);
  // A superseded attempt never covers itself or another superseded attempt.
  const uncovered = uncoveredSupersededAttempts({
    abandoned: [abandoned(a), abandoned(b)],
    submissions: [a, b],
  });
  expect(uncovered).toEqual([
    { transition: a, exclusion: [ref("a1"), ref("e1")] },
    { transition: b, exclusion: [ref("b1"), ref("b2")] },
  ]);
  expect(supersededAttemptFundingOutRefs(uncovered)).toEqual([
    [ref("a1"), ref("e1")],
    [ref("b1"), ref("b2")],
  ]);
  const admits = (inputs: readonly string[], reserved: readonly string[]) =>
    excludesEveryUncoveredAttempt({
      uncovered,
      signedTransactionCborHex: attempt(inputs, []).signedTransactionCborHex,
      reservedFundingOutRefs: reserved,
    });
  const reserved = [ref("a1"), ref("b1"), ref("b2"), ref("d1")];
  // Shared node with a, shared funding with b.
  expect(admits([hash("e1"), hash("b2")], reserved)).toBe(true);
  // Shares nothing with b although b's funding is still reserved.
  expect(admits([hash("e1"), hash("d1")], reserved)).toBe(false);
  // Nothing of b is left: no shared input is possible, so it is admitted.
  expect(admits([hash("e1"), hash("d1")], [ref("a1"), ref("d1")])).toBe(true);
  expect(
    excludesEveryUncoveredAttempt({
      uncovered: [],
      signedTransactionCborHex: attempt([hash("d1")], [])
        .signedTransactionCborHex,
      reservedFundingOutRefs: reserved,
    }),
  ).toBe(true);
});
