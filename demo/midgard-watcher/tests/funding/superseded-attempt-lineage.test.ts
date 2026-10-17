import { rm } from "node:fs/promises";

import { afterEach, expect, it } from "vitest";

import type { WatcherProverFundingReservationPlan } from "../../src/funding/prover-funding-reservation.js";
import {
  excludesEveryUncoveredAttempt,
  uncoveredSupersededAttempts,
} from "../../src/funding/sqlite-prover-funding-reservation-store.superseded-exclusion.js";
import {
  abandonmentHandoff,
  openStore,
  plan,
  prepareTransition,
  signedTransition,
  submissionHandoff,
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

type Store = Awaited<ReturnType<typeof openStore>>["runtime"]["store"];

const first = `${"11".repeat(32)}#0`;
const second = `${"13".repeat(32)}#0`;

/** One reservation with two funding inputs, so a replacement could avoid
 * every superseded attempt if the store let it. */
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

/** The pending attempt expired at the tip: abandoned without retirement. */
const supersedePending = async (
  store: Store,
  owner: WatcherProverFundingReservationPlan,
  pending: Awaited<ReturnType<typeof prepareTransition>>,
  actionKind: string,
) => {
  const {
    reconciliation: { retirement: _retirement, ...reconciliation },
    ...rest
  } = abandonmentHandoff(
    owner,
    pending.pendingTransition!.transactionHash,
    actionKind,
  );
  const handoff = { ...rest, reconciliation };
  const abandoned = await store.abandonPendingTransition({
    plan: owner,
    expectedRevision: pending.revision,
    transitionDigest: pending.pendingTransition!.transitionDigest,
    handoff,
  });
  return await store.acknowledgeAbandonment({
    plan: owner,
    expectedRevision: abandoned.revision,
    handoff,
  });
};

/** A rollback drops a confirmed attempt: the store makes it pending again
 * from the inputs its lineage records. */
const reobserve = async (
  store: Store,
  owner: WatcherProverFundingReservationPlan,
  revision: string,
  transactionHash: string,
  adoption?: unknown,
) => {
  const candidates = await store.readReobservationInputs!({
    reservationId: owner.reservationId,
    transactionHash,
  });
  return await store.reobserveTransition!({
    plan: owner,
    expectedRevision: revision,
    transactionHash,
    inputs: owner.inputs.filter(({ outRef }) =>
      candidates.some((candidate) => candidate.outRef === outRef),
    ),
    ...(adoption === undefined ? {} : { adoption }),
  });
};

const exclusionSets = async (store: Store, owner: { reservationId: string }) =>
  (
    await store.readSupersededAttemptFundingOutRefs!({
      reservationId: owner.reservationId,
    })
  )
    .map((set) => [...set])
    .sort((left, right) => left.length - right.length);

it("admits a replacement for a rolled-back proof and its rolled-back child through the shared lineage input", async () => {
  const opened = await openStore();
  const { store } = opened.runtime;
  try {
    const owner = twoFundingPlan();
    await store.reserve(owner);
    // A confirms; C spends A's change.
    const a = signedTransition({
      inputHash: "11".repeat(32),
      validityUpperBound: 100n,
    });
    const pa = await prepareTransition(store, {
      plan: owner,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...a,
      consumedOutRefs: [first],
    });
    const ca = await store.confirmTransition({
      plan: owner,
      expectedRevision: pa.revision,
      transactionHash: a.transactionHash,
      transitionDigest: pa.pendingTransition!.transitionDigest,
    });
    const aChange = `${a.transactionHash}#0`;
    const c = signedTransition({
      inputHash: a.transactionHash,
      outputLovelace: 98_000_000n,
      validityUpperBound: 200n,
    });
    const pc = await prepareTransition(store, {
      plan: owner,
      expectedRevision: ca.revision,
      actionKind: "step-one",
      ...c,
      consumedOutRefs: [aChange],
    });
    // A rollback drops both: C expires at the tip, then A is reobserved and
    // cannot land either.
    const kc = await supersedePending(store, owner, pc, "step-one");
    const ra = await reobserve(store, owner, kc.revision, a.transactionHash);
    const ka = await supersedePending(store, owner, ra, "proof.init");
    expect(
      ka.activeInputs
        .filter(({ role }) => role === "funding")
        .map(({ outRef }) => outRef),
    ).toEqual([first, second]);
    // C's exclusion set reaches A's input: C cannot land unless A does.
    expect(await exclusionSets(store, owner)).toEqual([
      [first],
      [first, aChange].sort(),
    ]);
    // Negative: a replacement that spends neither could land beside both.
    await expect(
      prepareTransition(store, {
        plan: owner,
        expectedRevision: ka.revision,
        actionKind: "proof.init",
        ...signedTransition({
          inputHash: "13".repeat(32),
          validityUpperBound: 300n,
        }),
        consumedOutRefs: [second],
      }),
    ).rejects.toThrow("must share an input with each superseded attempt");
    // A' spends A's input, which excludes A and, through A, C.
    const replacement = signedTransition({
      inputHash: "11".repeat(32),
      outputLovelace: 97_000_000n,
      validityUpperBound: 300n,
    });
    const admitted = await prepareTransition(store, {
      plan: owner,
      expectedRevision: ka.revision,
      actionKind: "proof.init",
      ...replacement,
      consumedOutRefs: [first],
    });
    expect(admitted.pendingTransition?.transactionHash).toBe(
      replacement.transactionHash,
    );
  } finally {
    opened.runtime.close();
  }
});

it("lets a child's replacement proceed at once when a rollback lands the superseded original instead of its confirmed replacement", async () => {
  const opened = await openStore();
  const { store } = opened.runtime;
  try {
    const owner = twoFundingPlan();
    await store.reserve(owner);
    // A is superseded; its replacement A' confirms; C spends A''s change.
    const a = signedTransition({
      inputHash: "11".repeat(32),
      validityUpperBound: 100n,
    });
    const pa = await prepareTransition(store, {
      plan: owner,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...a,
      consumedOutRefs: [first],
    });
    const ka = await supersedePending(store, owner, pa, "proof.init");
    const a2 = signedTransition({
      inputHash: "11".repeat(32),
      outputLovelace: 97_000_000n,
      validityUpperBound: 300n,
    });
    const pa2 = await prepareTransition(store, {
      plan: owner,
      expectedRevision: ka.revision,
      actionKind: "proof.init",
      ...a2,
      consumedOutRefs: [first],
    });
    const ca2 = await store.confirmTransition({
      plan: owner,
      expectedRevision: pa2.revision,
      transactionHash: a2.transactionHash,
      transitionDigest: pa2.pendingTransition!.transitionDigest,
    });
    const c = signedTransition({
      inputHash: a2.transactionHash,
      outputLovelace: 96_000_000n,
      validityUpperBound: 400n,
    });
    const pc = await prepareTransition(store, {
      plan: owner,
      expectedRevision: ca2.revision,
      actionKind: "step-one",
      ...c,
      consumedOutRefs: [`${a2.transactionHash}#0`],
    });
    // A rollback lands A instead of A': C is impossible at the tip, A' is
    // reobserved and cannot land, and A is adopted as the action's result.
    const kc = await supersedePending(store, owner, pc, "step-one");
    const ra2 = await reobserve(store, owner, kc.revision, a2.transactionHash);
    const ka2 = await supersedePending(store, owner, ra2, "proof.init");
    const adopted = await reobserve(
      store,
      owner,
      ka2.revision,
      a.transactionHash,
      submissionHandoff({
        plan: owner,
        expectedRevision: ka2.revision,
        actionKind: "proof.init",
        ...a,
        consumedOutRefs: [first],
      }),
    );
    const confirmed = await store.confirmTransition({
      plan: owner,
      expectedRevision: adopted.revision,
      transactionHash: a.transactionHash,
      transitionDigest: adopted.pendingTransition!.transitionDigest,
    });
    // A covers A' (both spend first) and, through A''s lineage, C. Nothing
    // asks C's replacement to share an input, and nothing waits for k.
    expect(await exclusionSets(store, owner)).toEqual([]);
    const replacement = signedTransition({
      inputHash: a.transactionHash,
      outputLovelace: 95_000_000n,
      validityUpperBound: 500n,
    });
    const admitted = await prepareTransition(store, {
      plan: owner,
      expectedRevision: confirmed.revision,
      actionKind: "step-one",
      ...replacement,
      consumedOutRefs: [`${a.transactionHash}#0`],
    });
    expect(admitted.pendingTransition?.transactionHash).toBe(
      replacement.transactionHash,
    );
  } finally {
    opened.runtime.close();
  }
});

it("expands each superseded attempt through the recorded attempts whose outputs it spends", () => {
  // p spends funding a1; its child c spends p's output and nothing else.
  const p = attempt([hash("a1")], [ref("a1")]);
  const pOutput = `${p.transactionHash}#0`;
  const c = attempt([hash("d9")], [], [pOutput]);
  const cInputs = [pOutput, ref("d9")].sort();
  // Both superseded: c's exclusion set reaches p's input.
  const both = uncoveredSupersededAttempts({
    abandoned: [abandoned(p), abandoned(c)],
    submissions: [p, c],
  });
  expect(both.map(({ exclusion }) => exclusion)).toEqual([
    [ref("a1")],
    [...cInputs, ref("a1")].sort(),
  ]);
  // A replacement spending only a1 excludes both, though p's output is still
  // reserved: c cannot land unless p does.
  const replacement = attempt([hash("a1")], [ref("a1")]);
  expect(
    excludesEveryUncoveredAttempt({
      uncovered: both,
      signedTransactionCborHex: replacement.signedTransactionCborHex,
      reservedFundingOutRefs: [ref("a1"), pOutput],
    }),
  ).toBe(true);
  // Negative: one spending neither is refused while a1 is still reserved,
  // even though c's own inputs are not.
  expect(
    excludesEveryUncoveredAttempt({
      uncovered: both.slice(1),
      signedTransactionCborHex: attempt([hash("f1")], [ref("f1")])
        .signedTransactionCborHex,
      reservedFundingOutRefs: [ref("a1"), ref("f1")],
    }),
  ).toBe(false);
  // A live replacement of p covers p and, through the lineage, c.
  const pReplacement = attempt([hash("a1"), hash("e2")], [ref("a1")]);
  expect(
    uncoveredSupersededAttempts({
      abandoned: [abandoned(p), abandoned(c)],
      submissions: [p, c, pReplacement],
    }),
  ).toEqual([]);
  // A live ancestor never covers its own child: q is confirmed, c2 spends
  // its output and was superseded.
  const q = attempt([hash("a2")], [ref("a2")]);
  const c2 = attempt([hash("d8")], [], [`${q.transactionHash}#0`]);
  expect(
    uncoveredSupersededAttempts({
      abandoned: [abandoned(c2)],
      submissions: [q, c2],
    }).map(({ transition }) => transition),
  ).toEqual([c2]);
});
