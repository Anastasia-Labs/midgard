import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import {
  kupoExchange,
  kupoMatch,
  type L1Recording,
  type L1RecordingName,
  loadL1Recording,
  recordedFetch,
  recordedOgmiosWebSocket,
  recordedTransaction,
} from "@al-ft/midgard-test-support/l1-recordings";
import { describe, expect, it } from "vitest";

import {
  fetchKupoAncestorPoint,
  fetchKupoSpend,
  readOgmiosBlockTransaction,
} from "../src/l1-tx-order-carriage.js";

/**
 * What the node reads out of Kupo's `spent_at`, against what a live Kupo
 * actually served.
 *
 * A live preprod node's correction observer failed every pass with "Kupo
 * spent_at.redeemer is not base16 data": Kupo v2.11.0 served `redeemer: null`
 * for a Plutus spend. Kupo's `matchBlock` attributes a transaction's inputs in
 * mirrored order — the input at ledger position `i` of `n` is reported at
 * `n - 1 - i` and handed the redeemer found there — so the null was a *key*
 * input's empty redeemer slot.
 *
 * The answers here are recorded off preprod Kupo v2.11.0 and Ogmios v7.0.0 (see
 * `@al-ft/midgard-test-support/l1-recordings`), not written by hand:
 *
 * - `a2a47d2e`, a state-queue timeout removal with four inputs. Three are Plutus
 *   spends; Kupo reports `null` for one of them, another script's redeemer for
 *   the other two, and a script's redeemer for the key input.
 * - `e1e70271`, a five-input key-wallet sweep with no redeemers, where Kupo's
 *   input numbering shows with nothing else in the way.
 *
 * The read must accept those answers, surface neither Kupo value, and leave the
 * redeemer to the transaction itself; and it must still refuse a `spent_at` that
 * is not Kupo's schema, at the check that names it. The refusals mutate a
 * recording rather than invent an answer: each changes one field of what Kupo
 * really sent.
 */

const KUPO_URL = "http://kupo.recorded";
const OGMIOS_URL = "ws://ogmios.recorded";

const REMOVAL = "preprod-state-queue-removal-a2a47d2e";
const SWEEP = "preprod-reference-sweep-e1e70271";

type OutRef = { readonly txHash: string; readonly outputIndex: number };

/** The recorded transaction's inputs, ascending as the ledger orders them. */
const ledgerInputs = (recording: L1Recording): OutRef[] =>
  (
    recordedTransaction(recording).inputs as {
      transaction: { id: string };
      index: number;
    }[]
  )
    .map((input) => ({
      txHash: input.transaction.id,
      outputIndex: input.index,
    }))
    .sort(compareOutRefs);

/** The spend redeemers the transaction carries, by the ledger's input pointer. */
const ledgerSpendRedeemers = (recording: L1Recording): Map<number, string> =>
  new Map(
    (
      recordedTransaction(recording).redeemers as
        | {
            redeemer: string;
            validator: { purpose: string; index: number };
          }[]
        | undefined
    )
      ?.filter(({ validator }) => validator.purpose === "spend")
      .map(({ validator, redeemer }) => [validator.index, redeemer]) ?? [],
  );

const spentAtOf = (
  recording: L1Recording,
  outRef: OutRef,
): Record<string, unknown> =>
  kupoMatch(recording, outRef).spent_at as Record<string, unknown>;

/** A copy of `name` whose Kupo `spent_at` for `outRef` has been rewritten. */
const withSpentAt = (
  name: L1RecordingName,
  outRef: OutRef,
  rewrite: (spentAt: Record<string, unknown>) => unknown,
): L1Recording => {
  const recording = loadL1Recording(name);
  const [match] = kupoExchange(
    recording,
    `/matches/${outRef.outputIndex.toString()}@${outRef.txHash}?resolve_hashes`,
  ).response.body as Record<string, unknown>[];
  const rewritten = rewrite(match!.spent_at as Record<string, unknown>);
  if (rewritten === undefined) delete match!.spent_at;
  else match!.spent_at = rewritten;
  return recording;
};

const readSpend = (recording: L1Recording, outRef: OutRef) =>
  fetchKupoSpend({
    kupoUrl: KUPO_URL,
    outRef,
    fetchImpl: recordedFetch(recording),
  });

const removal = loadL1Recording(REMOVAL);
const removalBlock = removal.transaction!.block;
const removalSpend = {
  point: { slot: removalBlock.slot, headerHash: removalBlock.id },
  transactionId: removal.transaction!.id,
};
const [queueNodeInput, queueRootInput, keyInput, correctionLockInput] =
  ledgerInputs(removal);

describe("Kupo spent_at, read off recorded preprod spends", () => {
  it.each([REMOVAL, SWEEP] as const)(
    "%s: Kupo reports input n - 1 - i with the spend redeemer at that mirrored pointer",
    (name) => {
      // Guards the recordings, not the reader: the premise every assertion
      // below rests on, read off what Kupo really served.
      const recording = loadL1Recording(name);
      const inputs = ledgerInputs(recording);
      const redeemers = ledgerSpendRedeemers(recording);
      expect(inputs.length).toBeGreaterThan(1);
      inputs.forEach((input, ledgerIndex) => {
        const mirrored = inputs.length - 1 - ledgerIndex;
        expect(spentAtOf(recording, input)).toStrictEqual({
          slot_no: recording.transaction!.block.slot,
          header_hash: recording.transaction!.block.id,
          transaction_id: recording.transaction!.id,
          input_index: mirrored,
          redeemer: redeemers.get(mirrored) ?? null,
        });
      });
    },
  );

  it("records every wrong answer the mirror can give on the removal", () => {
    const redeemers = ledgerSpendRedeemers(removal);
    // The ledger: three Plutus spends and one key spend.
    expect([...redeemers.keys()].sort()).toStrictEqual([0, 1, 3]);
    // A Plutus spend Kupo reports as redeemer-less.
    expect(redeemers.get(1)).toBeDefined();
    expect(spentAtOf(removal, queueRootInput!).redeemer).toBeNull();
    // A key spend Kupo credits with a script's redeemer.
    expect(redeemers.has(2)).toBe(false);
    expect(spentAtOf(removal, keyInput!).redeemer).toBe(redeemers.get(1));
    // Plutus spends Kupo credits with another script's redeemer.
    expect(spentAtOf(removal, queueNodeInput!).redeemer).toBe(redeemers.get(3));
    expect(redeemers.get(3)).not.toBe(redeemers.get(0));
    expect(spentAtOf(removal, correctionLockInput!).redeemer).toBe(
      redeemers.get(0),
    );
  });

  it.each([
    ["the queue node, a Plutus spend Kupo credits with another's redeemer", 0],
    ["the queue root, a Plutus spend Kupo reports as null", 1],
    ["a key spend Kupo credits with a script's redeemer", 2],
    ["the CorrectionLock, a Plutus spend Kupo credits with another's", 3],
  ])("reads the spend of %s", async (_label, ledgerIndex) => {
    await expect(
      readSpend(removal, ledgerInputs(removal)[ledgerIndex]!),
    ).resolves.toStrictEqual(removalSpend);
  });

  it("reads the spends of a sweep Kupo numbers backwards", async () => {
    const sweep = loadL1Recording(SWEEP);
    for (const input of ledgerInputs(sweep)) {
      await expect(readSpend(sweep, input)).resolves.toStrictEqual({
        point: {
          slot: sweep.transaction!.block.slot,
          headerHash: sweep.transaction!.block.id,
        },
        transactionId: sweep.transaction!.id,
      });
    }
  });

  it("reads an unspent output as unspent", async () => {
    // A reference input of the removal: referenced, never spent.
    await expect(
      readSpend(removal, {
        txHash:
          "cbeddfdf94390715ce39f0c79798faed1cfcf5bf31aa6a4b309edde3e7962927",
        outputIndex: 1,
      }),
    ).resolves.toBeNull();
  });

  it("finds every spend redeemer on the spending transaction, at the ledger's pointer", async () => {
    const fetchImpl = recordedFetch(removal);
    const spend = await fetchKupoSpend({
      kupoUrl: KUPO_URL,
      outRef: queueRootInput!,
      fetchImpl,
    });
    const ogmios = recordedOgmiosWebSocket(removal);
    const observed = await readOgmiosBlockTransaction({
      ogmiosUrl: OGMIOS_URL,
      intersection: await fetchKupoAncestorPoint({
        kupoUrl: KUPO_URL,
        slot: spend!.point.slot,
        fetchImpl,
      }),
      blockPoint: spend!.point,
      txHash: spend!.transactionId,
      webSocketFactory: (url) => new ogmios.WebSocket(url),
    });
    expect(observed.blockPoint).toStrictEqual({
      slot: removalBlock.slot,
      headerHash: removalBlock.id,
      blockNo: 5_220_548,
    });
    expect(observed.spentInputs).toStrictEqual(ledgerInputs(removal));
    const spendRedeemers = observed.redeemers.filter(
      ({ purpose }) => purpose === "spend",
    );
    expect(spendRedeemers).toStrictEqual(
      [...ledgerSpendRedeemers(removal)].map(([index, redeemer]) => ({
        purpose: "spend",
        index,
        redeemer,
      })),
    );
    // And on no spent input does the transaction agree with Kupo.
    observed.spentInputs!.forEach((input, ledgerIndex) => {
      const onTransaction =
        spendRedeemers.find(({ index }) => index === ledgerIndex)?.redeemer ??
        null;
      expect(spentAtOf(removal, input).redeemer).not.toBe(onTransaction);
    });
  });

  it.each<[string, (spentAt: Record<string, unknown>) => unknown]>([
    [
      "the ledger's own input index",
      (spentAt) => ({ ...spentAt, input_index: 1 }),
    ],
    [
      "the redeemer the transaction ran for it",
      (spentAt) => ({ ...spentAt, redeemer: "d87980" }),
    ],
    ["no redeemer at all", (spentAt) => ({ ...spentAt, redeemer: null })],
    [
      "an input index past the transaction's inputs",
      (spentAt) => ({ ...spentAt, input_index: 4_096 }),
    ],
  ])("reads the same spend when Kupo reports %s", async (_label, rewrite) => {
    await expect(
      readSpend(
        withSpentAt(REMOVAL, queueRootInput!, rewrite),
        queueRootInput!,
      ),
    ).resolves.toStrictEqual(removalSpend);
  });

  it.each<[string, (spentAt: Record<string, unknown>) => unknown, string]>([
    [
      "a redeemer that is not a string",
      (spentAt) => ({ ...spentAt, redeemer: 7 }),
      "Kupo spent_at.redeemer is not base16 data",
    ],
    [
      "a redeemer that is not base16",
      (spentAt) => ({ ...spentAt, redeemer: "d8798" }),
      "Kupo spent_at.redeemer is not base16 data",
    ],
    [
      "a redeemer that is empty",
      (spentAt) => ({ ...spentAt, redeemer: "" }),
      "Kupo spent_at.redeemer is not base16 data",
    ],
    [
      // Schema-legal for a row indexed before Kupo's v2.7.0 migration, and still
      // no spend the observer can locate.
      "a null transaction id",
      (spentAt) => ({ ...spentAt, transaction_id: null }),
      "Kupo spent_at.transaction_id is not a transaction id",
    ],
    [
      "a negative input index",
      (spentAt) => ({ ...spentAt, input_index: -1 }),
      "Kupo spent_at.input_index is not an input index",
    ],
    [
      "an input index that is not a number",
      (spentAt) => ({ ...spentAt, input_index: "2" }),
      "Kupo spent_at.input_index is not an input index",
    ],
    [
      "no spent_at field at all",
      () => undefined,
      "omitted its required spent_at field",
    ],
  ])("refuses a spent_at with %s", async (_label, rewrite, message) => {
    await expect(
      readSpend(
        withSpentAt(REMOVAL, queueRootInput!, rewrite),
        queueRootInput!,
      ),
    ).rejects.toThrow(message);
  });
});
