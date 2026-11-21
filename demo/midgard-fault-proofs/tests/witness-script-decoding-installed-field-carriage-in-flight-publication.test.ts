/**
 * The witness field's publication through the watcher's installed
 * application with no block between its own submissions: each publication
 * spends what the mempool leaves, and none is offered a coin a mempool
 * transaction spends.
 *
 * When its own wallet spends a confirmed publication while the next one
 * waits in our node's mempool, the first is published again; the one in the
 * mempool is the result for its chunk once it lands, and is never published
 * again.
 */
import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
}));
vi.mock("../src/workflow/deployment-manifest-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/workflow/deployment-manifest-binding.js")
  >()),
  bindFraudProofWorkflowDeployment: async () => hooks.binding,
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      actual.createFraudProofFamilyRawL1ObservationPort({
        ...input,
        authority: hooks.authority as Parameters<
          typeof actual.createFraudProofFamilyRawL1ObservationPort
        >[0]["authority"],
      }),
  };
});

import { layerMethod } from "@al-ft/midgard-test-support/method-layer";
import type { Emulator, UTxO } from "@lucid-evolution/lucid";

import { publicationsOf } from "./support/own-field-carriage-installed.js";
import {
  type LedgerTransaction,
  type LedgerWatch,
  removeOutput,
} from "./support/own-outputs-installed.js";
import {
  expectOwnFieldCarriage,
  type Harness,
  runWith,
  sitesOf,
} from "./support/witness-field-carriage-installed-run.js";

/** The hashes of the transactions in `harness`'s mempool. */
const mempoolHashes = (harness: Harness): ReadonlySet<string> =>
  new Set(Object.keys(harness.emulator.mempool).map((key) => key.slice(0, 64)));

const outRefOf = ({ txHash, outputIndex }: UTxO) =>
  `${txHash}#${outputIndex.toString()}`;

/** A decodable witness wide enough that the field takes three
 * publications. */
const WIDE_PAD_BYTES = 30_000;

describe("witness field publication through the mempool, in the installed watcher application", () => {
  let baseline: Awaited<ReturnType<typeof runWith>>["cost"];

  it("publishes the field's chunks one after another with no block between, each read offering no coin a mempool transaction spends, and certifies it: the cost the other row matches", async () => {
    /** Each publication, and the block height it was sent at. */
    const published: { transaction: LedgerTransaction; height: number }[] = [];
    /** Every coin a read offered that a mempool transaction spends. */
    const offeredSpent: string[] = [];
    let sites!: ReturnType<typeof sitesOf>;
    const { run, cost } = await runWith(hooks, () => [], {
      widePadBytes: WIDE_PAD_BYTES,
      decide: (transaction, { harness }) => {
        sites = sitesOf(harness);
        if (publicationsOf(transaction, sites).length > 0)
          published.push({
            transaction,
            height: harness.emulator.blockHeight,
          });
        return "accept";
      },
      during: (_, { harness, watch }) => {
        const spentInMempool = (watched: LedgerWatch) => {
          const pending = mempoolHashes(harness);
          return new Set(
            watched.accepted
              .filter(({ txHash }) => pending.has(txHash))
              .flatMap(({ inputs }) => inputs),
          );
        };
        const reads = layerMethod(
          harness.emulator,
          "getUtxos",
          (next) =>
            async function (this: Emulator, addressOrCredential) {
              const utxos = await next.call(this, addressOrCredential);
              const spent = spentInMempool(watch);
              for (const utxo of utxos)
                if (spent.has(outRefOf(utxo)))
                  offeredSpent.push(outRefOf(utxo));
              return utxos;
            },
        );
        return () => {
          reads.restore();
        };
      },
    });
    expect(run.convicted).toBe(true);
    expect(offeredSpent).toStrictEqual([]);
    // Three chunks or more, in a chain of publications sent in one block's
    // time, each after the one before it is in the mempool.
    const chunks = published.map(
      ({ transaction }) => publicationsOf(transaction, sites).length,
    );
    expect(published.length, chunks.join(",")).toBeGreaterThanOrEqual(2);
    expect(
      chunks.reduce((sum, count) => sum + count, 0),
      chunks.join(","),
    ).toBeGreaterThanOrEqual(3);
    expect(new Set(published.map(({ height }) => height)).size).toBe(1);
    const consumed = published.flatMap(({ transaction }) => transaction.inputs);
    expect(new Set(consumed).size).toBe(consumed.length);
    baseline = cost;
  }, 900_000);

  it("takes the publication waiting in the mempool as the result for its chunk when its own wallet spent an earlier one, publishing only the earlier one again", async () => {
    let first: LedgerTransaction | undefined;
    let waiting: LedgerTransaction | undefined;
    let spent: string | undefined;
    let replacement:
      | { readonly txHash: string; readonly waiting: boolean }
      | undefined;
    const moved = new Set<string>();
    const { run, watch, harness, cost } = await runWith(hooks, () => [], {
      widePadBytes: WIDE_PAD_BYTES,
      decide: (transaction, { harness }) => {
        if (publicationsOf(transaction, sitesOf(harness)).length === 0)
          return "accept";
        if (first === undefined) {
          first = transaction;
          return "accept";
        }
        if (waiting === undefined) {
          // A block takes the first publication just before the next is
          // sent; the next waits in the mempool.
          harness.emulator.awaitBlock();
          waiting = transaction;
          return "accept";
        }
        if (spent !== undefined && replacement === undefined)
          replacement = {
            txHash: transaction.txHash,
            waiting: mempoolHashes(harness).has(waiting.txHash),
          };
        return "accept";
      },
      // At the application's next observation, the next publication still
      // in the mempool: the prover's wallet spends the first publication's
      // output back to itself.
      during: ({ clock }, { harness, watch }) => {
        clock.state.onPoll = async () => {
          if (
            waiting === undefined ||
            spent !== undefined ||
            !mempoolHashes(harness).has(waiting.txHash)
          )
            return;
          const [output] = publicationsOf(first!, sitesOf(harness));
          const [txHash, index] = output!.outRef.split("#");
          const utxo = harness.emulator.ledger[`${txHash!}${index!}`]?.utxo;
          if (utxo === undefined) return;
          spent = output!.outRef;
          await removeOutput(
            watch,
            harness.proverLucid,
            utxo,
            harness.proverSigner.address,
          );
          moved.add(`${watch.accepted.at(-1)!.txHash}#0`);
        };
        return () => {
          clock.state.onPoll = undefined;
        };
      },
    });
    // The first publication's replacement was sent while the next one
    // waited in the mempool.
    expect(replacement).toMatchObject({ waiting: true });
    expect(run.convicted).toBe(true);
    const carriage = expectOwnFieldCarriage(watch, harness, moved);
    // The publication that waited landed and is the one its chunk has.
    const waitedOutputs = publicationsOf(waiting!, sitesOf(harness));
    const live = carriage.publications
      .flatMap(({ outputs }) => outputs)
      .filter(({ outRef }) => outRef !== spent);
    for (const { outRef, datum } of waitedOutputs)
      expect(
        live.filter((output) => output.datum === datum).map((o) => o.outRef),
      ).toEqual([outRef]);
    expect(new Set(live.map(({ datum }) => datum)).size).toBe(live.length);
    // Exactly one publication is added: the first one's replacement.
    const publishes = (rows: readonly { kind: string }[]) =>
      rows.filter(({ kind }) => kind === "publish").length;
    expect(publishes(cost)).toBe(publishes(baseline) + 1);
  }, 900_000);
});
