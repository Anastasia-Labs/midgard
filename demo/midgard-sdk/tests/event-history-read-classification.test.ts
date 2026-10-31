import { Data, datumToHash, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { OutputReference } from "../src/common.js";
import {
  type EventHistoryDeployment,
  EventHistoryNode,
  type EventHistoryPayload,
  eventHistoryReadErrorOf,
  fetchDepositUTxOsProgram,
  fetchEventHistoryWitness,
  prepareEventHistoryPayload,
  readEventHistoryOrders,
} from "../src/user-events/index.js";

type ClassifiedRead = {
  readonly message: string;
  readonly classification?: string;
  readonly retryable?: boolean;
};

const owner = "aa".repeat(28);
const id = { transactionId: "bb".repeat(32), outputIndex: 0n };
const auth = { PublicKeyCredential: [owner] as [string] };
const deployment: EventHistoryDeployment = {
  policyId: "dd".repeat(28),
  address: "history-classification-fixture",
  retentionAddress: "history-classification-retention",
  inlineLimitBytes: 512n,
};
const key = datumToHash(Data.to(id, OutputReference));
const root = (next: string | null, txHash = "11".repeat(32)): UTxO => ({
  txHash,
  outputIndex: 0,
  address: deployment.address,
  assets: { lovelace: 3_000_000n, [deployment.policyId]: 1n },
  datum: Data.to(
    { position: "Root", next, protected_until: 0n, payload: "RootContent" },
    EventHistoryNode,
  ),
});

/** A listed external Deposit order and the retention output holding its
 * payload, the shape whose two reads can straddle a retirement. */
const externalDeposit = () => {
  const payload: EventHistoryPayload = {
    DepositPayload: {
      event: {
        id,
        info: {
          l2_address: { paymentCredential: auth, stakeCredential: null },
          l2_network_id: 0n,
          l2_datum: new Map<Data, Data>([["bbaa", "ab".repeat(600)]]),
        },
      },
    },
  };
  const plan = prepareEventHistoryPayload(payload, auth, {
    inlineLimitBytes: deployment.inlineLimitBytes,
    maxPayloadBytes: 15000n,
    maxPayloadNodes: 1024n,
  });
  if (plan.kind !== "External")
    throw new Error("expected the payload to be stored externally");
  const node: EventHistoryNode = {
    position: { Key: [key] },
    next: null,
    protected_until: 0n,
    payload: {
      Order: {
        facts: {
          event_id: id,
          inclusion_time: 1_000n,
          location: plan.location,
          structural_lovelace: 2_000_000n,
          structural_refund_key: owner,
        },
      },
    },
  };
  const order: UTxO = {
    txHash: "22".repeat(32),
    outputIndex: 1,
    address: deployment.address,
    assets: { lovelace: 7_000_000n, [deployment.policyId + key]: 1n },
    datum: Data.to(node, EventHistoryNode),
  };
  const retained: UTxO = {
    txHash: "33".repeat(32),
    outputIndex: 0,
    address: deployment.retentionAddress,
    assets: { lovelace: 3_000_000n },
    datum: plan.datumCbor,
  };
  return { order, retained, listed: [root(key), order] };
};

/** Serves the history list snapshots in turn (the last one for every later
 * read) and one retention snapshot, counting the reads of each. */
const provider = (
  lists: readonly (readonly UTxO[])[],
  retention: readonly UTxO[],
) => {
  const reads = { list: 0, retention: 0 };
  return {
    reads,
    utxosAt: async (address: string) => {
      if (address === deployment.retentionAddress) {
        reads.retention++;
        return [...retention];
      }
      reads.list++;
      return [...lists[Math.min(reads.list, lists.length) - 1]!];
    },
  };
};

const thrown = (run: () => unknown): ClassifiedRead => {
  try {
    run();
  } catch (error) {
    return error as ClassifiedRead;
  }
  throw new Error("expected the read to refuse");
};

describe("event-history reads take their two provider reads as one snapshot", () => {
  const { listed } = externalDeposit();
  const retired = [root(null, "44".repeat(32))];

  it("reads the list again when it moved between the list and retention reads", async () => {
    const source = provider([listed, retired], []);
    const events = await Effect.runPromise(
      fetchDepositUTxOsProgram(source, deployment),
    );
    expect(events).toEqual([]);
    expect(source.reads).toEqual({ list: 2, retention: 1 });
  });

  it("resolves the witness from the list as it stands after the move", async () => {
    const source = provider([listed, retired], []);
    const witness = await fetchEventHistoryWitness(source, deployment, id);
    expect(witness.kind).toBe("Absent");
    expect(witness.anchor.utxo).toBe(retired[0]);
    expect(source.reads).toEqual({ list: 2, retention: 1 });
  });

  it("still refuses an order whose retained data is missing from an unchanged list", async () => {
    const source = provider([listed], []);
    const failure = await Effect.runPromise(
      Effect.flip(fetchDepositUTxOsProgram(source, deployment)),
    );
    const cause = failure.cause as ClassifiedRead;
    expect(cause.message).toBe(
      "Authenticated retained event data is unavailable on L1",
    );
    expect(cause.classification).toBe("snapshot-stale");
    expect(cause.retryable).toBe(true);
    expect(eventHistoryReadErrorOf(failure)).toBe(cause);
    expect(source.reads).toEqual({ list: 2, retention: 1 });
    await expect(
      fetchEventHistoryWitness(provider([listed], []), deployment, id),
    ).rejects.toMatchObject({ classification: "snapshot-stale" });
  });

  it("stops re-reading a list that keeps moving", async () => {
    const moving = [0, 1, 2, 3, 4].map((index) => [
      { ...listed[0]!, txHash: index.toString(16).padStart(64, "0") },
      listed[1]!,
    ]);
    const source = provider(moving, []);
    const failure = await Effect.runPromise(
      Effect.flip(fetchDepositUTxOsProgram(source, deployment)),
    );
    expect((failure.cause as ClassifiedRead).classification).toBe(
      "snapshot-stale",
    );
    expect(source.reads).toEqual({ list: 3, retention: 3 });
  });
});

describe("event-history read refusals say whether a later read can clear them", () => {
  const { order, retained, listed } = externalDeposit();

  it.each([
    ["a missing Root", [order]],
    ["a missing successor", [root(key)]],
  ] as const)("classifies %s as snapshot-stale", (_case, utxos) => {
    const error = thrown(() =>
      readEventHistoryOrders(utxos, [retained], deployment),
    );
    expect(error.message).toMatch(/refresh/);
    expect(error.classification).toBe("snapshot-stale");
    expect(error.retryable).toBe(true);
  });

  it.each([
    [
      "a key its token does not authenticate",
      [
        root(key),
        {
          ...order,
          assets: { ...order.assets, [deployment.policyId + key]: 2n },
        },
      ],
      /complete key/,
    ],
    [
      "a duplicated key",
      [root(key), order, { ...order, txHash: "55".repeat(32) }],
      /Duplicate/,
    ],
  ] as const)(
    "classifies %s as authenticated-state-invalid without re-reading",
    async (_case, utxos, message) => {
      const error = thrown(() =>
        readEventHistoryOrders(utxos, [retained], deployment),
      );
      expect(error.message).toMatch(message);
      expect(error.classification).toBe("authenticated-state-invalid");
      expect(error.retryable).toBe(false);
      const source = provider([utxos], [retained]);
      const failure = await Effect.runPromise(
        Effect.flip(fetchDepositUTxOsProgram(source, deployment)),
      );
      expect(failure.message).toMatch(message);
      expect((failure.cause as ClassifiedRead).classification).toBe(
        "authenticated-state-invalid",
      );
      expect(source.reads.list).toBe(1);
    },
  );

  it("opens the order when both reads agree", async () => {
    const source = provider([listed], [retained]);
    const events = await Effect.runPromise(
      fetchDepositUTxOsProgram(source, deployment),
    );
    expect(events.map((event) => event.utxo)).toEqual([order]);
    expect(source.reads).toEqual({ list: 1, retention: 1 });
  });
});
