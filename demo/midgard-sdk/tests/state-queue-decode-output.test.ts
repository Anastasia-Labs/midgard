/**
 * `decodeStateQueueOutput`, the one reading of a state-queue output every
 * role shares (plan §5.5 P1, N2), in both polarities: an output that is a
 * queue element decodes with its key, link, header hash and DA status; an
 * output carrying nothing under the queue policy is not a queue output; a
 * queue output that is malformed decodes as `invalid` (or with its
 * problems), never as a valid element and never dropped.
 */
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as SDK from "../src/index.js";

const POLICY = "71".repeat(28);
const OTHER_POLICY = "72".repeat(28);
const ROOT_ASSET = SDK.STATE_QUEUE_ROOT_ASSET_NAME;
const PREFIX = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX;

const hex32 = (n: number): string => n.toString(16).padStart(64, "0");

const header = (nonce: number, protocolVersion = 1n): SDK.Header => ({
  prevUtxosRoot: hex32(nonce),
  utxosRoot: hex32(nonce),
  withdrawalsRoot: hex32(nonce),
  forcedTransactionsRoot: hex32(nonce),
  transactionsRoot: hex32(nonce),
  depositsRoot: hex32(nonce),
  transitionTraceRoot: hex32(nonce),
  eventToStepRoot: hex32(nonce),
  validationTracesRoot: hex32(nonce),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: 1_000n,
  endTime: 2_000n + BigInt(nonce),
  blockSlot: BigInt(nonce),
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "00".repeat(28),
  operatorVkey: "ab".repeat(28),
  protocolVersion,
});

const datum = (value: SDK.LinkedListDatum): Uint8Array =>
  Buffer.from(Data.to(value, SDK.LinkedListDatum), "hex");

const rootDatum = (headerHash: string, link: string | null) =>
  datum({
    data: {
      Root: {
        data: Data.castTo(
          {
            headerHash,
            prevHeaderHash: "00".repeat(28),
            utxoRoot: hex32(0),
            startTime: 0n,
            endTime: 7_000n,
            protocolVersion: 1n,
          },
          SDK.ConfirmedState,
        ),
      },
    },
    link,
  });

const nodeDatum = (
  value: SDK.Header,
  status: SDK.DaAvailabilityStateQueueStatus,
  link: string | null,
) =>
  datum({
    data: {
      Node: {
        data: Data.castTo(
          { header: value, da_attestation: status, proven_fraud: null },
          SDK.StateQueueNode,
        ),
      },
    },
    link,
  });

const value = (
  assets: ReadonlyArray<readonly [string, string, bigint]>,
  inline: Uint8Array | null,
): SDK.StateQueueOutputValue => {
  const map = new Map<string, Map<string, bigint>>();
  for (const [policy, name, quantity] of assets) {
    const names = map.get(policy) ?? new Map<string, bigint>();
    names.set(name, quantity);
    map.set(policy, names);
  }
  return { assets: map, datum: inline };
};

const decode = (output: SDK.StateQueueOutputValue) =>
  SDK.decodeStateQueueOutput(output, POLICY);

describe("decodeStateQueueOutput: queue elements", () => {
  it("reads the root: its confirmed header hash, link and end time", () => {
    const link = "cd".repeat(28);
    expect(
      decode(
        value([[POLICY, ROOT_ASSET, 1n]], rootDatum("ef".repeat(28), link)),
      ),
    ).toMatchObject({
      kind: "root",
      assetName: ROOT_ASSET,
      nodeKey: null,
      nextKey: link,
      headerHash: "ef".repeat(28),
      daStatus: null,
      endTimeMs: 7_000n,
      problems: [],
    });
  });

  it("reads a node: its key is its header hash, which equals the SDK's block hash", async () => {
    const block = header(1);
    const hash = SDK.stateQueueHeaderHash(block);
    expect(hash).toBe(await Effect.runPromise(SDK.hashBlockHeader(block)));
    const decoded = decode(
      value(
        [[POLICY, PREFIX + hash, 1n]],
        nodeDatum(
          block,
          { Attested: { commitment_hash: "11".repeat(32) } },
          null,
        ),
      ),
    );
    expect(decoded).toMatchObject({
      kind: "node",
      assetName: PREFIX + hash,
      nodeKey: hash,
      nextKey: null,
      headerHash: hash,
      daStatus: `Attested:${"11".repeat(32)}`,
      endTimeMs: 2_001n,
      problems: [],
    });
    expect(decoded!.datum).not.toBeNull();
  });
});

describe("decodeStateQueueOutput: what is not a valid queue element", () => {
  it("is not a queue output when nothing is under the queue policy (third-party payment)", () => {
    expect(decode(value([], null))).toBeNull();
    expect(
      decode(
        value(
          [[OTHER_POLICY, ROOT_ASSET, 1n]],
          rootDatum("ef".repeat(28), null),
        ),
      ),
    ).toBeNull();
  });

  it.each([
    [
      "a second token under the policy",
      [
        [POLICY, ROOT_ASSET, 1n],
        [POLICY, PREFIX + "00", 1n],
      ],
    ],
    [
      "a token of another policy beside it",
      [
        [POLICY, ROOT_ASSET, 1n],
        [OTHER_POLICY, "00", 1n],
      ],
    ],
    ["a quantity of two", [[POLICY, ROOT_ASSET, 2n]]],
  ] as const)("is malformed_nft with %s", (_, assets) => {
    expect(
      decode(value(assets, rootDatum("ef".repeat(28), null))),
    ).toMatchObject({
      kind: "invalid",
      assetName: ROOT_ASSET,
      problems: ["malformed_nft"],
      datum: null,
    });
  });

  it("is malformed_datum without an inline datum, with a non-list datum or a non-V1 node", () => {
    const block = header(2, 2n);
    const hash = "99".repeat(28);
    for (const [name, inline] of [
      [ROOT_ASSET, null],
      [ROOT_ASSET, Buffer.from(Data.to(42n), "hex")],
      [PREFIX + hash, nodeDatum(block, "Unattested", null)],
    ] as const)
      expect(decode(value([[POLICY, name, 1n]], inline))).toMatchObject({
        kind: "invalid",
        problems: ["malformed_datum"],
      });
  });

  it("keeps a node whose asset name is not its header hash, with each problem named", () => {
    const block = header(3);
    const hash = SDK.stateQueueHeaderHash(block);
    const wrong = "99".repeat(28);
    const node = (name: string) =>
      decode(value([[POLICY, name, 1n]], nodeDatum(block, "Unattested", null)));
    expect(node(PREFIX + wrong)).toMatchObject({
      kind: "node",
      headerHash: hash,
      nodeKey: wrong,
      problems: ["linked_list_key_mismatch", "block_asset_suffix_mismatch"],
    });
    expect(node(hash)).toMatchObject({
      kind: "node",
      problems: ["block_asset_prefix_mismatch"],
    });
  });
});
