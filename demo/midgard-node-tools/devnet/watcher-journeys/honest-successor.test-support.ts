import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { createSyntheticStateQueueHeader } from "midgard-watcher/tests/support/state-queue-observation-fixture.commit-transaction";

import type { JourneyBlock } from "./fixture.js";

const servers: Server[] = [];

/** Closes every server `serve` started; call after each test. */
export const closeServers = () =>
  Promise.all(
    servers
      .splice(0)
      .map(
        (server) =>
          new Promise<void>((resolve) => server.close(() => resolve())),
      ),
  );

export const serve = async (
  handler: (url: URL) => { status?: number; body: unknown },
) => {
  const server = createServer((request, response) => {
    const answer = handler(new URL(request.url ?? "/", "http://127.0.0.1"));
    response.statusCode = answer.status ?? 200;
    response.setHeader("content-type", "application/json");
    response.end(JSON.stringify(answer.body));
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  servers.push(server);
  return `http://127.0.0.1:${(server.address() as AddressInfo).port.toString()}`;
};

export const policyId = "c3".repeat(28);
export const honestOperator = "a1".repeat(28);
export const operator2 = "b2".repeat(28);

const headerHashOf = (header: SDK.Header) =>
  Effect.runPromise(SDK.hashBlockHeader(header));

/** A header on `parent` by `operator`, at `blockSlot` for distinct hashes. */
export const headerOn = async (
  parent: string,
  operator: string,
  blockSlot: bigint,
): Promise<JourneyBlock> => {
  const header: SDK.Header = {
    ...createSyntheticStateQueueHeader(),
    prevHeaderHash: parent,
    operatorVkey: operator,
    blockSlot,
    expectedNetworkId: 0n,
  };
  return {
    header,
    headerHash: await headerHashOf(header),
    payloadEnvelopeCbor: Buffer.alloc(0),
  };
};

export type IndexedOutput = Readonly<{ block: JourneyBlock; slot: number }>;

/** The stack's Kupo, unpruned: every state-queue output, spent ones too. */
export const kupo = (outputs: () => readonly IndexedOutput[]) =>
  serve((url) => {
    const pattern = decodeURIComponent(url.pathname.slice("/matches/".length));
    const [policy, name] = pattern.split(".");
    if (!url.searchParams.has("resolve_hashes"))
      return { status: 400, body: { hint: "datums unresolved" } };
    const after = url.searchParams.get("created_after");
    const root = {
      transaction_id: "00".repeat(32),
      output_index: 0,
      address: "addr_test1wroot",
      value: { coins: 2_000_000, assets: { [`${policyId}.4d524f4f54`]: 1 } },
      datum_type: "inline",
      datum: "d87980",
      created_at: { slot_no: 1 },
      spent_at: null,
    };
    const matches = [
      ...(name === "*" ? [root] : []),
      ...outputs().map(({ block, slot }, index) => {
        const assetName = `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${block.headerHash}`;
        const node: SDK.StateQueueNode = {
          header: block.header,
          block_time: block.header.endTime,
          da_attestation: "Unattested",
          proven_fraud: null,
        };
        return {
          transaction_id: (index + 1).toString(16).padStart(64, "0"),
          output_index: 1,
          address: "addr_test1wqueue",
          value: {
            coins: 2_000_000,
            assets: { [`${policyId}.${assetName}`]: 1 },
          },
          datum_type: "inline",
          datum: Data.to(
            SDK.nodeViewToLinkedListDatum({
              key: { Key: { key: block.headerHash } },
              next: "Empty",
              data: Data.castTo(node, SDK.StateQueueNode),
            }),
            SDK.LinkedListDatum,
          ),
          created_at: { slot_no: slot },
          spent_at: null,
          assetName,
        };
      }),
    ].filter(
      (match) =>
        policy === policyId &&
        (name === "*" || ("assetName" in match && match.assetName === name)) &&
        (after === null || match.created_at.slot_no > Number(after)),
    );
    return { body: matches };
  });

export const faultHold = (headerHash: string) => ({
  ready: false,
  reasons: ["landed_block_invalid"],
  l1Follower: {
    readiness: [
      {
        reason: "landed_block_invalid",
        detail: `block ${headerHash} replays to root ${"00".repeat(32)}, its header commits ${"11".repeat(32)}`,
      },
    ],
  },
});
