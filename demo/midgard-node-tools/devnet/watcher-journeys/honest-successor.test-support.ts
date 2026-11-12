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

export const schedulerPolicyId = "d4".repeat(28);

/**
 * A state-queue output, created at `slot` and spent at `spentAt` by
 * `spentBy`, which Kupo leaves unnamed when absent. `txHash` names the
 * transaction that created it (by default one numbered by its place), and
 * `attested` sets its DA attestation.
 */
export type IndexedOutput = Readonly<{
  block: JourneyBlock;
  slot: number;
  spentAt?: number;
  spentBy?: string;
  txHash?: string;
  attested?: boolean;
}>;

/** A scheduler output appointing `operator` from `startTime`, made at `slot`. */
export type IndexedAppointment = Readonly<{
  operator: string;
  startTime: number;
  slot: number;
}>;

const schedulerMatches = (appointments: readonly IndexedAppointment[]) =>
  appointments.map(({ operator, startTime, slot }, index) => ({
    transaction_id: (index + 1).toString(16).padStart(64, "e"),
    output_index: 0,
    address: "addr_test1wscheduler",
    value: {
      coins: 2_000_000,
      assets: { [`${schedulerPolicyId}.${SDK.SCHEDULER_ASSET_NAME}`]: 1 },
    },
    datum_type: "inline",
    datum: Data.to(
      { ActiveOperator: { operator, start_time: BigInt(startTime) } },
      SDK.SchedulerDatum,
    ),
    created_at: { slot_no: slot },
    spent_at: null,
  }));

/**
 * The stack's Kupo, unpruned: every state-queue output, spent ones too, and
 * every scheduler appointment.
 */
export const kupo = (
  outputs: () => readonly IndexedOutput[],
  appointments: () => readonly IndexedAppointment[] = () => [],
) =>
  serve((url) => {
    const pattern = decodeURIComponent(url.pathname.slice("/matches/".length));
    const [policy, name] = pattern.split(".");
    if (!url.searchParams.has("resolve_hashes"))
      return { status: 400, body: { hint: "datums unresolved" } };
    const after = url.searchParams.get("created_after");
    if (policy === schedulerPolicyId)
      return {
        body: schedulerMatches(appointments()).filter(
          (match) =>
            name === SDK.SCHEDULER_ASSET_NAME &&
            (after === null || match.created_at.slot_no > Number(after)),
        ),
      };
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
      ...outputs().map(
        ({ block, slot, spentAt, spentBy, txHash, attested }, index) => {
          const assetName = `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${block.headerHash}`;
          const node: SDK.StateQueueNode = {
            header: block.header,
            block_time: block.header.endTime,
            da_attestation:
              attested === true
                ? { Attested: { commitment_hash: "ee".repeat(32) } }
                : SDK.NO_DA_ATTESTATION,
            proven_fraud: null,
          };
          return {
            transaction_id:
              txHash ?? (index + 1).toString(16).padStart(64, "0"),
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
            spent_at:
              spentAt === undefined
                ? null
                : {
                    slot_no: spentAt,
                    ...(spentBy === undefined
                      ? {}
                      : { transaction_id: spentBy }),
                  },
            assetName,
          };
        },
      ),
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
