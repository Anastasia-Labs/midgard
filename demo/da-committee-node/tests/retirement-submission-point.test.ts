import { createServer } from "node:http";
import type { Socket } from "node:net";

import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";
import { WebSocketServer } from "ws";

import {
  committeeOwnedReadTransports,
  registerCommitteeReadOwner,
} from "../src/availability/committee-owned-read-transports.js";
import { readCommitteeRetirementSubmission } from "../src/l1/retirement-submission-point.js";

const txHash = "aa".repeat(32);
const inclusion = { slot: 100, blockHash: "bb".repeat(32), blockNo: 90 };
const boundary = { slot: 4000, blockHash: "cc".repeat(32), blockNo: 3000 };
const limits = {
  requestRefusalMs: 1000,
  httpResponseBytes: 4096,
  webSocketMessageBytes: 4096,
  rawUtxos: 32,
};
const probe = async (mode: "included" | "missing" | "foreign" | "moving") => {
  const peers = new Set<Socket>();
  let webSocketReads = 0;
  const server = createServer((request, response) => {
    response.setHeader("content-type", "application/json");
    response.end(
      JSON.stringify(
        request.url?.startsWith("/matches/")
          ? mode === "missing"
            ? []
            : [
                {
                  transaction_id: txHash,
                  output_index: 0,
                  created_at: {
                    slot_no: inclusion.slot,
                    header_hash: inclusion.blockHash,
                  },
                },
              ]
          : { slot_no: 99, header_hash: "dd".repeat(32) },
      ),
    );
  });
  server.on("connection", (socket) => {
    peers.add(socket);
    socket.on("close", () => peers.delete(socket));
  });
  const sockets = new WebSocketServer({ server });
  sockets.on("connection", (socket) => {
    socket.on("message", (data) => {
      const request = JSON.parse(data.toString()) as {
        id: number;
        method: string;
      };
      webSocketReads++;
      const result =
        request.method === "findIntersection"
          ? { intersection: { slot: 99, id: "dd".repeat(32) } }
          : {
              direction: "forward",
              block: {
                id: inclusion.blockHash,
                slot: inclusion.slot,
                height: inclusion.blockNo,
                transactions: [
                  {
                    id: mode === "foreign" ? "ee".repeat(32) : txHash,
                    inputs: [],
                    mint: {},
                    redeemers: [],
                  },
                ],
              },
              tip: {
                id: mode === "moving" ? "ff".repeat(32) : boundary.blockHash,
                slot: boundary.slot,
                height: boundary.blockNo,
              },
            };
      socket.send(JSON.stringify({ jsonrpc: "2.0", id: request.id, result }));
    });
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const address = server.address();
  if (!address || typeof address === "string")
    throw new Error("Missing fixture address");
  const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 3000 });
  const owner = committeeOwnedReadTransports();
  registerCommitteeReadOwner(scope, owner);
  try {
    const proof = await readCommitteeRetirementSubmission({
      kupoUrl: `http://127.0.0.1:${address.port}`,
      ogmiosUrl: `ws://127.0.0.1:${address.port}`,
      txHash,
      boundary,
      scope,
      limits,
    });
    return { proof, webSocketReads };
  } finally {
    scope.close();
    await owner.drain();
    owner.assertDrained();
    for (const socket of peers) socket.destroy();
    await new Promise<void>((resolve) => sockets.close(() => resolve()));
    await new Promise<void>((resolve) => server.close(() => resolve()));
  }
};

describe("retirement submission selected-chain evidence", () => {
  it("binds exact raw transaction inclusion height to the selected boundary using owned HTTP and WS", async () => {
    expect(await probe("included")).toEqual({
      proof: { point: inclusion, tip: boundary },
      webSocketReads: 2,
    });
  });
  it("holds absent Kupo evidence without opening a native replay session", async () => {
    expect(await probe("missing")).toEqual({ proof: null, webSocketReads: 0 });
  });
  it("refuses a Kupo proposal whose raw native block omits the transaction", async () => {
    await expect(probe("foreign")).rejects.toThrow("absent from Ogmios block");
  });
  it("refuses otherwise valid inclusion from a different selected boundary", async () => {
    await expect(probe("moving")).rejects.toThrow(
      "differs from the selected boundary",
    );
  });
});
