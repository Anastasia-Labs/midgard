/**
 * The provider against a node that answers from memory (design D9), over
 * real HTTP: a tx the node acknowledged and then reports `not_found` is
 * resubmitted exactly once, and a spent-input refusal (design D6) for a tx
 * the node already holds resolves to that tx's status instead of failing.
 */
import { once } from "node:events";
import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";

import { decodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import { afterEach, describe, expect, it } from "vitest";

import { MidgardNodeProvider } from "../src/index.js";
import { protocolInfoJson, submitTx } from "./provider.protocol-info.js";

type Reply = { readonly status: number; readonly body: unknown };

const queued: Reply = {
  status: 202,
  body: { txId: submitTx.txId, status: "queued", duplicate: false },
};
const statusReply = (status: string): Reply => ({
  status: status === "not_found" ? 404 : 200,
  body: { txId: submitTx.txId, status },
});
const lost = statusReply("not_found");
const spentInput: Reply = {
  status: 409,
  body: {
    error: "E_INPUT_SPENT",
    reason: "not_in_ledger",
    message: `Input 00 is spent or unknown to this node; read /tx-status for transaction ${submitTx.txId} before treating it as failed`,
    txId: submitTx.txId,
    outRef: "00",
  },
};

const servers: Server[] = [];

afterEach(async () => {
  for (const server of servers.splice(0)) {
    server.closeAllConnections();
    await new Promise<void>((resolve) => server.close(() => resolve()));
  }
});

/**
 * A node double: `/submit` and `/tx-status` answer their replies in order,
 * repeating the last one. It records every transaction body `/submit` got.
 */
const startNode = async (routes: {
  readonly submit: readonly Reply[];
  readonly txStatus: readonly Reply[];
}) => {
  const submitted: string[] = [];
  const statusReads: string[] = [];
  const next = (replies: readonly Reply[], index: number) =>
    replies[Math.min(index, replies.length - 1)]!;
  const server = createServer((request, response) => {
    const url = new URL(request.url ?? "/", "http://node");
    const chunks: Buffer[] = [];
    request.on("data", (chunk: Buffer) => chunks.push(chunk));
    request.on("end", () => {
      let reply: Reply;
      if (url.pathname === "/protocol-info") {
        reply = { status: 200, body: protocolInfoJson };
      } else if (url.pathname === "/submit" && request.method === "POST") {
        const { transactionCbor } = decodeMidgardProofSubmission(
          Buffer.concat(chunks),
        );
        reply = next(routes.submit, submitted.length);
        submitted.push(transactionCbor.toString("hex"));
      } else if (url.pathname === "/tx-status") {
        reply = next(routes.txStatus, statusReads.length);
        statusReads.push(url.searchParams.get("tx_hash") ?? "");
      } else {
        reply = { status: 404, body: { error: "not found" } };
      }
      response.writeHead(reply.status, { "content-type": "application/json" });
      response.end(JSON.stringify(reply.body));
    });
  });
  servers.push(server);
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const { port } = server.address() as AddressInfo;
  const provider = await MidgardNodeProvider.create({
    endpoint: `http://127.0.0.1:${port.toString()}`,
  });
  return { provider, submitted, statusReads };
};

describe("MidgardNodeProvider against a node that acknowledges from memory", () => {
  it("resubmits a tx the node lost after a 202 exactly once", async () => {
    const node = await startNode({
      submit: [queued],
      // Lost after the 202, held again after the resubmit, then lost again.
      txStatus: [lost, statusReply("queued"), lost],
    });

    await expect(node.provider.submitTx(submitTx.txHex)).resolves.toMatchObject(
      { txId: submitTx.txId, httpStatus: 202 },
    );
    await expect(node.provider.getTxStatus(submitTx.txId)).resolves.toEqual({
      kind: "queued",
      txId: submitTx.txId,
    });
    expect(node.submitted).toEqual([submitTx.txHex, submitTx.txHex]);

    // Lost a second time: reported, not resubmitted again.
    for (let read = 0; read < 3; read += 1) {
      await expect(node.provider.getTxStatus(submitTx.txId)).resolves.toEqual({
        kind: "not_found",
        txId: submitTx.txId,
      });
    }
    expect(node.submitted).toHaveLength(2);
  });

  it("never resubmits a tx it did not submit", async () => {
    const node = await startNode({ submit: [queued], txStatus: [lost] });

    await expect(node.provider.getTxStatus(submitTx.txId)).resolves.toEqual({
      kind: "not_found",
      txId: submitTx.txId,
    });
    expect(node.submitted).toEqual([]);
  });

  it("retries a resubmission the node refused with 503 on the next read", async () => {
    const node = await startNode({
      submit: [queued, { status: 503, body: { error: "busy" } }, queued],
      txStatus: [lost, lost, statusReply("queued")],
    });

    await node.provider.submitTx(submitTx.txHex);
    await expect(node.provider.getTxStatus(submitTx.txId)).resolves.toEqual({
      kind: "not_found",
      txId: submitTx.txId,
    });
    await expect(node.provider.getTxStatus(submitTx.txId)).resolves.toEqual({
      kind: "queued",
      txId: submitTx.txId,
    });
    expect(node.submitted).toHaveLength(3);
  });

  it("resolves a spent-input refusal for an accepted tx to accepted", async () => {
    const node = await startNode({
      submit: [spentInput],
      txStatus: [statusReply("accepted")],
    });

    await expect(node.provider.submitTx(submitTx.txHex)).resolves.toEqual({
      txId: submitTx.txId,
      status: "accepted",
      httpStatus: 200,
      duplicate: true,
    });
    expect(node.statusReads).toEqual([submitTx.txId]);
  });

  it("resolves a spent-input refusal for a queued tx to queued", async () => {
    const node = await startNode({
      submit: [spentInput],
      txStatus: [statusReply("queued")],
    });

    await expect(node.provider.submitTx(submitTx.txHex)).resolves.toEqual({
      txId: submitTx.txId,
      status: "queued",
      httpStatus: 200,
      duplicate: true,
    });
  });

  it("fails a spent-input refusal for a tx the node does not hold", async () => {
    const node = await startNode({ submit: [spentInput], txStatus: [lost] });

    await expect(node.provider.submitTx(submitTx.txHex)).rejects.toMatchObject({
      statusCode: 409,
      message: "E_INPUT_SPENT",
      retryable: false,
    });
    expect(node.submitted).toHaveLength(1);
  });

  it("keeps a 503 retryable", async () => {
    const node = await startNode({
      submit: [{ status: 503, body: { error: "busy" } }],
      txStatus: [lost],
    });

    await expect(node.provider.submitTx(submitTx.txHex)).rejects.toMatchObject({
      statusCode: 503,
      retryable: true,
    });
  });
});
