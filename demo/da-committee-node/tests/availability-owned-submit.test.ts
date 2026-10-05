import { rmSync } from "node:fs";
import { createServer } from "node:http";
import type { Socket } from "node:net";

import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import { committeeOwnedAvailabilitySubmit } from "../src/l1/availability-owned-submit.js";
import {
  dirs,
  journals,
  scene,
} from "./helpers/availability-sdk-read-scope.js";

afterEach(() => {
  journals.splice(0).forEach((journal) => journal.close());
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true }));
});

const fixture = async (loseResponse: boolean) => {
  const s = scene();
  let drop = loseResponse;
  const received: string[] = [];
  const peers = new Set<Socket>();
  const server = createServer((request, response) => {
    void (async () => {
      const chunks: Buffer[] = [];
      for await (const chunk of request) chunks.push(Buffer.from(chunk));
      const body = JSON.parse(Buffer.concat(chunks).toString()) as {
        params: { transaction: { cbor: string } };
      };
      received.push(body.params.transaction.cbor);
      if (drop) return;
      const tx = CML.Transaction.from_cbor_hex(body.params.transaction.cbor);
      response.setHeader("content-type", "application/json");
      response.end(
        JSON.stringify({
          jsonrpc: "2.0",
          id: null,
          result: {
            transaction: { id: CML.hash_transaction(tx.body()).to_hex() },
          },
        }),
      );
    })().catch((error: Error) => response.destroy(error));
  });
  server.on("connection", (socket) => {
    peers.add(socket);
    socket.on("close", () => peers.delete(socket));
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const address = server.address();
  if (!address || typeof address === "string")
    throw new Error("Missing address");
  const url = `http://127.0.0.1:${address.port}`;
  const breach = vi.fn();
  const submit = (signedCbor: string) =>
    committeeOwnedAvailabilitySubmit({
      config: {
        network: "Custom",
        contractDeploymentInfo: {
          manifestId: s.context.deploymentIdentity,
        },
      },
      kupoUrl: url,
      ogmiosUrl: url,
      journal: s.journal,
      actorId: s.context.actor,
      signedCbor,
      breach,
    });
  const context = { ...s.context, nowMs: () => 1000, submit };
  return {
    s,
    context,
    submit,
    received,
    breach,
    peers,
    respond: () => {
      drop = false;
    },
    close: async () => {
      for (const socket of peers) socket.destroy();
      await new Promise<void>((resolve) => server.close(() => resolve()));
    },
  };
};

it("submits only the SDK's exact persisted signed bytes through the owned provider", async () => {
  const f = await fixture(false);
  try {
    expect(
      (await SDK.runDaAvailabilityOperation(f.context, f.s.operation)).status,
    ).toBe("submitted");
    const pending = f.s.journal.pending(
      f.context.deploymentIdentity,
      f.context.actor,
    );
    expect(f.received).toEqual([pending[0]!.intent.signedCbor]);
    expect(f.s.sign).toHaveBeenCalledTimes(1);
    expect(f.breach).not.toHaveBeenCalled();
  } finally {
    await f.close();
  }
});

it("keeps accepted-but-response-lost bytes and reservations for SDK recovery", async () => {
  const f = await fixture(true);
  try {
    const first = await SDK.runDaAvailabilityOperation(
      f.context,
      f.s.operation,
    );
    expect(first.status).toBe("waiting");
    const pending = f.s.journal.pending(
      f.context.deploymentIdentity,
      f.context.actor,
    );
    expect(pending).toHaveLength(1);
    expect(f.received).toEqual([pending[0]!.intent.signedCbor]);
    const refs = f.s.journal.reservedOutRefs(f.context.actor);
    expect(refs.length).toBeGreaterThan(0);
    await expect(f.submit("00")).rejects.toThrow("exact persisted");
    expect(f.received).toHaveLength(1);
    expect(f.s.journal.reservedOutRefs(f.context.actor)).toEqual(refs);
    f.respond();
    expect(
      (
        await SDK.runDaAvailabilityOperation(
          { ...f.context, nowMs: () => 2000 },
          f.s.operation,
        )
      ).status,
    ).toBe("submitted");
    expect(f.received).toEqual([
      pending[0]!.intent.signedCbor,
      pending[0]!.intent.signedCbor,
    ]);
    expect(f.s.journal.reservedOutRefs(f.context.actor)).toEqual(refs);
    const expired = await SDK.runDaAvailabilityOperation(
      {
        ...f.context,
        nowMs: () => 2000,
        observe: async () => ({
          status: "unspent" as const,
          currentSlot: 1000,
        }),
      },
      f.s.operation,
    );
    expect(expired.status).toBe("expired");
    expect(f.s.journal.reservedOutRefs(f.context.actor)).toEqual([]);
    expect(f.s.journal.get(pending[0]!.intent.id)?.intent.signedCbor).toBe(
      pending[0]!.intent.signedCbor,
    );
    expect(f.s.sign).toHaveBeenCalledTimes(1);
  } finally {
    await f.close();
  }
});

it("holds persisted bytes when SDK evidence is aborted before rebroadcast", async () => {
  const f = await fixture(false);
  try {
    await SDK.runDaAvailabilityOperation(f.context, f.s.operation);
    const pending = f.s.journal.pending(
      f.context.deploymentIdentity,
      f.context.actor,
    );
    const refs = f.s.journal.reservedOutRefs(f.context.actor);
    let monotonic = 0;
    const expired = SDK.createDaAvailabilityReadScope({
      attemptTimeoutMs: 1000,
      monotonicMs: () => monotonic,
    });
    monotonic = 1001;
    const result = await SDK.runDaAvailabilityOperation(
      {
        ...f.context,
        observe: async () => {
          expired.assertCurrent();
          throw new Error("unreachable");
        },
      },
      f.s.operation,
    );
    expect(result.status).toBe("waiting");
    expect(f.received).toHaveLength(1);
    expect(
      f.s.journal.pending(f.context.deploymentIdentity, f.context.actor)[0]!
        .intent.signedCbor,
    ).toBe(pending[0]!.intent.signedCbor);
    expect(f.s.journal.reservedOutRefs(f.context.actor)).toEqual(refs);
    expect(f.s.sign).toHaveBeenCalledTimes(1);
  } finally {
    await f.close();
  }
});
