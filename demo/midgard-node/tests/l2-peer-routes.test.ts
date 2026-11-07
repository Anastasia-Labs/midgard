/**
 * The operator-to-operator sharing routes, over real HTTP behind the node's
 * ingress, on a real database: a signed batch from an active operator is
 * admitted and validated in order; the inventory names exactly the ids the
 * node has never seen; every forged, misdirected or oversized request is
 * refused before any row is written; and peers and users read their bodies
 * from separate permit pools, so neither can fill the other's.
 */
import "./utils.js";

import { request as httpRequest } from "node:http";

import { Emulator, generateSeedPhrase, Lucid } from "@lucid-evolution/lucid";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  decodePeerList,
  encodePeerList,
  PEER_INVENTORY_PATH,
  PEER_MEDIA_TYPE,
  PEER_TXS_MAX_BODY_BYTES,
  PEER_TXS_PATH,
  peerAuthHeaders,
} from "../src/l2-peer/protocol.js";
import {
  buildChain as buildChainAgainst,
  buildTransfer,
  l2Output,
  operatorFromSeed,
  type PeerNode,
  type PeerNodeOptions,
  post,
  signedPeerPost,
  startShardNode,
  submitTx,
} from "./helpers/l2-peer-node.js";

const sender = operatorFromSeed(generateSeedPhrase());
const receiver = operatorFromSeed(generateSeedPhrase());
const outsider = operatorFromSeed(generateSeedPhrase());
const user = operatorFromSeed(generateSeedPhrase());
const funding = l2Output(user, 0x11, 50_000_000n);

let lucid: Awaited<ReturnType<typeof Lucid>>;
const nodes: PeerNode[] = [];

const startReceiver = async (options: Partial<PeerNodeOptions> = {}) => {
  const node = await startShardNode({
    operator: receiver,
    activeOperators: [sender.keyHash, receiver.keyHash],
    lucid,
    ...options,
  });
  nodes.push(node);
  await node.seedLedger([funding]);
  return node;
};

const buildChain = () =>
  buildChainAgainst(
    async () => {
      const node = await startReceiver();
      nodes.splice(nodes.indexOf(node), 1);
      return node;
    },
    user,
    funding,
  );

beforeAll(async () => {
  lucid = await Lucid(new Emulator([]), "Custom");
});

afterEach(async () => {
  for (const node of nodes.splice(0)) await node.close();
});

const statusOf = async (node: PeerNode, txId: Buffer) =>
  (await node.admissions()).find((row) => row.tx_id.equals(txId))?.status;

describe("POST /peer/txs and /peer/inventory", () => {
  it("admits a signed parent-and-child batch in order and validates both", async () => {
    const { parent, child } = await buildChain();
    const node = await startReceiver();

    const reply = await signedPeerPost(
      node.url,
      PEER_TXS_PATH,
      [parent.txCbor, child.txCbor],
      sender.signer,
    );
    expect(reply.status).toBe(200);
    expect(JSON.parse(reply.body.toString())).toEqual({
      admitted: 2,
      duplicates: 0,
      refused: 0,
    });
    const rows = await node.admissions();
    expect(rows.map((row) => [row.tx_id, row.submit_source])).toEqual([
      [parent.txId, "peer"],
      [child.txId, "peer"],
    ]);

    await node.drain();
    expect(await statusOf(node, parent.txId)).toBe("accepted");
    expect(await statusOf(node, child.txId)).toBe("accepted");

    // Offered again, both are acknowledged (the writer keeps no recent peer
    // ids, since a forgotten peer row must be admitted again) and their
    // writes resolve against the accepted rows, which stay as they were.
    const again = await signedPeerPost(
      node.url,
      PEER_TXS_PATH,
      [parent.txCbor, child.txCbor],
      sender.signer,
    );
    expect(JSON.parse(again.body.toString())).toEqual({
      admitted: 2,
      duplicates: 0,
      refused: 0,
    });
    expect(
      (await node.admissions()).map((row) => [row.tx_id, row.status]),
    ).toEqual([
      [parent.txId, "accepted"],
      [child.txId, "accepted"],
    ]);
  });

  it("answers an inventory with exactly the ids it has never seen", async () => {
    const { parent, child } = await buildChain();
    const node = await startReceiver();
    await signedPeerPost(
      node.url,
      PEER_TXS_PATH,
      [parent.txCbor],
      sender.signer,
    );
    const unknown = Buffer.alloc(32, 0x7e);

    const reply = await signedPeerPost(
      node.url,
      PEER_INVENTORY_PATH,
      [parent.txId, child.txId, unknown],
      sender.signer,
    );
    expect(reply.status).toBe(200);
    expect(reply.headers.get("content-type")).toBe(PEER_MEDIA_TYPE);
    expect(decodePeerList(reply.body, 3)).toEqual([child.txId, unknown]);
  });

  it("refuses unsigned, forged, outside, replayed, foreign and oversized requests and writes nothing", async () => {
    const { parent } = await buildChain();
    const node = await startReceiver();
    const body = encodePeerList([parent.txCbor]);
    const to = (path: string) => `${node.url}${path}`;
    const signedFor = (
      signer: typeof sender.signer,
      path: string,
      deployment = "5e".repeat(28),
    ) => peerAuthHeaders(signer, path, deployment, body);
    const cbor = { "content-type": PEER_MEDIA_TYPE };

    // No signature.
    expect((await post(to(PEER_TXS_PATH), body, cbor)).status).toBe(401);
    // The sender's key with the outsider's signature.
    const forged = {
      ...signedFor(sender.signer, PEER_TXS_PATH),
      "x-midgard-operator-signature": signedFor(outsider.signer, PEER_TXS_PATH)[
        "x-midgard-operator-signature"
      ]!,
    };
    expect(
      (await post(to(PEER_TXS_PATH), body, { ...cbor, ...forged })).status,
    ).toBe(401);
    // A correct signature by a key outside the active set.
    expect(
      (
        await post(to(PEER_TXS_PATH), body, {
          ...cbor,
          ...signedFor(outsider.signer, PEER_TXS_PATH),
        })
      ).status,
    ).toBe(403);
    // Signed for the inventory route, replayed to the transaction route.
    expect(
      (
        await post(to(PEER_TXS_PATH), body, {
          ...cbor,
          ...signedFor(sender.signer, PEER_INVENTORY_PATH),
        })
      ).status,
    ).toBe(401);
    // Signed for another deployment.
    expect(
      (
        await post(to(PEER_TXS_PATH), body, {
          ...cbor,
          ...signedFor(sender.signer, PEER_TXS_PATH, "a1".repeat(28)),
        })
      ).status,
    ).toBe(401);
    // A correctly signed body over the route's size bound.
    const oversized = encodePeerList([
      Buffer.alloc(PEER_TXS_MAX_BODY_BYTES, 0x01),
    ]);
    expect(
      (
        await post(to(PEER_TXS_PATH), oversized, {
          ...cbor,
          ...peerAuthHeaders(
            sender.signer,
            PEER_TXS_PATH,
            "5e".repeat(28),
            oversized,
          ),
        })
      ).status,
    ).toBe(413);

    expect(await node.admissions()).toEqual([]);
  });
});

/** Client addresses: each held request comes from its own client. */
let nextClient = 1;
const client = () => ({
  "x-midgard-client-ip": `10.0.${Math.floor(nextClient / 250).toString()}.${((nextClient++ % 250) + 1).toString()}`,
});

/**
 * A request that declares `declared` body bytes (or no length: a chunked
 * body), sends one and then holds: the node reads its body under a permit
 * it cannot finish.
 */
const holdRequest = (
  url: string,
  path: string,
  headers: Record<string, string>,
  declared: number | undefined,
) => {
  const target = new URL(`${url}${path}`);
  const req = httpRequest({
    hostname: target.hostname,
    port: target.port,
    path,
    method: "POST",
    headers:
      declared === undefined
        ? headers
        : { ...headers, "content-length": declared.toString() },
  });
  req.on("error", () => undefined);
  req.write(Buffer.alloc(1));
  return req;
};

/** Polls `probe` until it answers 503 with one of `errors`. */
const untilFull = async (
  probe: () => Promise<{ status: number; body: Buffer }>,
  ...errors: string[]
) => {
  const deadline = performance.now() + 10_000;
  for (;;) {
    const reply = await probe();
    if (
      reply.status === 503 &&
      errors.includes(
        (JSON.parse(reply.body.toString()) as { error: string }).error,
      )
    )
      return;
    if (performance.now() >= deadline)
      throw new Error(
        `The pool never filled; last answer ${reply.status.toString()}`,
      );
    await new Promise((resolve) => setTimeout(resolve, 50));
  }
};

describe("peer and submit ingress permits", () => {
  const behindProxy = { config: { HTTP_TRUSTED_PROXY_PEERS: ["127.0.0.1"] } };

  it("an unsigned flood from many clients leaves a signed peer admitted", async () => {
    const node = await startReceiver(behindProxy);
    const body = encodePeerList([Buffer.alloc(32, 0x5a)]);
    const flood: number[] = [];
    for (let i = 0; i < 64; i++)
      flood.push(
        (
          await post(`${node.url}${PEER_INVENTORY_PATH}`, body, {
            "content-type": PEER_MEDIA_TYPE,
            ...client(),
          })
        ).status,
      );
    // Each is refused for its missing signature; none spends a token any
    // signed peer needs.
    expect(new Set(flood)).toEqual(new Set([401]));

    const reply = await signedPeerPost(
      node.url,
      PEER_INVENTORY_PATH,
      [Buffer.alloc(32, 0x5a)],
      sender.signer,
      { headers: client() },
    );
    expect(reply.status).toBe(200);
  });

  it.each([
    {
      length: "declared",
      declared: PEER_TXS_MAX_BODY_BYTES as number | undefined,
    },
    // A body of unknown length reserves the route's whole bound at the
    // ingress, which every public route shares.
    { length: "unknown", declared: undefined },
  ])(
    "peers holding every peer permit with $length lengths leave /submit admitting",
    async ({ declared }) => {
      const node = await startReceiver(behindProxy);
      const tx = await buildTransfer(user, [funding], 10_000_000n, node.config);
      const held = Array.from(
        { length: node.config.SUBMIT_INGRESS_MAX_CONCURRENCY },
        () =>
          holdRequest(
            node.url,
            PEER_TXS_PATH,
            { "content-type": PEER_MEDIA_TYPE, ...client() },
            declared,
          ),
      );
      try {
        await untilFull(
          () =>
            signedPeerPost(node.url, PEER_TXS_PATH, [], sender.signer, {
              headers: client(),
            }),
          "Peer ingress capacity is full; retry later",
          "HTTP ingress capacity is full",
        );
        const reply = await submitTx(node.url, tx.txCbor, client());
        expect(reply.status).toBe(202);
      } finally {
        for (const req of held) req.destroy();
      }
    },
  );

  it("users holding every submit permit leave /peer/txs admitting", async () => {
    const node = await startReceiver(behindProxy);
    const tx = await buildTransfer(user, [funding], 10_000_000n, node.config);
    const held = Array.from(
      { length: node.config.SUBMIT_INGRESS_MAX_CONCURRENCY },
      () =>
        holdRequest(
          node.url,
          "/submit",
          { "content-type": "application/vnd.midgard.v1+cbor", ...client() },
          100_000,
        ),
    );
    try {
      await untilFull(
        () =>
          post(`${node.url}/submit`, Buffer.from([0x80]), {
            "content-type": "application/vnd.midgard.v1+cbor",
            ...client(),
          }),
        "Submit ingress capacity is full; retry later",
        "HTTP ingress capacity is full",
      );
      const reply = await signedPeerPost(
        node.url,
        PEER_TXS_PATH,
        [tx.txCbor],
        sender.signer,
        { headers: client() },
      );
      expect(reply.status).toBe(200);
      expect(JSON.parse(reply.body.toString())).toMatchObject({
        admitted: 1,
      });
    } finally {
      for (const req of held) req.destroy();
    }
  });
});
