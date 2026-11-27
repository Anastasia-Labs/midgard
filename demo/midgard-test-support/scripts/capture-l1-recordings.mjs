#!/usr/bin/env node

/**
 * Records what a live Kupo and Ogmios actually answer, as test fixtures.
 *
 * Tests that stand a hand-written double in for Kupo or Ogmios encode the
 * author's assumptions about the wire, and a wrong assumption passes CI on both
 * sides of the seam. These recordings are the other half: the responses the
 * real services gave, kept verbatim, so a test can serve them to the production
 * parsers. A `mirrored Kupo spent_at`, an Ogmios tip without a `height`, or a
 * chain-sync handshake echo is then a fact read off the recording rather than
 * a belief written into a double.
 *
 * **Read-only by construction.** Kupo is only ever sent `GET`, and Ogmios only
 * state queries (`queryNetwork/*`) and chain-sync (`findIntersection`,
 * `nextBlock`), each on a connection this script opens and closes. Nothing is
 * submitted, evaluated or acquired. Every query names public chain data —
 * transaction ids, output references, addresses — and every answer is public
 * chain data or the service's own view of the chain tip; this script reads no
 * file other than its own recordings table.
 *
 * **What is kept, and how.** Each exchange stores the request exactly as sent
 * (for Ogmios the whole JSON-RPC envelope) and the response body parsed, but
 * only after checking that re-serializing it reproduces the received bytes; a
 * body that would not survive that round trip (a number beyond double
 * precision, say) is kept as its raw text instead. Response headers are kept
 * only where a reader depends on them (`content-type`, Kupo's `etag` and
 * `x-most-recent-checkpoint`). `elapsedMs` is how long the answer took, which is
 * itself a recorded fact: chain-sync's `nextBlock` at the tip does not answer
 * until the node adopts another block.
 *
 * Usage (from `demo/midgard-test-support`):
 *
 *   node scripts/capture-l1-recordings.mjs [--kupo=<url>] [--ogmios=<url>]
 *     [--out=<dir>] [--only=<name>[,<name>...]]
 *     [--transaction=<name>:<tx id>]...
 *
 * With no `--transaction`, every recording in {@link RECORDINGS} is taken
 * again; `--only` restricts that to the named ones. `--transaction` records one
 * more transaction under a new name, for a test that needs a shape the table
 * does not have yet — add it to the table when the fixture is committed.
 */

import { mkdirSync, writeFileSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const PACKAGE_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const TOOL = relative(
  resolve(PACKAGE_ROOT, "..", ".."),
  fileURLToPath(import.meta.url),
);

/** The only network these fixtures are named for. */
const PREPROD_NETWORK_MAGIC = 1;

/** How long chain-sync may wait at the tip for the node's next block. */
const NEXT_BLOCK_AT_TIP_TIMEOUT_MS = 300_000;
/** How often a follow recording re-reads the tip while it waits for a block. */
const TIP_POLL_INTERVAL_MS = 2_000;
const REQUEST_TIMEOUT_MS = 60_000;
const BLOCK_SCAN_LIMIT = 100;

const STATE_QUEUE_ADDRESS =
  "addr_test1wr405mwutmgnt0mntdl5jumg4rdkx6qglmjvarranq85hgcts4ka2";

/**
 * The committed recordings. A `transaction` recording holds everything a
 * reader fetches to take that transaction off L1: the Kupo matches that locate
 * it, the chain-sync walk from Kupo's checkpoint ancestor to its block, the
 * Kupo match of every input and reference input it names (each carrying
 * Kupo's `spent_at` for it), and an Ogmios tip read after all of that.
 */
const RECORDINGS = [
  {
    name: "preprod-state-queue-removal-a2a47d2e",
    kind: "transaction",
    transactionId:
      "a2a47d2eb2c5cb8679efb68f35e142bbb626d534f84a0b226290512b69e17190",
    description:
      "A Midgard state-queue timeout removal: four inputs (the removed block " +
      "node, the confirmed-state root, a key input, the CorrectionLock), three " +
      "of them Plutus spends. Includes every Kupo match on the state-queue " +
      "address.",
    kupoPaths: [`/matches/${STATE_QUEUE_ADDRESS}?resolve_hashes`],
  },
  {
    name: "preprod-reference-sweep-e1e70271",
    kind: "transaction",
    transactionId:
      "e1e7027199eaf1706db87dff2bc25cfbb18450bf3d10df5bf75eb5b1c9a926e7",
    description:
      "A reference-script wallet sweep: five key inputs, no redeemers, so " +
      "Kupo's input numbering is visible without any redeemer in the way.",
    kupoPaths: [],
  },
  {
    name: "preprod-ogmios-network",
    kind: "network",
    description:
      "Ogmios network queries and a chain-sync session opened at the tip, in " +
      "the order a DA committee member bootstraps and then idles there: " +
      "genesis, tip, findIntersection at the tip, a tip re-read, the " +
      "handshake nextBlock, the nextBlock that waits for the next block, and " +
      "the tip after it. Also Ogmios's HTTP JSON-RPC tip and height, and " +
      "Kupo's text/plain health with its ETag.",
  },
  {
    name: "preprod-ogmios-follow-tip",
    kind: "follow",
    description:
      "A chain-sync session that bootstraps at the tip and then follows it " +
      "the way a DA committee member does: genesis, tip, findIntersection at " +
      "the tip, then the tip re-read every " +
      `${(TIP_POLL_INTERVAL_MS / 1_000).toString()} s until the node adopts a ` +
      "block, and only then the handshake nextBlock and the nextBlock that " +
      "delivers the new block.",
  },
];

const option = (name) =>
  process.argv
    .filter((value) => value.startsWith(`--${name}=`))
    .map((value) => value.slice(name.length + 3));

const kupoUrl = (option("kupo")[0] ?? "http://127.0.0.1:1442").replace(
  /\/+$/u,
  "",
);
const ogmiosUrl = (option("ogmios")[0] ?? "ws://127.0.0.1:1337").replace(
  /\/+$/u,
  "",
);
const outDir = resolve(
  option("out")[0] ?? join(PACKAGE_ROOT, "fixtures", "l1"),
);
const only = new Set(option("only").flatMap((value) => value.split(",")));
const adHoc = option("transaction").map((value) => {
  const [name, transactionId] = value.split(":");
  if (
    name === undefined ||
    name.length === 0 ||
    !/^[0-9a-f]{64}$/u.test(transactionId ?? "")
  ) {
    throw new Error(`--transaction expects <name>:<tx id>, got ${value}`);
  }
  return {
    name,
    kind: "transaction",
    transactionId,
    description: "Recorded ad hoc with --transaction.",
    kupoPaths: [],
  };
});
const selected =
  adHoc.length > 0
    ? adHoc
    : RECORDINGS.filter(({ name }) => only.size === 0 || only.has(name));
if (selected.length === 0) {
  throw new Error(`no recording matches --only=${[...only].join(",")}`);
}

const ogmiosHttpUrl = (() => {
  const url = new URL(ogmiosUrl);
  url.protocol = url.protocol === "wss:" ? "https:" : "http:";
  return url.toString().replace(/\/+$/u, "");
})();

/**
 * The body as it will be committed: parsed when re-serializing reproduces the
 * received text exactly, the raw text otherwise.
 */
const recordedBody = (text, contentType) => {
  if (contentType?.includes("json")) {
    const parsed = JSON.parse(text);
    if (JSON.stringify(parsed) === text) return { body: parsed };
  }
  return { bodyText: text };
};

const KEPT_HEADERS = ["content-type", "etag", "x-most-recent-checkpoint"];

const keptHeaders = (headers) =>
  Object.fromEntries(
    KEPT_HEADERS.flatMap((name) => {
      const value = headers.get(name);
      return value === null ? [] : [[name, value]];
    }),
  );

const kupoGet = async (path, accept = "application/json") => {
  const started = Date.now();
  const response = await fetch(`${kupoUrl}${path}`, {
    headers: { accept },
    signal: AbortSignal.timeout(REQUEST_TIMEOUT_MS),
  });
  const text = await response.text();
  const elapsedMs = Date.now() - started;
  const headers = keptHeaders(response.headers);
  return {
    surface: "kupo",
    request: { method: "GET", path, accept },
    response: {
      status: response.status,
      headers,
      ...recordedBody(text, headers["content-type"]),
    },
    elapsedMs,
  };
};

const ogmiosHttp = async (method, params) => {
  const message = {
    jsonrpc: "2.0",
    method,
    ...(params === undefined ? {} : { params }),
    id: null,
  };
  const started = Date.now();
  const response = await fetch(ogmiosHttpUrl, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(message),
    signal: AbortSignal.timeout(REQUEST_TIMEOUT_MS),
  });
  const text = await response.text();
  const elapsedMs = Date.now() - started;
  const headers = keptHeaders(response.headers);
  return {
    surface: "ogmios-http",
    request: message,
    response: {
      status: response.status,
      headers,
      ...recordedBody(text, headers["content-type"]),
    },
    elapsedMs,
  };
};

/** One Ogmios WebSocket connection, recording every exchange on it in order. */
const openOgmiosSession = async () => {
  const socket = new WebSocket(ogmiosUrl);
  const waiters = new Map();
  let nextId = 0;
  socket.addEventListener("message", (event) => {
    const text = String(event.data);
    const message = JSON.parse(text);
    const waiter = waiters.get(message.id);
    if (waiter === undefined) {
      throw new Error(`unsolicited Ogmios message: ${text.slice(0, 200)}`);
    }
    waiters.delete(message.id);
    waiter(text);
  });
  await new Promise((resolveOpen, rejectOpen) => {
    socket.addEventListener("open", resolveOpen, { once: true });
    socket.addEventListener("error", rejectOpen, { once: true });
  });
  const exchanges = [];
  const request = async (method, params, timeoutMs = REQUEST_TIMEOUT_MS) => {
    const message = {
      jsonrpc: "2.0",
      method,
      ...(params === undefined ? {} : { params }),
      id: `capture-${(nextId += 1).toString()}`,
    };
    const started = Date.now();
    const text = await new Promise((resolveMessage, rejectMessage) => {
      const timer = setTimeout(() => {
        waiters.delete(message.id);
        rejectMessage(
          new Error(`Ogmios ${method} did not answer within ${timeoutMs}ms`),
        );
      }, timeoutMs);
      waiters.set(message.id, (answer) => {
        clearTimeout(timer);
        resolveMessage(answer);
      });
      socket.send(JSON.stringify(message));
    });
    const recorded = recordedBody(text, "application/json");
    exchanges.push({
      surface: "ogmios-websocket",
      request: message,
      response: recorded,
      elapsedMs: Date.now() - started,
    });
    const parsed = recorded.body ?? JSON.parse(text);
    if (parsed.error !== undefined) {
      throw new Error(
        `Ogmios ${method} failed: ${JSON.stringify(parsed.error)}`,
      );
    }
    return parsed.result;
  };
  return { request, exchanges, close: () => socket.close() };
};

/** Service identity, reduced to the version strings `/health` reports. */
const serviceVersions = async () => {
  const kupoHealth = await (
    await fetch(`${kupoUrl}/health`, {
      headers: { accept: "application/json" },
      signal: AbortSignal.timeout(REQUEST_TIMEOUT_MS),
    })
  ).json();
  const ogmiosHealth = await (
    await fetch(`${ogmiosHttpUrl}/health`, {
      signal: AbortSignal.timeout(REQUEST_TIMEOUT_MS),
    })
  ).json();
  return {
    kupo: String(kupoHealth.version),
    ogmios: String(ogmiosHealth.version),
  };
};

const networkOf = async () => {
  const session = await openOgmiosSession();
  try {
    const genesis = await session.request("queryNetwork/genesisConfiguration", {
      era: "shelley",
    });
    if (genesis.networkMagic !== PREPROD_NETWORK_MAGIC) {
      throw new Error(
        `these fixtures are preprod recordings; Ogmios reports network magic ${String(genesis.networkMagic)}`,
      );
    }
    return { network: "preprod", networkMagic: genesis.networkMagic };
  } finally {
    session.close();
  }
};

const kupoBody = (exchange) => {
  if (exchange.response.status !== 200 || !("body" in exchange.response)) {
    throw new Error(
      `Kupo ${exchange.request.path} answered ${String(exchange.response.status)}`,
    );
  }
  return exchange.response.body;
};

const recordTransaction = async ({ transactionId, kupoPaths }) => {
  const exchanges = [];
  const kupo = async (path, accept) => {
    const exchange = await kupoGet(path, accept);
    exchanges.push(exchange);
    return kupoBody(exchange);
  };

  const outputs = await kupo(`/matches/*@${transactionId}?resolve_hashes`);
  const createdAt = outputs[0]?.created_at;
  if (createdAt === undefined) {
    throw new Error(`Kupo holds no output of ${transactionId}`);
  }
  const ancestor = await kupo(
    `/checkpoints/${(createdAt.slot_no - 1).toString()}`,
  );

  const session = await openOgmiosSession();
  let transaction;
  try {
    await session.request("findIntersection", {
      points: [{ slot: ancestor.slot_no, id: ancestor.header_hash }],
    });
    for (let scanned = 0; transaction === undefined; scanned += 1) {
      if (scanned > BLOCK_SCAN_LIMIT) {
        throw new Error(`chain-sync did not reach ${createdAt.header_hash}`);
      }
      const next = await session.request("nextBlock", {});
      if (next.direction !== "forward") continue;
      if (next.block.id !== createdAt.header_hash) continue;
      transaction = next.block.transactions.find(
        ({ id }) => id === transactionId,
      );
      if (transaction === undefined) {
        throw new Error(`block ${next.block.id} lacks ${transactionId}`);
      }
    }
  } finally {
    session.close();
  }
  exchanges.push(...session.exchanges);

  for (const reference of [
    ...(transaction.inputs ?? []),
    ...(transaction.references ?? []),
  ]) {
    await kupo(
      `/matches/${reference.index.toString()}@${reference.transaction.id}?resolve_hashes`,
    );
  }
  for (const path of kupoPaths) {
    await kupo(path);
  }
  // The tip as the node's observer reads it: over HTTP, bracketing the height.
  exchanges.push(await ogmiosHttp("queryNetwork/tip"));
  exchanges.push(await ogmiosHttp("queryNetwork/blockHeight"));
  exchanges.push(await ogmiosHttp("queryNetwork/tip"));
  return {
    transaction: {
      id: transactionId,
      block: { slot: createdAt.slot_no, id: createdAt.header_hash },
    },
    exchanges,
  };
};

const sameTip = (left, right) =>
  left.slot === right.slot && left.id === right.id;

const recordNetwork = async () => {
  const session = await openOgmiosSession();
  try {
    await session.request("queryNetwork/genesisConfiguration", {
      era: "shelley",
    });
    const tip = await session.request("queryNetwork/tip", {});
    await session.request("queryNetwork/blockHeight", {});
    await session.request("findIntersection", {
      points: [{ slot: tip.slot, id: tip.id }, "origin"],
    });
    const reread = await session.request("queryNetwork/tip", {});
    if (!sameTip(tip, reread)) {
      throw new Error("a block landed during the capture; run it again");
    }
    await session.request("nextBlock", {});
    await session.request("nextBlock", {}, NEXT_BLOCK_AT_TIP_TIMEOUT_MS);
    await session.request("queryNetwork/tip", {});
  } finally {
    session.close();
  }
  return {
    exchanges: [
      ...session.exchanges,
      await ogmiosHttp("queryNetwork/tip"),
      await ogmiosHttp("queryNetwork/blockHeight"),
      await kupoGet("/health", "text/plain"),
    ],
  };
};

const recordFollow = async () => {
  const session = await openOgmiosSession();
  try {
    await session.request("queryNetwork/genesisConfiguration", {
      era: "shelley",
    });
    const tip = await session.request("queryNetwork/tip", {});
    await session.request("findIntersection", {
      points: [{ slot: tip.slot, id: tip.id }, "origin"],
    });
    const deadline = Date.now() + NEXT_BLOCK_AT_TIP_TIMEOUT_MS;
    for (;;) {
      await new Promise((resolveWait) =>
        setTimeout(resolveWait, TIP_POLL_INTERVAL_MS),
      );
      const current = await session.request("queryNetwork/tip", {});
      if (!sameTip(tip, current)) break;
      if (Date.now() > deadline) {
        throw new Error("the node adopted no block while the tip was followed");
      }
    }
    await session.request("nextBlock", {});
    await session.request("nextBlock", {});
  } finally {
    session.close();
  }
  return { exchanges: session.exchanges };
};

const versions = await serviceVersions();
const network = await networkOf();
mkdirSync(outDir, { recursive: true });
for (const recording of selected) {
  const capturedAt = new Date().toISOString();
  const captured =
    recording.kind === "network"
      ? await recordNetwork()
      : recording.kind === "follow"
        ? await recordFollow()
        : await recordTransaction(recording);
  const document = {
    name: recording.name,
    description: recording.description,
    provenance: {
      ...network,
      kupo: { url: kupoUrl, version: versions.kupo },
      ogmios: { url: ogmiosUrl, version: versions.ogmios },
      capturedAt,
      tool: TOOL,
    },
    ...captured,
  };
  const path = join(outDir, `${recording.name}.json`);
  writeFileSync(path, `${JSON.stringify(document, null, 2)}\n`);
  console.log(
    `${recording.name}: ${captured.exchanges.length.toString()} exchanges -> ${relative(process.cwd(), path)}`,
  );
}
