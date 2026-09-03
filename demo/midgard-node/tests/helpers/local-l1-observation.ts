import { createHash } from "node:crypto";
import {
  createServer,
  type IncomingMessage,
  type Server,
  type ServerResponse,
} from "node:http";
import type { Duplex } from "node:stream";

import { CML } from "@lucid-evolution/lucid";

/**
 * A local L1 observation surface: one Ogmios chain-sync endpoint and one Kupo
 * endpoint, both serving a chain built out of **real signed transactions**.
 *
 * This is not a fixture of an observation. Every field either comes off the
 * transaction's own CBOR — the transaction id, its reference inputs in the order
 * the ledger serialised them, its minted policies, its redeemers with the
 * validator pointers the ledger assigned — or is chain metadata (slots, header
 * hashes) that an emulated ledger has no opinion about and this file therefore
 * has to name. Nothing that the reader authenticates against is authored here.
 *
 * The two protocols are implemented rather than mocked, because the thing under
 * test is a read: the node opens a real WebSocket, speaks Ogmios's JSON-RPC
 * chain-sync (`findIntersection`, then `nextBlock` forward), and fetches Kupo's
 * `/matches/{index}@{id}` and `/checkpoints/{slot}` over HTTP. A stub at the
 * function boundary would leave exactly the layer this ticket adds untested.
 *
 * **Fidelity notes, so a reader knows what this does and does not prove.**
 * - Transaction JSON follows Ogmios v6's schema (`redeemers` as an array of
 *   `{redeemer, executionUnits, validator: {purpose, index}}`, `references` as
 *   `{transaction: {id}, index}`), which is what `l1-services/docker-compose.yml`
 *   pins at v6.11.0. `cbor` is deliberately absent: a default Ogmios omits it
 *   unless started with `--include-transaction-cbor`.
 * - Kupo match records are the **v2.11.0** `Match` shape, which is what
 *   `l1-services/docker-compose.yml` now pins: `additionalProperties: false` over
 *   `transaction_index, transaction_id, output_index, address, value, datum_hash,
 *   datum, datum_type, script_hash, script, created_at, spent_at`. `datum_type` is
 *   *omitted* rather than nulled for an output with no datum, because the schema
 *   says it "is only present when `datum_hash` is not `null`".
 * - **`datum` and `script` are served if and only if the request carries the bare
 *   `?resolve_hashes` flag**, which is the v2.10.0 contract the deployment floor
 *   now buys: those two fields are "only and always present (yet may be `null`) if
 *   `?resolve_hashes` was set". *Only* — a flagless request gets a match with
 *   neither key, so a reader that forgot the flag cannot be greened by a harness
 *   that volunteers the bytes anyway. *Always* — with the flag, `datum` is present
 *   on every match including outputs that never had one, where it is `null`. A
 *   `null` under the flag is also Kupo's answer for a datum whose bytes it does
 *   not hold, and the reader has to survive both.
 *   {@link LocalL1.ignoreResolveHashes} serves the flag-ignoring shape a Kupo
 *   below the floor answers with, so the deployment-mismatch refusal is testable.
 * - The datum hash is content-derived here rather than Blake2b-256: nothing
 *   authenticates it, it is only the key joining a match to the datum store the
 *   `?resolve_hashes` join reads, and every byte it leads to still terminates in
 *   the §4 hash door.
 * - `/checkpoints/{slot}` answers with the most recent checkpoint **before or at**
 *   the requested slot, which is Kupo's documented flexible lookup and the whole
 *   reason the point-fetch can find an ancestor to intersect at. Blocks are
 *   spaced several slots apart here so that lookup has to do that work rather
 *   than hit an exact match.
 * - Header hashes are content-derived from the block's transaction ids. An
 *   emulated ledger produces no headers, and the reader only ever compares them
 *   for equality against the one Kupo reported.
 */

const WEBSOCKET_GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

/** Slots between consecutive blocks, so checkpoint lookup is never exact. */
const SLOTS_PER_BLOCK = 20;

export type LocalL1Point = {
  readonly slot: number;
  readonly headerHash: string;
};

export type LocalL1Block = LocalL1Point & {
  readonly height: number;
  readonly transactions: readonly unknown[];
};

export type LocalL1 = {
  /** Value for `L1_OGMIOS_KEY`. */
  readonly ogmiosUrl: string;
  /** Value for `L1_KUPO_KEY`. */
  readonly kupoUrl: string;
  /** Appends one block carrying the given signed transactions. */
  readonly appendBlock: (transactionCbors: readonly string[]) => LocalL1Block;
  /**
   * Rewrites the datum the `?resolve_hashes` join returns on a match, for the
   * negatives that prove the read stays fail-closed. The rewrite is handed the
   * real datum and returns either replacement base16 or `null`, which serves
   * Kupo's answer for a datum it does not hold: the `datum` field present, as the
   * flag always makes it, and `null`. Passing `null` restores honest answers.
   */
  readonly rewriteDatums: (
    rewrite: ((datum: string) => string | null) | null,
  ) => void;
  /**
   * Serves matches the way a Kupo **below the v2.10.0 deployment floor** does:
   * `?resolve_hashes` is an unknown query flag, so it is silently ignored rather
   * than rejected and the match comes back with no `datum` field at all. This is
   * the deployment-mismatch shape, and the read must refuse it loudly instead of
   * reading a missing field as an output that carries no carriage.
   */
  readonly ignoreResolveHashes: (ignore: boolean) => void;
  readonly close: () => Promise<void>;
};

/**
 * A match as Kupo serves it **without** `?resolve_hashes` — the schema's required
 * properties and nothing else. `datum` and `script` are not optional members of
 * this type on purpose: they exist only on the resolved shape below.
 */
type KupoMatch = {
  readonly transaction_index: number;
  readonly transaction_id: string;
  readonly output_index: number;
  readonly address: string;
  readonly value: { readonly coins: string; readonly assets: object };
  readonly datum_hash: string | null;
  /** Only present when `datum_hash` is not `null`, as the schema states. */
  readonly datum_type?: "hash" | "inline";
  readonly script_hash: string | null;
  readonly created_at: {
    readonly slot_no: number;
    readonly header_hash: string;
  };
  readonly spent_at: null;
};

/** The same match under `?resolve_hashes`: both joins present, either may be null. */
type KupoResolvedMatch = KupoMatch & {
  readonly datum: string | null;
  readonly script: null;
};

/** The key joining a match to its bytes. Not Blake2b-256; nothing checks it. */
const datumHashOf = (datumCbor: string): string =>
  createHash("sha256").update(datumCbor).digest("hex");

const redeemerPurpose = (tag: CML.RedeemerTag): string => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return "spend";
    case CML.RedeemerTag.Mint:
      return "mint";
    case CML.RedeemerTag.Cert:
      return "publish";
    case CML.RedeemerTag.Reward:
      return "withdraw";
    case CML.RedeemerTag.Voting:
      return "vote";
    case CML.RedeemerTag.Proposing:
      return "propose";
    default:
      throw new Error(`unsupported redeemer tag ${String(tag)}`);
  }
};

type ObservedRedeemer = {
  readonly redeemer: string;
  readonly executionUnits: { readonly memory: number; readonly cpu: number };
  readonly validator: { readonly purpose: string; readonly index: number };
};

const transactionRedeemers = (
  witnessSet: CML.TransactionWitnessSet,
): readonly ObservedRedeemer[] => {
  const redeemers = witnessSet.redeemers();
  if (redeemers === undefined) {
    return [];
  }
  const collected: ObservedRedeemer[] = [];
  const push = (
    tag: CML.RedeemerTag,
    index: bigint,
    data: CML.PlutusData,
    exUnits: CML.ExUnits,
  ): void => {
    collected.push({
      redeemer: data.to_cbor_hex(),
      executionUnits: {
        memory: Number(exUnits.mem()),
        cpu: Number(exUnits.steps()),
      },
      validator: { purpose: redeemerPurpose(tag), index: Number(index) },
    });
  };
  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    for (let index = 0; index < legacy.len(); index += 1) {
      const redeemer = legacy.get(index);
      push(
        redeemer.tag(),
        redeemer.index(),
        redeemer.data(),
        redeemer.ex_units(),
      );
    }
    return collected;
  }
  const mapped = redeemers.as_map_redeemer_key_to_redeemer_val();
  if (mapped === undefined) {
    throw new Error("witness set carries an unreadable redeemer map");
  }
  const keys = mapped.keys();
  for (let index = 0; index < keys.len(); index += 1) {
    const key = keys.get(index);
    const value = mapped.get(key);
    if (value === undefined) {
      throw new Error("witness set redeemer map is missing a value");
    }
    push(key.tag(), key.index(), value.data(), value.ex_units());
  }
  return collected;
};

const transactionMint = (
  body: CML.TransactionBody,
): Record<string, Record<string, string>> => {
  const mint = body.mint();
  const assets: Record<string, Record<string, string>> = {};
  if (mint === undefined) {
    return assets;
  }
  const policies = mint.keys();
  for (let index = 0; index < policies.len(); index += 1) {
    const policy = policies.get(index);
    const minted = mint.get_assets(policy);
    if (minted === undefined) {
      continue;
    }
    const names = minted.keys();
    const byName: Record<string, string> = {};
    for (let nameIndex = 0; nameIndex < names.len(); nameIndex += 1) {
      const name = names.get(nameIndex);
      byName[name.to_hex()] = (minted.get(name) ?? 0n).toString();
    }
    assets[policy.to_hex()] = byName;
  }
  return assets;
};

const outRefsOf = (
  inputs: CML.TransactionInputList | undefined,
): readonly { transaction: { id: string }; index: number }[] => {
  if (inputs === undefined) {
    return [];
  }
  const collected: { transaction: { id: string }; index: number }[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    collected.push({
      transaction: { id: input.transaction_id().to_hex() },
      index: Number(input.index()),
    });
  }
  return collected;
};

type DecodedTransaction = {
  readonly txHash: string;
  readonly json: unknown;
  readonly outputs: readonly {
    readonly address: string;
    readonly datum: string | null;
  }[];
};

/**
 * Ogmios's JSON view of a real transaction, derived from its bytes.
 *
 * **Reference inputs are emitted in the transaction's own CBOR order and are not
 * sorted here, deliberately.** A real Ogmios emits them in the ledger's order —
 * `reference_inputs` is a `Set TxIn`, so what a validator is handed is
 * `(txHash, outputIndex)` ascending — while a builder writes them into the CBOR
 * in whatever order it collected them, which for the Lucid builder is insertion
 * order. Serving the unsorted view is the *harder* of the two for the reader, and
 * it is what makes the reader's own canonical sort testable: a reader that took
 * the wire order as given would address the wrong inputs and fail the §4
 * commitment check.
 */
const decodeTransaction = (cborHex: string): DecodedTransaction => {
  const transaction = CML.Transaction.from_cbor_hex(cborHex);
  const body = transaction.body();
  const txHash = CML.hash_transaction(body).to_hex();
  const outputs = body.outputs();
  const outputViews: { address: string; datum: string | null }[] = [];
  const outputsJson: unknown[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    const address = output.address().to_bech32(undefined);
    const datum = output.datum()?.as_datum()?.to_cbor_hex() ?? null;
    outputViews.push({ address, datum });
    outputsJson.push({
      address,
      value: { ada: { lovelace: output.amount().coin().toString() } },
      ...(datum === null ? {} : { datum }),
    });
  }
  return {
    txHash,
    outputs: outputViews,
    json: {
      id: txHash,
      spends: "inputs",
      inputs: outRefsOf(body.inputs()),
      references: outRefsOf(body.reference_inputs()),
      outputs: outputsJson,
      mint: transactionMint(body),
      redeemers: transactionRedeemers(transaction.witness_set()),
      signatories: [],
      fee: { ada: { lovelace: body.fee().toString() } },
    },
  };
};

const headerHashOf = (slot: number, txHashes: readonly string[]): string =>
  createHash("sha256")
    .update(`local-l1-block:${slot.toString()}:${txHashes.join(",")}`)
    .digest("hex");

const encodeWebSocketFrame = (text: string): Buffer => {
  const payload = Buffer.from(text, "utf8");
  if (payload.length < 126) {
    return Buffer.concat([Buffer.from([0x81, payload.length]), payload]);
  }
  if (payload.length < 65_536) {
    const header = Buffer.alloc(4);
    header[0] = 0x81;
    header[1] = 126;
    header.writeUInt16BE(payload.length, 2);
    return Buffer.concat([header, payload]);
  }
  const header = Buffer.alloc(10);
  header[0] = 0x81;
  header[1] = 127;
  header.writeBigUInt64BE(BigInt(payload.length), 2);
  return Buffer.concat([header, payload]);
};

const readWebSocketFrames = (
  socket: Duplex,
  onText: (text: string) => void,
): void => {
  let buffered = Buffer.alloc(0);
  socket.on("data", (chunk: Buffer) => {
    buffered = Buffer.concat([buffered, chunk]);
    for (;;) {
      if (buffered.length < 2) {
        return;
      }
      const opcode = buffered[0]! & 0x0f;
      const masked = (buffered[1]! & 0x80) !== 0;
      let length = buffered[1]! & 0x7f;
      let offset = 2;
      if (length === 126) {
        if (buffered.length < 4) {
          return;
        }
        length = buffered.readUInt16BE(2);
        offset = 4;
      } else if (length === 127) {
        if (buffered.length < 10) {
          return;
        }
        length = Number(buffered.readBigUInt64BE(2));
        offset = 10;
      }
      const maskOffset = offset;
      if (masked) {
        offset += 4;
      }
      if (buffered.length < offset + length) {
        return;
      }
      const payload = Buffer.from(buffered.subarray(offset, offset + length));
      if (masked) {
        const mask = buffered.subarray(maskOffset, maskOffset + 4);
        for (let index = 0; index < payload.length; index += 1) {
          payload[index] ^= mask[index % 4]!;
        }
      }
      buffered = buffered.subarray(offset + length);
      if (opcode === 0x8) {
        socket.end();
        return;
      }
      if (opcode === 0x1) {
        onText(payload.toString("utf8"));
      }
    }
  });
};

const listen = async (server: Server): Promise<number> => {
  await new Promise<void>((resolve) => {
    server.listen(0, "127.0.0.1", resolve);
  });
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("local L1 server did not bind a port");
  }
  return address.port;
};

export const startLocalL1Observation = async (): Promise<LocalL1> => {
  const blocks: LocalL1Block[] = [];
  const matches = new Map<string, KupoMatch>();
  const datums = new Map<string, string>();
  let datumRewrite: ((datum: string) => string | null) | null = null;
  let ignoringResolveHashes = false;
  // A genesis checkpoint, so the first real block always has an ancestor to
  // intersect at — exactly as a synced Kupo always does.
  blocks.push({
    slot: 0,
    headerHash: headerHashOf(0, []),
    height: 0,
    transactions: [],
  });

  const appendBlock = (transactionCbors: readonly string[]): LocalL1Block => {
    const previous = blocks[blocks.length - 1]!;
    const slot = previous.slot + SLOTS_PER_BLOCK;
    const decoded = transactionCbors.map(decodeTransaction);
    const headerHash = headerHashOf(
      slot,
      decoded.map((transaction) => transaction.txHash),
    );
    const block: LocalL1Block = {
      slot,
      headerHash,
      height: previous.height + 1,
      transactions: decoded.map((transaction) => transaction.json),
    };
    blocks.push(block);
    decoded.forEach((transaction, transactionIndex) => {
      transaction.outputs.forEach((output, outputIndex) => {
        const datumHash =
          output.datum === null ? null : datumHashOf(output.datum);
        if (datumHash !== null && output.datum !== null) {
          datums.set(datumHash, output.datum);
        }
        matches.set(`${transaction.txHash}#${outputIndex.toString()}`, {
          transaction_index: transactionIndex,
          transaction_id: transaction.txHash,
          output_index: outputIndex,
          address: output.address,
          value: { coins: "0", assets: {} },
          datum_hash: datumHash,
          // Omitted, not nulled, when the output carries no datum.
          ...(datumHash === null ? {} : { datum_type: "inline" as const }),
          script_hash: null,
          created_at: { slot_no: slot, header_hash: headerHash },
          spent_at: null,
        });
      });
    });
    return block;
  };

  /**
   * The `?resolve_hashes` join, as Kupo performs it: `datum` and `script` both
   * become present on the match, each carrying the stored value or `null`. A datum
   * the store cannot produce — which is what {@link LocalL1.rewriteDatums} can
   * turn any datum into — is `null` under a **present** key, never an absent one.
   */
  const resolveHashesOn = (match: KupoMatch): KupoResolvedMatch => {
    const stored =
      match.datum_hash === null ? undefined : datums.get(match.datum_hash);
    const datum =
      stored === undefined
        ? null
        : datumRewrite === null
          ? stored
          : datumRewrite(stored);
    return { ...match, datum, script: null };
  };

  const kupoServer = createServer(
    (request: IncomingMessage, response: ServerResponse) => {
      const url = new URL(request.url ?? "/", "http://127.0.0.1");
      const send = (body: unknown, status = 200): void => {
        response.writeHead(status, {
          "content-type": "application/json;charset=utf-8",
        });
        response.end(JSON.stringify(body));
      };
      const matchPath = /^\/matches\/(\d+)@([0-9a-f]{64})$/u.exec(url.pathname);
      if (matchPath !== null) {
        const key = `${matchPath[2]!}#${Number(matchPath[1]!).toString()}`;
        const match = matches.get(key);
        if (match === undefined) {
          send([]);
          return;
        }
        // `?resolve_hashes` is a bare flag (`allowEmptyValue: true`), so its mere
        // presence is the request. Without it — and under the pre-v2.10.0 shape,
        // which ignores it — the match is served with neither join, which is what
        // makes a reader that does not ask, or an index that cannot answer,
        // visibly different from one that got its bytes.
        send([
          url.searchParams.has("resolve_hashes") && !ignoringResolveHashes
            ? resolveHashesOn(match)
            : match,
        ]);
        return;
      }
      const checkpointPath = /^\/checkpoints\/(\d+)$/u.exec(url.pathname);
      if (checkpointPath !== null) {
        const slot = Number(checkpointPath[1]!);
        // Kupo's flexible lookup: the most recent checkpoint at or before the
        // requested slot, `null` when the index has none.
        const ancestor = [...blocks]
          .reverse()
          .find((block) => block.slot <= slot);
        send(
          ancestor === undefined
            ? null
            : { slot_no: ancestor.slot, header_hash: ancestor.headerHash },
        );
        return;
      }
      send({ hint: "unsupported endpoint" }, 404);
    },
  );

  const ogmiosServer = createServer(
    (_request: IncomingMessage, response: ServerResponse) => {
      response.writeHead(400).end();
    },
  );

  const sockets = new Set<Duplex>();
  ogmiosServer.on("upgrade", (request: IncomingMessage, socket: Duplex) => {
    const key = request.headers["sec-websocket-key"];
    if (typeof key !== "string") {
      socket.destroy();
      return;
    }
    sockets.add(socket);
    socket.on("close", () => sockets.delete(socket));
    socket.write(
      [
        "HTTP/1.1 101 Switching Protocols",
        "Upgrade: websocket",
        "Connection: Upgrade",
        `Sec-WebSocket-Accept: ${createHash("sha1")
          .update(`${key}${WEBSOCKET_GUID}`)
          .digest("base64")}`,
        "",
        "",
      ].join("\r\n"),
    );
    let cursor = -1;
    let pendingRollback: LocalL1Point | null = null;
    const tip = (): unknown => {
      const last = blocks[blocks.length - 1]!;
      return { slot: last.slot, id: last.headerHash, height: last.height };
    };
    readWebSocketFrames(socket, (text) => {
      const request_ = JSON.parse(text) as {
        id?: unknown;
        method?: unknown;
        params?: { points?: readonly { slot: number; id: string }[] };
      };
      const reply = (result: unknown): void => {
        socket.write(
          encodeWebSocketFrame(
            JSON.stringify({
              jsonrpc: "2.0",
              method: request_.method,
              result,
              id: request_.id,
            }),
          ),
        );
      };
      if (request_.method === "findIntersection") {
        const points = request_.params?.points ?? [];
        const index = blocks.findIndex((block) =>
          points.some(
            (point) =>
              point.slot === block.slot && point.id === block.headerHash,
          ),
        );
        if (index === -1) {
          socket.write(
            encodeWebSocketFrame(
              JSON.stringify({
                jsonrpc: "2.0",
                method: "findIntersection",
                error: { code: 1000, message: "IntersectionNotFound" },
                id: request_.id,
              }),
            ),
          );
          return;
        }
        cursor = index;
        pendingRollback = {
          slot: blocks[index]!.slot,
          headerHash: blocks[index]!.headerHash,
        };
        reply({
          intersection: {
            slot: blocks[index]!.slot,
            id: blocks[index]!.headerHash,
          },
          tip: tip(),
        });
        return;
      }
      if (request_.method === "nextBlock") {
        if (pendingRollback !== null) {
          const point = pendingRollback;
          pendingRollback = null;
          reply({
            direction: "backward",
            tip: tip(),
            point: { slot: point.slot, id: point.headerHash },
          });
          return;
        }
        const next = blocks[cursor + 1];
        if (next === undefined) {
          // At the tip a real chain-sync simply does not answer until a block
          // arrives. Leaving the request outstanding is that behaviour.
          return;
        }
        cursor += 1;
        reply({
          direction: "forward",
          tip: tip(),
          block: {
            type: "praos",
            era: "conway",
            id: next.headerHash,
            ancestor: blocks[cursor - 1]?.headerHash ?? "genesis",
            height: next.height,
            slot: next.slot,
            size: { bytes: 1 },
            transactions: next.transactions,
            protocol: { version: { major: 10, minor: 0 } },
          },
        });
        return;
      }
      socket.write(
        encodeWebSocketFrame(
          JSON.stringify({
            jsonrpc: "2.0",
            method: request_.method,
            error: { code: 1001, message: "unsupported method" },
            id: request_.id,
          }),
        ),
      );
    });
  });

  const kupoPort = await listen(kupoServer);
  const ogmiosPort = await listen(ogmiosServer);

  return {
    kupoUrl: `http://127.0.0.1:${kupoPort.toString()}`,
    ogmiosUrl: `http://127.0.0.1:${ogmiosPort.toString()}`,
    appendBlock,
    rewriteDatums: (rewrite) => {
      datumRewrite = rewrite;
    },
    ignoreResolveHashes: (ignore) => {
      ignoringResolveHashes = ignore;
    },
    close: async () => {
      for (const socket of sockets) {
        socket.destroy();
      }
      await Promise.all(
        [kupoServer, ogmiosServer].map(
          (server) =>
            new Promise<void>((resolve) => {
              server.close(() => resolve());
            }),
        ),
      );
    },
  };
};
