import { execFileSync } from "node:child_process";
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

import { afterAll, describe, expect, inject, it } from "vitest";

import {
  type BlockPoint,
  CborMap,
  type ChainSyncEvent,
  decodeCbor,
  L1NodeTransport,
  ORIGIN,
} from "../src/index.js";
import { headerHash, MockNode, within } from "./mock-node.js";

/**
 * Opt-in conformance against a real cardano-node: a phase4 devnet run
 * directory (running-the-devnet skill) whose cardano-node is up. Set
 * MIDGARD_L1_TRANSPORT_DEVNET_RUN_DIR to that run directory.
 */
const runDirectory = process.env.MIDGARD_L1_TRANSPORT_DEVNET_RUN_DIR?.trim();
const enabled = runDirectory !== undefined && runDirectory !== "";

const runEnv = (): Record<string, string> =>
  Object.fromEntries(
    readFileSync(join(runDirectory!, "run.env"), "utf8")
      .split("\n")
      .filter((line) => line.includes("="))
      .map((line) => [
        line.slice(0, line.indexOf("=")),
        line.slice(line.indexOf("=") + 1),
      ]),
  );

describe.skipIf(!enabled)("real node conformance (devnet)", () => {
  const env = enabled ? runEnv() : {};
  const magic = Number(env.MIDGARD_PHASE4_NETWORK_MAGIC);
  const socketPath = enabled
    ? join(runDirectory!, "cardano/ipc/node.socket")
    : "";
  const cli = (...args: string[]): string =>
    execFileSync(
      "docker",
      [
        "run",
        "--rm",
        "--user",
        `${process.getuid!()}:${process.getgid!()}`,
        "--volume",
        `${runDirectory}:/run`,
        "--entrypoint",
        "cardano-cli",
        env.MIDGARD_PHASE4_CARDANO_NODE_IMAGE!,
        "latest",
        ...args,
      ],
      { encoding: "utf8", timeout: 120_000 },
    ).trim();
  const transport = enabled
    ? new L1NodeTransport({
        binaryPath: inject("sidecarBinary"),
        socketPath,
        networkMagic: magic,
        requestTimeoutMs: 60_000,
      })
    : undefined;
  afterAll(async () => await transport?.close());

  it("follows the chain from the origin with raw blocks that hash to their points", async () => {
    await transport!.whenReady(30_000);
    const stream = transport!.openChainSync({ points: [ORIGIN], credit: 50 });
    const opened = await stream.opened;
    expect(opened.intersection).toEqual(ORIGIN);
    const tip = opened.tip.blockNo;
    expect(tip).toBeGreaterThan(2n);
    const events: ChainSyncEvent[] = [];
    let previous: string | null = null;
    while (events.length < Number(tip)) {
      const event = await within(stream.next(), 30_000);
      if (event === "timeout" || event === undefined) throw new Error("stall");
      events.push(event);
      stream.ack(event.seq);
      expect(event.seq).toBe(BigInt(events.length));
      expect(event.kind).toBe("roll_forward");
      if (event.kind !== "roll_forward") continue;
      expect(headerHash(event.block)).toBe(event.point.hash);
      if (previous !== null) expect(event.prevHash).toBe(previous);
      previous = event.point.hash;
    }
    await stream.close();
    // FindIntersect takes the first point of the list the node knows.
    const known = events[1]!.point as BlockPoint;
    const second = transport!.openChainSync({
      points: [
        { kind: "point", slot: known.slot + 1n, hash: "ab".repeat(32) },
        known,
        ORIGIN,
      ],
      credit: 1,
    });
    expect((await second.opened).intersection).toEqual(known);
    const next = await second.next();
    expect(next?.point).toEqual(events[2]!.point);
    await second.close();
  });

  it("resumes after the sidecar is killed with no gap and no duplicate", async () => {
    // The sidecar is a child of this process; SIGKILL leaves no orderly end.
    const sidecarPids = (): number[] =>
      readdirSync("/proc")
        .filter((entry) => /^\d+$/.test(entry))
        .filter((pid) => {
          try {
            const stat = readFileSync(`/proc/${pid}/stat`, "utf8");
            const parent = Number(
              stat.slice(stat.lastIndexOf(")") + 2).split(" ")[1],
            );
            return (
              parent === process.pid &&
              readFileSync(`/proc/${pid}/cmdline`, "utf8").startsWith(
                inject("sidecarBinary"),
              )
            );
          } catch {
            return false;
          }
        })
        .map(Number);
    await transport!.whenReady(30_000);
    const stream = transport!.openChainSync({ points: [ORIGIN], credit: 3 });
    const tip = (await stream.opened).tip.blockNo;
    const seen: ChainSyncEvent[] = [];
    let killed = false;
    while (seen.length < Number(tip)) {
      const event = await within(stream.next(), 60_000);
      if (event === "timeout" || event === undefined) throw new Error("stall");
      seen.push(event);
      expect(event.seq).toBe(BigInt(seen.length));
      expect(event.kind).toBe("roll_forward");
      if (event.kind === "roll_forward" && seen.length > 1) {
        const before = seen[seen.length - 2]!;
        expect(event.prevHash).toBe(
          before.point.kind === "point" ? before.point.hash : null,
        );
      }
      stream.ack(event.seq);
      if (!killed && seen.length === 4) {
        const pids = sidecarPids();
        expect(pids.length).toBe(1);
        process.kill(pids[0]!, "SIGKILL");
        killed = true;
      }
    }
    expect(killed).toBe(true);
    await stream.close();
  });

  it("answers local state queries with the node's raw results", async () => {
    const genesis = JSON.parse(
      readFileSync(join(runDirectory!, "genesis/shelley-genesis.json"), "utf8"),
    ) as { systemStart: string };
    const start = new Date(genesis.systemStart);
    const [year, day, picos] = decodeCbor(
      await transport!.query({ query: "system_start" }),
    ) as [number, number, number | bigint];
    const dayOfYear =
      (Date.UTC(
        start.getUTCFullYear(),
        start.getUTCMonth(),
        start.getUTCDate(),
      ) -
        Date.UTC(start.getUTCFullYear(), 0, 1)) /
        86_400_000 +
      1;
    expect([year, day]).toEqual([start.getUTCFullYear(), dayOfYear]);
    expect(BigInt(picos)).toBe(
      BigInt(
        start.getUTCHours() * 3600 +
          start.getUTCMinutes() * 60 +
          start.getUTCSeconds(),
      ) * 1_000_000_000_000n,
    );
    expect(decodeCbor(await transport!.query({ query: "current_era" }))).toBe(
      6,
    );
    expect(
      Array.isArray(
        decodeCbor(await transport!.query({ query: "era_history" })),
      ),
    ).toBe(true);
    const params = decodeCbor(
      await transport!.query({ query: "protocol_params" }),
    ) as unknown[];
    expect(Array.isArray(params) && params.length).toBeGreaterThan(20);
    await transport!.withLedgerState("tip", async (state) => {
      const point = decodeCbor(await state.query({ query: "chain_point" }));
      expect(Array.isArray(point) && point.length).toBe(2);
    });
  });

  it("returns the node's raw rejection and agrees with the mempool", async () => {
    // A well-formed transaction from another chain: the ledger rejects it.
    const mock = await MockNode.start(inject("mockNodeBinary"));
    const sample = await mock.command({ op: "sampleTx" });
    await mock.close();
    const foreign = await transport!.submit(
      Uint8Array.from(Buffer.from(sample.tx as string, "hex")),
    );
    expect(foreign.accepted).toBe(false);
    if (!foreign.accepted) {
      expect(foreign.rejection.length).toBeGreaterThan(0);
      expect(() => decodeCbor(foreign.rejection)).not.toThrow();
    }
    expect(await transport!.hasTx(sample.id as string)).toBe(false);

    // A valid transaction built and signed by cardano-cli, submitted here.
    const address = cli(
      "address",
      "build",
      "--payment-verification-key-file",
      "/run/genesis/utxo-keys/utxo1/utxo.vkey",
      "--testnet-magic",
      String(magic),
    );
    cli(
      "query",
      "utxo",
      "--socket-path",
      "/run/cardano/ipc/node.socket",
      "--testnet-magic",
      String(magic),
      "--address",
      address,
      "--out-file",
      "/run/work/f1-transport-utxos.json",
    );
    const utxos = JSON.parse(
      readFileSync(join(runDirectory!, "work/f1-transport-utxos.json"), "utf8"),
    ) as Record<string, { value: { lovelace: number } }>;
    // The largest output: earlier runs leave 5 ADA outputs behind.
    const input = Object.entries(utxos).sort(
      ([, a], [, b]) => b.value.lovelace - a.value.lovelace,
    )[0]![0];
    cli(
      "transaction",
      "build",
      "--socket-path",
      "/run/cardano/ipc/node.socket",
      "--testnet-magic",
      String(magic),
      "--tx-in",
      input,
      "--tx-out",
      `${address}+5000000`,
      "--change-address",
      address,
      "--out-file",
      "/run/work/f1-transport.txbody",
    );
    cli(
      "transaction",
      "sign",
      "--tx-body-file",
      "/run/work/f1-transport.txbody",
      "--signing-key-file",
      "/run/genesis/utxo-keys/utxo1/utxo.skey",
      "--testnet-magic",
      String(magic),
      "--out-file",
      "/run/work/f1-transport.tx",
    );
    const envelope = JSON.parse(
      readFileSync(join(runDirectory!, "work/f1-transport.tx"), "utf8"),
    ) as { cborHex: string };
    const txId = (
      JSON.parse(
        cli(
          "transaction",
          "txid",
          "--tx-file",
          "/run/work/f1-transport.tx",
          "--output-json",
        ),
      ) as { txhash: string }
    ).txhash;
    const result = await transport!.submit(
      Uint8Array.from(Buffer.from(envelope.cborHex, "hex")),
    );
    expect(result).toEqual({ accepted: true });
    const cliHasTx = (): boolean =>
      (
        JSON.parse(
          cli(
            "query",
            "tx-mempool",
            "--socket-path",
            "/run/cardano/ipc/node.socket",
            "--testnet-magic",
            String(magic),
            "tx-exists",
            txId,
          ),
        ) as { exists: boolean }
      ).exists;
    // Present: ours sees it at once, and cardano-cli agrees whenever the
    // transaction is still pending on both sides of its (slower) query.
    expect(await transport!.hasTx(txId)).toBe(true);
    const theirs = cliHasTx();
    if (await transport!.hasTx(txId)) expect(theirs).toBe(true);
    // Once a block includes it, both say it has left the mempool.
    let present = true;
    for (let attempt = 0; present && attempt < 120; attempt++) {
      await new Promise((resolve) => setTimeout(resolve, 2000));
      present = await transport!.hasTx(txId);
    }
    expect(present).toBe(false);
    expect(cliHasTx()).toBe(false);
    const included = decodeCbor(
      await transport!.query({
        query: "utxo_by_txin",
        txIns: [{ txId, index: 0 }],
      }),
    );
    // The included transaction's first output is now in the ledger.
    expect(included instanceof CborMap && included.entries.length).toBe(1);
  }, 300_000);
});
