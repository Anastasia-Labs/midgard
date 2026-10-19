import { mkdtemp, realpath, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import { afterEach, describe, expect, it, onTestFinished } from "vitest";

import {
  EXIT_NOT_FOUND,
  EXIT_NOT_READY,
  runFollowerCli,
} from "../src/cli/run.js";
import { blake2b256, findOrigin } from "../src/index.js";
import {
  type FakeBlock,
  fakeChains,
  rawBlock,
  txBody,
} from "./support/fake-chain.js";

const fakes = fakeChains();
afterEach(async () => {
  await fakes.cleanup();
});

const fill = (byte: number): Buffer => Buffer.alloc(32, byte);
/**
 * A scan that misses its stop at the node tip waits for a block that never
 * comes: this bound turns that into a failure instead of a hang under the
 * package's long default.
 */
const SCAN_BOUND = { timeout: 10_000 };
const PREPARE = txBody(fill(0x77), 200_000);
const PREPARE_ID = blake2b256(PREPARE).toString("hex");

/**
 * genesis <- b1(10) <- b2(20) <- b3(30, holds the prepare tx at index 1)
 * <- b4(40). A Byron-typed block with an undecodable payload sits at b1 to
 * show the scan skips Byron blocks.
 */
const makeChain = (): FakeBlock[] => {
  const b1 = {
    ...rawBlock(10, 1, null, []),
    blockType: 1,
    block: "ff",
  };
  const b2 = rawBlock(20, 2, b1.hash, [txBody(fill(0x01), 1)]);
  const b3 = rawBlock(30, 3, b2.hash, [txBody(fill(0x02), 2), PREPARE]);
  const b4 = rawBlock(40, 4, b3.hash, []);
  return [b1, b2, b3, b4];
};

describe("findOrigin over the fake sidecar", SCAN_BOUND, () => {
  const blocks = makeChain();
  const [b1, b2, b3] = blocks as [FakeBlock, FakeBlock, FakeBlock];
  const expected = {
    kind: "found",
    origin: { slot: 20, blockHash: b2.hash },
    nonceBlock: { slot: 30, blockHash: b3.hash, height: 3 },
    txIndex: 1,
    depth: 2,
  };

  it("returns the point immediately before the block holding the tx", async () => {
    const transport = await fakes.transport("origin", blocks);
    expect(await findOrigin({ transport, txHash: PREPARE_ID })).toEqual(
      expected,
    );
  });

  it("starts from a known earlier point", async () => {
    const transport = await fakes.transport("origin", blocks);
    expect(
      await findOrigin({
        transport,
        txHash: PREPARE_ID,
        from: { slot: b1.slot, blockHash: b1.hash },
      }),
    ).toEqual(expected);
  });

  it("is not_found at the tip when started after the block, or for an absent tx", async () => {
    const transport = await fakes.transport("origin", blocks);
    expect(
      await findOrigin({
        transport,
        txHash: PREPARE_ID,
        from: { slot: b3.slot, blockHash: b3.hash },
      }),
    ).toEqual({
      kind: "not_found",
      scannedTo: { slot: 40, blockHash: blocks[3]!.hash },
    });
    expect(
      await findOrigin({ transport, txHash: "ab".repeat(32) }),
    ).toMatchObject({ kind: "not_found" });
  });

  it("is not_found at once when started at the tip", async () => {
    const transport = await fakes.transport("origin", blocks);
    const tip = blocks[3]!;
    expect(
      await findOrigin({
        transport,
        txHash: PREPARE_ID,
        from: { slot: tip.slot, blockHash: tip.hash },
      }),
    ).toEqual({
      kind: "not_found",
      scannedTo: { slot: 40, blockHash: tip.hash },
    });
  });

  it("refuses a start point the node does not have", async () => {
    const transport = await fakes.transport("origin", blocks);
    expect(
      await findOrigin({
        transport,
        txHash: PREPARE_ID,
        from: { slot: 20, blockHash: "ee".repeat(32) },
      }),
    ).toEqual({ kind: "from_not_on_chain" });
  });

  it("has no origin for a tx in the chain's first block", async () => {
    const first = rawBlock(10, 1, null, [PREPARE]);
    const transport = await fakes.transport("origin", [first]);
    expect(await findOrigin({ transport, txHash: PREPARE_ID })).toEqual({
      kind: "no_preceding_point",
    });
  });
});

describe("midgard-l1-follower find-origin", SCAN_BOUND, () => {
  const run = async (args: readonly string[], env = {}) => {
    const out: string[] = [];
    const err: string[] = [];
    const code = await runFollowerCli(args, env, {
      stdout: (text) => out.push(text),
      stderr: (text) => err.push(text),
    });
    return { code, stdout: out.join(""), stderr: err.join("") };
  };

  it("prints l1Origin as JSON and exits 0", async () => {
    const blocks = makeChain();
    const sidecar = await fakes.sidecar("origin", blocks);
    const result = await run(
      ["find-origin", "--tx", PREPARE_ID, "--network-magic", "42"],
      {
        CARDANO_NODE_SOCKET_PATH: join(sidecar, "..", "node.socket"),
        MIDGARD_L1_NODE_TRANSPORT_BINARY: sidecar,
      },
    );
    expect(result.stderr).toBe("");
    expect(result.code).toBe(0);
    expect(JSON.parse(result.stdout)).toEqual({
      l1Origin: `20.${blocks[1]!.hash}`,
      origin: { slot: 20, blockHash: blocks[1]!.hash },
      prepareHubOracleNonceBlock: {
        slot: 30,
        blockHash: blocks[2]!.hash,
        height: 3,
      },
      txIndex: 1,
      depth: 2,
    });
  });

  it("exits 3 when the tx is not found", async () => {
    const sidecar = await fakes.sidecar("origin", makeChain());
    const result = await run([
      "find-origin",
      "--tx",
      "ab".repeat(32),
      "--network-magic",
      "42",
      "--socket",
      join(sidecar, "..", "node.socket"),
      "--sidecar",
      sidecar,
    ]);
    expect(result.code).toBe(EXIT_NOT_FOUND);
    expect(result.stdout).toBe("");
    expect(result.stderr).toMatch(/not on the node's chain/u);
  });

  describe("--recorded", () => {
    const recorded = async (
      point: (blocks: readonly FakeBlock[]) => string,
      extra: readonly string[] = [],
    ) => {
      const blocks = makeChain();
      const sidecar = await fakes.sidecar("origin", blocks);
      return {
        blocks,
        result: await run(
          [
            "find-origin",
            "--tx",
            PREPARE_ID,
            "--network-magic",
            "42",
            "--recorded",
            point(blocks),
            ...extra,
          ],
          {
            CARDANO_NODE_SOCKET_PATH: join(sidecar, "..", "node.socket"),
            MIDGARD_L1_NODE_TRANSPORT_BINARY: sidecar,
          },
        ),
      };
    };
    const at = (index: number) => (blocks: readonly FakeBlock[]) =>
      `${blocks[index]!.slot}.${blocks[index]!.hash}`;

    it.each([
      ["an earlier block", 0],
      ["the block just before the nonce's block", 1],
    ])(
      "FO-a: prints the recorded origin when it is %s on the node's chain",
      async (_name, index) => {
        const { blocks, result } = await recorded(at(index));
        expect(result.stderr).toBe("");
        expect(result.code).toBe(0);
        const point = blocks[index]!;
        expect(JSON.parse(result.stdout)).toEqual({
          l1Origin: `${point.slot}.${point.hash}`,
          origin: { slot: point.slot, blockHash: point.hash },
          prepareHubOracleNonceBlock: {
            slot: 30,
            blockHash: blocks[2]!.hash,
            height: 3,
          },
          txIndex: 1,
          depth: 2,
        });
      },
    );

    it.each([
      ["is the nonce's block", at(2), /does not precede the tx's block/u],
      ["follows the nonce's block", at(3), /does not precede the tx's block/u],
      [
        "is not on the node's chain",
        () => `20.${"ee".repeat(32)}`,
        /--recorded is not on the node's chain/u,
      ],
    ])(
      "FO-b: exits 3, printing nothing, when the recorded origin %s",
      async (_name, point, message) => {
        const { result } = await recorded(point);
        expect(result.code).toBe(EXIT_NOT_FOUND);
        expect(result.stdout).toBe("");
        expect(result.stderr).toMatch(message);
      },
    );

    it.each([
      [
        "with --from",
        at(0),
        ["--from", `20.${"ee".repeat(32)}`],
        /^--recorded and --from exclude each other/u,
      ],
      ["malformed", () => "20", [], /^--recorded: expected/u],
    ])(
      "FO-b: refuses --recorded %s with exit 2",
      async (_name, point, extra, message) => {
        const { result } = await recorded(point, extra);
        expect(result.code).toBe(2);
        expect(result.stdout).toBe("");
        expect(result.stderr).toMatch(message);
      },
    );
  });

  const WIRED = [
    "find-origin",
    "--tx",
    PREPARE_ID,
    "--network-magic",
    "42",
    "--socket",
    "/nonexistent/node.socket",
    "--sidecar",
    "/nonexistent/sidecar",
  ];
  it.each([
    [["find-origin", "--network-magic", "42"], /--tx is required/u],
    [
      ["find-origin", "--tx", "AB".repeat(32), "--network-magic", "42"],
      /--tx must be/u,
    ],
    [
      ["find-origin", "--tx", PREPARE_ID, "--network-magic", "x"],
      /--network-magic must be/u,
    ],
    [
      ["find-origin", "--tx", PREPARE_ID, "--network-magic", "42"],
      /--socket is required/u,
    ],
    [[...WIRED, "--from", "20"], /^--from: expected/u],
    [[...WIRED, "--bogus", "1"], /^unknown flag --bogus/u],
    [["origin"], /Usage:/u],
  ])("refuses bad usage %j with exit 2", async (args, message) => {
    const result = await run(args);
    expect(result.code).toBe(2);
    expect(result.stderr).toMatch(message);
  });
});

describe("midgard-l1-follower find-origin on a node transport that never becomes ready", () => {
  const notReady = async (sidecar: (directory: string) => Promise<string>) => {
    const directory = await realpath(
      await mkdtemp(join(tmpdir(), "l1-follower-not-ready-")),
    );
    onTestFinished(() => rm(directory, { recursive: true, force: true }));
    const out: string[] = [];
    const err: string[] = [];
    const startedAt = Date.now();
    const code = await runFollowerCli(
      [
        "find-origin",
        "--tx",
        PREPARE_ID,
        "--network-magic",
        "42",
        "--socket",
        join(directory, "node.socket"),
        "--sidecar",
        await sidecar(directory),
      ],
      {},
      { stdout: (text) => out.push(text), stderr: (text) => err.push(text) },
      { readyTimeoutMs: 3_000 },
    );
    return {
      code,
      stdout: out.join(""),
      stderr: err.join(""),
      elapsedMs: Date.now() - startedAt,
    };
  };

  it("R-1: exits 5 naming sidecar_unavailable when the sidecar binary is missing", async () => {
    const result = await notReady(async (directory) =>
      join(directory, "missing-sidecar"),
    );
    expect(result.code).toBe(EXIT_NOT_READY);
    expect(result.stdout).toBe("");
    expect(result.stderr).toMatch(
      /not ready within 3 s \(sidecar_unavailable: .*--sidecar/u,
    );
    expect(result.elapsedMs).toBeLessThan(10_000);
  }, 20_000);

  it("R-1: exits 5 naming node_unreachable when the node socket does not accept", async () => {
    const result = await notReady(async (directory) => {
      const lostPath = join(directory, "node-lost");
      await writeFile(lostPath, "lost");
      return writeFakeSidecar({
        path: join(directory, "sidecar"),
        handlerModule: fileURLToPath(
          new URL("./fixtures/node-loss-handler.mjs", import.meta.url),
        ),
        options: {
          base: "origin",
          blocks: makeChain(),
          lostPath,
          backPath: join(directory, "node-back"),
        },
      });
    });
    expect(result.code).toBe(EXIT_NOT_READY);
    expect(result.stdout).toBe("");
    expect(result.stderr).toMatch(
      /not ready within 3 s \(node_unreachable: .*--socket/u,
    );
    expect(result.elapsedMs).toBeLessThan(10_000);
  }, 20_000);
});
