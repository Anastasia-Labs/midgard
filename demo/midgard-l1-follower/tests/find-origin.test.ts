import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import { EXIT_NOT_FOUND, runFollowerCli } from "../src/cli/run.js";
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

describe("findOrigin over the fake sidecar", () => {
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

describe("midgard-l1-follower find-origin", () => {
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
