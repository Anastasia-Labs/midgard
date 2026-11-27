/**
 * The origin the journey deployment records in its manifest, read from a
 * node (a fake sidecar) serving a chain whose third block holds the
 * hub-oracle nonce tx: the recorded origin is the point before that block,
 * and the journey session's own proof (`deriveL1Origin`) accepts it on the
 * same chain. A nonce the chain does not hold leaves no origin to record.
 */
import { mkdtemp, realpath, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import {
  SIM_ORIGIN,
  SimChain,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import { afterEach, describe, expect, it } from "vitest";

import {
  deriveL1Origin,
  L1OriginUndeterminedError,
} from "../../src/l1-origin.js";
import { deploymentL1Origin } from "./deployment-l1-origin.js";

const handlerModule = fileURLToPath(
  new URL(
    "./deployment-l1-origin.chain-handler.test-support.mjs",
    import.meta.url,
  ),
);

/** Five blocks: two empty, the nonce's, two empty up to the tip. */
const deployedChain = () => {
  const universe = simUniverse();
  const chain = new SimChain(universe, SIM_ORIGIN);
  const transfer = () => ({
    inputs: [chain.outsideInput()],
    outputs: [{ address: universe.untrackedAddress, lovelace: 2_000_000n }],
    nonce: chain.nonce(),
  });
  const blocks = [
    chain.forward([transfer()]),
    chain.forward([]),
    chain.forward([transfer(), transfer()]),
    chain.forward([transfer()]),
    chain.forward([]),
  ];
  return {
    blocks: blocks.map(({ event }) => ({
      slot: Number(event.point.slot),
      hash: event.point.hash,
      block: Buffer.from(event.block).toString("hex"),
      blockType: event.blockType,
    })),
    points: blocks.map(({ event }) => ({
      slot: Number(event.point.slot),
      blockHash: event.point.hash,
    })),
    nonceTxHash: blocks[2]!.encoded.txHashes[1]!.toString("hex"),
  };
};

let directory: string | undefined;
afterEach(async () => {
  if (directory !== undefined)
    await rm(directory, { recursive: true, force: true });
  directory = undefined;
});

const nodeServing = async (chain: ReturnType<typeof deployedChain>) => {
  directory = await realpath(
    await mkdtemp(join(tmpdir(), "midgard-journey-origin-")),
  );
  const binaryPath = await writeFakeSidecar({
    path: join(directory, "sidecar"),
    handlerModule,
    options: { blocks: chain.blocks },
  });
  return {
    socketPath: join(directory, "node.socket"),
    binaryPath,
    networkMagic: 42,
  };
};

describe("the journey deployment's recorded L1 origin", () => {
  it("is the point before the nonce's block, and the session proves it", async () => {
    const chain = deployedChain();
    const node = await nodeServing(chain);
    const origin = await deploymentL1Origin({
      node,
      nonceTxHash: chain.nonceTxHash,
    });
    expect(origin).toEqual(chain.points[1]);
    const proven = await deriveL1Origin({
      node,
      nonceTxHash: chain.nonceTxHash,
      recorded: origin,
    });
    expect(proven).toEqual({
      origin: chain.points[1],
      nonceTxHash: chain.nonceTxHash,
      nonceBlock: chain.points[2],
    });
  });

  it("is refused for a nonce the node's chain does not hold", async () => {
    const chain = deployedChain();
    const node = await nodeServing(chain);
    const absent = "ab".repeat(32);
    const refusal = deploymentL1Origin({ node, nonceTxHash: absent });
    await expect(refusal).rejects.toBeInstanceOf(L1OriginUndeterminedError);
    await expect(refusal).rejects.toThrow(absent);
  });
});
