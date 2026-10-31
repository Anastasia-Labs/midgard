import { mkdtemp, realpath, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { L1NodeTransport } from "@al-ft/l1-node-transport";
import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";

import { blake2b256 } from "../../src/index.js";
import * as c from "../../src/testing/cbor-writer.js";

/** One block as the fake chain handler (fixtures/chain-handler.mjs) serves it. */
export type FakeBlock = Readonly<{
  slot: number;
  hash: string;
  blockNo: number;
  prevHash: string | null;
  blockType?: number;
  /** The raw block, hex. */
  block: string;
}>;

export type FakeBase = "origin" | Readonly<{ slot: number; hash: string }>;

const handlerModule = fileURLToPath(
  new URL("../fixtures/chain-handler.mjs", import.meta.url),
);

/** A minimal tx body spending `input`: its hash is blake2b-256 of these bytes. */
export const txBody = (input: Buffer, fee: number): Buffer =>
  c.map(
    [c.uint(0), c.tag(258, c.array(c.array(c.bytes(input), c.uint(0))))],
    [c.uint(1), c.array()],
    [c.uint(2), c.uint(fee)],
  );

/**
 * A raw Conway-shaped block at `slot` and `height` extending `parent`
 * (null for the chain's first block), holding `bodies`. Its hash is
 * blake2b-256 of the header, as on chain.
 */
export const rawBlock = (
  slot: number,
  height: number,
  parent: string | null,
  bodies: readonly Buffer[],
): FakeBlock => {
  const header = c.array(
    c.array(
      c.uint(height),
      c.uint(slot),
      parent === null ? c.nul : c.bytes(Buffer.from(parent, "hex")),
      c.bytes(Buffer.alloc(32)),
    ),
    c.bytes(Buffer.alloc(64)),
  );
  const block = c.array(
    header,
    c.array(...bodies),
    c.array(...bodies.map(() => c.map())),
    c.map(),
    c.array(),
  );
  return {
    slot,
    hash: blake2b256(header).toString("hex"),
    blockNo: height,
    prevHash: parent,
    block: block.toString("hex"),
  };
};

/** Fake sidecars and transports over fixed chains, removed by `cleanup`. */
export const fakeChains = () => {
  const transports: L1NodeTransport[] = [];
  let directory: string | undefined;
  const scratch = async (): Promise<string> =>
    (directory ??= await realpath(
      await mkdtemp(join(tmpdir(), "l1-follower-fake-")),
    ));
  return {
    /** Writes an executable fake sidecar serving the chain; returns its path. */
    sidecar: async (base: FakeBase, blocks: readonly FakeBlock[]) =>
      writeFakeSidecar({
        path: join(
          await scratch(),
          `fake-${transports.length}-${blocks.length}-${String(Math.random()).slice(2)}`,
        ),
        handlerModule,
        options: { base, blocks },
      }),
    transport: async (
      base: FakeBase,
      blocks: readonly FakeBlock[],
      /** See fixtures/chain-handler.mjs. */
      faults: Readonly<{
        failingOpens?: number;
        servedBeforeFailing?: number;
      }> = {},
    ): Promise<L1NodeTransport> => {
      const dir = await scratch();
      const transport = new L1NodeTransport({
        binaryPath: await writeFakeSidecar({
          path: join(dir, `fake-${transports.length}`),
          handlerModule,
          options: { base, blocks, ...faults },
        }),
        socketPath: join(dir, "node.socket"),
        networkMagic: 42,
        requestTimeoutMs: 10_000,
      });
      transports.push(transport);
      return transport;
    },
    cleanup: async () => {
      await Promise.all(transports.splice(0).map((t) => t.close()));
      if (directory !== undefined)
        await rm(directory, { recursive: true, force: true });
      directory = undefined;
    },
  };
};
