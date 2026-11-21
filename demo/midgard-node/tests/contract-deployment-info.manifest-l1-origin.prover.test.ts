/**
 * The finalization proof of the recorded L1 origin over the node's local
 * node (`localNodeManifestL1OriginProver`), against a fake sidecar serving a
 * chain whose second block holds the hub-oracle nonce tx: the recorded
 * origin is proved only when the chain from it reaches that block, so an
 * origin after the nonce's block and one off the chain are both refused,
 * though the nonce tx itself is on the chain.
 */
import { mkdtemp, realpath, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import {
  SIM_ORIGIN,
  SimChain,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import { Effect, Exit } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import {
  localNodeManifestL1OriginProver,
  provenManifestL1Origin,
} from "../src/commands/contract-deployment-info.manifest-l1-origin.js";

const handlerModule = fileURLToPath(
  new URL("./fixtures/origin-chain-handler.mjs", import.meta.url),
);

/**
 * Three blocks after the simulated origin: an empty one, the one holding
 * the nonce tx, and an empty one at the tip, as the fixture serves them.
 */
const deployedChain = () => {
  const universe = simUniverse();
  const chain = new SimChain(universe, SIM_ORIGIN);
  const blocks = [
    chain.forward([]),
    chain.forward([
      {
        inputs: [chain.outsideInput()],
        outputs: [{ address: universe.untrackedAddress, lovelace: 2_000_000n }],
        nonce: chain.nonce(),
      },
    ]),
    chain.forward([]),
  ];
  const points = blocks.map(({ event }) => ({
    slot: Number(event.point.slot),
    blockHash: event.point.hash,
  }));
  const forwards = blocks.map(({ event }) => ({
    forward: {
      slot: Number(event.point.slot),
      hash: event.point.hash,
      block: Buffer.from(event.block).toString("hex"),
      blockType: event.blockType,
    },
  }));
  return {
    points,
    forwards,
    nonceTxHash: blocks[1]!.encoded.txHashes[0]!.toString("hex"),
    tip: { slot: points[2]!.slot, hash: points[2]!.blockHash, blockNo: 3 },
  };
};

let directory: string | undefined;
afterEach(async () => {
  if (directory !== undefined)
    await rm(directory, { recursive: true, force: true });
  directory = undefined;
});

/**
 * Proves `recorded` on a local node serving `chain`: every scan from a block
 * is answered by `fromRecorded`, and one from genesis by the whole chain.
 */
const prove = async (
  chain: ReturnType<typeof deployedChain>,
  recorded: L1Origin,
  fromRecorded: { notFound: true } | { events: readonly unknown[] },
) => {
  directory = await realpath(
    await mkdtemp(join(tmpdir(), "midgard-manifest-origin-")),
  );
  await writeFile(
    join(directory, "shelley-genesis.json"),
    JSON.stringify({ networkMagic: 42 }),
  );
  await writeFile(
    join(directory, "config.json"),
    JSON.stringify({ ShelleyGenesisFile: "shelley-genesis.json" }),
  );
  const binaryPath = await writeFakeSidecar({
    path: join(directory, "sidecar"),
    handlerModule,
    options: {
      tip: chain.tip,
      settles: [fromRecorded],
      genesis: { events: chain.forwards },
    },
  });
  return Effect.runPromise(
    Effect.scoped(
      Effect.gen(function* () {
        const prover = yield* localNodeManifestL1OriginProver({
          NETWORK: "Custom",
          L1_NATIVE_LEDGER: {
            socketPath: join(directory!, "node.socket"),
            nodeConfigPath: join(directory!, "config.json"),
            binaryPath,
          },
        });
        const result = yield* Effect.promise(() =>
          prover!(recorded, chain.nonceTxHash),
        );
        const proven = yield* Effect.exit(
          provenManifestL1Origin({
            recordedOrigin: recorded,
            nonceTxHash: chain.nonceTxHash,
            prover,
          }),
        );
        return { result, proven };
      }),
    ),
  );
};

const refusal = (exit: Exit.Exit<unknown, Error>): string =>
  Exit.isFailure(exit) && exit.cause._tag === "Fail"
    ? exit.cause.error.message
    : "not refused";

describe("the finalization proof of the recorded L1 origin on the local node", () => {
  it("M-p: proves an origin the chain from which reaches the nonce's block", async () => {
    const chain = deployedChain();
    const { result, proven } = await prove(chain, chain.points[0]!, {
      events: chain.forwards.slice(1),
    });
    expect(result).toMatchObject({
      kind: "found",
      origin: chain.points[0],
      nonceBlock: chain.points[1],
    });
    expect(proven).toEqual(Exit.succeed(chain.points[0]));
  });

  it("M-p: refuses an origin after the nonce's block", async () => {
    const chain = deployedChain();
    const { result, proven } = await prove(chain, chain.points[2]!, {
      events: [],
    });
    expect(result.kind).toBe("not_found");
    expect(refusal(proven)).toContain("does not hold the hub-oracle nonce tx");
  });

  it("M-p: refuses an origin that is not on the local node's chain", async () => {
    const chain = deployedChain();
    const offChain = {
      slot: chain.points[0]!.slot,
      blockHash: "9e".repeat(32),
    };
    const { result, proven } = await prove(chain, offChain, {
      notFound: true,
    });
    expect(result.kind).toBe("from_not_on_chain");
    expect(refusal(proven)).toContain("is not on the local node's chain");
  });
});
