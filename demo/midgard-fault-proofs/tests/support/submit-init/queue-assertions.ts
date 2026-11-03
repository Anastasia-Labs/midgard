import {
  headerHashFromStateQueueUTxO,
  type MidgardValidators,
  sortStateQueueUTxOs,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  utxosToStateQueueUTxOs,
} from "@al-ft/midgard-sdk";
import { Lucid } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

export const expectStateQueueHeaderOrder = async ({
  lucid,
  contracts,
  expectedHeaderHashes,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly expectedHeaderHashes: readonly string[];
}) => {
  const utxos = await lucid.utxosAt(contracts.stateQueue.spendingScriptAddress);
  const parsedStateQueueUtxos = await Effect.runPromise(
    utxosToStateQueueUTxOs(utxos, contracts.stateQueue.policyId),
  );
  expect(parsedStateQueueUtxos).toHaveLength(expectedHeaderHashes.length + 1);
  expect(
    parsedStateQueueUtxos.map(({ assetName }) => assetName).sort(),
  ).toEqual(
    [
      STATE_QUEUE_ROOT_ASSET_NAME,
      ...expectedHeaderHashes.map(
        (headerHash) => STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
      ),
    ].sort(),
  );

  const sortedStateQueueUtxos = await Effect.runPromise(
    Effect.succeed(parsedStateQueueUtxos).pipe(
      Effect.andThen(sortStateQueueUTxOs),
    ),
  );
  expect(sortedStateQueueUtxos).toHaveLength(parsedStateQueueUtxos.length);
  const [root, ...blocks] = sortedStateQueueUtxos;
  if (root === undefined) {
    throw new Error("Expected state-queue topology to include the root node");
  }
  expect(root.assetName).toBe(STATE_QUEUE_ROOT_ASSET_NAME);
  expect(root.datum.key).toBe("Empty");
  expect(root.datum.next).toEqual(
    expectedHeaderHashes[0] === undefined
      ? "Empty"
      : { Key: { key: expectedHeaderHashes[0] } },
  );

  const observedHeaderHashes = await Promise.all(
    blocks.map((block) =>
      Effect.runPromise(headerHashFromStateQueueUTxO(block)),
    ),
  );
  expect(observedHeaderHashes).toEqual(expectedHeaderHashes);
  expect(new Set(observedHeaderHashes).size).toBe(observedHeaderHashes.length);

  for (let index = 0; index < blocks.length; index += 1) {
    const block = blocks[index]!;
    const expectedHeaderHash = expectedHeaderHashes[index]!;
    const nextExpectedHeaderHash = expectedHeaderHashes[index + 1];
    expect(block.datum.key).toEqual({ Key: { key: expectedHeaderHash } });
    expect(block.datum.next).toEqual(
      nextExpectedHeaderHash === undefined
        ? "Empty"
        : { Key: { key: nextExpectedHeaderHash } },
    );
  }
};
