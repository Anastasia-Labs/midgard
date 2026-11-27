import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import {
  emptyLedger,
  LedgerRootMismatchError,
} from "@al-ft/midgard-fault-proofs/ledger-at";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { memoryLedger } from "@al-ft/midgard-fault-proofs/test-support/fixture-ledger";
import { expect, it } from "vitest";

import { decodeJourneyRetainedBlock } from "./staging.decode-journey-retained-block.js";

it("requires separately available replay state and checks it against the retained header root", async () => {
  const entry = {
    outRef: outRefCbor(0x71, 0n),
    output: encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x72)]),
      value: { lovelace: 8_000_000n, assets: new Map() },
    }),
  };
  const block = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: [{ key: entry.outRef, value: entry.output }],
  });
  const requested: string[] = [];
  const decoded = await decodeJourneyRetainedBlock(block, async (hash) => {
    requested.push(hash);
    return await memoryLedger([entry], block.header.utxosRoot);
  });
  expect(requested).toEqual([block.headerHash]);
  expect(decoded.ledgerEntries[0]?.outRef).toEqual(entry.outRef);
  expect(decoded.payload.block_body).not.toHaveProperty("utxos");
  await expect(
    decodeJourneyRetainedBlock(block, async () => undefined),
  ).rejects.toThrow(/ledger.*unavailable/i);
  await expect(
    decodeJourneyRetainedBlock(block, async () => emptyLedger()),
  ).rejects.toBeInstanceOf(LedgerRootMismatchError);
});
