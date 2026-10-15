import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { fixtureLedgerEntries } from "@al-ft/midgard-fault-proofs/test-support/fixture-ledger";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import type { JourneyBlock } from "./fixture.js";
import { persistableJourneyBlock } from "./journey-ledger.js";

const fundedRecoveryPredecessor = async (base: JourneyBlock, address: string) =>
  persistableJourneyBlock(
    await buildCanonicalBlockFixture({
      ...base.header,
      transactions: [],
      utxos: [
        {
          key: outRefCbor(0x81, 0n),
          value: encodeMidgardTxOutput({
            address: Buffer.from(
              CML.Address.from_bech32(address).to_raw_bytes(),
            ),
            value: { lovelace: 100_000_000n, assets: new Map() },
          }),
        },
      ],
    }),
  );

/** Input-only checkpoint blocks with independently recorded fixture state. */
export const buildRecoveryStageBlocks = async (input: {
  operatorVkey: string;
  address: string;
  funded: boolean;
}) => {
  const predecessor = {
    ...persistableJourneyBlock(
      await depositEventsRetainedBlock({
        operatorVkey: input.operatorVkey,
        startTime: 0n,
        endTime: 60_000n,
        blockSlot: 60n,
        prevHeaderHash: SDK.GENESIS_HEADER_HASH,
        prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        priorLedger: [],
        events: [],
      }),
    ),
    commitTxHash: "11".repeat(32),
  };
  if (input.funded)
    Object.assign(
      predecessor,
      await fundedRecoveryPredecessor(predecessor, input.address),
    );
  const current = persistableJourneyBlock(
    await depositEventsRetainedBlock({
      operatorVkey: input.operatorVkey,
      startTime: predecessor.header.endTime,
      endTime: 120_000n,
      blockSlot: 120n,
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: predecessor.header.utxosRoot,
      priorLedger: fixtureLedgerEntries(predecessor.header.utxosRoot),
      events: [],
    }),
  );
  return { predecessor, current };
};
