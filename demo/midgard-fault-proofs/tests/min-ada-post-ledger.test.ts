import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import { MIN_ADA_VIOLATION_ID } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/index.js";
import { type LedgerAt, type LedgerEntry } from "../src/ledger-at.js";
import { prepareMinAdaUtxoFromCanonicalEvidence } from "../src/min-ada/prepare.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  completeCanonicalReplayPostLedger,
  completeCanonicalReplayPredecessorLedger,
  MIN_ADA_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  type FixtureTransaction,
} from "./helpers/canonical-block-evidence-fixture.js";
import { ledgerEntriesAfter } from "./support/predecessor-ledger.js";

const POST_LEDGER_UNAVAILABLE = "authenticated-post-ledger-context-unavailable";

const utxo = (lovelace: bigint, txByte: number) => ({
  key: encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, txByte),
    outputIndex: 0,
  }),
  value: encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x44)]),
    value: { lovelace, assets: new Map() },
  }),
});

const blockWith = async (
  utxos: readonly ReturnType<typeof utxo>[],
  transactions: readonly FixtureTransaction[] = [],
): Promise<CanonicalBlockEvidence> => {
  const fixture = await buildCanonicalBlockFixture({ transactions, utxos });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/min-ada-post-ledger-test",
      grade: "security",
    },
  });
};

/** A source holding the empty predecessor ledger and `post` for `block`. */
const sourceFor =
  (
    block: CanonicalBlockEvidence,
    post: readonly LedgerEntry[] | undefined,
  ): LedgerAt =>
  async (headerHash) => {
    if (headerHash === block.header.prevHeaderHash) return [];
    return headerHash === block.headerHash ? post : undefined;
  };

const replayWith = async (
  block: CanonicalBlockEvidence,
  post: readonly LedgerEntry[] | undefined,
) => {
  const context = Object.freeze({
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      ledgerAt: sourceFor(block, post),
      currentEvidence: block,
    }),
  });
  const decision = await MIN_ADA_COMPLETE_CANONICAL_REPLAY.replay(
    block,
    context,
  );
  return { context, detections: decision.detections };
};

const violationIds = (
  detections: readonly { readonly violationId: string }[],
): string[] => detections.map(({ violationId }) => violationId);

describe("MIN-ADA-UTXO reads the ledger after the challenged block", () => {
  it("detects an underfunded introduced output and proves it from the admitted post ledger", async () => {
    const block = await blockWith([utxo(1n, 0x55), utxo(2_000_000n, 0x66)]);
    const { context, detections } = await replayWith(
      block,
      ledgerEntriesAfter(block),
    );
    expect(detections.map(({ detectionId }) => detectionId)).toEqual([
      `${MIN_ADA_VIOLATION_ID}:utxo:${"55".repeat(32)}:0`,
    ]);
    const prepared = await prepareMinAdaUtxoFromCanonicalEvidence({
      evidence: block,
      predecessor: completeCanonicalReplayPredecessorLedger({
        evidence: block,
        context,
      }),
      post: completeCanonicalReplayPostLedger({ evidence: block, context }),
    });
    expect(prepared.outRef).toEqual({
      transactionId: "55".repeat(32),
      outputIndex: 0n,
    });
    expect(prepared.postUtxosRoot).toBe(block.header.utxosRoot);
    await expect(
      prepareMinAdaUtxoFromCanonicalEvidence({
        evidence: block,
        predecessor: undefined,
        post: undefined,
      }),
    ).rejects.toThrow("min-ada UTxO post ledger");
  });

  it("detects nothing when every introduced output is funded", async () => {
    const block = await blockWith([utxo(2_000_000n, 0x66)]);
    const { detections } = await replayWith(block, ledgerEntriesAfter(block));
    expect(detections).toEqual([]);
  });

  it("skips the UTxO scan, without a gap, when the post ledger does not open utxos_root", async () => {
    const block = await blockWith([utxo(1n, 0x55)]);
    const other = await blockWith([utxo(2_000_000n, 0x66)]);
    const { context, detections } = await replayWith(
      block,
      ledgerEntriesAfter(other),
    );
    expect(detections).toEqual([]);
    expect(
      completeCanonicalReplayPostLedger({ evidence: block, context }),
    ).toBeUndefined();
  });

  it("fails closed with a post-ledger gap when the post ledger is unavailable", async () => {
    const block = await blockWith([utxo(1n, 0x55)]);
    const { detections } = await replayWith(block, undefined);
    expect(violationIds(detections)).toEqual([POST_LEDGER_UNAVAILABLE]);
    const withoutContext =
      await MIN_ADA_COMPLETE_CANONICAL_REPLAY.replay(block);
    expect(violationIds(withoutContext.detections)).toEqual([
      POST_LEDGER_UNAVAILABLE,
    ]);
  });

  it("checks a supplied post ledger even when the committed root is empty", async () => {
    const block = await blockWith([]);
    const { detections } = await replayWith(block, undefined);
    expect(violationIds(detections)).toEqual([POST_LEDGER_UNAVAILABLE]);
    const checked = await replayWith(block, []);
    expect(checked.detections).toEqual([]);
  });
  it("does not read payload UTxOs for detection or proof preparation", async () => {
    const block = await blockWith([utxo(1n, 0x55)]);
    const entries = ledgerEntriesAfter(block);
    const withoutPayloadLedger = {
      ...block,
      reconstruction: { ...block.reconstruction, utxos: [] },
    };
    const requested: string[] = [];
    const context = {
      predecessor: await admitCompleteCanonicalReplayPredecessor({
        currentEvidence: withoutPayloadLedger,
        ledgerAt: async (headerHash) => {
          requested.push(headerHash);
          return headerHash === block.headerHash ? entries : [];
        },
      }),
    };
    expect(requested).toEqual([block.header.prevHeaderHash, block.headerHash]);
    const decision = await MIN_ADA_COMPLETE_CANONICAL_REPLAY.replay(
      withoutPayloadLedger,
      context,
    );
    expect(violationIds(decision.detections)).toEqual([MIN_ADA_VIOLATION_ID]);
    const prepared = await prepareMinAdaUtxoFromCanonicalEvidence({
      evidence: withoutPayloadLedger,
      predecessor: completeCanonicalReplayPredecessorLedger({
        evidence: withoutPayloadLedger,
        context,
      }),
      post: completeCanonicalReplayPostLedger({
        evidence: withoutPayloadLedger,
        context,
      }),
    });
    expect(prepared.outRef.transactionId).toBe("55".repeat(32));
  });

  it("preserves transaction detections when the post ledger root mismatches", async () => {
    const tx = buildFixtureTransaction({
      spendInputs: [],
      outputs: [utxo(1n, 0x55).value],
      fee: 0n,
    });
    const block = await blockWith([utxo(1n, 0x55)], [tx]);
    const { detections } = await replayWith(block, []);
    expect(detections.map(({ detectionId }) => detectionId)).toEqual([
      `${MIN_ADA_VIOLATION_ID}:tx:${tx.txId}:0`,
    ]);
  });

  it("fails closed when the post source throws or contains an invalid entry", async () => {
    const block = await blockWith([utxo(1n, 0x55)]);
    for (const loadPost of [
      async () => {
        throw new Error("post ledger read failed");
      },
      async () => [{ outRef: Buffer.alloc(38), output: Buffer.from([0xff]) }],
    ]) {
      const context = {
        predecessor: await admitCompleteCanonicalReplayPredecessor({
          currentEvidence: block,
          ledgerAt: async (headerHash) =>
            headerHash === block.header.prevHeaderHash ? [] : await loadPost(),
        }),
      };
      const decision = await MIN_ADA_COMPLETE_CANONICAL_REPLAY.replay(
        block,
        context,
      );
      expect(violationIds(decision.detections)).toEqual([
        POST_LEDGER_UNAVAILABLE,
      ]);
    }
  });

  it("refuses a post-ledger context admitted for another challenged header", async () => {
    const block = await blockWith([utxo(1n, 0x55)]);
    const other = await blockWith([utxo(2_000_000n, 0x66)]);
    const { context } = await replayWith(block, ledgerEntriesAfter(block));
    expect(() =>
      completeCanonicalReplayPostLedger({ evidence: other, context }),
    ).toThrow("was not admitted for this challenged header");
  });
});
