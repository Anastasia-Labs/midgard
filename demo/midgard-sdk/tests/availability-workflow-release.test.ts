import "@lucid-evolution/lucid";
import "vitest";
import "../src/availability-challenge.js";
import "../src/availability-challenge-operation.js";
import "../src/linked-list.js";
import "./availability-workflow-release.transaction.js";

import { describe, expect, it } from "vitest";

import {
  DA_AVAILABILITY_WORKFLOW_RELEASE_MAX_HOPS,
  DaAvailabilityWorkflowReleaseHopCapError,
} from "../src/availability-challenge-operation.js";
import {
  APPENDED,
  AVAILABILITY,
  BOUNDARY,
  chain,
  CHALLENGE_ASSET,
  closeOf,
  DEEP,
  DESCENDANT,
  HEADER,
  MIN_DEPTH,
  nodeUnit,
  OPEN,
  openRecord,
  openTx,
  Q0,
  R0,
  RECORD_DATUM,
  ref,
  release,
  SECOND_DESCENDANT,
  type Spend,
  timeoutOf,
  timeoutWithDescendantOf,
  transaction,
} from "./availability-workflow-release.transaction.js";

describe("availability workflow release (P20)", () => {
  it("releases on a verified Close by anyone at the finality depth", async () => {
    const close = closeOf(Q0);
    await expect(
      release(chain({ [Q0]: { tx: close, blockNo: DEEP } })),
    ).resolves.toStrictEqual({
      reason: "challenge-closed",
      txHash: close.id,
      spendPoint: `${(DEEP * 20).toString()}:${close.id}`,
      confirmationDepth: MIN_DEPTH,
    });
  });

  it("releases on a rival Timeout that burns the header's node", async () => {
    const timeout = timeoutOf(Q0);
    await expect(
      release(chain({ [Q0]: { tx: timeout, blockNo: DEEP } })),
    ).resolves.toMatchObject({
      reason: "header-node-burned",
      txHash: timeout.id,
    });
  });

  it("releases when the header is pruned as another challenge's descendant", async () => {
    // The ancestor's Timeout removes this header's node; our record is never
    // consumed.
    const prune = transaction({
      inputs: [`${"c0".repeat(32)}#0`, Q0, `${"c1".repeat(32)}#0`],
      outputs: [{}, {}],
      mint: {
        [AVAILABILITY + "c2".repeat(32)]: -1n,
        [nodeUnit(HEADER)]: -1n,
      },
    });
    await expect(
      release(chain({ [Q0]: { tx: prune, blockNo: DEEP } })),
    ).resolves.toMatchObject({
      reason: "header-node-burned",
      txHash: prune.id,
    });
  });

  it("keeps the row through a rival Timeout with a descendant and its prunes, and releases on the remove", async () => {
    const timeout = timeoutWithDescendantOf(Q0);
    const prune = transaction({
      inputs: [ref(timeout, 0), `${"d1".repeat(32)}#0`],
      outputs: [{ assets: { [nodeUnit(HEADER)]: 1n } }, {}],
      mint: { [nodeUnit(SECOND_DESCENDANT)]: -1n },
    });
    const remove = transaction({
      inputs: [ref(prune, 0), `${"d2".repeat(32)}#0`],
      outputs: [{}],
      mint: { [nodeUnit(HEADER)]: -1n },
    });
    const walked = {
      [Q0]: { tx: timeout, blockNo: DEEP - 20 },
      [ref(timeout, 0)]: { tx: prune, blockNo: DEEP - 10 },
    };
    // The record is consumed, yet the header's removal chain still holds it.
    await expect(release(chain({ [Q0]: walked[Q0] }))).resolves.toBeUndefined();
    await expect(release(chain(walked))).resolves.toBeUndefined();
    await expect(
      release(
        chain({ ...walked, [ref(prune, 0)]: { tx: remove, blockNo: DEEP } }),
      ),
    ).resolves.toMatchObject({
      reason: "header-node-burned",
      txHash: remove.id,
    });
  });

  it("walks through a commit that appends after the header", async () => {
    const append = transaction({
      inputs: [Q0, `${"e0".repeat(32)}#0`],
      outputs: [
        { assets: { [nodeUnit(APPENDED)]: 1n } },
        { assets: { [nodeUnit(HEADER)]: 1n } },
      ],
      mint: { [nodeUnit(APPENDED)]: 1n },
    });
    const close = closeOf(ref(append, 1));
    await expect(
      release(
        chain({
          [Q0]: { tx: append, blockNo: DEEP - 5 },
          [ref(append, 1)]: { tx: close, blockNo: DEEP },
        }),
      ),
    ).resolves.toMatchObject({ reason: "challenge-closed", txHash: close.id });
  });

  it("finds the node and record by asset, not by output index", async () => {
    const open = openTx([
      { assets: { [nodeUnit(HEADER)]: 1n } },
      { assets: { ["c3".repeat(28) + "00"]: 1n } },
      { assets: { [AVAILABILITY + CHALLENGE_ASSET]: 1n }, datum: RECORD_DATUM },
    ]);
    const close = transaction({
      inputs: [ref(open, 2), ref(open, 0)],
      outputs: [{ assets: { [nodeUnit(HEADER)]: 1n } }],
      mint: { [AVAILABILITY + CHALLENGE_ASSET]: -1n },
    });
    await expect(
      release(
        chain({ [ref(open, 0)]: { tx: close, blockNo: DEEP } }),
        openRecord(open),
      ),
    ).resolves.toMatchObject({ reason: "challenge-closed" });
  });

  describe("keeps the row", () => {
    it("on a terminal spend one block short of the finality depth", async () => {
      await expect(
        release(chain({ [Q0]: { tx: closeOf(Q0), blockNo: DEEP + 1 } })),
      ).resolves.toBeUndefined();
    });

    it("on an intermediate hop short of the finality depth", async () => {
      const append = transaction({
        inputs: [Q0],
        outputs: [{ assets: { [nodeUnit(HEADER)]: 1n } }],
      });
      await expect(
        release(
          chain({
            [Q0]: { tx: append, blockNo: DEEP + 1 },
            [ref(append, 0)]: { tx: closeOf(ref(append, 0)), blockNo: DEEP },
          }),
        ),
      ).resolves.toBeUndefined();
    });

    it("and refuses a spend above the canonical boundary", async () => {
      await expect(
        release(
          chain({ [Q0]: { tx: closeOf(Q0), blockNo: BOUNDARY + 1 } }),
          openRecord(),
        ),
      ).rejects.toThrow(
        "Availability input spend lies above the canonical boundary",
      );
    });

    it("when the named spender does not list the node", async () => {
      const unrelated = transaction({
        inputs: [R0],
        outputs: [{}],
        mint: { [nodeUnit(HEADER)]: -1n },
      });
      await expect(
        release(chain({ [Q0]: { tx: unrelated, blockNo: DEEP } })),
      ).resolves.toBeUndefined();
    });

    it("when the spender failed phase 2", async () => {
      await expect(
        release(chain({ [Q0]: { tx: closeOf(Q0, false), blockNo: DEEP } })),
      ).resolves.toBeUndefined();
    });

    it("when no spend is reported", async () => {
      await expect(release(chain({}))).resolves.toBeUndefined();
    });

    it("unless the Open is confirmed", async () => {
      const readers = chain({ [Q0]: { tx: closeOf(Q0), blockNo: DEEP } });
      for (const state of [
        "pending",
        "included",
        "conflict",
        "expired",
      ] as const)
        await expect(
          release(readers, openRecord(OPEN, state)),
        ).resolves.toBeUndefined();
      await expect(
        release(readers, openRecord(OPEN, "confirmed", { action: "settle" })),
      ).resolves.toBeUndefined();
      await expect(
        release(
          readers,
          openRecord(OPEN, "confirmed", { headerHash: DESCENDANT }),
        ),
      ).resolves.toBeUndefined();
      await expect(
        release(
          readers,
          openRecord(OPEN, "confirmed", { txHash: "ee".repeat(32) }),
        ),
      ).resolves.toBeUndefined();
    });

    it("when a transaction spends the record but burns a descendant's node", async () => {
      await expect(
        release(
          chain({ [Q0]: { tx: timeoutWithDescendantOf(Q0), blockNo: DEEP } }),
        ),
      ).resolves.toBeUndefined();
    });

    it("when the Open's node or record is missing or ambiguous", async () => {
      const record = {
        assets: { [AVAILABILITY + CHALLENGE_ASSET]: 1n },
        datum: RECORD_DATUM,
      };
      const node = { assets: { [nodeUnit(HEADER)]: 1n } };
      const minted = { [AVAILABILITY + CHALLENGE_ASSET]: 1n };
      for (const open of [
        openTx([record], minted),
        openTx([record, node, node], minted),
        openTx(
          [
            record,
            { assets: { ["c4".repeat(28) + nodeUnit(HEADER).slice(56)]: 1n } },
            node,
          ],
          minted,
        ),
        openTx([{ assets: record.assets }, node], minted),
        openTx([record, record, node], minted),
        // A record whose challenge asset the Open did not mint.
        openTx([record, node], {}),
      ]) {
        const close = transaction({
          inputs: [ref(open, 0), ref(open, 1)],
          outputs: [{}],
          mint: { [nodeUnit(HEADER)]: -1n },
        });
        await expect(
          release(
            chain({
              [ref(open, 0)]: { tx: close, blockNo: DEEP },
              [ref(open, 1)]: { tx: close, blockNo: DEEP },
            }),
            openRecord(open),
          ),
        ).resolves.toBeUndefined();
      }
    });

    it("when a hop leaves the header's node in several outputs", async () => {
      const split = transaction({
        inputs: [Q0],
        outputs: [
          { assets: { [nodeUnit(HEADER)]: 1n } },
          { assets: { [nodeUnit(HEADER)]: 1n } },
        ],
      });
      await expect(
        release(
          chain({
            [Q0]: { tx: split, blockNo: DEEP - 1 },
            [ref(split, 0)]: { tx: closeOf(ref(split, 0)), blockNo: DEEP },
          }),
        ),
      ).resolves.toBeUndefined();
    });

    it("and reports a node chain longer than the hop cap", async () => {
      const hops: Record<string, Spend> = {};
      let anchor = Q0;
      for (
        let hop = 0;
        hop < DA_AVAILABILITY_WORKFLOW_RELEASE_MAX_HOPS;
        hop++
      ) {
        const next = transaction({
          inputs: [anchor],
          outputs: [{ assets: { [nodeUnit(HEADER)]: 1n } }],
        });
        hops[anchor] = { tx: next, blockNo: DEEP - 1 };
        anchor = ref(next, 0);
      }
      await expect(release(chain(hops))).rejects.toBeInstanceOf(
        DaAvailabilityWorkflowReleaseHopCapError,
      );
      // The terminal step one hop inside the cap still releases.
      const last = Object.values(hops).at(-1)!.tx;
      const trimmed = Object.fromEntries(
        Object.entries(hops).filter(([, spend]) => spend.tx.id !== last.id),
      );
      const lastAnchor = Object.keys(hops).at(-1)!;
      await expect(
        release(
          chain({
            ...trimmed,
            [lastAnchor]: { tx: closeOf(lastAnchor), blockNo: DEEP },
          }),
        ),
      ).resolves.toMatchObject({ reason: "challenge-closed" });
    });
  });
});
