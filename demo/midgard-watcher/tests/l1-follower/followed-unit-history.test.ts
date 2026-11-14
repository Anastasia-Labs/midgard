import type { OutRef } from "@al-ft/midgard-l1-follower";
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { WatcherProjectionDeployment } from "../../src/l1-follower/projection.js";
import {
  bech32,
  D,
  harness,
  okValue,
  reasonOf,
  X,
} from "../support/l1-follower-raw-reads-fixture.js";
import { initTx } from "../support/l1-follower-state-queue-traffic.js";

// The fault-proof families' snapshots read the history of deployment units
// (hub oracle, computation thread, proof token, user events) and the UTxOs
// at deployment script addresses (computation-thread steps, operator
// directories, scheduler). The projection follows every manifest script,
// so the raw reads serve both; an unfollowed script stays refused.

const FOLLOWED = "70".repeat(28);
const UNFOLLOWED = "71".repeat(28);
const FOLLOWING: WatcherProjectionDeployment = {
  ...D,
  followedScripts: [FOLLOWED],
};

const scriptAddress = (hash: string): Buffer =>
  Buffer.concat([Buffer.of(0x70), Buffer.from(hash, "hex")]);

const one = (policy: string, name: string, quantity = 1n) =>
  new Map([[policy, new Map([[name, quantity]])]]);

const outside = (n: number): OutRef => ({
  txHash: Buffer.alloc(32, 0xd0 + n),
  index: 0,
});

/** Mints a unit to its script's address, moves it there, burns it. */
const unitLifecycle = (
  policy: string,
  input: number,
  nonce: () => number,
): Readonly<{
  mint: SimTx;
  move: (minted: string) => SimTx;
  burn: (moved: string) => SimTx;
}> => ({
  mint: {
    inputs: [outside(input)],
    outputs: [
      {
        address: scriptAddress(policy),
        lovelace: 2_000_000n,
        assets: one(policy, "aa"),
      },
      { address: scriptAddress(policy), lovelace: 3_000_000n },
    ],
    mint: one(policy, "aa"),
    nonce: nonce(),
  },
  move: (minted) => ({
    inputs: [{ txHash: Buffer.from(minted, "hex"), index: 0 }],
    outputs: [
      {
        address: scriptAddress(policy),
        lovelace: 2_000_000n,
        assets: one(policy, "aa"),
      },
    ],
    nonce: nonce(),
  }),
  burn: (moved) => ({
    inputs: [{ txHash: Buffer.from(moved, "hex"), index: 0 }],
    outputs: [{ address: X, lovelace: 2_000_000n }],
    mint: one(policy, "aa", -1n),
    nonce: nonce(),
  }),
});

describe("watcher projection: followed deployment units", () => {
  it("serves the hub-oracle unit's history from the protocol-init tx", async () => {
    const h = await harness();
    try {
      const [init] = (await h.forward([initTx(D)])).hashes as [string];
      const history = okValue(
        await h
          .reads()
          .unitHistoryAtPoint(
            D.hubOracleMint + SDK.HUB_ORACLE_ASSET_NAME,
            h.tipPoint(),
          ),
      );
      expect(history.transactions.map(({ txHash }) => txHash)).toEqual([init]);
    } finally {
      await h.store.close();
    }
  });

  it("serves a followed script's unit history and address, and refuses an unfollowed one", async () => {
    const h = await harness(FOLLOWING);
    try {
      const nonce = () => h.chain.nonce();
      const followed = unitLifecycle(FOLLOWED, 1, nonce);
      const unfollowed = unitLifecycle(UNFOLLOWED, 2, nonce);
      const [minted] = (await h.forward([followed.mint])).hashes as [string];
      await h.forward([unfollowed.mint]);
      const [moved] = (await h.forward([followed.move(minted)])).hashes as [
        string,
      ];
      const [burned] = (await h.forward([followed.burn(moved)])).hashes as [
        string,
      ];
      const reads = h.reads();

      const history = okValue(
        await reads.unitHistoryAtPoint(`${FOLLOWED}aa`, h.tipPoint()),
      );
      // The move neither mints nor burns: it is in the history because the
      // output carrying the unit sits at a followed script's address.
      expect(history.transactions.map(({ txHash }) => txHash)).toEqual([
        minted,
        moved,
        burned,
      ]);
      for (const { txHash, inclusionPoint } of history.transactions)
        expect(
          okValue(await reads.rawTransaction(txHash, inclusionPoint))
            .transaction.txHash,
        ).toBe(txHash);
      const atScript = okValue(
        await reads.addressUtxosAtPoint(
          bech32(scriptAddress(FOLLOWED)),
          h.tipPoint(),
        ),
      );
      expect(atScript.map(({ outRef }) => outRef)).toEqual([`${minted}#1`]);

      expect(
        reasonOf(
          await reads.unitHistoryAtPoint(`${UNFOLLOWED}aa`, h.tipPoint()),
        ),
      ).toBe("unit_not_projected");
      expect(
        reasonOf(
          await reads.addressUtxosAtPoint(
            bech32(scriptAddress(UNFOLLOWED)),
            h.tipPoint(),
          ),
        ),
      ).toBe("untracked_address");
    } finally {
      await h.store.close();
    }
  });
});
