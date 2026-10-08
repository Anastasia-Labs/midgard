import { openSqliteFactStore } from "@al-ft/midgard-l1-follower";
import { simStoreOptions } from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import { createWatcherFundingInputFacts } from "../../src/l1-follower/funding-input-facts.js";
import { watcherProjection } from "../../src/l1-follower/projection.js";
import {
  D,
  harness,
  K,
  SEED_LABEL,
  T,
  X,
} from "../support/l1-follower-raw-reads-fixture.js";
import { RECOVERY_DEPTH } from "../support/l1-follower-raw-source-fixture.js";

/**
 * Where a held reservation's funding inputs stand, read from the follower:
 * spent only when the spend is at or below the release-final point (the
 * highest block at least `automaticRecoveryMaxDepth + 2` deep), unspent only
 * when unspent there and still at the tip, and undetermined otherwise. The
 * reads never throw, so a held reservation is only ever re-read.
 */
describe("watcher funding input facts", () => {
  it("reports inputs unspent, spent or undetermined only from final follower facts", async () => {
    const h = await harness();
    // As in the watcher, the release-final depth is the store's k.
    const facts = createWatcherFundingInputFacts({
      store: h.store,
      recoveryDepth: K,
    });
    const deep = async (): Promise<void> => {
      for (let i = 0; i <= K; i += 1) await h.forward([]);
    };
    // A chain deeper than the recovery depth, so a release-final point exists.
    await deep();
    const A = {
      inputs: [h.chain.outsideInput()],
      outputs: [
        { address: T, lovelace: 2_000_000n },
        { address: T, lovelace: 3_000_000n },
        { address: X, lovelace: 4_000_000n },
      ],
      nonce: h.chain.nonce(),
    };
    const [a] = (await h.forward([A])).hashes as [string];
    const [a0, a1, untracked] = [0, 1, 2].map((index) => `${a}#${index}`) as [
      string,
      string,
      string,
    ];

    // Created above the release-final point: not final yet.
    expect(await facts.standing([a0])).toEqual({
      spent: [],
      unspent: [],
      undetermined: `${a0} was created or seeded above the release-final point`,
    });

    await deep();
    expect(await facts.standing([a0, a1, SEED_LABEL])).toEqual({
      spent: [],
      unspent: [a0, a1, SEED_LABEL],
      undetermined: null,
    });
    // An output the follower never tracks is never final.
    expect((await facts.standing([a0, untracked])).undetermined).toMatch(
      new RegExp(`^${untracked} has no tracked row`, "u"),
    );

    // Spent at the tip, above the release-final point: not final yet.
    await h.forward([
      {
        inputs: [{ txHash: Buffer.from(a, "hex"), index: 1 }],
        outputs: [{ address: T, lovelace: 2_500_000n }],
        nonce: h.chain.nonce(),
      },
    ]);
    expect(await facts.standing([a0, a1])).toEqual({
      spent: [],
      unspent: [a0],
      undetermined: `${a1} is spent above the release-final point`,
    });
    // Within k of the tip a rollback undoes the spend: still undetermined.
    for (let i = 0; i < K - 2; i += 1) await h.forward([]);
    expect((await facts.standing([a1])).undetermined).toBe(
      `${a1} is spent above the release-final point`,
    );

    // The spend is final.
    await deep();
    expect(await facts.standing([a0, a1])).toEqual({
      spent: [a1],
      unspent: [a0],
      undetermined: null,
    });

    // The pruning removes the spent row: a pruned input never reads unspent.
    await h.pruneAll();
    expect(
      await h.store.output({ txHash: Buffer.from(a, "hex"), index: 1 }),
    ).toBeNull();
    expect(await facts.standing([a0, a1])).toEqual({
      spent: [a1],
      unspent: [a0],
      undetermined: null,
    });
    // Pruned through a slot above a deeper release-final point: not final.
    const deeper = await createWatcherFundingInputFacts({
      store: h.store,
      recoveryDepth: RECOVERY_DEPTH,
    }).standing([a1]);
    expect(deeper.undetermined).toMatch(
      new RegExp(`^${a1} has no tracked row`, "u"),
    );
  });

  it("answers undetermined, never throws, when the follower cannot read", async () => {
    const store = openSqliteFactStore({
      ...simStoreOptions([watcherProjection(D)], K, "sqlite"),
      path: ":memory:",
    });
    expect((await store.start()).kind).toBe("ready");
    const facts = createWatcherFundingInputFacts({
      store,
      recoveryDepth: RECOVERY_DEPTH,
    });
    const standing = await facts.standing([SEED_LABEL]);
    expect(standing).toMatchObject({ spent: [], unspent: [] });
    expect(standing.undetermined).toEqual(expect.any(String));
  });
});
