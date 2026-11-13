import { afterEach, describe, expect, it } from "vitest";

import {
  advance,
  closeJournals,
  fund,
  MANIFEST,
  openJournal,
  pass,
  recorded,
  RECOVERY,
  row,
  spendElsewhere,
  spendOutput,
  stored,
  unpinned,
} from "../support/availability-pruned-journal.js";
import { harness, K } from "../support/l1-follower-raw-reads-fixture.js";

/**
 * A watcher down past the retained depth: the follower has pruned past a
 * signed intent's validity start, so its inclusion read can no longer say
 * the transaction is absent, and each case below threw on every pass. The
 * journal, the follower reads and the reconciliation pass are the real ones.
 */

afterEach(closeJournals);

describe("a signed intent the store may have pruned", () => {
  it("expires one that never landed, past its validity, from its unspent inputs", async () => {
    const fx = await harness();
    const journal = openJournal();
    const [input, collateral] = await fund(fx, 2);
    const ttl = Number(fx.tipPoint().slot) + 4;
    const { intent } = recorded(fx, journal, "prepare", {
      inputs: [input!],
      collaterals: [collateral!],
      invalidAfter: ttl,
    });
    await advance(fx, K + 4);
    expect(await stored(fx, intent.txHash)).toBe("beyond_retention");
    expect(Number(fx.tipPoint().slot)).toBeGreaterThanOrEqual(ttl);
    expect(await pass(fx, journal)).toMatchObject([
      { status: "expired", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)).toMatchObject({
      state: "expired",
      detail: "Expired with every normal input canonically unspent",
    });
  });

  it("expires one whose normal input someone else spent, and the store pruned, while another is unspent", async () => {
    const fx = await harness();
    const journal = openJournal();
    const [taken, kept, collateral] = await fund(fx, 3);
    await spendElsewhere(fx, taken!);
    const ttl = Number(fx.tipPoint().slot) + 4;
    const { intent } = recorded(fx, journal, "prepare", {
      inputs: [taken!, kept!],
      collaterals: [collateral!],
      invalidAfter: ttl,
    });
    await advance(fx, K + 4);
    expect(await row(fx, taken!)).toBe("beyond_retention");
    expect(await pass(fx, journal)).toMatchObject([
      { status: "expired", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)).toMatchObject({
      state: "expired",
      detail:
        "Expired with a normal input spent elsewhere and another still unspent",
    });
  });

  it("expires one whose every normal input is unspent but whose collateral someone else spent", async () => {
    const fx = await harness();
    const journal = openJournal();
    const [input, collateral] = await fund(fx, 2);
    await spendElsewhere(fx, collateral!);
    const ttl = Number(fx.tipPoint().slot) + 4;
    const { intent } = recorded(fx, journal, "prepare", {
      inputs: [input!],
      collaterals: [collateral!],
      invalidAfter: ttl,
    });
    await advance(fx, K + 4);
    expect(await row(fx, collateral!)).toBe("beyond_retention");
    expect(await pass(fx, journal)).toMatchObject([
      { status: "expired", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)).toMatchObject({
      state: "expired",
      detail: "Expired with every normal input canonically unspent",
    });
  });

  it("expires one whose every normal input another transaction spent, while the store still holds the spend", async () => {
    const fx = await harness();
    const journal = openJournal();
    const [input, collateral] = await fund(fx, 2);
    const ttl = Number(fx.tipPoint().slot) + 4;
    const { intent } = recorded(fx, journal, "prepare", {
      inputs: [input!],
      collaterals: [collateral!],
      invalidAfter: ttl,
    });
    await advance(fx, K + 4);
    await spendElsewhere(fx, input!);
    await advance(fx, 1);
    expect(await stored(fx, intent.txHash)).toBe("beyond_retention");
    expect(await row(fx, input!)).toBe("spent");
    expect(await pass(fx, journal)).toMatchObject([
      { status: "expired", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)).toMatchObject({
      state: "expired",
      detail:
        "Expired with a normal input finally spent by another transaction",
    });
  });

  // No input is left to prove it absent: it waits only until its
  // validity ends, then expires under its own name.
  it("waits on one whose every normal input left the store until its validity ends, then expires it as beyond retention", async () => {
    const fx = await harness();
    const journal = openJournal();
    const [input, collateral] = await fund(fx, 2);
    await spendElsewhere(fx, input!);
    await advance(fx, K + 2);
    expect(await row(fx, input!)).toBe("beyond_retention");
    const ttl = Number(fx.tipPoint().slot) + 6;
    const { intent } = recorded(fx, journal, "prepare", {
      inputs: [input!],
      collaterals: [collateral!],
      invalidAfter: ttl,
    });
    expect(await pass(fx, journal)).toMatchObject([
      { status: "waiting", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)?.state).toBe("pending");
    await advance(fx, 6);
    expect(Number(fx.tipPoint().slot)).toBeGreaterThanOrEqual(ttl);
    expect(await pass(fx, journal)).toMatchObject([
      { status: "expired", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)).toMatchObject({
      state: "expired",
      detail: expect.stringMatching(/^Expired beyond the source's retention/u),
    });
  });

  // A confirmed transaction an earlier watcher never pinned: it landed,
  // its inputs and outputs are spent, and the store pruned all of it.
  it("keeps a confirmed one the store pruned before it was pinned, and retires it past the recovery depth", async () => {
    const fx = await harness();
    const journal = openJournal();
    const [input, collateral] = await fund(fx, 2);
    const { tx, intent } = recorded(fx, journal, "prepare", {
      inputs: [input!],
      collaterals: [collateral!],
    });
    await fx.forward([tx]);
    await fx.forward([]);
    expect(await pass(fx, journal, MANIFEST, unpinned)).toMatchObject([
      { status: "confirmed", txHash: intent.txHash },
    ]);
    await spendOutput(fx, intent.txHash);
    await advance(fx, K + 2);
    expect(await stored(fx, intent.txHash)).toBe("beyond_retention");
    expect(await pass(fx, journal)).toMatchObject([
      { status: "confirmed", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)?.state).toBe("confirmed");
    await advance(fx, RECOVERY + 1);
    await pass(fx, journal);
    expect(journal.get(intent.id)).toBeNull();
  });

  it("retires an earlier deployment's confirmed anchor the store pruned before it was pinned", async () => {
    const fx = await harness();
    const journal = openJournal();
    const earlier = "b2".repeat(32);
    const [input, collateral] = await fund(fx, 2);
    const { tx, intent } = recorded(fx, journal, "prepare", {
      deploymentIdentity: earlier,
      inputs: [input!],
      collaterals: [collateral!],
    });
    await fx.forward([tx]);
    await fx.forward([]);
    await pass(fx, journal, earlier, unpinned);
    expect(journal.get(intent.id)?.state).toBe("confirmed");
    await spendOutput(fx, intent.txHash);
    await advance(fx, K + 2);
    expect(await stored(fx, intent.txHash)).toBe("beyond_retention");
    expect(await pass(fx, journal)).toEqual([]);
    expect(journal.get(intent.id)?.state).toBe("confirmed");
    await advance(fx, RECOVERY + 1);
    await pass(fx, journal);
    expect(journal.get(intent.id)).toBeNull();
  });
});
