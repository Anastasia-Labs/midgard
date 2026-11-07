/**
 * Finding our own outputs by the out-refs our signed transactions give them:
 * what is claimed from a signed body, and where a step's slots stand when the
 * chain also holds other outputs with the same content.
 */
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type OutRef,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  claimOwnOutputs,
  createOwnOutputJournal,
  inlineDatumCarriageSlot,
  type OwnOutputSlotClaims,
  resolveOwnOutputs,
} from "../src/workflow/own-outputs.js";

const ADDRESS =
  "addr_test1vz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerspjrlsz";
const FIRST = Data.to(1n);
const SECOND = Data.to(2n);

const output = (
  txHashByte: string,
  outputIndex: number,
  datum: string,
  extra: Partial<UTxO> = {},
): UTxO => ({
  txHash: txHashByte.repeat(32),
  outputIndex,
  address: ADDRESS,
  assets: { lovelace: 5_000_000n },
  datum,
  datumHash: undefined,
  scriptRef: undefined,
  ...extra,
});

const label = ({ txHash, outputIndex }: OutRef) =>
  `${txHash}#${outputIndex.toString()}`;

/** A chain read that answers exact out-refs from `held`, and counts reads. */
const chain = (held: readonly UTxO[]) => {
  const reads: string[][] = [];
  return {
    reads,
    read: async (outRefs: readonly OutRef[]) => {
      const asked = outRefs.map(label);
      reads.push(asked);
      return held.filter((utxo) => asked.includes(label(utxo)));
    },
  };
};

const slot = (datum: string, ...outRefs: string[]): OwnOutputSlotClaims => ({
  slot: inlineDatumCarriageSlot(ADDRESS, datum),
  claims: outRefs.map((outRef) => ({ outRef })),
});

describe("own outputs", () => {
  it("claims one output of our signed body per slot, two for two slots with the same content", async () => {
    const account = generateEmulatorAccount({ lovelace: 100_000_000n });
    const lucid = await Lucid(new Emulator([account]), "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const address = account.address;
    const unsigned = await lucid
      .newTx()
      .pay.ToAddressWithData(address, { kind: "inline", value: SECOND }, {})
      .pay.ToAddressWithData(address, { kind: "inline", value: FIRST }, {})
      .pay.ToAddressWithData(address, { kind: "inline", value: FIRST }, {})
      .complete();
    const signed = await unsigned.sign.withWallet().complete();
    const txHash = signed.toHash();
    const first = inlineDatumCarriageSlot(address, FIRST);
    const second = inlineDatumCarriageSlot(address, SECOND);

    const claims = claimOwnOutputs({ signed, slots: [first, first, second] });
    expect(claims.map(({ outRef }) => outRef)).toEqual([
      `${txHash}#1`,
      `${txHash}#2`,
      `${txHash}#0`,
    ]);
    expect(() =>
      claimOwnOutputs({ signed, slots: [first, first, first] }),
    ).toThrow(/not in the signed transaction/u);
    expect(() =>
      claimOwnOutputs({
        signed,
        slots: [inlineDatumCarriageSlot(ADDRESS, FIRST)],
      }),
    ).toThrow(/not in the signed transaction/u);
  });

  it("takes only our claimed outputs, whatever else at the address holds the same content", async () => {
    const ours = [output("ff", 0, FIRST), output("ff", 1, SECOND)];
    const { read, reads } = chain([
      // Outputs of other parties, sorting before ours: exact copies, and
      // copies with a token, a reference script or a datum hash besides.
      output("00", 0, FIRST),
      output("00", 1, SECOND),
      output("01", 0, FIRST, {
        assets: { lovelace: 5_000_000n, [`${"ab".repeat(28)}cd`]: 1n },
      }),
      output("01", 1, FIRST, {
        scriptRef: {
          type: "PlutusV3",
          script: "4e4d01000033222220051200120011",
        },
      }),
      output("01", 2, FIRST, { datumHash: "11".repeat(32) }),
      ...ours,
    ]);

    await expect(
      resolveOwnOutputs({
        slots: [
          slot(FIRST, `${"ff".repeat(32)}#0`),
          slot(SECOND, `${"ff".repeat(32)}#1`),
        ],
        read,
      }),
    ).resolves.toEqual({ kind: "ready", utxos: ours });
    // One read, of our out-refs alone.
    expect(reads).toEqual([[`${"ff".repeat(32)}#0`, `${"ff".repeat(32)}#1`]]);

    // With no claim on a slot, a copy at the address is never taken for it.
    await expect(
      resolveOwnOutputs({
        slots: [slot(FIRST, `${"ff".repeat(32)}#0`), slot(SECOND)],
        read,
      }),
    ).resolves.toEqual({ kind: "publish", ordinal: 1, replaces: null });
  });

  it("keeps an out-ref through a read that lags, and publishes again, replacing it, once it is gone", async () => {
    const gone = `${"ff".repeat(32)}#0`;
    const { read } = chain([output("00", 0, FIRST)]);
    const claimed = [slot(FIRST, gone)];

    await expect(
      resolveOwnOutputs({ slots: claimed, read, recent: () => true }),
    ).resolves.toEqual({ kind: "pending", ordinal: 0, outRef: gone });
    await expect(
      resolveOwnOutputs({ slots: claimed, read, recent: () => false }),
    ).resolves.toEqual({ kind: "publish", ordinal: 0, replaces: gone });
  });

  it("takes an earlier claim that is live again over a newer one that is gone", async () => {
    const earlier = output("ee", 0, FIRST);
    const { read } = chain([earlier]);
    await expect(
      resolveOwnOutputs({
        slots: [slot(FIRST, `${"ee".repeat(32)}#0`, `${"ff".repeat(32)}#0`)],
        read,
        recent: () => true,
      }),
    ).resolves.toEqual({ kind: "ready", utxos: [earlier] });
  });

  it("refuses a claimed out-ref whose output no longer holds the slot's content", async () => {
    const { read } = chain([output("ff", 0, SECOND)]);
    await expect(
      resolveOwnOutputs({
        slots: [slot(FIRST, `${"ff".repeat(32)}#0`)],
        read,
      }),
    ).resolves.toEqual({
      kind: "publish",
      ordinal: 0,
      replaces: `${"ff".repeat(32)}#0`,
    });
  });

  it("takes a carriage output listing another asset at quantity zero, never one that also names a datum hash", async () => {
    const zero = output("ff", 0, FIRST, {
      assets: { lovelace: 5_000_000n, [`${"ab".repeat(28)}cd`]: 0n },
    });
    const hashed = output("ee", 0, FIRST, { datumHash: "11".repeat(32) });
    const { read } = chain([zero, hashed]);

    await expect(
      resolveOwnOutputs({
        slots: [slot(FIRST, `${"ff".repeat(32)}#0`)],
        read,
      }),
    ).resolves.toEqual({ kind: "ready", utxos: [zero] });
    await expect(
      resolveOwnOutputs({
        slots: [slot(FIRST, `${"ee".repeat(32)}#0`)],
        read,
      }),
    ).resolves.toEqual({
      kind: "publish",
      ordinal: 0,
      replaces: `${"ee".repeat(32)}#0`,
    });
  });

  it("keeps claims in memory per key, oldest first", () => {
    const journal = createOwnOutputJournal();
    journal.claim("chunk:0", `${"ff".repeat(32)}#0`);
    journal.claim("chunk:0", `${"ee".repeat(32)}#2`);
    expect(journal.claims("chunk:0")).toEqual([
      { outRef: `${"ff".repeat(32)}#0` },
      { outRef: `${"ee".repeat(32)}#2` },
    ]);
    expect(journal.claims("chunk:1")).toEqual([]);
    expect(() => journal.claim("chunk:1", "not-an-out-ref")).toThrow(
      /not an exact out-ref/u,
    );
  });
});
