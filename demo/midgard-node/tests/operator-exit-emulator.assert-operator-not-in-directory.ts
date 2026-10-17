import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  OPERATOR_A,
  OPERATOR_B,
} from "./operator-exit-emulator.duplicate-operator-slashing.js";

const syntheticUtxo = (txHash: string, lovelace: bigint): UTxO =>
  ({
    txHash,
    outputIndex: 0,
    address: "addr_test1vqfakeaddressfakeaddressfakeaddressfakeaddress",
    assets: { lovelace },
    datumHash: null,
    datum: null,
    scriptRef: null,
  }) as unknown as UTxO;

const syntheticNode = ({
  txHash,
  key,
  next,
  data,
  lovelace = 900_000_000n,
  assetName = "node",
}: {
  readonly txHash: string;
  readonly key: string | null;
  readonly next: string | null;
  readonly data: unknown;
  readonly lovelace?: bigint;
  readonly assetName?: string;
}): SDK.NodeWithDatum => ({
  utxo: syntheticUtxo(txHash, lovelace),
  datum: {
    key: key === null ? "Empty" : { Key: { key } },
    next: next === null ? "Empty" : { Key: { key: next } },
    data: data as SDK.LinkedListNodeView["data"],
  },
  assetName,
});

const emptyData = SDK.castRegisteredOperatorDatumToData({
  operator: "00".repeat(28),
});

export const registeredNodeFor = (
  operator: string,
  activationTime: bigint,
): SDK.NodeWithDatum =>
  syntheticNode({
    txHash: "11".repeat(32),
    key: SDK.posixTimeToRegisteredNodeKey(activationTime),
    next: null,
    data: SDK.castRegisteredOperatorDatumToData({ operator }),
  });

export const activeNodeFor = (
  operator: string,
  inactivityStrikes: bigint,
): SDK.NodeWithDatum =>
  syntheticNode({
    txHash: "22".repeat(32),
    key: operator,
    next: null,
    data: SDK.castActiveOperatorDatumToData({
      bond_unlock_time: null,
      inactivity_strikes: inactivityStrikes,
    }),
  });

export const retiredNodeFor = (
  operator: string,
  bondUnlockTime: bigint | null,
): SDK.NodeWithDatum =>
  syntheticNode({
    txHash: "33".repeat(32),
    key: operator,
    next: null,
    data: SDK.castRetiredOperatorDatumToData({
      bond_unlock_time: bondUnlockTime,
    }),
  });

export const rootNode = (
  txHash: string,
  next: string | null,
): SDK.NodeWithDatum =>
  syntheticNode({
    txHash,
    key: null,
    next,
    data: emptyData,
    lovelace: 2_000_000n,
    assetName: "root",
  });

export const syntheticScheduler = (
  datum: SDK.SchedulerDatum,
): SDK.SchedulerUTxO => ({
  utxo: syntheticUtxo("44".repeat(32), 2_000_000n),
  datum,
  assetName: "scheduler",
});

export const emptyDirectory = {
  registered: [rootNode("aa".repeat(32), null)],
  active: [rootNode("ab".repeat(32), null)],
  retired: [rootNode("ac".repeat(32), null)],
};

describe("assertOperatorNotInDirectory", () => {
  it("accepts a key that occupies none of the three lists", () => {
    expect(() =>
      SDK.assertOperatorNotInDirectory(emptyDirectory, OPERATOR_A),
    ).not.toThrow();
  });

  it("refuses an active key and names the membership", () => {
    try {
      SDK.assertOperatorNotInDirectory(
        {
          ...emptyDirectory,
          active: [
            rootNode("ab".repeat(32), OPERATOR_A),
            activeNodeFor(OPERATOR_A, 0n),
          ],
        },
        OPERATOR_A,
      );
      throw new Error("Expected the active membership to be refused");
    } catch (cause) {
      expect(cause).toBeInstanceOf(SDK.OperatorAlreadyInDirectoryError);
      const error = cause as SDK.OperatorAlreadyInDirectoryError;
      expect(error.operatorKeyHash).toEqual(OPERATOR_A);
      expect(error.occupancies.map(({ kind }) => kind)).toEqual(["active"]);
      expect(error.message).toContain("active");
      expect(error.message).toContain(OPERATOR_A);
    }
  });

  it("refuses a retired key, which the registration skip checks never look at", () => {
    expect(() =>
      SDK.assertOperatorNotInDirectory(
        {
          ...emptyDirectory,
          retired: [
            rootNode("ac".repeat(32), OPERATOR_A),
            retiredNodeFor(OPERATOR_A, null),
          ],
        },
        OPERATOR_A,
      ),
    ).toThrow(/retired/);
  });

  it("refuses a key that already holds a registration", () => {
    expect(() =>
      SDK.assertOperatorNotInDirectory(
        {
          ...emptyDirectory,
          registered: [
            rootNode("aa".repeat(32), null),
            registeredNodeFor(OPERATOR_A, 1_700_000_000_000n),
          ],
        },
        OPERATOR_A,
      ),
    ).toThrow(/registered/);
  });

  it("ignores memberships of other operators", () => {
    expect(() =>
      SDK.assertOperatorNotInDirectory(
        {
          registered: [
            rootNode("aa".repeat(32), null),
            registeredNodeFor(OPERATOR_B, 1_700_000_000_000n),
          ],
          active: [
            rootNode("ab".repeat(32), OPERATOR_B),
            activeNodeFor(OPERATOR_B, 0n),
          ],
          retired: [
            rootNode("ac".repeat(32), OPERATOR_B),
            retiredNodeFor(OPERATOR_B, null),
          ],
        },
        OPERATOR_A,
      ),
    ).not.toThrow();
  });
});
