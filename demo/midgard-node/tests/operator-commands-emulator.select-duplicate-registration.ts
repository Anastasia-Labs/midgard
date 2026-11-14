/**
 * Duplicate selection (`selectDuplicateRegistration`) over directory
 * snapshots, for the operator programs `operator-commands-emulator.test.ts`
 * runs: which registration a duplicate slash removes, and which membership
 * proves it.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { selectDuplicateRegistration } from "../src/transactions/operators/exit.js";

describe("selectDuplicateRegistration", () => {
  const node = (
    key: string,
    data: unknown,
    lovelace = 900_000_000n,
  ): SDK.NodeWithDatum => ({
    utxo: {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: "addr_test1",
      assets: { lovelace },
    },
    datum: {
      key: { Key: { key } },
      next: "Empty",
      data: data as SDK.LinkedListNodeView["data"],
    },
    assetName: key,
  });
  const operator = "11".repeat(28);
  const registeredNode = (activationKey: string) =>
    node(activationKey, SDK.encodeRegisteredOperatorDatumValue(operator));
  const hubOracle = {
    utxo: node("", null).utxo,
  } as unknown as SDK.OperatorDirectorySnapshot["hubOracle"];

  it("prefers an active membership as the proof", () => {
    const selection = selectDuplicateRegistration(
      {
        registered: [registeredNode("0000000000000001")],
        active: [node(operator, null)],
        retired: [],
        hubOracle,
      },
      operator,
    );
    expect(selection?.proof.kind).toBe("active");
    expect(SDK.nodeKeyHex(selection!.removed.datum.key)).toBe(
      "0000000000000001",
    );
  });

  it("removes the later of two registrations and proves it by the earlier one", () => {
    const selection = selectDuplicateRegistration(
      {
        registered: [
          registeredNode("0000000000000001"),
          registeredNode("0000000000000009"),
        ],
        active: [],
        retired: [],
        hubOracle,
      },
      operator,
    );
    expect(selection?.proof.kind).toBe("registered");
    expect(SDK.nodeKeyHex(selection!.removed.datum.key)).toBe(
      "0000000000000009",
    );
    expect(SDK.nodeKeyHex(selection!.proof.node.datum.key)).toBe(
      "0000000000000001",
    );
  });

  it("finds nothing to slash for a single honest registration", () => {
    expect(
      selectDuplicateRegistration(
        {
          registered: [registeredNode("0000000000000001")],
          active: [],
          retired: [],
          hubOracle,
        },
        operator,
      ),
    ).toBeNull();
  });
});
