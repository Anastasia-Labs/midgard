import { readFileSync } from "node:fs";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildCekContextTail,
  l1SlotConfigParameterData,
  parseFaultProofBlueprint,
} from "../src/index.js";

// The txInfo stage of the CEK context family turns a committed body's slot
// interval into the POSIX validity range a script sees, so it is applied with
// the deployment's L1 slot mapping (`script_context_v1.L1SlotConfigV1`) and no
// other stage is.

const DEVNET = { zeroTime: 1_789_056_000_000, zeroSlot: 0, slotLength: 1000 };
const AWARD = "aa".repeat(28);
const CT_POLICY = "bb".repeat(28);
const FIELD_PREIMAGE_POLICY = "cc".repeat(28);

const blueprint = () =>
  parseFaultProofBlueprint(
    JSON.parse(
      readFileSync(
        new URL("../../../onchain/aiken/plutus.json", import.meta.url),
        "utf8",
      ),
    ) as unknown,
  );

describe("l1SlotConfigParameterData", () => {
  it("encodes Constr 0 [zeroTime, zeroSlot, slotLength]", () => {
    expect(
      Data.to(l1SlotConfigParameterData(NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod)),
    ).toBe(
      // 121([1655769600000, 86400, 1000])
      "d8799f1b00000181838f10001a000151801903e8ff",
    );
    expect(Data.to(l1SlotConfigParameterData(DEVNET))).toBe(
      "d8799f1b000001a08c0c2000001903e8ff",
    );
  });

  it("refuses a mapping outside the parameter's domain", () => {
    expect(() =>
      l1SlotConfigParameterData({ ...DEVNET, slotLength: 0 }),
    ).toThrow(/slotLength: must be positive/u);
    expect(() =>
      l1SlotConfigParameterData({ ...DEVNET, zeroTime: -1 }),
    ).toThrow(/zeroTime: must be a non-negative safe integer/u);
  });
});

describe("buildCekContextTail slot mapping", () => {
  it("applies the mapping to the txInfo stage alone", () => {
    const loaded = blueprint();
    const preprod = buildCekContextTail(
      loaded,
      "Preprod",
      AWARD,
      CT_POLICY,
      FIELD_PREIMAGE_POLICY,
      NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    );
    const devnet = buildCekContextTail(
      loaded,
      "Preprod",
      AWARD,
      CT_POLICY,
      FIELD_PREIMAGE_POLICY,
      DEVNET,
    );
    expect(devnet.txInfo.spendingScriptHash).not.toBe(
      preprod.txInfo.spendingScriptHash,
    );
    for (const stage of Object.keys(preprod) as (keyof typeof preprod)[]) {
      if (stage === "txInfo") continue;
      expect(devnet[stage].spendingScriptHash, stage).toBe(
        preprod[stage].spendingScriptHash,
      );
    }
  });
});
