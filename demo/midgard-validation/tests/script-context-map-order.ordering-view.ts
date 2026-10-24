import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";

import { encodeMidgardCekPlutusData } from "../src/cek-constant.js";
import {
  type MidgardLedgerRedeemer,
  MidgardRedeemerTag,
  type MidgardScriptPurpose,
  type ScriptContextView,
} from "../src/index.js";
import { DataConstr, DataI, DataMap } from "../src/uplc-data.js";
import { outRefFromByte } from "./validation-fixtures.js";

/** The context's redeemer map, as (purpose constructor, redeemer int). */
export const redeemerMapEntries = (
  context: DataConstr,
  redeemersField: number,
): readonly (readonly [number, bigint])[] => {
  const txInfo = context.fields[0] as DataConstr;
  const map = txInfo.fields[redeemersField] as DataMap<DataConstr, DataI>;
  return map.map.map(
    (pair) => [Number(pair.fst.constr), BigInt(pair.snd.int)] as const,
  );
};

export const orderingView = (): {
  readonly view: ScriptContextView;
  readonly spend: {
    readonly purpose: MidgardScriptPurpose;
    readonly redeemer: MidgardLedgerRedeemer;
  };
} => {
  const scriptHash = "ab".repeat(28);
  const entry = (
    purpose: MidgardScriptPurpose,
    tag: number,
    index: bigint,
    value: bigint,
  ) => ({
    purpose,
    redeemer: {
      tag,
      index,
      dataCbor: encodeMidgardCekPlutusData(new DataI(value)),
      exUnits: { memory: 0n, steps: 0n },
    },
  });
  const spend0 = entry(
    { kind: "spend", scriptHash, outRefHex: outRefFromByte(1).toString("hex") },
    MidgardRedeemerTag.Spend,
    0n,
    0n,
  );
  const spend1 = entry(
    { kind: "spend", scriptHash, outRefHex: outRefFromByte(2).toString("hex") },
    MidgardRedeemerTag.Spend,
    1n,
    1n,
  );
  const mint = entry(
    { kind: "mint", scriptHash, policyId: scriptHash },
    MidgardRedeemerTag.Mint,
    0n,
    2n,
  );
  const observe = entry(
    { kind: "observe", scriptHash },
    MidgardRedeemerTag.Reward,
    0n,
    3n,
  );
  const receive = entry(
    { kind: "receive", scriptHash },
    MidgardRedeemerTag.Receiving,
    0n,
    4n,
  );
  return {
    view: {
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      txId: Buffer.alloc(32, 0),
      inputs: [],
      referenceInputs: [],
      outputs: [],
      fee: 1n,
      observers: [scriptHash],
      signatories: [],
      mint: new Map(),
      // Deliberately not in ledger order.
      redeemers: [receive, observe, spend1, mint, spend0],
    },
    spend: spend0,
  };
};
