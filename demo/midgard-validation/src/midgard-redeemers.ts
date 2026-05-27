import {
  asArray,
  asBigInt,
  asMap,
  decodeSingleCbor,
  encodeCbor,
} from "@al-ft/midgard-core/codec/cbor";
import { CML, Constr, Data } from "@lucid-evolution/lucid";

import { txOutRefData } from "./tx-out-ref.js";

export const MidgardRedeemerTag = {
  Spend: CML.RedeemerTag.Spend,
  Mint: CML.RedeemerTag.Mint,
  Reward: CML.RedeemerTag.Reward,
  Receiving: 6,
} as const;

export type MidgardRedeemerPointer = {
  readonly tag: number;
  readonly index: bigint;
};

export type DecodedMidgardRedeemer = MidgardRedeemerPointer & {
  readonly dataCborHex: string;
  readonly exUnits: {
    readonly memory: bigint;
    readonly steps: bigint;
  };
};

export const midgardRedeemerPointerKey = (
  pointer: MidgardRedeemerPointer,
): string => `${pointer.tag}:${pointer.index.toString(10)}`;

const ensureSupportedTag = (tag: number, fieldName: string): void => {
  if (
    tag !== MidgardRedeemerTag.Spend &&
    tag !== MidgardRedeemerTag.Mint &&
    tag !== MidgardRedeemerTag.Reward &&
    tag !== MidgardRedeemerTag.Receiving
  ) {
    throw new Error(`${fieldName} has unsupported redeemer tag ${tag}`);
  }
};

const decodeExUnits = (
  value: unknown,
  fieldName: string,
): DecodedMidgardRedeemer["exUnits"] => {
  const arr = asArray(value, fieldName);
  if (arr.length !== 2) {
    throw new Error(`${fieldName} must have exactly 2 elements`);
  }
  return {
    memory: asBigInt(arr[0], `${fieldName}.memory`),
    steps: asBigInt(arr[1], `${fieldName}.steps`),
  };
};

const decodeRedeemerDataCborHex = (
  value: unknown,
  fieldName: string,
): string => {
  const dataCborHex =
    value instanceof Uint8Array
      ? Buffer.from(value).toString("hex")
      : encodeCbor(value).toString("hex");
  try {
    Data.from(dataCborHex);
  } catch (e) {
    throw new Error(`${fieldName} must encode Plutus Data: ${String(e)}`);
  }
  return dataCborHex;
};

const decodeRedeemerArray = (
  value: unknown,
  fieldName: string,
): DecodedMidgardRedeemer => {
  const arr = asArray(value, fieldName);
  if (arr.length !== 4) {
    throw new Error(`${fieldName} must have exactly 4 elements`);
  }
  const tag = Number(asBigInt(arr[0], `${fieldName}.tag`));
  ensureSupportedTag(tag, `${fieldName}.tag`);
  return {
    tag,
    index: asBigInt(arr[1], `${fieldName}.index`),
    dataCborHex: decodeRedeemerDataCborHex(arr[2], `${fieldName}.data`),
    exUnits: decodeExUnits(arr[3], `${fieldName}.ex_units`),
  };
};

const decodeRedeemerMapEntry = (
  key: unknown,
  value: unknown,
  fieldName: string,
): DecodedMidgardRedeemer => {
  const keyArr = asArray(key, `${fieldName}.key`);
  if (keyArr.length !== 2) {
    throw new Error(`${fieldName}.key must have exactly 2 elements`);
  }
  const tag = Number(asBigInt(keyArr[0], `${fieldName}.key.tag`));
  ensureSupportedTag(tag, `${fieldName}.key.tag`);
  const valArr = asArray(value, `${fieldName}.value`);
  if (valArr.length !== 2) {
    throw new Error(`${fieldName}.value must have exactly 2 elements`);
  }
  return {
    tag,
    index: asBigInt(keyArr[1], `${fieldName}.key.index`),
    dataCborHex: decodeRedeemerDataCborHex(
      valArr[0],
      `${fieldName}.value.data`,
    ),
    exUnits: decodeExUnits(valArr[1], `${fieldName}.value.ex_units`),
  };
};

export const decodeMidgardRedeemers = (
  preimageCbor: Uint8Array,
): readonly DecodedMidgardRedeemer[] => {
  if (preimageCbor.length === 0) {
    return [];
  }
  const decoded = decodeSingleCbor(preimageCbor);
  if (Array.isArray(decoded)) {
    return decoded.map((item, index) =>
      decodeRedeemerArray(item, `redeemers[${index}]`),
    );
  }

  const map = asMap(decoded, "redeemers");
  return Array.from(map.entries(), ([key, value], index) =>
    decodeRedeemerMapEntry(key, value, `redeemers[${index}]`),
  );
};

export const findRedeemerByPointer = (
  redeemers: readonly DecodedMidgardRedeemer[],
  pointer: MidgardRedeemerPointer,
): DecodedMidgardRedeemer | undefined => {
  return redeemers.find(
    (redeemer) =>
      redeemer.tag === pointer.tag && redeemer.index === pointer.index,
  );
};

export const redeemerDataFromCborHex = (cborHex: string): unknown =>
  Data.from(cborHex) as unknown;

export type MidgardScriptPurpose =
  | {
      readonly kind: "mint";
      readonly scriptHash: string;
      readonly policyId: string;
    }
  | {
      readonly kind: "spend";
      readonly scriptHash: string;
      readonly outRefHex: string;
    }
  | { readonly kind: "observe"; readonly scriptHash: string }
  | { readonly kind: "receive"; readonly scriptHash: string };

const scriptCredentialData = (scriptHash: string): Constr<unknown> =>
  new Constr(1, [scriptHash]);

export const cardanoScriptPurposeData = (
  purpose: MidgardScriptPurpose,
): Constr<unknown> => {
  switch (purpose.kind) {
    case "mint":
      return new Constr(0, [purpose.policyId]);
    case "spend":
      return new Constr(1, [txOutRefData(purpose.outRefHex)]);
    case "observe":
      return new Constr(2, [scriptCredentialData(purpose.scriptHash)]);
    case "receive":
      throw new Error("Receiving scripts do not have a Cardano script purpose");
  }
};

export const midgardScriptPurposeData = (
  purpose: MidgardScriptPurpose,
): Constr<unknown> => {
  switch (purpose.kind) {
    case "mint":
      return new Constr(0, [purpose.policyId]);
    case "spend":
      return new Constr(1, [
        purpose.scriptHash,
        txOutRefData(purpose.outRefHex),
      ]);
    case "observe":
      return new Constr(2, [purpose.scriptHash]);
    case "receive":
      return new Constr(3, [purpose.scriptHash]);
  }
};
