import {
  decodeMidgardRedeemerWitnessFieldPreimageV1,
  MIDGARD_REDEEMER_PURPOSE_TAGS_V1,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
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

/**
 * §5.1/§5.3: field 8 is the enveloped list of `enc_8` items. The §5.3 decoder
 * owns the wire grammar — the `84` head, the purpose-tag value set, minimal
 * `index`/`ex_units` uints, and trailing bytes after the execution units all
 * reject there — and this function only adapts the result into the shape the
 * validation machine consumes, re-checking the narrower Midgard builder tag set
 * and that the redeemer payload is Plutus `Data`.
 *
 * The retired counted scheme accepted two spellings here (a raw array of
 * four-element arrays, or a CBOR map keyed by pointer). §6.1 admits one byte
 * form per value, so both are gone.
 */
export const decodeMidgardRedeemers = (
  preimageCbor: Uint8Array,
): readonly DecodedMidgardRedeemer[] =>
  decodeMidgardRedeemerWitnessFieldPreimageV1(preimageCbor).map(
    (witness, index) => {
      const fieldName = `redeemers[${index}]`;
      const tag = MIDGARD_REDEEMER_PURPOSE_TAGS_V1[witness.purpose];
      ensureSupportedTag(tag, `${fieldName}.tag`);
      return {
        tag,
        index: witness.index,
        dataCborHex: decodeRedeemerDataCborHex(
          witness.redeemerCbor,
          `${fieldName}.data`,
        ),
        exUnits: {
          memory: witness.executionUnits.memory,
          steps: witness.executionUnits.steps,
        },
      };
    },
  );

export const findRedeemerByPointer = <T extends MidgardRedeemerPointer>(
  redeemers: readonly T[],
  pointer: MidgardRedeemerPointer,
): T | undefined => {
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
