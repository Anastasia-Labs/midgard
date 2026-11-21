/** Spend, mint and receive purposes of a `missingRedeemer` subject: mint
 * policies named by their scripts, outputs at protected script addresses,
 * the redeemers those purposes carry, and the scripts they bring as witnesses. */
import {
  encodeMidgardFieldPreimageForField,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardTxOutput,
  hashMidgardVersionedScript,
  type MidgardRedeemerPurpose,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";

export type MissingRedeemerPurposeScript = {
  readonly script: MidgardVersionedScript;
  /** Carry the script as a field-6 witness; otherwise it must resolve elsewhere. */
  readonly witness?: boolean;
  readonly redeemer: boolean;
};

export type MissingRedeemerPurposeShape = {
  /** Mint policies, each the hash of its script; one unit minted per policy. */
  readonly mints?: readonly MissingRedeemerPurposeScript[];
  /** One output per entry at the protected script address of its script. */
  readonly receives?: readonly MissingRedeemerPurposeScript[];
  /** Further field-6 scripts, such as a spent output's validator. */
  readonly witnesses?: readonly MidgardVersionedScript[];
  /** Spend redeemer indices, by canonical spend rank. */
  readonly spendRedeemers?: readonly bigint[];
};

export const redeemerItem = (
  purpose: MidgardRedeemerPurpose,
  index: bigint,
  data: Buffer,
) =>
  encodeMidgardRedeemerWitnessItem({
    purpose,
    index,
    redeemerCbor: data,
    executionUnits: { memory: 1_000n, steps: 1_000n },
  });

const byHash = (entries: readonly MissingRedeemerPurposeScript[]) =>
  entries
    .map((entry) => ({
      ...entry,
      hash: Buffer.from(hashMidgardVersionedScript(entry.script), "hex"),
    }))
    .sort((left, right) => Buffer.compare(left.hash, right.hash));

/** The protected script address paying the script's credential. */
export const protectedScriptAddress = (script: MidgardVersionedScript) =>
  Buffer.from(`78${hashMidgardVersionedScript(script)}`, "hex");

/** The enterprise script address paying the script's credential. */
export const scriptAddress = (script: MidgardVersionedScript) =>
  Buffer.from(`70${hashMidgardVersionedScript(script)}`, "hex");

/** Fields 2 and 5, the purpose redeemers by tag, and the purpose witnesses. */
export const missingRedeemerPurposeFields = (
  shape: MissingRedeemerPurposeShape,
  data: Buffer,
) => {
  const mints = byHash(shape.mints ?? []);
  const receives = shape.receives ?? [];
  const credentials = byHash([
    ...new Map(
      receives.map((entry) => [
        hashMidgardVersionedScript(entry.script),
        entry,
      ]),
    ).values(),
  ]);
  const redeemers = (
    purpose: MidgardRedeemerPurpose,
    entries: ReturnType<typeof byHash>,
  ) =>
    entries.flatMap((entry, index) =>
      entry.redeemer ? [redeemerItem(purpose, BigInt(index), data)] : [],
    );
  return {
    outputsPreimageCbor: encodeCbor(
      receives.map((entry) =>
        encodeMidgardTxOutput({
          address: protectedScriptAddress(entry.script),
          value: { lovelace: 2_000_000n, assets: new Map() },
        }),
      ),
    ),
    mintPreimageCbor: encodeMidgardFieldPreimageForField({
      fieldIndex: 5,
      items: mints.map((entry) => ({
        policyId: entry.hash,
        assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
      })),
    }),
    spendRedeemers: (shape.spendRedeemers ?? []).map((index) =>
      redeemerItem("Spend", index, data),
    ),
    mintRedeemers: redeemers("Mint", mints),
    receiveRedeemers: redeemers("Receive", credentials),
    witnesses: [
      ...[...(shape.mints ?? []), ...receives]
        .filter((entry) => entry.witness ?? true)
        .map((entry) => entry.script),
      ...(shape.witnesses ?? []),
    ],
  };
};
