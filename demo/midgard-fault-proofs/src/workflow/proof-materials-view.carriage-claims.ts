/**
 * Carriage slots claimed by content. A slot at the proof-materials address
 * is held by the plain output the process's view finds for its datum, the
 * k-th copy for the k-th slot holding those bytes, whoever paid it: only the
 * prover can spend there and the chain checks content alone. That output is
 * the slot's newest claim, so it is the one a step references, and the need
 * is registered for the workflow before the read. A claim this process made on
 * the slot stays behind it unless the view gave that output to another slot.
 */
import type { OwnOutputClaimed, OwnOutputJournal } from "./own-outputs.js";

/** One carriage slot: its claim key, where it sits and the datum it holds. */
export type ContentCarriageSlot = Readonly<{
  key: string;
  address: string;
  datumCbor: string;
}>;

const outRefOf = (utxo: { txHash: string; outputIndex: number }): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * `ownOutputs` with each of `slots` at its view's address claimed by the
 * output found for it, newest; `ownOutputs` itself when it has no view.
 */
export const withContentFoundCarriage = async (
  ownOutputs: OwnOutputJournal,
  slots: readonly ContentCarriageSlot[],
): Promise<OwnOutputJournal> => {
  const materials = ownOutputs.materials;
  if (materials === undefined) return ownOutputs;
  const atView = slots.filter(
    ({ address }) => address === materials.view.address,
  );
  if (atView.length === 0) return ownOutputs;
  const found = await materials.view.find(
    atView.map(({ datumCbor }) => datumCbor),
    materials.holder,
  );
  const foundFor = new Map<string, string>();
  for (const [index, { key }] of atView.entries()) {
    const utxo = found[index];
    if (utxo !== undefined) foundFor.set(key, outRefOf(utxo));
  }
  const taken = new Set(foundFor.values());
  const claims = (key: string): readonly OwnOutputClaimed[] => {
    const claimed = ownOutputs.claims(key);
    if (!atView.some((slot) => slot.key === key)) return claimed;
    const own = foundFor.get(key);
    return Object.freeze([
      ...claimed.filter(({ outRef }) => !taken.has(outRef)),
      ...(own === undefined ? [] : [Object.freeze({ outRef: own })]),
    ]);
  };
  return Object.freeze({
    claims,
    claim: ownOutputs.claim,
    materials,
  });
};
