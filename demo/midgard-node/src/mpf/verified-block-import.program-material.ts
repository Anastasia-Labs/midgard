import {
  decodeMidgardCekProgramMaterialDaEntry,
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramMaterialEntry,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import { collectMidgardEventProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import type * as SDK from "@al-ft/midgard-sdk";

export const importedProgramMaterial = (
  entries: readonly SDK.DaPayloadEntry[],
): readonly MidgardCekProgramMaterialEntry[] =>
  entries.map(([key, value]) =>
    decodeMidgardCekProgramMaterialDaEntry(
      Buffer.from(key, "hex"),
      Buffer.from(value, "hex"),
    ),
  );

/** Select only the programs reachable by this event against its own pre-state.
 * DA carries a block union; the ordinary replay sidecar requires an exact event set. */
export const importedEventProgramSidecar = (input: {
  readonly cbor: Buffer;
  readonly sourceKind: "normal" | "forced";
  readonly ledger: ReadonlyMap<string, Buffer>;
  readonly material: readonly MidgardCekProgramMaterialEntry[];
  readonly reached: Set<string>;
}): Buffer => {
  const transaction = (
    input.sourceKind === "forced"
      ? decodeMidgardForcedTxFullFromCanonicalCbor
      : decodeMidgardNativeTxFullFromCanonicalCbor
  )(input.cbor);
  const envelopes = collectMidgardEventProgramEnvelopes(
    transaction,
    (key) => input.ledger.get(key),
    input.sourceKind,
  );
  const results = verifyMidgardCekProgramMaterialBundle(
    envelopes,
    input.material,
    { allowUnreachable: true },
  );
  const eventRoots = new Set(
    results.flatMap((result) => [...result.reachableRoots]),
  );
  for (const root of eventRoots) input.reached.add(root);
  return encodeMidgardCekProgramMaterialSidecar(
    input.material.filter((entry) =>
      eventRoots.has(Buffer.from(entry.root).toString("hex")),
    ),
  );
};
