import {
  decodeMidgardCekProgramMaterialDaEntry,
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramMaterialEntry,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { collectMidgardEventProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import type * as SDK from "@al-ft/midgard-sdk";

/** The block's CEK program material entries that decode. The header commits
 * none of them: an entry that does not decode is ignored like an unreached
 * one, and an event that needs it finds its root missing. */
export const importedProgramMaterial = (
  entries: readonly SDK.DaPayloadEntry[],
): readonly MidgardCekProgramMaterialEntry[] =>
  entries.flatMap(([key, value]) => {
    try {
      return [
        decodeMidgardCekProgramMaterialDaEntry(
          Buffer.from(key, "hex"),
          Buffer.from(value, "hex"),
        ),
      ];
    } catch {
      return [];
    }
  });

/** Select only the programs reachable by this event against its own pre-state.
 * DA carries a block union; the ordinary replay sidecar requires an exact event
 * set, and entries no event reaches are ignored.
 * A forced transaction runs no Plutus program, so its sidecar is empty and its
 * items are never decoded here: an admitted order whose items do not decode
 * still replays to its committed rejection. */
export const importedEventProgramSidecar = (input: {
  readonly cbor: Buffer;
  readonly sourceKind: "normal" | "forced";
  readonly ledger: ReadonlyMap<string, Buffer>;
  readonly material: readonly MidgardCekProgramMaterialEntry[];
}): Buffer => {
  if (input.sourceKind === "forced")
    return encodeMidgardCekProgramMaterialSidecar([]);
  const transaction = decodeMidgardNativeTxFullFromCanonicalCbor(input.cbor);
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
  return encodeMidgardCekProgramMaterialSidecar(
    input.material.filter((entry) =>
      eventRoots.has(Buffer.from(entry.root).toString("hex")),
    ),
  );
};
