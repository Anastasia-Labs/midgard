import {
  decodeMidgardCekProgramMaterialDaEntry,
  encodeMidgardCekProgramMaterialDaValue,
  type MidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core/cek-proof";

export const entryTableName = "cek_program_material_entries";

export const membershipTableName = "cek_program_material_memberships";

export const admissionOwnerTableName = "cek_program_material_admission_owners";

export const retainedStateOwnerTableName =
  "cek_program_material_retained_state_owners";

export const STORE_ADVISORY_LOCK_NAMESPACE = 1_129_606_987;

export const STORE_ADVISORY_LOCK_KEY = 1_296_651_247;

export type MaterialRow = {
  readonly material_root: Buffer;
  readonly da_value_cbor: Buffer;
};

export type StoreUsageRow = {
  readonly total_bytes: string;
};

export const postgresByteaArray = (
  values: readonly Buffer[],
): readonly string[] => values.map((value) => `\\x${value.toString("hex")}`);

export const rootHex = (root: Uint8Array): string =>
  Buffer.from(root).toString("hex");

export const canonicalEntries = (
  entries: readonly MidgardCekProgramMaterialEntry[],
): readonly MidgardCekProgramMaterialEntry[] => {
  const byRoot = new Map<string, MidgardCekProgramMaterialEntry>();
  for (const entry of entries) {
    const daValue = encodeMidgardCekProgramMaterialDaValue(entry);
    const canonical = decodeMidgardCekProgramMaterialDaEntry(
      entry.root,
      daValue,
    );
    const key = rootHex(canonical.root);
    const existing = byRoot.get(key);
    if (
      existing !== undefined &&
      !encodeMidgardCekProgramMaterialDaValue(existing).equals(daValue)
    ) {
      throw new Error(`conflicting CEK material for root ${key}`);
    }
    byRoot.set(key, canonical);
  }
  return [...byRoot.values()].sort((left, right) =>
    Buffer.compare(Buffer.from(left.root), Buffer.from(right.root)),
  );
};
