/** The script purposes a direct purpose leaf carries, as the purpose phase
 * walks them: every purpose whose credential is a script and whose
 * membership the bound source can show. */
import type * as SDK from "@al-ft/midgard-sdk";

import {
  directPurposeAt,
  directReceiveCredentials,
  directSpendInputAt,
} from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";

type Coordinate = SDK.DirectPurposeCoordinate;

/** Whether the spend at canonical rank `index` has a predecessor descriptor;
 * without one no purpose or membership can be shown for it. */
export const directPurposeSpendDescriptorPresent = (
  material: DirectScriptMaterial,
  index: bigint,
): boolean => {
  const spend = directSpendInputAt(material, index);
  return (
    spend !== null &&
    material.ledger.root.entries.some((entry) => entry.key.equals(spend.item))
  );
};

/** Whether every spent and referenced input has a predecessor descriptor;
 * a leaf with an unresolved one is convicted by the input families, and no
 * source index can be built for it. */
export const directPurposeInputDescriptorsPresent = (
  material: DirectScriptMaterial,
): boolean => {
  const keys = new Set(
    material.ledger.root.entries.map((entry) => entry.key.toString("hex")),
  );
  return [...material.fields[0]!, ...material.fields[1]!].every((key) =>
    keys.has(key.toString("hex")),
  );
};

const coordinates = (kind: bigint, count: number): Coordinate[] =>
  Array.from({ length: count }, (_, index) => ({
    kind,
    index: BigInt(index),
  }));

/** Every script purpose the leaf carries, observers first, then mints,
 * receives and spends; key-locked spends and spends with no predecessor
 * descriptor carry none. */
export const directPurposeScriptPurposes = (
  material: DirectScriptMaterial,
): readonly SDK.DirectPurpose[] =>
  [
    ...coordinates(2n, material.fields[3]!.length),
    ...coordinates(1n, material.fields[5]!.length),
    ...coordinates(3n, directReceiveCredentials(material.fields[2]!).length),
    ...coordinates(0n, material.fields[0]!.length).filter(({ index }) =>
      directPurposeSpendDescriptorPresent(material, index),
    ),
  ].flatMap((coordinate) => directPurposeAt(material, coordinate) ?? []);
