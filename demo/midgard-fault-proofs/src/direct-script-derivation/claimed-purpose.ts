import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { keyValuePhasMembershipProofs } from "../transition-trace/phas.js";
import { createDirectFieldPlan } from "./field-plan.js";
import type { DirectScriptMaterial } from "./material.js";
import { directSpendInputAt } from "./purposes.js";
import { buildDirectReceiveEvidence } from "./receive-evidence.js";

/** Prepare authenticated domain evidence, never a trusted claimed receipt.
 * The leaf is the sole selector; the physical suffix checks each transition. */
export const prepareClaimedPurposeEvidence = async ({
  material,
  observed,
  owner,
}: {
  readonly material: DirectScriptMaterial;
  /** Only the authenticated source selection is read from the caller state. */
  readonly observed: { readonly authenticated: SDK.DirectSourceSelection };
  readonly owner: string;
}) => {
  const selection = observed.authenticated.selection;
  if (
    Data.to(selection.bound, SDK.DirectScriptBound) !==
    Data.to(material.bound, SDK.DirectScriptBound)
  )
    throw new Error("claimed purpose evidence: observed source bound differs");
  const reason = material.bound.subject.rejection_reason;
  const coordinate =
    reason === null ? null : SDK.purposeCoordinateOfReason(reason);
  if (
    coordinate === null ||
    !SDK.purposeCoordinateIsValid(coordinate) ||
    (coordinate.kind === selection.coordinate.kind &&
      coordinate.index === selection.coordinate.index)
  ) {
    if (
      coordinate !== null &&
      SDK.purposeCoordinateIsValid(coordinate) &&
      selection.purpose === null
    )
      throw new Error("claimed purpose evidence: reused actual purpose absent");
    return { route: "Complete", coordinate } as const;
  }
  if (coordinate.kind === 0n) {
    const field = createDirectFieldPlan(material, 0, owner);
    const selected = directSpendInputAt(material, coordinate.index);
    if (selected === null)
      return { route: "SpendOutOfRange", coordinate, field } as const;
    const descriptor = material.ledger.root.entries.find((entry) =>
      entry.key.equals(selected.item),
    );
    if (descriptor === undefined)
      throw new Error("claimed purpose evidence: spent descriptor absent");
    const [proof] = await keyValuePhasMembershipProofs(material.ledger.root, [
      descriptor,
    ]);
    if (proof === undefined)
      throw new Error("claimed purpose evidence: spent membership absent");
    return {
      route: "Spend",
      coordinate,
      field,
      rawItemIndex: BigInt(selected.rawItemIndex),
      descriptorCbor: descriptor.value.toString("hex"),
      proof,
    } as const;
  }
  if (coordinate.kind === 1n)
    return {
      route: "Mint",
      coordinate,
      field: createDirectFieldPlan(material, 5, owner),
    } as const;
  if (coordinate.kind === 2n)
    return {
      route: "Observe",
      coordinate,
      field: createDirectFieldPlan(material, 3, owner),
    } as const;
  return {
    route: "Receive",
    coordinate,
    field: createDirectFieldPlan(material, 2, owner),
    receive: await buildDirectReceiveEvidence(material.fields[2]!),
  } as const;
};
