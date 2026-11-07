/** Derive both original coordinates from retained source bytes, including the
 * compact carried by the on-chain thread. Artifact admission reruns this. */
import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxCompact,
  decodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";

import { planFaultProofFieldOpening } from "../field-opening.js";
import {
  forcedTxFromCoreCompact,
  nativeTxFromCoreCompact,
} from "../step-support.js";
import type { OpenedProofThreadSource } from "./source.js";

const decodedInput = (item: Buffer | undefined): SDK.MidgardTxInput | null => {
  if (item === undefined) return null;
  const decoded = decodeMidgardSpendInputItem(item);
  return {
    tx_id: Buffer.from(decoded.txId).toString("hex"),
    output_index: BigInt(decoded.outputIndex),
  };
};

export const deriveInputExistenceSelection = ({
  source,
  field,
  inputIndex,
  owner,
}: {
  readonly source: OpenedProofThreadSource;
  readonly field: 0 | 1;
  readonly inputIndex?: bigint;
  readonly owner: string;
}) => {
  const reason = source.subject.rejection_reason;
  const exactClaim =
    reason !== null &&
    typeof reason === "object" &&
    "InputNotFound" in reason &&
    reason.InputNotFound.source_kind === BigInt(field)
      ? reason.InputNotFound.input_index
      : undefined;
  const selectedIndex = inputIndex ?? exactClaim;
  if (selectedIndex === undefined)
    throw new Error("input-existence: observed input index is required");
  const bytes = source.fieldPreimages[field];
  if (bytes === undefined)
    throw new Error("input-existence: missing source field bytes");
  const inputItems = decodeMidgardFieldPreimage(bytes);
  const selectedItem =
    selectedIndex >= 0n && selectedIndex < BigInt(inputItems.length)
      ? inputItems[Number(selectedIndex)]
      : undefined;
  if (selectedItem === undefined && selectedIndex !== exactClaim)
    throw new Error(
      "input-existence: only the exact named input may be out of domain",
    );
  const selectedInput = decodedInput(selectedItem);
  const compact: SDK.InputExistenceCompact =
    source.kind === "accepted"
      ? nativeTxFromCoreCompact(
          decodeMidgardNativeTxCompact(Buffer.from(source.compactCbor, "hex")),
        )
      : {
          ...forcedTxFromCoreCompact(
            decodeMidgardForcedTxCompact(
              Buffer.from(source.compactCbor, "hex"),
            ),
          ),
          validity_code: null,
        };
  const plan = (fieldIndex: 0 | 1, itemCbors: readonly Buffer[]) =>
    planFaultProofFieldOpening({
      anchorSourceKind: source.kind === "accepted" ? 0n : 1n,
      fieldIndex,
      anchorTxId: source.subject.transaction_id,
      nativeTxCompactCbor: source.compactCbor,
      itemCbors,
      owner,
      label: "input-existence event field",
    });
  const coordinate =
    reason === null ? null : SDK.resolutionCoordinateOfReason(reason);
  let claimedInput: SDK.MidgardTxInput | null = null;
  let claimedFieldPlan: ReturnType<typeof plan> | null = null;
  if (
    coordinate !== null &&
    (coordinate.sourceKind === 0n || coordinate.sourceKind === 1n) &&
    coordinate.inputIndex >= 0n
  ) {
    const claimedField = coordinate.sourceKind === 0n ? 0 : 1;
    const claimedBytes = source.fieldPreimages[claimedField];
    if (claimedBytes === undefined)
      throw new Error("input-existence: missing claimed source field bytes");
    const claimedItems =
      claimedField === field
        ? inputItems
        : decodeMidgardFieldPreimage(claimedBytes);
    claimedInput = decodedInput(
      coordinate.inputIndex < BigInt(claimedItems.length)
        ? claimedItems[Number(coordinate.inputIndex)]
        : undefined,
    );
    if (claimedField !== field)
      claimedFieldPlan = plan(claimedField, claimedItems);
  }
  return {
    compact,
    inputIndex: selectedIndex,
    inputItems,
    selectedInput,
    fieldPlan: plan(field, inputItems),
    claimedInput,
    claimedFieldPlan,
  };
};
