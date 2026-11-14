/**
 * The CanonicalDecode producer: every transaction field's items in order,
 * each checked for a legal width as its field's encoded length grows.
 */
import { encodeCbor } from "@al-ft/midgard-core";

import { canonicalCborArgumentHeaderSize } from "./canonical-field-item.js";
import { canonicalOrderedItemEncodedLength } from "./canonical-field-order.js";
import { countedMachineFieldTrace } from "./field-carriage.js";
import { proofBinding } from "./prepare-producers.shared.js";
import {
  preparedFieldPreimage,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import {
  phaseContinues,
  type PhaseOutcome,
  phaseStops,
  type TraceWalk,
} from "./trace-walk.js";

export const produceCanonicalDecode = (
  prepared: Pick<
    PreparedValidationTransaction,
    "proofSource" | "contextCbor" | "fieldPreimages"
  >,
  { push }: Pick<TraceWalk, "push">,
): PhaseOutcome => {
  for (const field of prepared.fieldPreimages) {
    const { fieldIndex } = field;
    const witness = (itemIndex: number, count: number, length: number) =>
      encodeCbor([
        ...proofBinding(prepared),
        BigInt(fieldIndex),
        BigInt(itemIndex),
        0n,
        BigInt(count),
        BigInt(length),
      ]);
    const collection = countedMachineFieldTrace(fieldIndex, field.preimageCbor);
    if (collection.items.length === 0) {
      push("canonicalDecode", witness(0, -1, 0));
      continue;
    }
    let itemCount = -1;
    let encodedLength = 0;
    for (const item of collection.items) {
      push(
        "canonicalDecode",
        witness(item.itemIndex, itemCount, encodedLength),
        {
          kind: "transactionFieldItem",
          fieldIndex,
          fieldPreimage: preparedFieldPreimage(prepared, fieldIndex),
        },
      );
      if (itemCount === -1) {
        itemCount = collection.items.length;
        encodedLength = canonicalCborArgumentHeaderSize(itemCount);
      }
      const itemEncodedLength = canonicalOrderedItemEncodedLength(
        fieldIndex,
        collection.items,
        item.itemIndex,
      );
      if (itemEncodedLength === null) {
        return phaseStops(
          {
            FieldItemWidthIllegal: {
              field_index: BigInt(fieldIndex),
              item_index: BigInt(item.itemIndex),
            },
          },
          "canonical decode found an item of illegal width",
        );
      }
      encodedLength += itemEncodedLength;
    }
  }
  return phaseContinues;
};
