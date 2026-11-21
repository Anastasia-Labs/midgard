/**
 * InputSets' descending merge of fields 0 (spend inputs) and 1 (reference
 * inputs), one machine step at a time.
 *
 * TypeScript twin of `verify_input_sets_item` in
 * `onchain/aiken/lib/midgard/validation-machine/input-sets.ak`: the same
 * control gives the same opened item and the same successor. CanonicalDecode
 * has already refused either field out of ascending order, so the merge reads
 * both from the tail and consumes the larger head each step. The node's
 * `validateInputSets` and the trace builder both run it, so the DuplicateInput
 * they report is the one the machine reaches.
 */

/** Field 0 or field 1, as item keys (out-ref CBOR) in field order. */
export type InputSetFields = readonly [
  spend: readonly Uint8Array[],
  reference: readonly Uint8Array[],
];

/**
 * The merge part of `InputSetsControlV1`. A count is −1 until the merge first
 * opens that field (0 when the field is empty from the start). `lastKey` is
 * empty, and `lastField`/`lastItem` are −1, before the first consumed item.
 * `cachedOtherHead` is the other field's next item when it has been read and
 * not consumed, else empty.
 */
export type InputSetMergeControl = {
  readonly spendCount: number;
  readonly referenceCount: number;
  readonly spendSeen: number;
  readonly referenceSeen: number;
  readonly lastKey: Uint8Array;
  readonly lastField: number;
  readonly lastItem: number;
  readonly cachedOtherHead: Uint8Array;
};

/** One item of field 0 or 1, by field index and item ordinal. */
export type InputSetPosition = {
  readonly fieldIndex: number;
  readonly itemIndex: number;
};

/**
 * What one step does:
 *
 *   * `seed` caches field 0's tail and consumes nothing;
 *   * `consume` takes the larger head and prepends it to the schedule;
 *   * `duplicate` ends InputSets at DuplicateInput, `first` being the item
 *     scanned earlier (pd-design §G11.6).
 *
 * `opened` is the item the step opens through the field door.
 */
export type InputSetMergeStep =
  | {
      readonly kind: "seed";
      readonly opened: InputSetPosition;
      readonly next: InputSetMergeControl;
    }
  | {
      readonly kind: "consume";
      readonly opened: InputSetPosition;
      readonly consumed: InputSetPosition;
      readonly key: Uint8Array;
      readonly next: InputSetMergeControl;
      readonly complete: boolean;
    }
  | {
      readonly kind: "duplicate";
      readonly opened: InputSetPosition;
      readonly key: Uint8Array;
      readonly first: InputSetPosition;
      readonly second: InputSetPosition;
    };

const EMPTY: Uint8Array = Buffer.alloc(0);

export const initialInputSetMergeControl = (
  fields: InputSetFields,
): InputSetMergeControl => ({
  spendCount: fields[0].length === 0 ? 0 : -1,
  referenceCount: fields[1].length === 0 ? 0 : -1,
  spendSeen: 0,
  referenceSeen: 0,
  lastKey: EMPTY,
  lastField: -1,
  lastItem: -1,
  cachedOtherHead: EMPTY,
});

/** `bytearray.compare`: byte order, an empty array below every other. */
const compareKeys = (left: Uint8Array, right: Uint8Array): number =>
  Buffer.compare(left, right);

/** The field this step opens; the control fixes it. */
export const inputSetMergePendingField = (
  control: InputSetMergeControl,
): number => {
  if (control.lastKey.length === 0)
    return control.cachedOtherHead.length === 0 ? 0 : 1;
  if (control.lastField === 0)
    return control.spendSeen < control.spendCount ? 0 : 1;
  return control.referenceSeen < control.referenceCount ? 1 : 0;
};

/**
 * The step from `control` over `fields`. Fails when the control has nothing
 * left to open (InputSets completed earlier) or field 0 is empty (InputSets
 * rejects that as `EmptyInputs` before merging).
 */
export const inputSetMergeStep = (
  control: InputSetMergeControl,
  fields: InputSetFields,
): InputSetMergeStep => {
  if (fields[0].length === 0)
    throw new Error("input-set merge: no spend inputs");
  const field = inputSetMergePendingField(control);
  const items = fields[field]!;
  const seen = field === 0 ? control.spendSeen : control.referenceSeen;
  const itemIndex = items.length - seen - 1;
  if (itemIndex < 0) throw new Error("input-set merge: field exhausted");
  const opened = { fieldIndex: field, itemIndex };
  const key = items[itemIndex]!;
  const counts =
    field === 0
      ? { spendCount: items.length, referenceCount: control.referenceCount }
      : { spendCount: control.spendCount, referenceCount: items.length };

  if (
    control.lastKey.length === 0 &&
    control.cachedOtherHead.length === 0 &&
    control.referenceCount !== 0
  ) {
    return {
      kind: "seed",
      opened,
      next: { ...control, ...counts, cachedOtherHead: key },
    };
  }

  const order = compareKeys(key, control.cachedOtherHead);
  const otherField = 1 - field;
  const otherSeen =
    otherField === 0 ? control.spendSeen : control.referenceSeen;
  const cachedPosition = {
    fieldIndex: otherField,
    itemIndex: fields[otherField]!.length - otherSeen - 1,
  };
  const consumedKey = order > 0 ? key : control.cachedOtherHead;
  const consumedPosition = order > 0 ? opened : cachedPosition;

  if (compareKeys(consumedKey, control.lastKey) === 0) {
    return {
      kind: "duplicate",
      opened,
      key: consumedKey,
      first: { fieldIndex: control.lastField, itemIndex: control.lastItem },
      second: consumedPosition,
    };
  }
  if (order === 0) {
    const [first, second] =
      field === 0 ? [opened, cachedPosition] : [cachedPosition, opened];
    return { kind: "duplicate", opened, key, first, second };
  }

  const consumedField = consumedPosition.fieldIndex;
  const spendSeen = control.spendSeen + (consumedField === 0 ? 1 : 0);
  const referenceSeen = control.referenceSeen + (consumedField === 1 ? 1 : 0);
  const consumedCount =
    consumedField === 0 ? counts.spendCount : counts.referenceCount;
  const consumedSeen = consumedField === 0 ? spendSeen : referenceSeen;
  const next: InputSetMergeControl = {
    ...counts,
    spendSeen,
    referenceSeen,
    lastKey: consumedKey,
    lastField: consumedField,
    lastItem: consumedCount - consumedSeen,
    cachedOtherHead:
      consumedSeen === consumedCount
        ? EMPTY
        : order > 0
          ? control.cachedOtherHead
          : key,
  };
  return {
    kind: "consume",
    opened,
    consumed: consumedPosition,
    key: consumedKey,
    next,
    complete:
      spendSeen === fields[0].length && referenceSeen === fields[1].length,
  };
};

/**
 * Runs the merge to its end: the first duplicate, or `null` when every item of
 * both fields was consumed. `onStep` sees each step with the control it
 * started from.
 */
export const runInputSetMerge = (
  fields: InputSetFields,
  onStep?: (control: InputSetMergeControl, step: InputSetMergeStep) => void,
): Extract<InputSetMergeStep, { kind: "duplicate" }> | null => {
  let control = initialInputSetMergeControl(fields);
  for (;;) {
    const step = inputSetMergeStep(control, fields);
    onStep?.(control, step);
    if (step.kind === "duplicate") return step;
    if (step.kind === "consume" && step.complete) return null;
    control = step.next;
  }
};
