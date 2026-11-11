/**
 * The row shape of the readiness catalogue (`readiness-catalogue.ts`): one
 * code `/readyz` can name, where it appears, and the format of each
 * `:`-separated parameter its producer puts after it.
 */

/**
 * Where a code appears: a line of `reasons`, of `details`, of either, or
 * only as the `reason` field of a nested report (a commit plan, a validation
 * worker, the state queue, the L1 transport), never as a line of its own.
 */
export type ReadinessCodeKind = "reason" | "detail" | "both" | "field";

/** A parameter a public profile shows when the value matches `format`. */
export type BoundedParam = Readonly<{
  kind: "bounded";
  format: RegExp;
  optional: boolean;
  /** Shown in place of a value that does not match, when set. */
  otherwise?: string;
}>;

/** Free text, always the last parameter: it may hold `:` itself, and a
 * public profile shows the code alone. */
export const TEXT = { kind: "text" } as const;

export type ReadinessParam = BoundedParam | typeof TEXT;

type Params =
  | readonly BoundedParam[]
  | readonly [...BoundedParam[], typeof TEXT];

export type ReadinessRow = Readonly<{
  code: string;
  kind: ReadinessCodeKind;
  params: readonly ReadinessParam[];
}>;

const row =
  (kind: ReadinessCodeKind) =>
  (code: string, ...params: Params): ReadinessRow => ({ code, kind, params });

export const reason = row("reason");
export const detail = row("detail");
export const both = row("both");
export const field = (code: string): ReadinessRow => row("field")(code);

/** A parameter whose whole value matches the regular expression `source`. */
export const format = (source: string): BoundedParam => ({
  kind: "bounded",
  format: new RegExp(`^(?:${source})$`, "u"),
  optional: false,
});

/** A decimal count, a duration or an age in ms. */
export const COUNT = format("[0-9]+");

/** Lowercase hex of `min` to `max` characters. */
export const hex = (min: number, max = min): BoundedParam =>
  format(`[0-9a-f]{${min.toString()},${max.toString()}}`);

/** One of a fixed vocabulary of names, each matched literally. */
export const oneOf = (...names: readonly string[]): BoundedParam =>
  format(
    names
      .map((name) => name.replace(/[.*+?^${}()|[\]\\/]/gu, "\\$&"))
      .join("|"),
  );

/** A trailing parameter the producer may leave out. */
export const optional = (param: BoundedParam): BoundedParam => ({
  ...param,
  optional: true,
});

/** A parameter shown as `value` when it falls outside its format. */
export const otherwise = (
  param: BoundedParam,
  value: string,
): BoundedParam => ({
  ...param,
  otherwise: value,
});

/** Every member of the string union `C`; the record's keys make the
 * compiler hold the list complete. */
export const members = <C extends string>(
  record: Readonly<Record<C, null>>,
): readonly C[] => Object.keys(record) as C[];
