import {
  blueprintDefinitionName,
  type BlueprintRef,
  type BlueprintSchema,
} from "./abi.js";

/** One place a TypeScript schema does not refine its blueprint definition. */
export type BlueprintMismatch = {
  /** `$` is the definition; `.Title.field`, `[]`, `[i]`, `{key}`, `{value}` descend. */
  readonly path: string;
  readonly message: string;
};

/**
 * A TypeScript Data schema in the shape `@lucid-evolution/lucid`'s `Data.*`
 * builders produce. Only the keys the comparison reads are named.
 */
type TsSchema = {
  readonly $ref?: unknown;
  readonly title?: unknown;
  readonly dataType?: unknown;
  readonly anyOf?: readonly TsSchema[];
  readonly index?: unknown;
  readonly hasConstr?: unknown;
  readonly fields?: readonly TsSchema[];
  readonly items?: TsSchema | readonly TsSchema[];
  readonly keys?: TsSchema;
  readonly values?: TsSchema;
};

/** `prevUtxosRoot`, `prev_utxos_root` and `PrevUTXOsRoot` all read `prev_utxos_root`. */
const snake = (title: string): string =>
  title
    .replace(/([A-Z]+)([A-Z][a-z])/gu, "$1_$2")
    .replace(/([a-z0-9])([A-Z])/gu, "$1_$2")
    .toLowerCase();

// Aiken's `Data` and every opaque alias of it (`cardano/transaction/Redeemer`)
// carry this description and nothing else to compare.
const isOpaqueData = (schema: BlueprintSchema): boolean =>
  schema.dataType === undefined &&
  schema.anyOf === undefined &&
  schema.description === "Any Plutus data.";

const tsKind = (schema: TsSchema): string => {
  if (schema.$ref !== undefined) {
    throw new Error("A TypeScript schema must be inline; it carries a $ref");
  }
  if (Array.isArray(schema.anyOf)) {
    return schema.anyOf.length === 1 && schema.anyOf[0]?.hasConstr === false
      ? "record encoded as a list"
      : "constructors";
  }
  switch (schema.dataType) {
    case "integer":
    case "bytes":
    case "map":
      return schema.dataType;
    case "list":
      return Array.isArray(schema.items) ? "tuple" : "list";
    case undefined:
      return "any Data";
    default:
      throw new Error(
        `Unsupported TypeScript schema dataType ${JSON.stringify(schema.dataType)}`,
      );
  }
};

const one = (message: string): BlueprintMismatch[] => [{ path: "", message }];

const under = (
  segment: string,
  mismatches: readonly BlueprintMismatch[],
): BlueprintMismatch[] =>
  mismatches.map(({ path, message }) => ({
    path: `${segment}${path}`,
    message,
  }));

const asSchema = (value: unknown, label: string): TsSchema => {
  if (typeof value !== "object" || value === null) {
    throw new Error(`${label} must be a schema object`);
  }
  return value as TsSchema;
};

/**
 * Lists every place the TypeScript schema `ts` fails to refine the blueprint
 * definition `ref`: an empty list means every value `ts` encodes has the
 * shape the definition declares, constructor fields carry the same
 * snake-normalised titles in the same order, and constructor indices agree.
 *
 * TypeScript may be narrower than the blueprint (a fixed-width `Bytes`, or any
 * schema where Aiken declares opaque `Data`), never wider. Recursive
 * definitions are compared coinductively: each (TypeScript node, definition)
 * pair is compared once, and a pair met again while it is still being
 * compared is assumed to refine.
 */
export const refines = (
  ts: unknown,
  ref: BlueprintRef,
): readonly BlueprintMismatch[] => {
  const settled = new Map<TsSchema, Map<string, BlueprintMismatch[]>>();
  const inProgress = new Map<TsSchema, Set<string>>();

  const viaDefinition = (node: TsSchema, name: string): BlueprintMismatch[] => {
    const known = settled.get(node)?.get(name);
    if (known !== undefined) return known;
    const active = inProgress.get(node) ?? new Set<string>();
    if (active.has(name)) return [];
    inProgress.set(node, active.add(name));
    const result = compare(node, ref.abi.ref(name).schema);
    active.delete(name);
    const byName = settled.get(node) ?? new Map<string, BlueprintMismatch[]>();
    settled.set(node, byName.set(name, result));
    return result;
  };

  const compareConstructors = (
    node: TsSchema,
    variants: readonly BlueprintSchema[],
  ): BlueprintMismatch[] => {
    const tsVariants = node.anyOf ?? [];
    const mismatches =
      tsVariants.length === variants.length
        ? []
        : one(
            `blueprint declares ${variants.length.toString()} constructor(s), TypeScript ${tsVariants.length.toString()}`,
          );
    for (const variant of variants) {
      if (
        variant.dataType !== "constructor" ||
        !Number.isSafeInteger(variant.index)
      ) {
        throw new Error("Blueprint constructor must be an indexed constructor");
      }
      const index = variant.index!;
      const label = `.${variant.title ?? `#${index.toString()}`}`;
      const tsVariant = tsVariants.find(
        (candidate) => candidate.index === index,
      );
      if (tsVariant === undefined) {
        mismatches.push(
          ...under(
            label,
            one(`TypeScript has no constructor ${index.toString()}`),
          ),
        );
        continue;
      }
      const fields = variant.fields ?? [];
      const tsFields = tsVariant.fields ?? [];
      if (fields.length !== tsFields.length) {
        mismatches.push(
          ...under(
            label,
            one(
              `blueprint declares ${fields.length.toString()} field(s), TypeScript ${tsFields.length.toString()}`,
            ),
          ),
        );
        continue;
      }
      fields.forEach((field, position) => {
        const tsField = asSchema(tsFields[position], "TypeScript field");
        const segment = `${label}.${field.title ?? position.toString()}`;
        if (field.title !== undefined) {
          if (typeof tsField.title !== "string") {
            mismatches.push(
              ...under(segment, one("TypeScript field is untitled")),
            );
          } else if (snake(tsField.title) !== snake(field.title)) {
            mismatches.push(
              ...under(
                segment,
                one(
                  `field ${position.toString()} is "${field.title}" in the blueprint, "${tsField.title}" in TypeScript`,
                ),
              ),
            );
          }
        }
        mismatches.push(...under(segment, compare(tsField, field)));
      });
    }
    return mismatches;
  };

  const compareTuple = (
    node: TsSchema,
    kind: string,
    items: readonly BlueprintSchema[],
  ): BlueprintMismatch[] => {
    const tsItems =
      kind === "tuple"
        ? (node.items as readonly TsSchema[])
        : kind === "record encoded as a list"
          ? (node.anyOf?.[0]?.fields ?? [])
          : undefined;
    if (tsItems === undefined) {
      return one(`blueprint declares a tuple, TypeScript ${kind}`);
    }
    if (tsItems.length !== items.length) {
      return one(
        `blueprint declares a ${items.length.toString()}-tuple, TypeScript ${tsItems.length.toString()} items`,
      );
    }
    return items.flatMap((item, position) =>
      under(
        `[${position.toString()}]`,
        compare(asSchema(tsItems[position], "TypeScript tuple item"), item),
      ),
    );
  };

  const compare = (
    node: TsSchema,
    schema: BlueprintSchema,
  ): BlueprintMismatch[] => {
    if (schema.$ref !== undefined) {
      return viaDefinition(node, blueprintDefinitionName(schema.$ref));
    }
    if (isOpaqueData(schema)) return [];
    const kind = tsKind(node);
    if (Array.isArray(schema.anyOf)) {
      return kind === "constructors"
        ? compareConstructors(node, schema.anyOf)
        : one(`blueprint declares constructors, TypeScript ${kind}`);
    }
    switch (schema.dataType) {
      case "integer":
      case "bytes":
        return kind === schema.dataType
          ? []
          : one(`blueprint declares ${schema.dataType}, TypeScript ${kind}`);
      case "constructor":
        return kind === "constructors"
          ? compareConstructors(node, [schema])
          : one(`blueprint declares a constructor, TypeScript ${kind}`);
      case "list":
        if (Array.isArray(schema.items)) {
          return compareTuple(node, kind, schema.items);
        }
        if (schema.items === undefined) {
          throw new Error("Blueprint list schema must name its items");
        }
        return kind === "list"
          ? under(
              "[]",
              compare(
                asSchema(node.items, "TypeScript list items"),
                schema.items as BlueprintSchema,
              ),
            )
          : one(`blueprint declares a list, TypeScript ${kind}`);
      case "map":
        if (schema.keys === undefined || schema.values === undefined) {
          throw new Error("Blueprint map schema must name its keys and values");
        }
        return kind === "map"
          ? [
              ...under(
                "{key}",
                compare(
                  asSchema(node.keys, "TypeScript map keys"),
                  schema.keys,
                ),
              ),
              ...under(
                "{value}",
                compare(
                  asSchema(node.values, "TypeScript map values"),
                  schema.values,
                ),
              ),
            ]
          : one(`blueprint declares a map, TypeScript ${kind}`);
      case undefined:
      default:
        throw new Error(
          `Unsupported blueprint schema ${JSON.stringify(schema).slice(0, 120)}`,
        );
    }
  };

  return under("$", viaDefinition(asSchema(ts, "TypeScript schema"), ref.name));
};
