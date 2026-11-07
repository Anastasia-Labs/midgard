import { readFileSync } from "node:fs";

import { realBlueprintPath } from "./emulator/blueprints.js";

/**
 * Reads constructor indices and field orders out of the compiled Aiken
 * blueprint's `definitions` section.
 *
 * The blueprint is the on-chain ABI as the compiler emitted it, so a test that
 * asserts an SDK encoding against these values is checking two independent
 * implementations against each other. Transcribing the same indices from an
 * Aiken source comment into a TypeScript constant is not: it only re-states
 * what a human believed at transcription time.
 *
 * Every lookup here fails closed. A missing blueprint, a missing definition or
 * a missing constructor throws rather than degrading to a skipped assertion.
 */
export type BlueprintConstructor = {
  readonly title: string;
  readonly index: number;
  /** Field titles in declared order; the order is the Plutus Data field order. */
  readonly fieldTitles: readonly string[];
};

type RawConstructor = {
  readonly title?: string;
  readonly index?: number;
  readonly fields?: readonly { readonly title?: string }[];
};

type RawBlueprint = {
  readonly definitions?: Readonly<
    Record<string, { readonly anyOf?: readonly RawConstructor[] }>
  >;
};

let cached: RawBlueprint | undefined;

const definitions = (): Readonly<
  Record<string, { readonly anyOf?: readonly RawConstructor[] }>
> => {
  cached ??= JSON.parse(
    readFileSync(realBlueprintPath, "utf8"),
  ) as RawBlueprint;
  const found = cached.definitions;
  if (found === undefined) {
    throw new Error(
      `Blueprint at ${realBlueprintPath} carries no "definitions" section; the compiled ABI is required.`,
    );
  }
  return found;
};

/**
 * All constructors of one blueprint sum type, keyed by their Aiken title.
 */
export const blueprintConstructors = (
  definitionKey: string,
): ReadonlyMap<string, BlueprintConstructor> => {
  const definition = definitions()[definitionKey];
  if (definition?.anyOf === undefined) {
    throw new Error(
      `Blueprint definition "${definitionKey}" is absent or declares no constructors.`,
    );
  }
  const entries = definition.anyOf.map((constructor): BlueprintConstructor => {
    if (constructor.title === undefined || constructor.index === undefined) {
      throw new Error(
        `Blueprint definition "${definitionKey}" carries an untitled or unindexed constructor.`,
      );
    }
    return {
      title: constructor.title,
      index: constructor.index,
      fieldTitles: (constructor.fields ?? []).map((field, position) => {
        if (field.title === undefined) {
          throw new Error(
            `Blueprint constructor "${definitionKey}.${constructor.title ?? "?"}" field ${position.toString()} has no title.`,
          );
        }
        return field.title;
      }),
    };
  });
  return new Map(entries.map((entry) => [entry.title, entry]));
};

/** One constructor, by sum-type definition key and Aiken constructor title. */
export const blueprintConstructor = (
  definitionKey: string,
  constructorTitle: string,
): BlueprintConstructor => {
  const found = blueprintConstructors(definitionKey).get(constructorTitle);
  if (found === undefined) {
    throw new Error(
      `Blueprint definition "${definitionKey}" declares no constructor "${constructorTitle}".`,
    );
  }
  return found;
};
