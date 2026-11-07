import { Data, type TSchema } from "@lucid-evolution/lucid";

/**
 * The single sanctioned home for Lucid's schema-to-type bridge.
 *
 * `@lucid-evolution/lucid` declares `Data.to<T>(data: Exact<T>, type?: T)` and
 * `Data.from<T>(raw, type?: T)`: the second parameter is typed as the *static
 * type* being encoded, while the value handed over at runtime is the TypeBox
 * schema that describes it. Schema and static type are the same object at
 * runtime and completely unrelated at the type level, so every schema-backed
 * codec has to cross that gap exactly once per schema.
 *
 * Those crossings used to be written inline as `FooSchema as unknown as Foo`,
 * several hundred of them, which drowned out the raw `as unknown as` casts that
 * do encode a real assumption about a value's shape — test doubles, structural
 * widenings, `globalThis` pokes. The idiom lives here now, so a grep for the
 * raw cast is a signal worth reading again.
 *
 * A raw `as unknown as` outside this module is therefore a claim that needs
 * justifying, not boilerplate.
 */

/** The loosely-typed `type` parameter of Lucid's `Data.to` / `Data.from`. */
export type LucidDataSchema = Parameters<typeof Data.to>[1];

/** The loosely-typed `data` parameter of Lucid's `Data.to`. */
export type LucidDataValue = Parameters<typeof Data.to>[0];

/**
 * Views a TypeBox schema as the static type it describes, which is the shape
 * Lucid's `Data.to`/`Data.from` type parameters expect.
 *
 * The canonical use is the companion constant a schema-backed type publishes
 * alongside itself:
 *
 * ```ts
 * export const HeaderSchema = Data.Object({ ... });
 * export type Header = Data.Static<typeof HeaderSchema>;
 * export const Header = asDataType<Header>(HeaderSchema);
 * ```
 */
export const asDataType = <T>(schema: TSchema): T => schema as unknown as T;

/**
 * Views a schema as Lucid's `Data.to`/`Data.from` `type` argument.
 *
 * Accepts `unknown` because both ends of the bridge are passed here in
 * practice: a bare TypeBox schema (`Data.Array(Data.Any())`) and a schema that
 * has already been viewed as its static type by {@link asDataType}
 * (`SDK.Proof`). No single type covers both, which is precisely the gap this
 * module exists to isolate.
 */
export const asLucidSchema = (schema: unknown): LucidDataSchema =>
  schema as unknown as LucidDataSchema;

/** Views a schema-typed value as Lucid's `Data.to` `data` argument. */
export const asLucidDataValue = (value: unknown): LucidDataValue =>
  value as unknown as LucidDataValue;
