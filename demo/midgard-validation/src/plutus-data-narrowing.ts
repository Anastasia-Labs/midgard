import { type Data, DataMap } from "@harmoniclabs/plutus-data";

/**
 * Narrowing helpers for the two places where `@harmoniclabs/plutus-data`'s
 * declarations hand back `any`.
 *
 * Both leaks are in the library, not in this package, and neither is a runtime
 * hazard on its own — but `any` is contagious, so a single unguarded
 * `entry.fst` silently unties every downstream check on the CEK encoder,
 * scanner, and executor paths. Routing the two patterns through these guards
 * keeps the `no-unsafe-*` ESLint rules usable here; see the ratcheted package
 * list in `demo/eslint.config.mjs`.
 */

/**
 * The structural shape of `@harmoniclabs/bytestring`'s `ByteString`, which is a
 * transitive dependency this package cannot name in an import.
 *
 * `toBuffer` is declared as returning `unknown` rather than `Uint8Array` on
 * purpose: the callers here receive untrusted values, and the alternative — the
 * `typeof value.toBuffer === "function"` probe each of them used to inline —
 * narrows to `Function`, whose call signature returns `any`. Callers must still
 * check the result with `instanceof Uint8Array`.
 */
export type ByteStringLike = { readonly toBuffer: () => unknown };

/** Whether `value` exposes `ByteString`'s `toBuffer` accessor. */
export const isByteStringLike = (value: unknown): value is ByteStringLike =>
  typeof value === "object" &&
  value !== null &&
  "toBuffer" in value &&
  typeof value.toBuffer === "function";

/**
 * A `DataMap` whose keys and values are known to be `Data`.
 *
 * The library's own `Data` union is spelled `... | DataMap<any, any> | ...`, so
 * a bare `value instanceof DataMap` narrows to `DataMap<any, any>` and every
 * `entry.fst` / `entry.snd` read off it is `any`. Every `DataMap` reachable
 * from a `Data` tree holds `Data` pairs by construction, so this is the honest
 * type for one; {@link isPlutusDataMap} is how to obtain it.
 */
export type PlutusDataMap = DataMap<Data, Data>;

/** Whether `value` is a `DataMap`, narrowed to {@link PlutusDataMap}. */
export const isPlutusDataMap = (value: unknown): value is PlutusDataMap =>
  value instanceof DataMap;
