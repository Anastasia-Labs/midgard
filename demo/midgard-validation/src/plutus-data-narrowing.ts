import { type Data, DataMap } from "@harmoniclabs/plutus-data";

/**
 * Narrowing helper for the place where `@harmoniclabs/plutus-data`'s
 * declarations hand back `any`.
 *
 * The leak is in the library, not in this package, and is not a runtime
 * hazard on its own — but `any` is contagious, so a single unguarded
 * `entry.fst` silently unties every downstream check on the CEK encoder,
 * scanner, and executor paths. Routing the pattern through this guard
 * keeps the `no-unsafe-*` ESLint rules usable here; see the ratcheted package
 * list in `demo/eslint.config.mjs`.
 */

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
