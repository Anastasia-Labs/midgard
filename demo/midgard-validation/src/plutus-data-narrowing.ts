import { type Data, DataMap } from "./uplc-data.js";

/**
 * Narrowing helper for the place where `instanceof DataMap` hands back `any`.
 *
 * The leak is not a runtime hazard on its own — but `any` is contagious, so a
 * single unguarded `entry.fst` silently unties every downstream check on the
 * CEK encoder, scanner, and executor paths. Routing the pattern through this
 * guard keeps the `no-unsafe-*` ESLint rules usable here; see the ratcheted
 * package list in `demo/eslint.config.mjs`.
 */

/**
 * A `DataMap` whose keys and values are known to be `Data`.
 *
 * `DataMap` is generic, so a bare `value instanceof DataMap` narrows to
 * `DataMap<any, any>` and every `entry.fst` / `entry.snd` read off it is
 * `any`. Every `DataMap` reachable from a `Data` tree holds `Data` pairs by
 * construction, so this is the honest type for one; {@link isPlutusDataMap}
 * is how to obtain it.
 */
export type PlutusDataMap = DataMap<Data, Data>;

/** Whether `value` is a `DataMap`, narrowed to {@link PlutusDataMap}. */
export const isPlutusDataMap = (value: unknown): value is PlutusDataMap =>
  value instanceof DataMap;
