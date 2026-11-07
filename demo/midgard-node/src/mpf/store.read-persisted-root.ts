import { Effect } from "effect";
import { Level } from "level";

import { MpfError } from "./errors.js";
import {
  JSON_LEVEL_ENCODING_OPTS,
  parseStoredRootHex,
  ROOT_KEY,
} from "./store-primitives.js";
import { type MpfStoredValue } from "./types.js";

export const readPersistedRoot = (
  level: Level<string, MpfStoredValue>,
): Effect.Effect<Buffer, MpfError> =>
  Effect.tryPromise({
    try: async () => {
      const rootHex = await level.get(ROOT_KEY, JSON_LEVEL_ENCODING_OPTS);
      return parseStoredRootHex(rootHex);
    },
    catch: (e) => MpfError.rootNotSet("persisted", e),
  });
