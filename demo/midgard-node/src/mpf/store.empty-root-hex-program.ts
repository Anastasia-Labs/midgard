import "./store.midgard-mpf.js";

import { Effect } from "effect";

import { MpfError } from "./errors.js";
import { MPF_EMPTY_ROOT_HEX } from "./store-primitives.js";

export const emptyRootHexProgram: Effect.Effect<string, MpfError> =
  Effect.succeed(MPF_EMPTY_ROOT_HEX);
