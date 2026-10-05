// fixture-path: midgard-node/src/example-handler.ts
import { Data } from "@lucid-evolution/lucid";
// ruleid: midgard/no-recursive-plutus-data
import { dataToCbor } from "@harmoniclabs/plutus-data";

declare const value: never;

// Lucid Data calls are only checked in midgard-core and midgard-validation.
// ok: midgard/no-recursive-plutus-data
export const encoded = [Data.to(value), dataToCbor];
