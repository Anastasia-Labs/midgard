// fixture-path: midgard-validation/src/plutus-data-iterative.pair.ts
import { type Data, DataPair } from "@harmoniclabs/plutus-data";

// The pair factory is the one place a DataPair is constructed.
// ok: midgard/no-recursive-plutus-data
export const midgardDataPair = (fst: Data, snd: Data) => new DataPair(fst, snd);
