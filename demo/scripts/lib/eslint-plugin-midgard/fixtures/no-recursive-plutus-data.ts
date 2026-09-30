// fixture-path: midgard-validation/src/example-data-reader.ts
import {
  // ruleid: midgard/no-recursive-plutus-data
  dataFromCbor,
  // ok: midgard/no-recursive-plutus-data
  DataI,
  DataPair,
  // ruleid: midgard/no-recursive-plutus-data
  dataToCbor as encode,
  type Data,
} from "@harmoniclabs/plutus-data";
// ok: midgard/no-recursive-plutus-data
import { type eqData } from "@harmoniclabs/plutus-data";
import {
  // ok: midgard/no-recursive-plutus-data
  BnCEK,
  // ruleid: midgard/no-recursive-plutus-data
  Machine,
} from "@harmoniclabs/plutus-machine";
// ruleid: midgard/no-recursive-plutus-data
import { Cbor } from "@harmoniclabs/cbor";
// ruleid: midgard/no-recursive-plutus-data
import { decode } from "cborg";
import { Data as LucidData } from "@lucid-evolution/lucid";

declare const left: Data;
declare const right: Data;
declare const hex: string;

// ruleid: midgard/no-recursive-plutus-data
export const pair = new DataPair(left, right);

// ruleid: midgard/no-recursive-plutus-data
export const decoded = LucidData.from(hex);

// ruleid: midgard/no-recursive-plutus-data
export const encoded = LucidData.to(new DataI(1n) as never);

// ok: midgard/no-recursive-plutus-data
export const schema = LucidData.Integer();

export const unused = [dataFromCbor, encode, BnCEK, Machine, Cbor, decode];
export type Unused = typeof eqData;
