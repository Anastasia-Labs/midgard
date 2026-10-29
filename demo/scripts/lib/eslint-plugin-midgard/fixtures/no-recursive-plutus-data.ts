// fixture-path: midgard-validation/src/example-data-reader.ts
// ruleid: midgard/no-recursive-plutus-data
import { decode } from "cborg";
// ruleid: midgard/no-recursive-plutus-data
import { encodedLength } from "cborg/length";
import { Data as LucidData } from "@lucid-evolution/lucid";
// ok: midgard/no-recursive-plutus-data
import { type Data as LucidValue } from "@lucid-evolution/lucid";

declare const hex: string;
declare const value: LucidValue;

// ruleid: midgard/no-recursive-plutus-data
export const decoded = LucidData.from(hex);

// ruleid: midgard/no-recursive-plutus-data
export const encoded = LucidData.to(value as never);

// ok: midgard/no-recursive-plutus-data
export const schema = LucidData.Integer();

export const unused = [decode, encodedLength];
