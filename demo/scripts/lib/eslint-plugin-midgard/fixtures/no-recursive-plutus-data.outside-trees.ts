// fixture-path: midgard-validation/tests/example-oracle.test.ts
import { Data as LucidData } from "@lucid-evolution/lucid";
// Tests may use the recursive functions as independent oracles.
// ok: midgard/no-recursive-plutus-data
import { decode } from "cborg";

declare const hex: string;

// ok: midgard/no-recursive-plutus-data
export const decoded = [LucidData.from(hex), decode(new Uint8Array())];
