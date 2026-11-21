// fixture-path: midgard-fault-proofs/tests/example-traversal-emulator.test.ts
import { applyParamsToScript } from "@lucid-evolution/lucid";

declare const code: string;
declare const params: unknown[];

// A test that deploys a raw application to the emulator is still reported:
// with the wrong arity the script it exercises always succeeds.
// ruleid: midgard/apply-params-through-blueprint
export const deployed = applyParamsToScript(code, params);
