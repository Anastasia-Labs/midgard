// fixture-path: midgard-node/src/example-contracts.ts
import { applyParamsToScript } from "@lucid-evolution/lucid";
// The alias is reported at its import, because its calls are not.
// ruleid: midgard/apply-params-through-blueprint
import { applyParamsToScript as apply } from "@lucid-evolution/lucid";

declare const code: string;
declare const param: unknown;
declare const uplc: any;
declare const lucidModule: any;
declare const blueprint: unknown;
declare const applyBlueprintParams: (...args: unknown[]) => string;

// ruleid: midgard/apply-params-through-blueprint
export const raw = applyParamsToScript(code, [param]);

// ruleid: midgard/apply-params-through-blueprint
export const wasm = uplc.apply_params_to_script(param, code);

// ruleid: midgard/apply-params-through-blueprint
const { applyParamsToScript: hidden } = lucidModule;

// ok: midgard/apply-params-through-blueprint
export const guarded = applyBlueprintParams(blueprint, "title", [param]);

export { apply, hidden };
