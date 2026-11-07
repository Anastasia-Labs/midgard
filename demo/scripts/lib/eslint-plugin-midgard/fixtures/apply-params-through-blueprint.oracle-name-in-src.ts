// fixture-path: midgard-sdk/src/expected-parameter-application.ts
declare const uplc: any;
declare const code: string;
declare const params: unknown;

// The oracle name buys nothing outside a tests/ tree.
// ruleid: midgard/apply-params-through-blueprint
export const applied = uplc.apply_params_to_script(params, code);
