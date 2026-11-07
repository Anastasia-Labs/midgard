// fixture-path: midgard-sdk/src/fraud-proof/contracts/blueprint.ts
declare const uplc: any;
declare const code: string;
declare const params: unknown;

// The shape guard itself is the one place allowed to apply parameters.
// ok: midgard/apply-params-through-blueprint
export const applied = uplc.apply_params_to_script(params, code);
