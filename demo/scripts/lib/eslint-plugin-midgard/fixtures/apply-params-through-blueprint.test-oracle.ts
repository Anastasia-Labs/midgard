// fixture-path: midgard-fault-proofs/tests/support/expected-parameter-application.ts
declare const uplc: any;
declare const code: string;
declare const params: unknown;

// A test package's independent oracle recomputes the applied script outside
// the shape guard on purpose; nothing deploys from it.
// ok: midgard/apply-params-through-blueprint
export const expected = uplc.apply_params_to_script(params, code);
