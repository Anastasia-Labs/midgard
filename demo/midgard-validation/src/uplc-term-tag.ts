/**
 * The UPLC term tags, as Cardano's Flat encoding numbers them (4 bits).
 *
 * Derived from @harmoniclabs/uplc 2.0.7 (Copyright Harmonic Laboratories,
 * Apache License 2.0, https://github.com/HarmonicLabs/uplc). Changes: a frozen
 * object rather than an enum.
 */
export const UPLCTermTag = Object.freeze({
  Var: 0,
  Delay: 1,
  Lambda: 2,
  Application: 3,
  Const: 4,
  Force: 5,
  Error: 6,
  Builtin: 7,
  Constr: 8,
  Case: 9,
} as const);
export type UPLCTermTag = (typeof UPLCTermTag)[keyof typeof UPLCTermTag];
