import { createRequire } from "node:module";

type UplcModule = typeof import("@lucid-evolution/uplc");

let instance: UplcModule | undefined;

/**
 * The UPLC evaluator, one wasm instance per load of its package entry. A trap
 * or a stack overflow inside wasm unwinds without restoring the instance's
 * own stack pointer or releasing its borrowed state, so every later call on
 * that instance can fail. {@link discardUplcInstance} drops it after an
 * evaluation throws, and the next call loads the package entry afresh, which
 * instantiates a new module: one transaction can never leave the evaluator
 * broken for the next.
 */
export const uplcInstance = (): UplcModule => {
  if (instance !== undefined) return instance;
  const load = createRequire(import.meta.url);
  const entry = load.resolve("@lucid-evolution/uplc");
  // A cached entry is the instance another import may already have broken.
  delete load.cache[entry];
  instance = load(entry) as UplcModule;
  // Uncached, so the next load instantiates again and this instance is
  // released once nothing here holds it.
  delete load.cache[entry];
  return instance;
};

/** Drops the current instance; the next evaluation loads a fresh one. */
export const discardUplcInstance = (): void => {
  instance = undefined;
};
