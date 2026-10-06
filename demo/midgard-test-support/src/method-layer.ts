/**
 * Stackable, exactly restorable layers over a real method.
 *
 * Test helpers observe real objects by wrapping one of their methods: the
 * raw-L1 recorder wraps `Emulator.prototype.submitTx` to keep the signed bytes
 * the real emulator accepted, and a fit-ledger measurement wraps the same
 * method to size every transaction. Both must see every real call, so their
 * wrappers have to compose.
 *
 * `vi.spyOn` does not compose them. Since Vitest 4, `vi.spyOn` on a method
 * that is already a mock (found on the object or anywhere up its prototype
 * chain) returns that same mock: a second wrapper's `mockImplementation`
 * replaces the first instead of stacking on it, and a wrapper that calls the
 * method it captured before spying calls itself.
 *
 * {@link layerMethod} installs a plain function, not a mock. It captures the
 * method currently visible through `target[key]` (the real method, or the
 * layer installed before it) and defines `layer(next)` as an own property of
 * `target`. Layers stack in installation order, the newest outermost, and
 * must be removed in reverse: `restore` puts back exactly the own property
 * `target` had before (or removes it, when the method was inherited), and
 * refuses when another layer still sits on top of it, so a prototype is never
 * left holding a stale wrapper. `restore` is idempotent.
 *
 * Vitest's `restoreMocks` and `vi.restoreAllMocks()` do not see these layers;
 * the caller restores each one, normally in a `finally`.
 */

type MethodKey<T> = {
  [K in keyof T]-?: T[K] extends (...args: never[]) => unknown ? K : never;
}[keyof T];

export type MethodLayer = Readonly<{ restore: () => void }>;

export const layerMethod = <T extends object, K extends MethodKey<T>>(
  target: T,
  key: K,
  layer: (next: T[K]) => T[K],
): MethodLayer => {
  const next = target[key];
  if (typeof next !== "function")
    throw new TypeError(`cannot layer ${String(key)}: it is not a method`);
  const previous = Object.getOwnPropertyDescriptor(target, key);
  const installed = layer(next);
  Object.defineProperty(target, key, {
    configurable: true,
    enumerable: previous?.enumerable ?? false,
    writable: true,
    value: installed,
  });
  let restored = false;
  return {
    restore: () => {
      if (restored) return;
      const current = Object.getOwnPropertyDescriptor(target, key);
      if (current?.value !== installed)
        throw new Error(
          `cannot restore ${String(key)}: a later layer is still installed over it; restore layers in reverse order`,
        );
      if (previous === undefined) Reflect.deleteProperty(target, key);
      else Object.defineProperty(target, key, previous);
      restored = true;
    },
  };
};
