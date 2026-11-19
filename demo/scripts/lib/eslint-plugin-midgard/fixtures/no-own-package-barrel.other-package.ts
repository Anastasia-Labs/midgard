// fixture-path: midgard-sdk/tests/narrowed-imports.test.ts
// A package that has not been narrowed is not checked.
// ok: midgard/no-own-package-barrel
import { makeSdk } from "../src/index.js";

export const sdk = makeSdk;
