import { expect } from "vitest";

/** `expect.stringMatching` with an honest type (vitest declares it `any`). */
export const matching = (pattern: RegExp): string =>
  expect.stringMatching(pattern) as string;
