import { describe } from "vitest";

import { registerDeepDataDepthCases } from "./cbor-iterative.depth.cases.js";

// Runs in the default forks pool: a child process's main thread, whose stack
// is the smallest a depth-sensitive site sees.
describe("deep Plutus Data at the protocol maximum (main thread)", () => {
  registerDeepDataDepthCases("main");
});
