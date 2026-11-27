import { describe } from "vitest";

import { registerNoRecursionDepthCases } from "./plutus-data-no-recursion.cases.js";

// Runs in the default forks pool: a child process's main thread, whose stack
// is the smallest a depth-sensitive site sees.
describe("validation Plutus Data at the protocol maximum depth (main thread)", () => {
  registerNoRecursionDepthCases("main");
});
