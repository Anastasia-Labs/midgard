import { describe } from "vitest";

import { registerNoRecursionDepthCases } from "./plutus-data-no-recursion.cases.js";

// Routed to the threads pool by vitest.config.ts, so these cases run on a
// worker thread, as the node's validation worker does.
describe("validation Plutus Data at the protocol maximum depth (worker thread)", () => {
  registerNoRecursionDepthCases("worker");
});
