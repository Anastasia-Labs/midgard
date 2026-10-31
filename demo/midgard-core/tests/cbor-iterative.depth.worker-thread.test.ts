import { describe } from "vitest";

import { registerDeepDataDepthCases } from "./cbor-iterative.depth.cases.js";

// Routed to the threads pool by vitest.config.ts, so these cases run on a
// worker thread, as the node's validation worker does.
describe("deep Plutus Data at the protocol maximum (worker thread)", () => {
  registerDeepDataDepthCases("worker");
});
