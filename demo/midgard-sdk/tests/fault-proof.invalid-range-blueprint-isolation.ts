import { Effect } from "effect";
import { expect, it } from "vitest";

import {
  buildInvalidRangeFaultProofContracts,
  FAULT_PROOF_SHARED_TITLES,
  INVALID_RANGE_FAULT_PROOF_TITLES,
} from "../src/index.js";
import {
  filterBlueprint,
  h28b,
  h28c,
  loadBlueprint,
} from "./fault-proof.publication-tx-overhead-bytes.js";

export const registerInvalidRangeBlueprintIsolationTest = () => {
  it("builds invalid-range without requiring unrelated category validators", async () => {
    const blueprint = filterBlueprint(loadBlueprint(), [
      ...Object.values(FAULT_PROOF_SHARED_TITLES),
      ...Object.values(INVALID_RANGE_FAULT_PROOF_TITLES),
    ]);

    const contracts = await Effect.runPromise(
      buildInvalidRangeFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.invalidRange.firstStep).toBe(
      contracts.invalidRange.steps[0],
    );
    expect(contracts.invalidRange.steps).toHaveLength(2);
  });
};
