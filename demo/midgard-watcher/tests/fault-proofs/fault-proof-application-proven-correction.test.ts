import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk";
import { expect, it } from "vitest";

import { unsafeCreateWatcherFaultProofApplicationForTest } from "../../src/fault-proofs/fault-proof-application.js";
import {
  admittedWatcherConfig,
  AUTHORITY,
  dependencies,
  HEADER,
  infrastructure,
  testFaultProofL1,
} from "./fault-proof-application.raw-config.js";

// A watcher that only observes never acts on L1. Asked to drive a proven
// correction's removal, it says so and returns, every pass, without reading
// its configuration or a wallet secret.

it("starts no correction removal from an application that only observes", async () => {
  const deps = dependencies();
  const application = unsafeCreateWatcherFaultProofApplicationForTest(
    {
      l1: testFaultProofL1(),
      deploymentIdentity: AUTHORITY.result,
      infrastructure: infrastructure(),
      watcherConfig: admittedWatcherConfig(),
    },
    deps,
    {},
  );
  try {
    await expect(
      application.driveProvenCorrection({
        correction: {
          targetHeaderHash: HEADER,
          fraudProofAssetName: `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleSpend}${HEADER}`,
        },
        reservedOutRefs: async () => [],
      }),
    ).resolves.toEqual({ kind: "observing" });
    expect(deps.readText).not.toHaveBeenCalled();
    expect(deps.resolveSigner).not.toHaveBeenCalled();
    expect(deps.makeLucid).not.toHaveBeenCalled();
  } finally {
    await application.close();
  }
});
