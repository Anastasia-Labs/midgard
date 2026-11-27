import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { beforeAll, describe } from "vitest";

import { registerCommitteeCleanup } from "./committee-service.test/fixtures.js";
import { registerL1AbsorptionTests } from "./committee-service.test/l1-absorption.js";
import { registerL1FinalityGateTests } from "./committee-service.test/l1-finality-gate.js";
import { registerL1IntegrityTests } from "./committee-service.test/l1-integrity.js";
import { registerLifecycleTests } from "./committee-service.test/lifecycle.js";
import { registerPayloadsTests } from "./committee-service.test/payloads.js";
import { registerReadinessTests } from "./committee-service.test/readiness.js";
import { registerRetentionFinalityTests } from "./committee-service.test/retention-finality.js";
import { registerSigningTests } from "./committee-service.test/signing.js";

// The ledger's Ed25519 check is libsodium's, which initialises
// asynchronously; production entry points await it, and so does this file.
beforeAll(() => ledgerEd25519SignatureReady());

registerCommitteeCleanup();
describe("CommitteeService", () => {
  registerReadinessTests();
  registerSigningTests();
  registerL1IntegrityTests();
  registerL1AbsorptionTests();
  registerL1FinalityGateTests();
  registerPayloadsTests();
  registerLifecycleTests();
  registerRetentionFinalityTests();
});
