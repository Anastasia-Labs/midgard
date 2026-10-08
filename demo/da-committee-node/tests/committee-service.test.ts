import { describe } from "vitest";

import { registerCommitteeCleanup } from "./committee-service.test/fixtures.js";
import { registerL1AbsorptionTests } from "./committee-service.test/l1-absorption.js";
import { registerL1FinalityGateTests } from "./committee-service.test/l1-finality-gate.js";
import { registerL1IntegrityTests } from "./committee-service.test/l1-integrity.js";
import { registerLifecycleTests } from "./committee-service.test/lifecycle.js";
import { registerParentStateTests } from "./committee-service.test/parent-state.js";
import { registerPayloadsTests } from "./committee-service.test/payloads.js";
import { registerReadinessTests } from "./committee-service.test/readiness.js";
import { registerRetentionFinalityTests } from "./committee-service.test/retention-finality.js";
import { registerSigningTests } from "./committee-service.test/signing.js";

registerCommitteeCleanup();
describe("CommitteeService", () => {
  registerReadinessTests();
  registerSigningTests();
  registerL1IntegrityTests();
  registerL1AbsorptionTests();
  registerL1FinalityGateTests();
  registerPayloadsTests();
  registerParentStateTests();
  registerLifecycleTests();
  registerRetentionFinalityTests();
});
