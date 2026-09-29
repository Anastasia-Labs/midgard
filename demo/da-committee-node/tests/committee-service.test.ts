import { describe } from "vitest";

import { registerCommitteeCleanup } from "./committee-service.test/fixtures.js";
import { registerL1FinalityTests } from "./committee-service.test/l1-finality.js";
import { registerL1IntegrityTests } from "./committee-service.test/l1-integrity.js";
import { registerLifecycleTests } from "./committee-service.test/lifecycle.js";
import { registerPayloadsTests } from "./committee-service.test/payloads.js";
import { registerReadinessTests } from "./committee-service.test/readiness.js";
import { registerRollbackTests } from "./committee-service.test/rollback.js";
import { registerSigningTests } from "./committee-service.test/signing.js";

registerCommitteeCleanup();
describe("CommitteeService", () => {
  registerReadinessTests();
  registerSigningTests();
  registerL1IntegrityTests();
  registerL1FinalityTests();
  registerRollbackTests();
  registerPayloadsTests();
  registerLifecycleTests();
});
