import { afterEach } from "vitest";

import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.sources-for.js";

afterEach(cleanupFundingRecoveryFixtures);
