import { beforeEach } from "vitest";

import { registerMintAuthorizationInstalledCases } from "./support/mint-authorization-installed-lifecycle-cases.js";
import { pinStepTranscript } from "./support/step-transcript.js";

beforeEach(pinStepTranscript);
registerMintAuthorizationInstalledCases("rest");
