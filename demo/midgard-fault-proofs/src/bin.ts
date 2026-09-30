#!/usr/bin/env node

import "./bin.parsed-args.js";
import "node:fs";
import "node:url";
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "./evidence/diagnostic-evidence.js";
import "./fabricated-cli-contracts.js";
import "./inspect-contracts.js";
import "./json-file.js";
import "./legacy-submission-boundary.js";
import "./prepare-transition-trace.js";
import "./remove-fraudulent-block.js";
import "./remove-unattested-block.js";
import "./submit-da-hash-preimage-step-01.js";
import "./submit-da-hash-preimage-step-02.js";
import "./submit-fabricated-deposit-step-01.js";
import "./submit-fabricated-deposit-step-02.js";
import "./submit-fabricated-deposit-step-03.js";
import "./submit-fabricated-deposit-step-04.js";
import "./submit-fabricated-withdrawal-step-01.js";
import "./submit-fabricated-withdrawal-step-02.js";
import "./submit-fabricated-withdrawal-step-03.js";
import "./submit-fabricated-withdrawal-step-04.js";
import "./submit-init.js";
import "./submit-input-no-idx-step-01.js";
import "./submit-input-no-idx-step-02.js";
import "./submit-input-no-idx-step-03.js";
import "./submit-input-no-idx-step-04.js";
import "./submit-invalid-range-step-01.js";
import "./submit-invalid-range-step-02.js";
import "./submit-invalid-signature-step-01.js";
import "./submit-invalid-signature-step-02.js";
import "./submit-no-reference-input-step-01.js";
import "./submit-no-reference-input-step-02.js";
import "./submit-no-reference-input-step-03.js";
import "./submit-no-reference-input-step-04.js";
import "./submit-reference-input-no-idx-step-01.js";
import "./submit-reference-input-no-idx-step-02.js";
import "./submit-reference-input-no-idx-step-03.js";
import "./submit-reference-input-no-idx-step-04.js";
import "./submit-transition-trace-proof.js";
import "./submit-zero-input-step-01.js";
import "./submit-zero-input-step-02.js";
import "./validation-dispute/from-files.js";
import "./workflow/cli.js";
import "./bin.parsed-args.js";
import "./bin.parse-args.js";
import "./bin.require-validation-one-step-cli-arguments.js";
import "./bin.main.js";

import { formatUnknownError } from "@al-ft/midgard-core";

import { main } from "./bin.main.js";
import { isCliEntrypoint } from "./bin.require-validation-one-step-cli-arguments.js";

if (
  isCliEntrypoint({ moduleUrl: import.meta.url, argvPath: process.argv[1] })
) {
  main().catch((error: unknown) => {
    process.stderr.write(
      `midgard-fault-proofs: ${formatUnknownError(error)}\n`,
    );
    process.exitCode = 1;
  });
}
export { main } from "./bin.main.js";
export { parseArgs } from "./bin.parse-args.js";
export { type ParsedArgs, parseFraudCategory } from "./bin.parsed-args.js";
export {
  buildRemoveFraudulentBlockCliConfig,
  buildRemoveUnattestedBlockCliConfig,
  isCliEntrypoint,
} from "./bin.require-validation-one-step-cli-arguments.js";
